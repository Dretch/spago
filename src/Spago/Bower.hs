module Spago.Bower
  ( path
  , generateBowerJson
  , runBowerInstall
  , verifyPackageSet
  ) where

import Spago.Prelude

import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Encode.Pretty   as Pretty
import qualified Data.ByteString.Lazy       as ByteString
import           Data.Either.Combinators    (mapLeft)
import qualified Data.HashMap.Strict        as HashMap
import qualified Data.Map                   as Map
import qualified Data.SemVer                as SemVer
import           Data.String                (IsString)
import qualified Data.Text                  as Text
import           Data.Text.Lazy             (fromStrict)
import           Data.Text.Lazy.Encoding    (encodeUtf8)
import qualified Distribution.System        as System
import           Distribution.System        (OS (..))
import qualified System.Console.Pretty      as Pretty
import qualified Turtle
import           Web.Bower.PackageMeta      (PackageMeta (..))
import qualified Web.Bower.PackageMeta      as Bower

import           Spago.Config               (Config (..), PublishConfig (..))
import qualified Spago.Config               as Config
import qualified Spago.Git                  as Git
import qualified Spago.Packages             as Packages
import qualified Spago.Templates            as Templates

import Spago.Types


path :: IsString t => t
path = "bower.json"


runBower :: Spago m => [Text] -> m (ExitCode, Text, Text)
runBower args = do
  -- workaround windows issue: https://github.com/haskell/process/issues/140
  cmd <- case System.buildOS of
    Windows -> do
      let bowers = Turtle.inproc "where" ["bower.cmd"] empty
      Turtle.lineToText <$> Turtle.single (Turtle.limit 1 bowers)
    _ ->
      pure "bower"
  Turtle.procStrictWithErr cmd args empty


generateBowerJson :: Spago m => m ByteString.ByteString
generateBowerJson = do
  echo $ "Generating a new Bower config using the package set versions.."
  config@Config{..} <- Config.ensureConfig
  PublishConfig{..} <- throws publishConfig

  bowerName <- mkPackageName name
  bowerDependencies <- mkDependencies config
  template <- templateBowerJson

  let bowerLicense = [publishLicense]
      bowerRepository = Just $ Bower.Repository publishRepository "git"
      bowerPkg = template { bowerLicense, bowerRepository, bowerName, bowerDependencies }
      prettyConfig = Pretty.defConfig
        { Pretty.confCompare = Pretty.keyOrder ["name", "license", "repository", "ignore", "dependencies"] <> compare
        , Pretty.confTrailingNewline = True
        }
      bowerJson = Pretty.encodePretty' prettyConfig bowerPkg

  ignored <- Git.isIgnored path
  when ignored $ do
    die $ path <> " is being ignored by git - change this before continuing"

  echo $ "Generated a valid Bower config using the package set"
  pure bowerJson


runBowerInstall :: Spago m => m ()
runBowerInstall = do
  echo "Running `bower install` so `pulp publish` can read resolved versions from it"
  shell "bower install --silent" empty >>= \case
    ExitSuccess   -> pure ()
    ExitFailure _ -> die "Failed to run `bower install` on your package"


templateBowerJson :: Spago m => m Bower.PackageMeta
templateBowerJson = do
  case Aeson.decodeStrict Templates.bowerJson of
    Just t  ->
      pure t
    Nothing ->
      die "Invalid bower.json template (this is a Spago bug)"


mkPackageName :: Spago m => Text -> m Bower.PackageName
mkPackageName spagoName = do
  let psName = "purescript-" <> spagoName
  case Bower.mkPackageName psName of
    Left err ->
      die $ psName <> " is not a valid Bower package name: " <> Bower.showPackageNameError err
    Right name ->
      pure name


-- | If the given version exists in bower, return a shorthand bower
-- | version, otherwise return a URL#version style bower version.
mkBowerVersion :: Spago m => Bower.PackageName -> Text -> Repo -> m Bower.VersionRange
mkBowerVersion packageName version (Repo repo) = do

  let args = ["info", "--json", Bower.runPackageName packageName <> "#" <> version]
  (code, stdout, stderr) <- runBower args

  when (code /= ExitSuccess) $ do
    die $ "Failed to run: `bower " <> Text.intercalate " " args <> "`\n" <> stderr

  info <- case Aeson.decode $ encodeUtf8 $ fromStrict stdout of
    Just (Object obj) -> pure obj
    _ -> die $ "Unable to decode output from `bower " <> Text.intercalate " " args <> "`: " <> stdout

  if HashMap.member "version" info
    then pure $ Bower.VersionRange $ "^" <> version
    else pure $ Bower.VersionRange $ repo <> "#" <> version


mkDependencies :: Spago m => Config -> m [(Bower.PackageName, Bower.VersionRange)]
mkDependencies config = do
  deps <- Packages.getDirectDeps config

  jobs <- getJobs

  withTaskGroup' jobs $ \taskGroup ->
    mapTasks' taskGroup $ mkDependency <$> deps

  where
    mkDependency :: Spago m => (PackageName, Package) -> m (Bower.PackageName, Bower.VersionRange)
    mkDependency (PackageName{..}, Package{..}) =
      case location of
        Local localPath ->
          die $ "Unable to create Bower version for local repo: " <> localPath
        Remote{..} -> do
          bowerName <- mkPackageName packageName
          bowerVersion <- mkBowerVersion bowerName version repo
          pure (bowerName, bowerVersion)

    getJobs = case System.buildOS of
      -- Windows sucks so lets make it slow for them!
      -- (just kidding, its a bug: https://github.com/bower/spec/issues/79)
      Windows -> pure 1
      _       -> asks globalJobs


verifyPackageSet :: Spago m => m ()
verifyPackageSet = do
  echo "Verifying bower.json version range compatibility..."

  Config{packageSet = packageSet@PackageSet{..}} <- Config.ensureConfig

  for_ (Map.assocs packagesDB) $ \(PackageName{..}, Package{..}) -> do
    echo $ "Verifying " <> packageName

    handleAny (\e -> echo $ Pretty.color Pretty.Red "Error calling bower" <> " " <> Text.pack (show e)) $ do
      
      PackageMeta{bowerDependencies} <- runBowerInfo packageName location
      for_ bowerDependencies $ \(name, versionRange) -> do

        let checked = case checkBowerVersion packageSet name versionRange of
              Left msg -> Pretty.color Pretty.Red "INCOMPATIBLE" <> " (" <> msg <> ")"
              Right msg -> Pretty.color Pretty.Green "COMPATIBLE" <> " (" <> msg <> ")"

        echo $ " - "
          <> Bower.runPackageName name
          <> ": " <> Bower.runVersionRange versionRange <> " .. "
          <> checked
  where
    runBowerInfo :: Spago m => Text -> PackageLocation -> m Bower.PackageMeta
    runBowerInfo packageName location = do

      version <- case location of
        Local localPath ->
          die $ "Unable to create Bower version for local repo: " <> localPath
        Remote{..} ->
          pure version

      bowerName <- mkPackageName packageName
      let args = ["info", "--json", Bower.runPackageName bowerName <> "#" <> version]
      (code, stdout, stderr) <- runBower args

      when (code /= ExitSuccess) $ do
        die $ "Failed to run: `bower " <> Text.intercalate " " args <> "`\n" <> stderr

      case Aeson.decode $ encodeUtf8 $ fromStrict stdout of
        Just obj -> pure obj
        _ -> die $ "Unable to decode output from `bower " <> Text.intercalate " " args <> "`: " <> stdout

    checkBowerVersion :: PackageSet -> Bower.PackageName -> Bower.VersionRange -> Either Text Text
    checkBowerVersion PackageSet{packagesDB} bowerName Bower.VersionRange{runVersionRange} =
      case Bower.runPackageName bowerName of
        name | Just pkgName <- Text.stripPrefix "purescript-" name ->
          case Map.lookup (PackageName pkgName) packagesDB of
            Nothing ->
              Left "No corresponding package in package set"
            Just (Package{location = Local _}) ->
              Left "Corresponding package is local"
            Just (Package{location = Remote{version}}) -> do

              pkgSetVersion <- mapLeft
                (const $ "Invalid package set version: " <> version)
                (SemVer.parseSemVer version)

              bowerRange <- mapLeft
                (const $ "Invalid bower version range: " <> runVersionRange)
                (SemVer.parseSemVerRange runVersionRange)

              if SemVer.matches bowerRange pkgSetVersion
                then Right $ version <> " in package set"
                else Left $ version <> " in package set"
        _name ->
          Left "invalid dependency name (must start with 'purescript-')"
