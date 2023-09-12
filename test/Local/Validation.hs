module Local.Validation where

import           Protolude

import           Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap

import           Data.String (String)
import           Test.Hspec

import qualified AlternateSchema as AS
import           JSONSchema.Draft4
import qualified JSONSchema.Types as JT

spec :: Spec
spec = do
    -- Fetching from filesystem.

    it "a readFile exception during validation is turned into a left"
        readFileException
    it "Relative reference to local file"
        (resolutionInvalid "test/Local/schema.json")
    it "Chained relative references to local files"
        (resolutionInvalid "./test/Local/schema-with-ref.json")

    -- Validation tests that were hard to write in JSON for whatever reason.

    it "Don't parse schemas that have Null in a forbidden location"
        forbidNull
    it "The 'Value' based Schema's checkSchema should catch Nulls"
        exampleForbidNull

readFileException :: Expectation
readFileException = do
    let schema = emptySchema { _schemaRef = Just "does-not-exist.json" }
    res <- referencesViaFilesystem (SchemaWithURI schema Nothing)
    case res of
        Left (FSReadFailure _) -> pure ()
        a                      -> expectationFailure (msg <> show a)
  where
    msg :: String
    msg = "expected referencesViaFilesystem to return ReadFailure,"
       <> " instead got: "

resolutionInvalid :: Text -> Expectation
resolutionInvalid ref = do
    let schema = emptySchema { _schemaRef = Just ref }
    res <- fetchFilesystemAndValidate (SchemaWithURI schema Nothing) badData
    case res of
        Left (FVData _) -> pure ()
        a               -> expectationFailure (msg <> show a)
  where
    badData :: Value
    badData = Aeson.toJSON [True, True]

    msg :: String
    msg = "expected fetchFilesystemAndValidate to return"
       <> " Left (FVData [_]), instead got: "

forbidNull :: Expectation
forbidNull =
    case Aeson.fromJSON (Object (KeyMap.singleton "type" Null)) of
        Aeson.Error _   -> pure ()
        Aeson.Success a -> expectationFailure ("parsed to: " <> show (a :: Schema))

exampleForbidNull :: Expectation
exampleForbidNull =
    case AS.checkSchema (JT.Schema (KeyMap.singleton "type" Null)) of
        [] -> expectationFailure "No checkSchema failures"
        _  -> pure ()
