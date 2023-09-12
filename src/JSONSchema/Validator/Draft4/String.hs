module JSONSchema.Validator.Draft4.String where

import           Import

import qualified Data.Text as T
import qualified Text.Regex.PCRE.Heavy as RE

--------------------------------------------------
-- * maxLength
--------------------------------------------------

newtype MaxLength
    = MaxLength { _unMaxLength :: Int }
    deriving (Eq, Show)

instance FromJSON MaxLength where
    parseJSON = withObject "MaxLength" $ \o ->
        MaxLength <$> o .: "maxLength"

data MaxLengthInvalid
    = MaxLengthInvalid MaxLength Text
    deriving (Eq, Show)

maxLengthVal :: MaxLength -> Text -> Maybe MaxLengthInvalid
maxLengthVal a@(MaxLength n) x
    | T.length x > n = Just (MaxLengthInvalid a x)
    | otherwise      = Nothing

--------------------------------------------------
-- * minLength
--------------------------------------------------

newtype MinLength
    = MinLength { _unMinLength :: Int }
    deriving (Eq, Show)

instance FromJSON MinLength where
    parseJSON = withObject "MinLength" $ \o ->
        MinLength <$> o .: "minLength"

data MinLengthInvalid
    = MinLengthInvalid MinLength Text
    deriving (Eq, Show)

minLengthVal :: MinLength -> Text -> Maybe MinLengthInvalid
minLengthVal a@(MinLength n) x
    | T.length x < n = Just (MinLengthInvalid a x)
    | otherwise      = Nothing

--------------------------------------------------
-- * pattern
--------------------------------------------------

newtype PatternValidator
    = PatternValidator { _unPatternValidator :: Text }
    deriving (Eq, Show)

instance FromJSON PatternValidator where
    parseJSON = withObject "PatternValidator" $ \o ->
        PatternValidator <$> o .: "pattern"

data PatternInvalid
    = PatternNotRegex -- TODO: let these pass successfully?
    | PatternInvalid PatternValidator Text
    deriving (Eq, Show)

patternVal :: PatternValidator -> Text -> Maybe PatternInvalid
patternVal a@(PatternValidator t) x =
    case RE.compileM (encodeUtf8 t) mempty of
        Left _   -> Just PatternNotRegex
        Right re -> if input RE.=~ re
                        then Nothing
                        else Just (PatternInvalid a x)
  where
    input = T.unpack x -- workaround for a bug in pcre-light
