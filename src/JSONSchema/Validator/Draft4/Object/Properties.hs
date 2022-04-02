module JSONSchema.Validator.Draft4.Object.Properties where

import           Import

import qualified Data.Hashable as HA
import qualified Data.HashMap.Strict as HM
import qualified HaskellWorks.Data.Aeson.Compat as J
import qualified HaskellWorks.Data.Aeson.Compat.Map as JM
import qualified Data.List.NonEmpty as NE
import           Data.Text.Encoding (encodeUtf8)
import qualified JSONPointer as JP
import qualified Text.Regex.PCRE.Heavy as RE

data PropertiesRelated schema = PropertiesRelated
    { _propProperties :: Maybe (JM.KeyMap schema)
        -- ^ 'Maybe' is used to distinguish whether the key is present or not.
    , _propPattern    :: Maybe (JM.KeyMap schema)
    , _propAdditional :: Maybe (AdditionalProperties schema)
    } deriving (Eq, Show)

instance FromJSON schema => FromJSON (PropertiesRelated schema) where
    parseJSON = withObject "PropertiesRelated" $ \o -> PropertiesRelated
        <$> o .:! "properties"
        <*> o .:! "patternProperties"
        <*> o .:! "additionalProperties"

emptyProperties :: PropertiesRelated schema
emptyProperties = PropertiesRelated
    { _propProperties = Nothing
    , _propPattern    = Nothing
    , _propAdditional = Nothing
    }

data AdditionalProperties schema
    = AdditionalPropertiesBool Bool
    | AdditionalPropertiesObject schema
    deriving (Eq, Show)

instance FromJSON schema => FromJSON (AdditionalProperties schema) where
    parseJSON v = fmap AdditionalPropertiesBool (parseJSON v)
              <|> fmap AdditionalPropertiesObject (parseJSON v)

instance ToJSON schema => ToJSON (AdditionalProperties schema) where
    toJSON (AdditionalPropertiesBool b)    = toJSON b
    toJSON (AdditionalPropertiesObject hm) = toJSON hm

instance Arbitrary schema => Arbitrary (AdditionalProperties schema) where
    arbitrary = oneof [ AdditionalPropertiesBool <$> arbitrary
                      , AdditionalPropertiesObject <$> arbitrary
                      ]

-- | A glorified @type@ alias.
newtype Regex
    = Regex { _unRegex :: Text }
    deriving (Eq, Show, Generic)

instance HA.Hashable Regex

-- NOTE: We'd like to enforce that at least one error exists here.
data PropertiesRelatedInvalid err = PropertiesRelatedInvalid
    { _prInvalidProperties :: JM.KeyMap [err]
    , _prInvalidPattern    :: HashMap (Regex, JP.Key) [err]
    , _prInvalidAdditional :: Maybe (APInvalid err)
    } deriving (Eq, Show)

data APInvalid err
    = APBoolInvalid   (JM.KeyMap Value)
    | APObjectInvalid (JM.KeyMap (NonEmpty err))
    deriving (Eq, Show)

-- | First @"properties"@ and @"patternProperties"@ are run simultaneously
-- on the data, then @"additionalProperties"@ is run on the remainder.
propertiesRelatedVal
    :: forall err schema.
       (schema -> Value -> [err])
    -> PropertiesRelated schema
    -> JM.KeyMap Value
    -> Maybe (PropertiesRelatedInvalid err)
propertiesRelatedVal f props x
    |  all null (JM.elems propFailures)
    && all null (HM.elems patFailures)
    && isNothing addFailures = Nothing
    | otherwise =
        Just PropertiesRelatedInvalid
            { _prInvalidProperties = propFailures
            , _prInvalidPattern    = patFailures
            , _prInvalidAdditional = addFailures
            }
  where
    propertiesHm :: JM.KeyMap schema
    propertiesHm = fromMaybe mempty (_propProperties props)

    patHm :: JM.KeyMap schema
    patHm = fromMaybe mempty (_propPattern props)

    propAndUnmatched :: (JM.KeyMap [err], Remaining)
    propAndUnmatched = ( JM.intersectionWith f propertiesHm x
                       , Remaining (JM.difference x propertiesHm)
                       )

    (propFailures, propRemaining) = propAndUnmatched

    patAndUnmatched :: (HashMap (Regex, JP.Key) [err], Remaining)
    patAndUnmatched = patternAndUnmatched f patHm x

    (patFailures, patRemaining) = patAndUnmatched

    finalRemaining :: Remaining
    finalRemaining = Remaining (JM.intersection (_unRemaining patRemaining)
                                                (_unRemaining propRemaining))

    addFailures :: Maybe (APInvalid err)
    addFailures = (\addProp -> additionalProperties f addProp finalRemaining)
              =<< _propAdditional props

-- | Internal.
newtype Remaining
    = Remaining { _unRemaining :: JM.KeyMap Value }

-- | Internal.
patternAndUnmatched
    :: forall err schema.
       (schema -> Value -> [err])
    -> JM.KeyMap schema
    -> JM.KeyMap Value
    -> (HashMap (Regex, JP.Key) [err], Remaining)
patternAndUnmatched f patPropertiesHm x =
    (JM.foldlWithKey' runMatches mempty perhapsMatches, remaining)
  where
    -- @[(Regex, schema)]@ will have one item per match.
    perhapsMatches :: JM.KeyMap ([(Regex, schema)], Value)
    perhapsMatches =
        JM.foldlWithKey' (matchingSchemas patPropertiesHm) mempty x
      where
        matchingSchemas
            :: JM.KeyMap schema
            -> JM.KeyMap ([(Regex, schema)], Value)
            -> J.Key
            -> Value
            -> JM.KeyMap ([(Regex, schema)], Value)
        matchingSchemas subSchemas acc k v =
            JM.insert k
                      (JM.foldlWithKey' (checkKey k) mempty subSchemas, v)
                      acc

        checkKey
            :: J.Key
            -> [(Regex, schema)]
            -> J.Key
            -> schema
            -> [(Regex, schema)]
        checkKey k acc r subSchema =
            case RE.compileM (encodeUtf8 (J.keyToText r)) mempty of
                Left _   -> acc
                Right re -> if (J.keyToText k) RE.=~ re
                                then (Regex (J.keyToText r), subSchema) : acc
                                else acc

    runMatches
        :: HashMap (Regex, JP.Key) [err]
        -> J.Key
        -> ([(Regex, schema)], Value)
        -> HashMap (Regex, JP.Key) [err]
    runMatches acc k (matches,v) =
        foldr runMatch acc matches
      where
        runMatch
            :: (Regex, schema)
            -> HashMap (Regex, JP.Key) [err]
            -> HashMap (Regex, JP.Key) [err]
        runMatch (r,schema) = HM.insert (r, JP.Key (J.keyToText k)) (f schema v)

    remaining :: Remaining
    remaining = Remaining . fmap snd . JM.filter (null . fst) $ perhapsMatches

-- Internal.
additionalProperties
    :: forall err schema.
       (schema -> Value -> [err])
    -> AdditionalProperties schema
    -> Remaining
    -> Maybe (APInvalid err)
additionalProperties f a x =
    case a of
        AdditionalPropertiesBool b ->
            APBoolInvalid <$> additionalPropertiesBool b x
        AdditionalPropertiesObject b ->
            APObjectInvalid <$> additionalPropertiesObject f b x

-- | Internal.
additionalPropertiesBool
    :: Bool
    -> Remaining
    -> Maybe (JM.KeyMap Value)
additionalPropertiesBool True _ = Nothing
additionalPropertiesBool False (Remaining x)
    | JM.size x > 0 = Just x
    | otherwise     = Nothing

-- | Internal.
additionalPropertiesObject
    :: forall err schema.
       (schema -> Value -> [err])
    -> schema
    -> Remaining
    -> Maybe (JM.KeyMap (NonEmpty err))
additionalPropertiesObject f schema (Remaining x) =
    let errs = JM.mapMaybe (NE.nonEmpty . f schema) x
    in if JM.null errs
        then Nothing
        else Just errs
