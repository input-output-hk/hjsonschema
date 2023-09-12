module JSONSchema.Validator.Draft4.Object.Properties where

import           Import

import           Data.Aeson.Key (toText)
import           Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Hashable as HA
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified JSONPointer as JP
import qualified Text.Regex.PCRE.Heavy as RE

data PropertiesRelated schema = PropertiesRelated
    { _propProperties :: Maybe (KeyMap schema)
        -- ^ 'Maybe' is used to distinguish whether the key is present or not.
    , _propPattern    :: Maybe (KeyMap schema)
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
    { _prInvalidProperties :: KeyMap [err]
    , _prInvalidPattern    :: HashMap (Regex, JP.Key) [err]
    , _prInvalidAdditional :: Maybe (APInvalid err)
    } deriving (Eq, Show)

data APInvalid err
    = APBoolInvalid   (KeyMap Value)
    | APObjectInvalid (KeyMap (NonEmpty err))
    deriving (Eq, Show)

-- | First @"properties"@ and @"patternProperties"@ are run simultaneously
-- on the data, then @"additionalProperties"@ is run on the remainder.
propertiesRelatedVal
    :: forall err schema.
       (schema -> Value -> [err])
    -> PropertiesRelated schema
    -> KeyMap Value
    -> Maybe (PropertiesRelatedInvalid err)
propertiesRelatedVal f props x
    |  all null (KeyMap.elems propFailures)
    && all null (HM.elems patFailures)
    && isNothing addFailures = Nothing
    | otherwise =
        Just PropertiesRelatedInvalid
            { _prInvalidProperties = propFailures
            , _prInvalidPattern    = patFailures
            , _prInvalidAdditional = addFailures
            }
  where
    propertiesHm :: KeyMap schema
    propertiesHm = fromMaybe mempty (_propProperties props)

    patHm :: KeyMap schema
    patHm = fromMaybe mempty (_propPattern props)

    propAndUnmatched :: (KeyMap [err], Remaining)
    propAndUnmatched = ( KeyMap.intersectionWith f propertiesHm x
                       , Remaining (KeyMap.difference x propertiesHm)
                       )

    (propFailures, propRemaining) = propAndUnmatched

    patAndUnmatched :: (HashMap (Regex, JP.Key) [err], Remaining)
    patAndUnmatched = patternAndUnmatched f patHm x

    (patFailures, patRemaining) = patAndUnmatched

    finalRemaining :: Remaining
    finalRemaining = Remaining (KeyMap.intersection (_unRemaining patRemaining)
                                                (_unRemaining propRemaining))

    addFailures :: Maybe (APInvalid err)
    addFailures = (\addProp -> additionalProperties f addProp finalRemaining)
              =<< _propAdditional props

-- | Internal.
newtype Remaining
    = Remaining { _unRemaining :: KeyMap Value }

-- | Internal.
patternAndUnmatched
    :: forall err schema.
       (schema -> Value -> [err])
    -> KeyMap schema
    -> KeyMap Value
    -> (HashMap (Regex, JP.Key) [err], Remaining)
patternAndUnmatched f patPropertiesHm x =
    (foldlWithKey' runMatches mempty perhapsMatches, remaining)
  where
    -- @[(Regex, schema)]@ will have one item per match.
    perhapsMatches :: KeyMap ([(Regex, schema)], Value)
    perhapsMatches =
        foldlWithKey' (matchingSchemas patPropertiesHm) mempty x
      where
        matchingSchemas
            :: KeyMap schema
            -> KeyMap ([(Regex, schema)], Value)
            -> Key
            -> Value
            -> KeyMap ([(Regex, schema)], Value)
        matchingSchemas subSchemas acc k v =
            KeyMap.insert k
                      (foldlWithKey' (checkKey k) mempty subSchemas, v)
                      acc

        checkKey
            :: Key
            -> [(Regex, schema)]
            -> Key
            -> schema
            -> [(Regex, schema)]
        checkKey k acc r subSchema =
            case RE.compileM (encodeUtf8 (toText r)) mempty of
                Left _   -> acc
                Right re -> if (toText k) RE.=~ re
                                then (Regex (toText r), subSchema) : acc
                                else acc

    runMatches
        :: HashMap (Regex, JP.Key) [err]
        -> Key
        -> ([(Regex, schema)], Value)
        -> HashMap (Regex, JP.Key) [err]
    runMatches acc k (matches,v) =
        foldr runMatch acc matches
      where
        runMatch
            :: (Regex, schema)
            -> HashMap (Regex, JP.Key) [err]
            -> HashMap (Regex, JP.Key) [err]
        runMatch (r,schema) = HM.insert (r, JP.Key (toText k)) (f schema v)

    remaining :: Remaining
    remaining = Remaining . fmap snd . KeyMap.filter (null . fst) $ perhapsMatches

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
    -> Maybe (KeyMap Value)
additionalPropertiesBool True _ = Nothing
additionalPropertiesBool False (Remaining x)
    | KeyMap.size x > 0 = Just x
    | otherwise     = Nothing

-- | Internal.
additionalPropertiesObject
    :: forall err schema.
       (schema -> Value -> [err])
    -> schema
    -> Remaining
    -> Maybe (KeyMap (NonEmpty err))
additionalPropertiesObject f schema (Remaining x) =
    let errs = KeyMap.mapMaybe (NE.nonEmpty . f schema) x
    in if KeyMap.null errs
        then Nothing
        else Just errs

foldlWithKey' :: (a -> Key -> b -> a) -> a -> KeyMap b -> a
foldlWithKey' f a = Map.foldlWithKey' f a . KeyMap.toMap
