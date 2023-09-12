module JSONSchema.Validator.Draft4.Object
  ( module JSONSchema.Validator.Draft4.Object
  , module JSONSchema.Validator.Draft4.Object.Properties
  ) where

import           Import

import           Data.Aeson.Key (fromText, toText)
import           Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Data.Text as T

import           JSONSchema.Validator.Draft4.Object.Properties
import           JSONSchema.Validator.Utils

--------------------------------------------------
-- * maxProperties
--------------------------------------------------

newtype MaxProperties
    = MaxProperties { _unMaxProperties :: Int }
    deriving (Eq, Show)

instance FromJSON MaxProperties where
    parseJSON = withObject "MaxProperties" $ \o ->
        MaxProperties <$> o .: "maxProperties"

data MaxPropertiesInvalid
    = MaxPropertiesInvalid MaxProperties (KeyMap Value)
    deriving (Eq, Show)

-- | The spec requires @"maxProperties"@ to be non-negative.
maxPropertiesVal
    :: MaxProperties
    -> KeyMap Value
    -> Maybe MaxPropertiesInvalid
maxPropertiesVal a@(MaxProperties n) x
    | n < 0         = Nothing
    | KeyMap.size x > n = Just (MaxPropertiesInvalid a x)
    | otherwise     = Nothing

--------------------------------------------------
-- * minProperties
--------------------------------------------------

newtype MinProperties
    = MinProperties { _unMinProperties :: Int }
    deriving (Eq, Show)

instance FromJSON MinProperties where
    parseJSON = withObject "MinProperties" $ \o ->
        MinProperties <$> o .: "minProperties"

data MinPropertiesInvalid
    = MinPropertiesInvalid MinProperties (KeyMap Value)
    deriving (Eq, Show)

-- | The spec requires @"minProperties"@ to be non-negative.
minPropertiesVal
    :: MinProperties
    -> KeyMap Value
    -> Maybe MinPropertiesInvalid
minPropertiesVal a@(MinProperties n) x
    | n < 0         = Nothing
    | KeyMap.size x < n = Just (MinPropertiesInvalid a x)
    | otherwise     = Nothing

--------------------------------------------------
-- * required
--------------------------------------------------

-- | From the spec:
--
-- > The value of this keyword MUST be an array.
-- > This array MUST have at least one element.
-- > Elements of this array MUST be strings, and MUST be unique.
newtype Required
    = Required { _unRequired :: Set Text }
    deriving (Eq, Show)

instance FromJSON Required where
    parseJSON = withObject "Required" $ \o ->
        Required <$> o .: "required"

instance Arbitrary Required where
    arbitrary = do
        x  <- arbitraryText -- Guarantee at least one element.
        xs <- (fmap.fmap) T.pack arbitrary
        pure . Required . Set.fromList $ x:xs

data RequiredInvalid
    = RequiredInvalid Required (Set Text) (KeyMap Value)
    deriving (Eq, Show)

requiredVal :: Required -> KeyMap Value -> Maybe RequiredInvalid
requiredVal r@(Required ts) x
    | Set.null ts        = Nothing
    | Set.null leftovers = Nothing
    | otherwise          = Just (RequiredInvalid r leftovers x)
  where
    leftovers :: Set Text
    leftovers =
        Set.difference -- Items of the first set not in the second.
            ts
            (Set.fromList (toText <$> KeyMap.keys x))

--------------------------------------------------
-- * dependencies
--------------------------------------------------

newtype DependenciesValidator schema
    = DependenciesValidator
        { _unDependenciesValidator :: KeyMap (Dependency schema) }
    deriving (Eq, Show)

instance FromJSON schema => FromJSON (DependenciesValidator schema) where
    parseJSON = withObject "DependenciesValidator" $ \o ->
        DependenciesValidator <$> o .: "dependencies"

data Dependency schema
    = SchemaDependency schema
    | PropertyDependency (Set Text)
    deriving (Eq, Show)

instance FromJSON schema => FromJSON (Dependency schema) where
    parseJSON v = fmap SchemaDependency (parseJSON v)
              <|> fmap PropertyDependency (parseJSON v)

instance ToJSON schema => ToJSON (Dependency schema) where
    toJSON (SchemaDependency schema) = toJSON schema
    toJSON (PropertyDependency ts)   = toJSON ts

instance Arbitrary schema => Arbitrary (Dependency schema) where
    arbitrary = oneof [ SchemaDependency <$> arbitrary
                      , PropertyDependency <$> arbitrarySetOfText
                      ]

data DependencyMemberInvalid err
    = SchemaDepInvalid   (NonEmpty err)
    | PropertyDepInvalid (Set Text) (KeyMap Value)
    deriving (Eq, Show)

newtype DependenciesInvalid err
    = DependenciesInvalid (KeyMap (DependencyMemberInvalid err))
    deriving (Eq, Show)

-- | From the spec:
-- <http://json-schema.org/latest/json-schema-validation.html#anchor70>
--
-- > This keyword's value MUST be an object.
-- > Each value of this object MUST be either an object or an array.
-- >
-- > If the value is an object, it MUST be a valid JSON Schema.
-- > This is called a schema dependency.
-- >
-- > If the value is an array, it MUST have at least one element.
-- > Each element MUST be a string, and elements in the array MUST be unique.
-- > This is called a property dependency.
dependenciesVal
    :: forall err schema.
       (schema -> Value -> [err])
    -> DependenciesValidator schema
    -> KeyMap Value
    -> Maybe (DependenciesInvalid err)
dependenciesVal f (DependenciesValidator hm) x =
    let res = KeyMap.mapMaybeWithKey g hm
    in if KeyMap.null res
        then Nothing
        else Just (DependenciesInvalid res)
    where
      g :: Key -> Dependency schema -> Maybe (DependencyMemberInvalid err)
      g k (SchemaDependency schema)
          | KeyMap.member k x = SchemaDepInvalid
                        <$> NE.nonEmpty (f schema (Object x))
          | otherwise = Nothing
      g k (PropertyDependency ts)
          | KeyMap.member k x && not allPresent = Just (PropertyDepInvalid ts x)
          | otherwise                       = Nothing
        where
          allPresent :: Bool
          allPresent = all (`KeyMap.member` x) (Set.map fromText ts)
