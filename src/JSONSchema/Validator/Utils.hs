module JSONSchema.Validator.Utils where

import           Import

import           Control.Monad (fail)
import           Data.Aeson.Key (fromString, fromText)
import           Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.List.NonEmpty as NE
import           Data.Scientific (Scientific, fromFloatDigits)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V

--------------------------------------------------
-- * QuickCheck
--------------------------------------------------

arbitraryText :: Gen Text
arbitraryText = T.pack <$> arbitrary

arbitraryScientific :: Gen Scientific
arbitraryScientific = (fromFloatDigits :: Double -> Scientific) <$> arbitrary

arbitraryPositiveScientific :: Gen Scientific
arbitraryPositiveScientific = (fromFloatDigits :: Double -> Scientific)
                            . getPositive
                          <$> arbitrary

newtype ArbitraryValue
    = ArbitraryValue { _unArbitraryValue :: Value }
    deriving (Eq, Show)

instance Arbitrary ArbitraryValue where
    arbitrary = ArbitraryValue <$> sized f
      where
        f :: Int -> Gen Value
        f n | n <= 1    = oneof nonRecursive
            | otherwise = oneof $
                  fmap (Array . V.fromList) (traverse (const (f (n `div` 10)))
                    =<< (arbitrary :: Gen [()]))
                : fmap (Object . KeyMap.fromList . fmap (first fromText)) (traverse (const (g (n `div` 10)))
                    =<< (arbitrary :: Gen [()]))
                : nonRecursive

        g :: Int -> Gen (Text, Value)
        g n = (,) <$> arbitraryText <*> f n

        nonRecursive :: [Gen Value]
        nonRecursive =
            [ pure Null
            , Bool <$> arbitrary
            , String <$> arbitraryText
            , Number <$> arbitraryScientific
            ]

arbitraryHashMap :: Arbitrary a => Gen (KeyMap a)
arbitraryHashMap = KeyMap.fromList . fmap (first fromString) <$> arbitrary

arbitrarySetOfText :: Gen (Set Text)
arbitrarySetOfText = S.fromList . fmap T.pack <$> arbitrary

newtype NonEmpty' a = NonEmpty' { _unNonEmpty' :: NonEmpty a }

instance FromJSON a => FromJSON (NonEmpty' a) where
    parseJSON v = do
        xs <- parseJSON v
        case NE.nonEmpty xs of
            Nothing -> fail "Must have at least one item."
            Just ne -> pure (NonEmpty' ne)

instance ToJSON a => ToJSON (NonEmpty' a) where
    toJSON = toJSON . NE.toList . _unNonEmpty'

instance Arbitrary a => Arbitrary (NonEmpty' a) where
    arbitrary = do
        xs <- arbitrary
        case NE.nonEmpty xs of
            Nothing -> NonEmpty' . pure <$> arbitrary
            Just ne -> pure (NonEmpty' ne)

--------------------------------------------------
-- * allUniqueValues
--------------------------------------------------

allUniqueValues :: (Foldable f, Functor f) => f Value -> Bool
allUniqueValues = allUnique . fmap OrdValue

allUnique :: (Foldable f, Ord a) => f a -> Bool
allUnique xs = S.size (S.fromList (toList xs)) == length xs

-- | OrdValue's Ord instance needs benchmarking, but it allows us to
-- use our 'allUnique' function instead of O(n^2) nub, so it's probably
-- worth it.
newtype OrdValue = OrdValue { _unOrdValue :: Value } deriving Eq

instance Ord OrdValue where
    (OrdValue Null) `compare` (OrdValue Null) = EQ
    (OrdValue Null) `compare` _               = LT
    _               `compare` (OrdValue Null) = GT

    (OrdValue (Bool x)) `compare` (OrdValue (Bool y)) = x `compare` y
    (OrdValue (Bool _)) `compare` _                   = LT
    _                   `compare` (OrdValue (Bool _)) = GT

    (OrdValue (Number x)) `compare` (OrdValue (Number y)) = x `compare` y
    (OrdValue (Number _)) `compare` _                     = LT
    _                     `compare` (OrdValue (Number _)) = GT

    (OrdValue (String x)) `compare` (OrdValue (String y)) = x `compare` y
    (OrdValue (String _)) `compare` _                     = LT
    _                     `compare` (OrdValue (String _)) = GT

    (OrdValue (Array xs)) `compare` (OrdValue (Array ys)) =
        (OrdValue <$> xs) `compare` (OrdValue <$> ys)
    (OrdValue (Array _))  `compare` _                     = LT
    _                     `compare` (OrdValue (Array _))  = GT

    (OrdValue (Object x)) `compare` (OrdValue (Object y)) =
        toList (OrdValue <$> x) `compare` toList (OrdValue <$> y)
