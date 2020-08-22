module Allergies (Allergen(..), allergies, isAllergicTo) where
import Data.Bits

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Show, Ord, Enum, Read, Bounded)

allergies :: Int -> [Allergen]
allergies score = map toEnum $ filter (testBit score) [0..7]

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = testBit score (fromEnum allergen)
