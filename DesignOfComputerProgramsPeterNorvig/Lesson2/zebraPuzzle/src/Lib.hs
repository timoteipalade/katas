module Lib
    ( 
        run, 
        firstLevel, 
        secondLevel, 
        thirdLevel, 
        fourthLevel,
        fifthLevel,
        zeroLevelWithNationalities,
        firstLevelWithAnimals,
        secondLevelWithDrinks,
        thirdLevelWithCigarettes,
        fourthLevelWithColors,
        Nationality(..),
        Animal(..),
        Drink(..),
        Cigarette(..),
        Color(..),
        Solution
    ) where

import Data.List

-- Data Types
data Nationality = Norwegian | Japanese | English | Spanish | Ukranian deriving (Eq, Show)
data Animal = Dog | Snail | Fox | Horse | Zebra deriving (Eq, Show) 
data Drink = Tea | Coffee | Milk | Juice | Water deriving (Eq, Show) 
data Cigarette = Gold | Chester | Kools | Lucky | Parliments deriving (Eq, Show) 
data Color = Yellow | Green | Ivory | Red | Blue deriving (Eq, Show) 

-- Solution Type 
type Solution = ([Nationality], [Animal], [Drink], [Cigarette], [Color])

-- Constraints
englishInRed :: Solution -> Maybe Bool
englishInRed (nat, ani, dri, cig, col) = do i <- elemIndex English nat
                                            j <- elemIndex Red col
                                            return (i == j)

spanishOwnsDog :: Solution -> Maybe Bool
spanishOwnsDog (nat, ani, dri, cig, col) = do i <- elemIndex Spanish nat
                                              j <- elemIndex Dog ani
                                              return (i == j)

coffeeDrunkInGreen :: Solution -> Maybe Bool
coffeeDrunkInGreen (nat, ani, dri, cig, col) = do i <- elemIndex Coffee dri
                                                  j <- elemIndex Green col
                                                  return (i == j)

ukranianDrinksTea :: Solution -> Maybe Bool
ukranianDrinksTea (nat, ani, dri, cig, col) = do i <- elemIndex Ukranian nat
                                                 j <- elemIndex Tea dri
                                                 return (i == j)

greenRightOfIvory :: Solution -> Maybe Bool
greenRightOfIvory (nat, ani, dri, cig, col) = do i <- elemIndex Green col
                                                 j <- elemIndex Ivory col
                                                 return (i - 1 == j)

oldGoldHasSnail :: Solution -> Maybe Bool
oldGoldHasSnail (nat, ani, dri, cig, col) = do i <- elemIndex Gold cig
                                               j <- elemIndex Snail ani
                                               return (i == j)

koolsSmokedInYellow :: Solution -> Maybe Bool
koolsSmokedInYellow (nat, ani, dri, cig, col) = do i <- elemIndex Kools cig
                                                   j <- elemIndex Yellow col
                                                   return (i == j)

milkDrunkInMiddleHouse :: Solution -> Maybe Bool
milkDrunkInMiddleHouse (nat, ani, dri, cig, col) = do i <- elemIndex Milk dri
                                                      return (i == 2)

norwegianInFirstHouse :: Solution -> Maybe Bool
norwegianInFirstHouse (nat, ani, dri, cig, col) = do i <- elemIndex Norwegian nat
                                                     return (i == 0)

chesterNextToFox :: Solution -> Maybe Bool
chesterNextToFox (nat, ani, dri, cig, col) = do i <- elemIndex Chester cig
                                                j <- elemIndex Fox ani
                                                return ((i == j + 1) || (i == j - 1))

koolsNextToHorse :: Solution -> Maybe Bool
koolsNextToHorse (nat, ani, dri, cig, col) = do i <- elemIndex Kools cig
                                                j <- elemIndex Horse ani
                                                return ((i == j + 1) || (i == j - 1))

luckyDrinksJuice :: Solution -> Maybe Bool
luckyDrinksJuice (nat, ani, dri, cig, col) = do i <- elemIndex Lucky cig
                                                j <- elemIndex Juice dri
                                                return (i == j)

japaneseSmokesParliments :: Solution -> Maybe Bool
japaneseSmokesParliments (nat, ani, dri, cig, col) = do i <- elemIndex Japanese nat
                                                        j <- elemIndex Parliments cig
                                                        return (i == j)

norwegianNextToBlue :: Solution -> Maybe Bool
norwegianNextToBlue (nat, ani, dri, cig, col) = do i <- elemIndex Norwegian nat
                                                   j <- elemIndex Blue col
                                                   return ((i == j + 1) || (i == j - 1))


allConstraints :: [Solution -> Maybe Bool]
allConstraints = [
    englishInRed, 
    spanishOwnsDog, 
    coffeeDrunkInGreen, 
    ukranianDrinksTea, 
    greenRightOfIvory, 
    oldGoldHasSnail, 
    koolsSmokedInYellow, 
    milkDrunkInMiddleHouse, 
    norwegianInFirstHouse, 
    chesterNextToFox, 
    koolsNextToHorse, 
    luckyDrinksJuice, 
    japaneseSmokesParliments, 
    norwegianNextToBlue
    ]

-- Permutations

nationalities :: [[Nationality]]
nationalities = permutations [Japanese, Ukranian, Spanish, English, Norwegian]

animals :: [[Animal]]
animals = permutations [Dog, Snail, Zebra, Horse, Fox]

drinks :: [[Drink]]
drinks = permutations [Milk, Water, Juice, Coffee, Tea]

cigarettes :: [[Cigarette]]
cigarettes = permutations [Gold, Kools, Parliments, Lucky, Chester]

colors :: [[Color]]
colors = permutations [Blue, Yellow, Red, Green, Ivory]

foldFunc :: Maybe Bool -> Bool -> Bool 
foldFunc a b = case a of
                Nothing -> b
                Just r -> r && b

-- Check
-- returns True if none of the constraints evaluates to False. It returns true even if some constraints return Nothing.
weakCheck :: Solution -> Bool
weakCheck sol = foldr foldFunc True result where
                    result = map (\constraint -> constraint sol) allConstraints


-- Build the solutions array
zeroLevel :: [Solution]
zeroLevel = [([] :: [Nationality], [] :: [Animal], [] :: [Drink], [] :: [Cigarette], [] :: [Color])]

zeroLevelWithNationalities :: [Solution]
zeroLevelWithNationalities = zeroLevel >>= (\(_, _, _, _, _) -> map (\n -> (n, [] :: [Animal], [] :: [Drink], [] :: [Cigarette], [] :: [Color])) nationalities)

firstLevel :: [Solution]
firstLevel = filter weakCheck zeroLevelWithNationalities

firstLevelWithAnimals :: [Solution]
firstLevelWithAnimals = firstLevel >>= (\(n, _, _, _, _) -> map (\a -> (n, a, [] :: [Drink], [] :: [Cigarette], [] :: [Color])) animals)

secondLevel :: [Solution]
secondLevel = filter weakCheck firstLevelWithAnimals

secondLevelWithDrinks :: [Solution]
secondLevelWithDrinks = secondLevel >>= (\(n, a, _, _, _) -> map (\d -> (n, a, d, [] :: [Cigarette], [] :: [Color])) drinks)

thirdLevel :: [Solution]
thirdLevel = filter weakCheck secondLevelWithDrinks

thirdLevelWithCigarettes :: [Solution]
thirdLevelWithCigarettes = thirdLevel >>= (\(n, a, d, _, _) -> map (\c -> (n, a, d, c, [] :: [Color])) cigarettes)

fourthLevel :: [Solution]
fourthLevel = filter weakCheck thirdLevelWithCigarettes

fourthLevelWithColors :: [Solution]
fourthLevelWithColors = fourthLevel >>= (\(n, a, d, c, _) -> map (\col -> (n, a, d, c, col)) colors)

fifthLevel :: [Solution]
fifthLevel = filter weakCheck fourthLevelWithColors

run :: [Solution]
run = fifthLevel