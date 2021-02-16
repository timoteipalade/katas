module Lib
    ( someFunc
    ) where

import Data.List

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Data Types
data Nationality = Norwegian | Japanese | English | Spanish | Ukranian deriving Eq 
data Animal = Dog | Snail | Fox | Horse | Zebra deriving Eq 
data Drink = Tea | Coffee | Milk | Juice | Water deriving Eq 
data Cigarette = Gold | Chester | Kools | Lucky | Parliments deriving Eq 
data Color = Yellow | Green | Ivory | Red | Blue deriving Eq 

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