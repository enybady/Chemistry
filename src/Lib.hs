{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Database.Bolt
import Data.Text

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Molecule = Molecule {molID :: Int, molSmiles :: Text, molIupacName :: Text} deriving (Show, Eq)

instance IsValue Molecule where
  toValue molecule = toValue $ props $ ["id" =: molID molecule, "smiles" =: molSmiles molecule, "iupacName" =: molIupacName molecule]

instance RecordValue Molecule where
  exact s = do 
    node        <- exact s
    let props   =  nodeProps node
    let id_v    =  nodeIdentity node
    smiles_v    <- props `at` "smiles" >>= exact
    iupacName_v <- props `at` "iupacName" >>= exact
    return (Molecule id_v smiles_v iupacName_v)

data Reaction = Reaction {reactID :: Int, reactName :: Text} deriving (Show, Eq)

instance IsValue Reaction where
  toValue reaction = toValue $ props $ ["id" =: reactID reaction, "name" =: reactName reaction]

instance RecordValue Reaction where
  exact s = do 
    node        <- exact s
    let props   =  nodeProps node
    let id_v    =  nodeIdentity node
    name_v    <- props `at` "name" >>= exact
    return (Reaction id_v name_v)
