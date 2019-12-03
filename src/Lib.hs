{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Database.Bolt
import Data.Default
import Data.Map
import Prelude hiding (id)
import Data.Text (pack, Text)
import Database.Bolt.Serialization

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Molecule = Molecule {id :: Int, smiles :: Text, iupacName :: Text} deriving (Show, Eq)
instance IsValue Molecule where
  toValue molecule = toValue $ props $ ["id" =: id molecule, "smiles" =: smiles molecule, "iupacName" =: iupacName molecule]

instance RecordValue Molecule where
  exact (S s) = do 
    map <- nodeProps <$> fromStructure s
    let (I id_v)        = map ! "id"
    let (T smiles_v)    = map ! "smiles"
    let (T iupacName_v) = map ! "iupacName"
    return (Molecule id_v smiles_v iupacName_v)

