{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Database.Bolt
import Data.Default

main :: IO ()
main = someFunc

defConnect :: IO Pipe
defConnect = connect $ def { user = "neo4j", password = "test" }

  {-querySearch :: Text -> BoltActionT IO [Molecule]
querySearch q = do records <- queryP cypher params           -- get record list by cypher query and params
                   nodes <- traverse (`at` "movie") records  -- from each record get only "movie" field
                   traverse toMovie nodes                    -- serialize movies to jsonable data
  where cypher = "MATCH (movie:Movie) WHERE movie.title =~ {title} RETURN movie"
        params = fromList [("title", T $ "(?i).*" <> q <> ".*")]
        -}
pushReaction :: Reaction -> BoltActionT IO [Record]
pushReaction reaction = queryP cypher params
  where cypher = "CREATE (r:Reaction {reaction})"
        params = props ["reaction" =: reaction]

getReaction :: Int -> BoltActionT IO Reaction
getReaction reactID = do records <- queryP cypher params
                         head records `at` "r" >>= exact
  where cypher = "MATCH (r:Reaction) WHERE ID(r) = {rid} return r"
        params = props ["rid" =: reactID]

shortestPath :: Molecule -> Molecule -> BoltActionT IO [Record]
shortestPath m1 m2 = queryP cypher params
  where cypher = "MATCH p=shortestPath( (bacon:Molecule {m1})-[*]->(meg:Molecule {m2}) ) RETURN p"
        params = props ["m1" =: m1, "m2" =: m2]

