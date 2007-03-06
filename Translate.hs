module Translate 
    (Language,
     translate,
     loadLanguage,
     nullLanguage)
    where

import System.IO
import Data.HashTable

type Language = HashTable Translatable Translatable

type Translatable = String

translate :: Language -> Translatable -> IO Translatable
translate lang from =
    do
      translated <- Data.HashTable.lookup lang from
      return $ maybe from id translated

loadLanguage :: String -> IO Language
loadLanguage filename =
    do
      h <- openFile filename ReadMode
      text <- hGetContents h
      table <- new (==) hashString
      addLines table (lines text)
      hClose h
      return table
    where addLines table [] =
              do
                --putStrLn "Done."
                return ()
          addLines table (from:to:rest) =
              do
                --putStrLn $ "'"++from++"' translates to '"++to++"'"
                insert table from to
                addLines table rest
          addLines table [odd] = putStrLn $ "Warning: discarding " ++ odd ++ " from " ++ filename

nullLanguage :: IO Language
nullLanguage = new (==) hashString