-- |A very primitive translation module.  Translations are kept in
-- files, where each pair of lines is first the \"original\" text, and
-- then the translated text.
module Translate 
    (Translatable,
     Language,
     loadLanguage,
     nullLanguage,
     translate)
    where

import System.IO
import Data.HashTable

type Language = HashTable Translatable Translatable

-- |This type should have more features, such as being able to insert
-- data in the middle.
type Translatable = String

-- |Translate a string from the \"source\" language to the given
-- language.  If the string is not present in the hashtable, return
-- the original.
translate :: Language -> Translatable -> IO Translatable
translate lang from =
    do
      translated <- Data.HashTable.lookup lang from
      return $ maybe from id translated

-- |Load a translation file into a hash table.
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

-- |Return an \"empty\" hash table, which will translate every string
-- to itself.
nullLanguage :: IO Language
nullLanguage = new (==) hashString