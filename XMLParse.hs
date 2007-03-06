module XMLParse 
    ( XMLElem(..)
    , xmlPath
    , getAttr
    , getCdata
    , xmlToString
    , attrsToString
    , getRest
    , xmppStreamStart
    , shallowTag
    , deepTag
    , deepTags
    , Text.ParserCombinators.Parsec.parse
    , Text.ParserCombinators.Parsec.Parser
    )
    where

import Text.ParserCombinators.Parsec
import List

data XMLElem = XML String [(String,String)] [XMLElem]
             | CData String
               deriving (Show, Eq)

xmlPath :: [String] -> XMLElem -> Maybe XMLElem
xmlPath [] el = return el
xmlPath (name:names) (XML _ _ els) =
    do
      el <- find (\stanza ->
                      case stanza of
                        (XML n _ _) -> name==n
                        _ -> False) els
      xmlPath names el

getAttr :: String -> XMLElem -> Maybe String
getAttr attr (XML _ attrs _) = lookup attr attrs

getCdata :: XMLElem -> Maybe String
getCdata (XML _ _ els) =
    case els of
      [CData s] -> Just s
      _ -> Nothing

xmlToString :: Bool -> XMLElem -> String
xmlToString _ (CData s) = s
xmlToString close (XML name attrs subels) =
    "<" ++ name ++ attrsToString attrs ++
        if close then
            ">" ++ (concat $ map (xmlToString True) subels) 
                ++ "</" ++ name ++ ">"
            else
                ">"

attrsToString :: [(String,String)] -> String
attrsToString [] = ""
attrsToString ((name,value):attrs) =
    " "++name++"='"++value++"'" ++ attrsToString attrs

getRest :: Parser a -> Parser (a, String)
getRest f = do x <- try f
               p <- getInput
               return (x, p)

xmppStreamStart :: Parser XMLElem
xmppStreamStart =
    do
      many $ try processingInstruction
      streamTag <- shallowTag
      return streamTag

shallowTag :: Parser XMLElem
shallowTag =
    do
      tag <- tagStart
      char '>'
      return tag

deepTags :: Parser [XMLElem]
deepTags = many $ try deepTag

deepTag :: Parser XMLElem
deepTag =
    do
      (XML name attrs _) <- tagStart
      subels <- 
          (try $ do
             char '/'
             char '>'
             return [])
          <|>
          do
            char '>'
            els <- many $ (try deepTag) <|> cdata
            char '<'
            char '/'
            string name
            char '>'
            return els
      return $ XML name attrs subels

tagStart :: Parser XMLElem
tagStart =
    do
      char '<'
      name <- many1 tokenChar
      many space
      attrs <- many $
               do
                 attr <- attribute
                 many space
                 return attr
      return $ XML name attrs []

attribute :: Parser (String, String)
attribute =
    do
      name <- many1 tokenChar
      char '='
      quote <- char '\'' <|> char '"'
      value <- many1 $ satisfy (/=quote)
      char quote
      return (name, value)

cdata :: Parser XMLElem
cdata =
    do
      text <- many1 $ plainCdata <|> predefinedEntity
      return $ CData text
    where plainCdata = satisfy (\c -> c/='<' && c/='&')
          predefinedEntity = do
            char '&'
            entity <- try (string "amp")
                      <|> try (string "lt")
                     <|> try (string "gt")
                     <|> try (string "quot")
                     <|> string "apos"
            char ';'
            return $ case entity of
                       "amp" -> '&'
                       "lt" -> '<'
                       "gt" -> '>'
                       "quot" -> '"'
                       "apos" -> '\''

tokenChar :: Parser Char
tokenChar = letter <|> char ':' <|> char '-'

processingInstruction :: Parser ()
processingInstruction =
    do
      char '<'
      char '?'
      many $ satisfy (/='?')
      char '?'
      char '>'
      return ()