module JID where

getUsername :: String -> String
getUsername jid =
    case break (=='@') jid of
      (username,'@':_) -> username
      _ -> ""

getResource :: String -> String
getResource jid =
    case dropWhile (/='/') jid of
      '/':resource -> resource
      _ -> ""

getBareJid :: String -> String
getBareJid jid = takeWhile (/='/') jid