module JID where

-- |Get username part of JID, i.e. the part before the \@ sign.
-- Return @\"\"@ if the JID contains no \@ sign.
getUsername :: String -> String
getUsername jid =
    case break (=='@') jid of
      (username,'@':_) -> username
      _ -> ""

-- |Get resource part of JID, i.e. the part after \/.
-- Return @\"\"@ if the JID has no resource.
getResource :: String -> String
getResource jid =
    case dropWhile (/='/') jid of
      '/':resource -> resource
      _ -> ""

-- |Get the bare JID, i.e. everything except the resource.
getBareJid :: String -> String
getBareJid jid = takeWhile (/='/') jid