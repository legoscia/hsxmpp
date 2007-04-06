module Main where

import XMPP
import MUC
import Translate

import Network
import Control.Monad
import Data.HashTable
import List
import System.Random
import System ( getArgs )
import System.IO

usage = "Usage: werewolf user@server [password]"

main = withSocketsDo $
       do
         args <- getArgs
         (username, server, password) <-
                 case args of
                   [us, pwd] ->
                       case break (=='@') us of
                         (u@(_:_), ('@':s)) ->
                             return (u, s, pwd)
                         _ ->
                             error usage
                   [us] ->
                       case break (=='@') us of
                         (u@(_:_), ('@':s)) ->
                             do
                               putStr $ "Password for "++us++": "
                               hFlush stdout
                               hSetEcho stdin False
                               pwd <- getLine
                               hSetEcho stdin True
                               return (u, s, pwd)
                         _ ->
                             error usage
                   _ ->
                       error usage

         c <- openStream server
         getStreamStart c

         runXMPP c $ do
           startAuth username server password
           sendPresence
           handleVersion "Werewolf Bot" "0.1" "HaskellOS"
           werewolf

         closeConnection c

werewolf :: XMPP ()
werewolf = do
  addHandler (isChat `conj` hasBody) listenForJoinRequest True

findLanguage :: String -> IO Language
findLanguage "sv" = loadLanguage "werewolf.txt.sv"
findLanguage "eo" = loadLanguage "werewolf.txt.eo"
findLanguage _ = nullLanguage

listenForJoinRequest :: XMLElem -> XMPP ()
listenForJoinRequest msg = do
  let sender = maybe "" id (getAttr "from" msg)
      text = maybe "" id (getMessageBody msg)
  case words text of
    ["join",langname,room] ->
        do
          lang <- liftIO $ findLanguage langname
          t' <- liftIO $ (translate lang "Joining room ")
          sendMessage sender (t' ++ room)
          nick <- liftIO $ (translate lang "werewolfbot")
          result <- joinGroupchat nick room
          case result of
            Nothing ->
                do
                  t <- liftIO $ translate lang "Error when joining room"
                  sendMessage sender t
            Just participantList ->
                inRoom lang nick room participantList
    _ ->
        sendMessage sender "I am a werewolf bot.  Type 'join lang room@server'."

inRoom lang nick room participantList = do
  t <- liftIO $ (translate lang "Hi, I am the werewolf bot.")
  sendGroupchatMessage room t
  gameTable <- liftIO $ new (==) hashString
  t <- liftIO $ translate lang "To join the game, send 'join' to me privately.  List participants with 'who' in public.  To start the game, say 'start' in public."
  sendGroupchatMessage room t
  beforeGame lang nick room participantList gameTable

data PlayerState = 
    Werewolf | Villager
    deriving (Show, Eq)

translateState :: Language -> PlayerState -> IO String
translateState lang state =
    translate lang $ case state of 
                       Werewolf -> "werewolf"
                       Villager -> "villager"

beforeGame lang nick room participantList gameTable = do
  msg <- waitForStanza $ matchesBare room `conj` hasBody
  let sender = maybe "" id (getAttr "from" msg)
      nick = getResource sender
      msgtype = maybe "" id (getAttr "type" msg)
      body = maybe "" id $ getMessageBody msg
  start <-
      case msgtype of
        "chat" ->
            case body of
              "join" ->
                  do
                    t <- liftIO $ translate lang " has joined the game."
                    sendGroupchatMessage room (nick ++ t)
                    liftIO $ update gameTable nick Villager
                    return False
              _ ->
                  do
                    t <- liftIO $ translate lang "You probably meant 'join'."
                    sendGroupchatPrivateMessage nick room t
                    return False
        "groupchat" ->
            case body of
              "who" ->
                  do
                    entries <- liftIO $ toList gameTable
                    t <- case entries of
                           [] ->
                               liftIO $ translate lang "No players yet."
                           _ ->
                               return (unwords $ map fst entries)
                    sendGroupchatMessage room t
                    return False
              "start" ->
                  do
                    n <- liftIO $ (liftM length) $ toList gameTable
                    if n > 3 then
                        return True
                        else
                            do
                              t <- liftIO $ translate lang "Not enough players."
                              sendGroupchatMessage room t
                              return False
              _ ->
                  return False
        _ ->
            return False
  if start then
      startGame lang nick room participantList gameTable
      else
          beforeGame lang nick room participantList gameTable

startGame :: Language -> String -> String -> (IO [String]) ->
             HashTable String PlayerState -> XMPP ()
startGame lang nick room participantList gameTable = do
  entries <- liftIO $ toList gameTable
  let nicks = map fst entries
      nPlayers = length nicks
      nWerewolves = if nPlayers < 6
                    then 1
                    else if nPlayers < 10
                         then 2
                         else 3
  werewolves <- liftIO $ pickWerewolves nWerewolves nicks
  --liftIO $ mapM_ (\nick -> update gameTable nick Werewolf) werewolves
  t <- liftIO $ translate lang
       "The game is starting.  Some of you are werewolves, and some of you are villagers.  The werewolves try to kill all the villagers during the night, while the villagers try to execute all the werewolves during the day."
  sendGroupchatMessage room t
  mapM_ (\nick ->
             do
               t <- liftIO $ translate lang "You are a werewolf.  "
               t' <- case nWerewolves of
                       1 ->
                           liftIO $ translate lang "There are no other werewolves."
                       _ ->
                           do
                             t'' <- liftIO $ translate lang "The other werewolves are: "
                             return $ t'' ++ unwords (werewolves \\ [nick])
               sendGroupchatPrivateMessage nick room (t++t'))
                                                     werewolves
  mapM_ (\nick ->
             do
               t <- liftIO $ translate lang "You are a villager."
               sendGroupchatPrivateMessage nick room t)
                                                     (nicks \\ werewolves)
  startNight lang nick room participantList werewolves (nicks \\ werewolves)
  where pickWerewolves :: Int -> [String] -> IO [String]
        pickWerewolves 0 _ = return []
        pickWerewolves n [] = return []
        pickWerewolves n nicks = do
            which <- randomRIO (0,length nicks - 1)
            let w = nicks !! which
                rest = nicks \\ [w]
            ws <- pickWerewolves (n-1) rest
            return (w:ws)

checkVictory [] villagers = Just Villager
checkVictory werewolves [] = Just Werewolf
checkVictory _ _ = Nothing

startNight lang nick room participantList werewolves villagers = do
  case checkVictory werewolves villagers of
    Just x ->
        do
          t <- liftIO $ translate lang "As the sun sets over the little village, the people realize that there is only one side left.  "
          t' <- liftIO $ translate lang $ case x of
                                            Villager -> "The villagers have won!"
                                            Werewolf -> "The werewolves have won!"
          sendGroupchatMessage room (t++t')
          inRoom lang nick room participantList
    Nothing ->
        do
          t <- liftIO $ translate lang "The villagers, tired after the hard work in the fields, go to bed, and the sun sets.  But in the middle of the night, the werewolves wake up!"
          sendGroupchatMessage room t
          mapM_ (\nick -> do
                   t <- liftIO $ translate lang "You and the other werewolves vote for a villager to kill by typing 'kill NICK' to me."
                   sendGroupchatPrivateMessage nick room t) werewolves
          werewolvesChoosing lang nick room participantList werewolves villagers []

werewolvesChoosing :: Language -> String -> String -> (IO [String]) ->
                      [String] -> [String] -> [(String,String)] ->
                      XMPP ()
werewolvesChoosing lang nick room participantList werewolves villagers choices = do
  case (werewolves \\ map fst choices, nub (map snd choices)) of
    -- all werewolves have voted for the same victim.
    ([], [victim]) ->
        do
          t <- liftIO $ translate lang "A scream of pain and agony is heard!"
          sendGroupchatMessage room t
          t <- liftIO $ translate lang " has been devoured by the werewolves."
          sendGroupchatMessage room (victim++t)
          startDay lang nick room participantList werewolves (villagers \\ [victim])
    _ ->
        do
          stanza <- waitForStanza $ isGroupchatPrivmsg room `conj` hasBody
          let sendernick = getResource $ maybe "" id (getAttr "from" stanza)
              msg = maybe "" id (getMessageBody stanza)
          if (sendernick `notElem` werewolves)
           then do
                t <- liftIO $ translate lang "You are not a werewolf.  You are supposed to be sleeping!"
                sendGroupchatPrivateMessage sendernick room t
                retry
           else case msg of
                 'w':'h':'o':_ ->
                     do
                       t1 <- liftIO $ translate lang "Werewolves:"
                       t2 <- liftIO $ translate lang "Villagers:"
                       t3 <- liftIO $ translate lang "Votes:"
                       t4 <- liftIO $ translate lang "Noone has voted yet."
                       t5 <- liftIO $ translate lang " has voted to kill "
                       let t = t1 ++ "\n" ++ concat (map (++" ") werewolves) ++
                               t2 ++ "\n" ++ concat (map (++" ") villagers) ++
                               t3 ++ "\n" ++ case choices of
                                               [] ->
                                                   t4
                                               _ ->
                                                   concat (map (\(who,whom) ->
                                                                    who ++ t5 ++ whom ++ "\n")
                                                           choices)
                       sendGroupchatPrivateMessage sendernick room t
                       retry
                 'k':'i':'l':'l':' ':victim ->
                     if victim `notElem` villagers
                     then do
                           t <- liftIO $ translate lang " is not a villager."
                           sendGroupchatPrivateMessage sendernick room (victim ++ t)
                           retry
                     else do
                           t <- liftIO $ translate lang " has voted to kill "
                           mapM_ (\nick -> sendGroupchatPrivateMessage nick room (sendernick++t++victim))
                                 (werewolves \\ [sendernick])
                           let choices' = (sendernick, victim):(filter ((/=sendernick).fst) choices)
                           werewolvesChoosing lang nick room participantList werewolves villagers choices'
                 _ ->
                     do
                       t <- liftIO $ translate lang "I don't understand that."
                       sendGroupchatPrivateMessage sendernick room t
                       retry
  where retry = werewolvesChoosing lang nick room participantList werewolves villagers choices

startDay lang nick room participantList werewolves villagers = do
  case checkVictory werewolves villagers of
    Just x ->
        do
          t <- liftIO $ translate lang "As the sun rises over the little village, the people realize that there is only one side left.  "
          t' <- liftIO $ translate lang $ case x of
                                            Villager -> "The villagers have won!"
                                            Werewolf -> "The werewolves have won!"
          sendGroupchatMessage room (t++t')
          inRoom lang nick room participantList
    Nothing ->
        do
          t <- liftIO $ translate lang "The villagers are outraged over this crime, and want to execute somebody.  They gather in the town square to vote about whom to kill.  Type 'kill NICK' in public to vote."
          sendGroupchatMessage room t
          executionVote lang nick room participantList werewolves villagers []

executionVote lang nick room participantList werewolves villagers choices = do
  let candidates = nub (map snd choices)
      candidatesVotes = map (\nick -> (nick, length (filter ((==nick).snd) choices))) candidates
      candidatesVotesSorted = sortBy (comparing ((0-).snd)) candidatesVotes
      result = case candidatesVotesSorted of
                 (votedfor, nVotes):_ ->
                     -- majority?
                     if nVotes > ((length werewolves + length villagers) `div` 2) 
                     then
                         Just votedfor
                     else
                         Nothing
                 _ -> Nothing
  case result of
    Just votedfor ->
        do
          t <- liftIO $ translate lang "The villagers have chosen by simple majority to execute "
          sendGroupchatMessage room (t++votedfor)
          startNight lang nick room participantList (werewolves \\ [votedfor]) (villagers \\ [votedfor])
    Nothing ->
        if length choices == length werewolves + length villagers
        then do
              t <- liftIO $ translate lang "All villagers have cast their vote, but no majority has been reached.  The disgruntled villagers go home."
              sendGroupchatMessage room t
              startNight lang nick room participantList werewolves villagers
        else do
              stanza <- waitForStanza $ matchesBare room `conj` isGroupchatMessage `conj` hasBody
              let sendernick = getResource $ maybe "" id (getAttr "from" stanza)
                  msg = maybe "" id (getMessageBody stanza)
              case msg of
                'w':'h':'o':_ ->
                    do
                      t1 <- liftIO $ translate lang "Villagers:"
                      t2 <- liftIO $ translate lang "Votes:"
                      t3 <- liftIO $ translate lang "Noone has voted yet."
                      t4 <- liftIO $ translate lang " has voted to kill "
                      let t = t1 ++ "\n" ++ concat (map (++"\n") (sort (werewolves ++ villagers))) ++
                              t2 ++ "\n" ++ case choices of
                                              [] ->
                                                  t3
                                              _ ->
                                                  concat (map (\(who,whom) ->
                                                                   who ++ t4 ++ whom ++ "\n")
                                                          choices)
                      sendGroupchatMessage room t
                      retry
                'k':'i':'l':'l':' ':whom ->
                    if sendernick `notElem` werewolves && sendernick `notElem` villagers
                    then do
                          t <- liftIO $ translate lang ": You are not entitled to vote."
                          sendGroupchatMessage room (sendernick++t)
                          retry
                    else if whom `notElem` werewolves && whom `notElem` villagers then do
                               t <- liftIO $ translate lang " is not a villager."
                               sendGroupchatMessage room (whom++t)
                               retry
                         else do
                          t <- liftIO $ translate lang " has voted to kill "
                          sendGroupchatMessage room (sendernick++t++whom)
                          let choices' = (sendernick, whom):(filter ((/=sendernick) . fst) choices)
                          executionVote lang nick room participantList werewolves villagers choices'
                "status" ->
                    do
                      sendGroupchatMessage room $ "Vote status: " ++ show choices
                      retry
                _ ->
                    retry
    where retry = executionVote lang nick room participantList werewolves villagers choices

comparing :: Ord b => (a -> b) -> a -> a -> Ordering
comparing p x y = compare (p x) (p y)
