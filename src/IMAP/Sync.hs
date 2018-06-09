{-|
Module : IMAP.Sync
Description : Supports dealing with a source and a target IMAP server.

Supports several 'ActionFlags'.
-}
module IMAP.Sync where

import           Control.Monad   (forM_, join, unless, when)
import           Data.List       (find, zip, intercalate, reverse)
import           Data.Maybe

import           Debug.Trace
import           IMAP.Connection
import           IMAP.Formatters
import           IMAP.Parsers
import           IMAP.Types

{-|
ImapConnection is parameter object which holds details to establish a connection to a IMAP server.
-}
data ImapConnection = ImapConnection { srvName  :: String
                                     , srvPort  :: String
                                     , srvFlags :: [ConnectionFlags]
                                     , srvUser  :: String
                                     , srvPass  :: String
                                     } deriving (Show)

-- | flags to trigger different actions
data ActionFlags
  -- | do NOT copy messages from source to target
  = DONT_TRANSFER_MESSAGES
  -- | do NOT create missing mailboxes in target
  | DONT_CREATE_MAILBOXES
  -- | do expunge source mailboxes TODO
  | DO_EXPUNGE_SRC_MAILBOXES
  -- | do delete messages only in target TODO
  | DO_CLEAR_TARGET_MESSAGES
  -- | do translate target mailbox names with slash
  | DO_TGT_MBNAME_SLASH
  -- | do translate target mailbox names with dot
  | DO_TGT_MBNAME_DOT
  deriving (Eq, Show, Read)
shouldCreateMailboxes flags = isNothing $ find (==DONT_CREATE_MAILBOXES) flags
shouldTransferMessages flags = isNothing $ find (==DONT_TRANSFER_MESSAGES) flags

class ConnectionManager a where
  imapConnect :: ImapConnection -> a Connection
  imapLogout :: Connection -> a()

instance ConnectionManager IO where
  imapConnect con  = connectAndLogin (srvName con) (read $ srvPort con) (srvFlags con) (srvUser con) (srvPass con)
  imapLogout con = logout con >> return ()


{-| rsyncImap is main entry point for CLI.

@src@      source connection parameters

@tgts@     target connections parameters

@actFlags@ action flags

All connection will be closed from server-side on LOGOUT.
-}
-- | ALL login / listMailboxes / syncMailbox / ALL logout
rsyncImap :: Monad a => ConnectionManager a => MailboxManager a => ImapConnection -> ImapConnection -> [ActionFlags] -> a()
rsyncImap src tgt actFlags = do
  srcCon <- imapConnect src
  tgtCon <- imapConnect tgt
  mailboxes <- imapMailboxes srcCon

  forM_ mailboxes $ imapMailboxSync srcCon tgtCon actFlags

  imapLogout srcCon
  imapLogout tgtCon
  return ()


class MailboxManager a where
  imapMailboxes :: Connection -> a [Mailbox]
  imapMailbox :: Connection -> Mailbox -> a Mailbox
  imapCreate :: Connection -> Mailbox -> a Bool
  imapMailboxSync :: Connection -> Connection -> [ActionFlags] -> Mailbox -> a()

instance MailboxManager IO where
  imapMailboxes con = do
    listResponse <- listMailboxes con
    let mailboxes = parse' parseListResponse listResponse
    return mailboxes
  imapMailbox con mbx = do
    selectResponse <- select con (mbName mbx)
    return $ parse' (parseSelectResponse mbx) selectResponse
  imapCreate con mbx = do
    createResponse <- create con $ mbName mbx
    return $ parse' parseCreateResponse createResponse
  imapMailboxSync srcCon tgtCon actFlags mbx = syncMailbox srcCon tgtCon actFlags mbx


-- | SRC select / TGT select / TGT create / SRC uidSearchAll / syncMessage
syncMailbox :: Monad a => MailboxManager a => MessageManager a => Connection -> Connection -> [ActionFlags] -> Mailbox -> a()
syncMailbox srcCon tgtCon actFlags mbx = do
  srcMbx <- imapMailbox srcCon mbx
  let tgtMbxName = tgtMbName (mbDelim mbx) (mbName mbx)
  tgtMbx <- imapMailbox tgtCon $ mbx { mbName = tgtMbxName }
  let tgtExists = mbExists tgtMbx

  unless tgtExists $ do
    if shouldCreateMailboxes actFlags
      then do
        created <- imapCreate tgtCon tgtMbx
        if created
           then imapMailbox tgtCon tgtMbx >> return ()
           else error $ unwords [show tgtCon, "did NOT create", show tgtMbx]
      else do
        error $ unwords [show tgtCon, "should NOT create", show tgtMbx]

  srcMessagesUids <- imapSearchAll srcCon
  forM_ srcMessagesUids $ imapMessageSync srcCon tgtCon actFlags tgtMbx
    where
      tgtMbName srcMbDelim srcMbName
        | tgtSlash && srcSlash = srcMbName
        | tgtDot && srcDot = srcMbName
        | tgtSlash && srcDot = replace '.' '/' srcMbName
        | tgtDot && srcSlash = replace '/' '.' srcMbName
        where
          tgtSlash = isJust $ find (==DO_TGT_MBNAME_SLASH) actFlags
          tgtDot = isJust $ find (==DO_TGT_MBNAME_DOT) actFlags
          srcSlash = srcMbDelim == '/'
          srcDot = srcMbDelim == '.'
          replace c1 c2 [] = ""
          replace c1 c2 (x:xs)
            | x == c1 = c2 : (replace c1 c2 xs)
            | otherwise = x : (replace c1 c2 xs)


class MessageManager a where
  imapSearchAll :: Connection -> a [MessageUid]
  imapMessageSync :: Connection -> Connection -> [ActionFlags] -> Mailbox -> MessageUid -> a()

instance MessageManager IO where
  imapSearchAll con = do
    uidSearchResponse <- uidSearchAll con
    return $ parse' parseUidSearchResponse uidSearchResponse
  imapMessageSync srcCon tgtCon actFlags tgtMbx = syncMessage  srcCon tgtCon actFlags $ mbName tgtMbx


-- SRC uidFetchFull / TGT uidSearchByMsgId / srcFetchBodies / tgtAppend
syncMessage srcCon tgtCon actFlags tgtMbName msgUid = do
  uidFetchResponse <- uidFetchFull srcCon msgUid
  let srcMsg = parse' parseMessage uidFetchResponse

  uidSearchResponse <- uidSearchByMsgId tgtCon $ mMessageId srcMsg
  let tgtMiss = null $ parse' parseUidSearchResponse uidSearchResponse

  when tgtMiss $ do
    msg <- fetchMessageBodies srcCon msgUid srcMsg

    if shouldTransferMessages actFlags
      then do
        appendResponse <- append tgtCon tgtMbName "\\Seen" $ formatMessage msg
        let appendUid = parse' parseAppendResponse appendResponse
        if isJust appendUid
          then traceM $ unwords [show tgtCon, "APPEND returned", show appendUid]
          else error  $ unwords [show tgtCon, "APPEND returned", show appendUid]
      else
        error $ unwords [show tgtCon, "should NOT transfer", show msgUid]

fetchMessageBodies :: Connection -> MessageUid -> Message -> IO Message
fetchMessageBodies srcCon msgUid srcMsg = do
  let idxParts = zip [1..] $ mBody srcMsg
  messageParts <- mapM' idxParts $ \(i, partOrGroup) ->
    fetchBodies srcCon msgUid (bodyNr [] i) partOrGroup
  return $ srcMsg { mBody = messageParts }

fetchBodies :: Connection -> MessageUid -> [Int] -> MessagePartOrGroup -> IO MessagePartOrGroup
fetchBodies srcCon msgUid idx (Left mp) = do
  bodyResponse <- uidFetchBody srcCon msgUid $ bodyNrStr idx
  return $ Left $ parse' (parseBodyResponse mp) bodyResponse
fetchBodies srcCon msgUid idx (Right grp) = do
  let nrParts = zip [1..] $ grpParts grp
  let idxParts = map' nrParts $ \(i, partOrGroup) -> (bodyNr idx i, partOrGroup)
  groupParts <- mapM' idxParts $ \(i, partOrGroup) -> fetchBodies srcCon msgUid idx partOrGroup
  return $ Right grp { grpParts = groupParts }

-- helper function

bodyNr idx nr = nr : idx
bodyNrStr :: [Int] -> String
bodyNrStr idx = intercalate "." $ mapM show $ reverse idx

map' x f = map f x
mapM' x f = mapM f x
parse' p res = fromJust $ maybeResult $ parse p res
