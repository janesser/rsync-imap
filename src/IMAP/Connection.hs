module IMAP.Connection ( connectAndLogin
                       , ConnectionFlags(..)
                       , logout
                       , listMailboxes
                       , select
                       , create
                       , uidSearchAll
                       , uidSearchByMsgId
                       , uidFetchFull
                       , uidFetchBody
                       , append
                       , expunge
                       -- re-export Network.Connection
                       , Connection) where

import           Control.Exception
import           Control.Monad         (when)
import qualified Data.ByteString       as B (ByteString, append, empty, length, null)
import           Data.ByteString.Char8 (pack, unpack)
import           Data.List             (find, isPrefixOf)
import           Data.Maybe
import           Debug.Trace
import           Network.BSD           (HostName)
import           Network.Connection 
import qualified Network.Socket as S
import           System.IO.Error

chunkTimeout = 500

-- | flags about IMAP connection modus
data ConnectionFlags
  -- | ignore invalid certificate
  = INVALID_CERT
  -- | upgrade connection to TLS
  | START_TLS
  -- | set socket keep-alive
  | KEEP_ALIVE
  deriving (Eq, Show, Read)

permitInvalidCert flags = isJust $ find (==INVALID_CERT) flags
startTls flags = isJust $ find (==START_TLS) flags
keepAlive flags = isJust $ find (==KEEP_ALIVE) flags

connectAndLogin server port flags user pass = do
  con <- connect server port flags
  login con user pass
  return con

connect :: String -> S.PortNumber -> [ConnectionFlags] -> IO Connection
connect server port flags = do
    ctx <- initConnectionContext
    let tls = TLSSettingsSimple { settingDisableCertificateValidation = permitInvalidCert flags
                                , settingDisableSession = False
                                , settingUseServerName = False
                                }

    let conParams = ConnectionParams { connectionHostname = server
                                     , connectionPort = port
                                     , connectionUseSecure = if startTls flags then Nothing else Just tls
                                     , connectionUseSocks = Nothing
                                     }

    conAddrs <- S.getAddrInfo (Just S.defaultHints) (Just server) (Just $ show port) 
    let conAddr = head conAddrs -- TODO tryFirst
    conSocket <- S.socket (S.addrFamily conAddr) (S.addrSocketType conAddr) (S.addrProtocol conAddr)

    S.connect conSocket (S.addrAddress conAddr)
    
    when ((keepAlive flags) && (S.isSupportedSocketOption S.KeepAlive)) $
      S.setSocketOption conSocket S.KeepAlive 1
    
    con <- connectFromSocket ctx conSocket conParams

  
    readAndTrace con

    when (startTls flags) $ do
      sendCommand con "STARTTLS"
      connectionSetSecure ctx con tls

    return con

login con user pass = do
    let loginCmd = unwords ["login", user, pass]
    sendCommand con loginCmd

logout con = sendCommand con "logout"

list con refName mbName = do
    let listCmd = unwords ["list", cmdQParam refName, cmdQParam mbName]
    sendCommand con listCmd where
        cmdQParam param = "\"" ++ param ++ "\""
listMailboxes con = list con "" "*"

select con mbName = do
    let selectCmd = unwords ["select", mbName]
    sendCommand con selectCmd

create con mbName = do
    let createCmd = unwords ["create", mbName]
    sendCommand con createCmd

uidSearch con term = do
    let uidSearchCmd = unwords ["uid", "search", term]
    sendCommand con uidSearchCmd
uidSearchAll con = uidSearch con "ALL"
uidSearchByMsgId con msgId = uidSearch con $ unwords ["HEADER"
                                                     , "Message-Id"
                                                     -- msgId with quotes
                                                     , show msgId]

uidFetch con uid mode = do
    let uidFetchCmd = unwords ["uid", "fetch", show uid, mode]
    sendCommand con uidFetchCmd
uidFetchFull con uid = uidFetch con uid "FULL"
uidFetchBody con uid bodyNr = uidFetch con uid $ "BODY[" ++ bodyNr ++ "]"

append :: Connection -> String -> String -> B.ByteString -> IO B.ByteString
append con mbName flags msgLiteral = do
  let msgLength = B.length msgLiteral
  let appendCmd = unwords ["append", mbName, "("++flags++")", "{"++show msgLength++"}"]
  sendCommand con appendCmd
  traceM $ unwords [show con, "$ ", unpack msgLiteral]
  connectionPut con msgLiteral
  connectionPut con $ pack "\r\n"
  readAndTrace con

expunge :: Connection -> IO B.ByteString
expunge con = do
  let expungeCmd = "expunge"
  traceM $ unwords [show con, "$ ", expungeCmd]
  sendCommand con expungeCmd
  readAndTrace con

-- low level

sendCommand con cmd = do
    let cmdLine = "tag " ++ cmd ++ "\r\n"
    connectionPut con $ pack cmdLine
    traceM $ unwords [show con, "$ ", cmdLine]
    readAndTrace con

readAndTrace :: Connection -> IO B.ByteString
readAndTrace con = do
  response <- readOrRetry con 10
  traceM . unpack $ response
  return response

readOrRetry con caps = do
  response <- connectionGetAll con B.empty
  if B.null response && caps > 0
    then readOrRetry con (caps - 1)
    else return response
  
connectionGetAll :: Connection -> B.ByteString -> IO B.ByteString
connectionGetAll con acc = catchEof acc $ do
  hasMore <- connectionWaitForInput con chunkTimeout
  if hasMore then do
    chunk <- connectionGetChunk con
    connectionGetAll con (B.append acc chunk)
  else return acc

instance Show Connection where
  show con = showCon $ connectionID con where
    showCon (hostName, portNumber) = unwords [show hostName, show portNumber]

catchEof acc f = catch f eofHandler where
   eofHandler e = if isEOFError e then return acc else ioError e
