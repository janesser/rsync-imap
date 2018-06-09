{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
module SyncSpec where

import Test.HUnit
import IMAP.Sync
import IMAP.Types

import Network.Connection
import Network.BSD(HostName)
import Network.Socket(PortNumber)
import Control.Concurrent.MVar
import Control.Monad (ap)
import Data.Maybe(Maybe(Nothing))
import System.IO(stdout)

import Unsafe.TrueName

import System.IO.Unsafe (unsafePerformIO)

data Mocked :: * -> *

instance Monad Mocked where
  (>>=) = (>>=)
instance Applicative Mocked where
  pure = return
  (<*>) = ap
instance Functor Mocked where
  fmap = fmap

testConnection :: Connection
testConnection = unsafePerformIO $ do
    connectionBackend <- newEmptyMVar -- ConnectionStream stdout
    connectionBuffer <- newEmptyMVar -- MVar Nothing
    let connectionId :: (HostName, PortNumber) = ("testHost", 123)

    let constr = [truename| ''Connection Connection |]
    return $ constr connectionBackend connectionBuffer connectionId

instance ConnectionManager Mocked where
  imapConnect con = return testConnection
  imapLogout con = return ()

instance MailboxManager Mocked where
    imapMailboxes c = return [testMailbox]
    imapMailbox c = return
    imapCreate c mb = return True
    imapMailboxSync src tgt actFlags mb = return ()

-- mocked data

testImapConnection = ImapConnection { srvName = "MOCKED_IMAP_SERVER"
                                    , srvPort = "MOCKED_PORT"
                                    , srvFlags = []
                                    , srvUser = "MOCKED_USER"
                                    , srvPass = "MOCKED_PASS"
                                    }


testMailbox = Mailbox { mbName = "MOCKED.MAILBOX1"
                      , mbExists = True
                      , mbDelim = '.'
                      , mbFlags = []
                      , mbPermanentFlags = []
                      , mbRecentCount = 0
                      , mbUidValidity = 1
                      , mbExistsCount = 1
                      , mbUidNext = 2
                      , mbHighestModeSeq = 3
                      }

testSimple = TestCase $ do
  let ret::Mocked() = mockedRsyncImap testConnection1 testConnection2 testActionFlags
  assertBool "Simple check failed" True where
    testConnection1 = testImapConnection { srvName = "MOCKED_SRC_SERVER" }
    testConnection2 = testImapConnection { srvName = "MOCKED_TGT_SERVER" }
    testActionFlags = []

-- mocked bindings
mockedRsyncImap :: ImapConnection -> ImapConnection -> [ActionFlags] -> Mocked ()
mockedRsyncImap = rsyncImap
