{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
module SyncSpec where

import Control.Monad.Identity
import Test.HUnit
import IMAP.Sync

import Network.Connection
import Network.BSD(HostName)
import Network.Socket(PortNumber)
import Control.Concurrent.MVar
import Data.Maybe(Maybe(Nothing))
import System.IO(stdout)

import Unsafe.TrueName

import System.IO.Unsafe (unsafePerformIO)

testConnection :: Connection
testConnection = unsafePerformIO $ do
    connectionBackend <- newEmptyMVar -- ConnectionStream stdout
    connectionBuffer <- newEmptyMVar -- MVar Nothing
    let connectionId :: (HostName, PortNumber) = ("testHost", 123)

    let constr = [truename| ''Connection Connection |]
    return $ constr connectionBackend connectionBuffer connectionId

instance ConnectionManager Identity where
  imapConnect con = Identity testConnection
  imapLogout con = return ()

testRsyncImap = TestCase $ do
  rsyncImap testConnection1 testConnection2 testActionFlags where
    testConnection1 = ImapConnection {}
    testConnection2 = ImapConnection {}
    testActionFlags = []
