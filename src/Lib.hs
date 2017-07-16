{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-|
Module      : Lib
Description : library of rsync-imap
License     : MIT
Maintainer  : jesser@gmx.de
Stability   : experimental
Portability : portable

The library part orchestrates the modules:

* "IMAP.Connection" network communication
* "IMAP.Parsers" de-serialization
* "IMAP.Formatters" serialization

-}
module Lib
    ( rsyncImap
    , ImapConnection(..)
    , ActionFlags(..)
    -- re-export from IMAP.Connection
    , ConnectionFlags(..)
    ) where

import IMAP.Sync
import IMAP.Connection
