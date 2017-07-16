module Main where

import           Console.Options
import           Data.Maybe
import           Data.Monoid
import           Lib

{-|

-- openssl s_client -crlf -connect mail.waskowski.de:993
-- connectAndLogin "mail.waskowski.de" 993 [INVALID_CERT] "jan@waskowski.de" "password1"

-- openssl s_client -crlf -connect esserjan-de.netcup-mail.de:143 -starttls imap
-- connectAndLogin "esserjan-de.netcup-mail.de" 143 [START_TLS] "jan@esserjan.de" "password2"

stack build --profile --haddock --test && stack exec rsync-imap-exe -- --srcSrv mail.waskowski.de --srcPort 993 --srcFlag=INVALID_CERT --srcUser jan@waskowski.de --srcPass password1 --tgtSrv esserjan-de.netcup-mail.de --tgtPort 143 --tgtFlag=START_TLS --tgtUser jan@esserjan.de --tgtPass password2 --actFlag=DO_TGT_MBNAME_SLASH

-}
main = defaultMain $ do
    programName "rsync-imap"
    programDescription "Synchronize IMAP mailboxes."

    let req = FlagRequired $ \s -> Right s

    srcSrv  <- flagParam (FlagLong "srcSrv" <> FlagDescription "Source server name.") req
    srcPort <- flagParam (FlagLong "srcPort" <> FlagDescription "Source server port.") req
    srcFlags <- flagMany $ flagParam (FlagLong "srcFlag" <> FlagDescription "Source server flags.") (FlagOptional [] Right)
    srcUser <- flagParam (FlagLong "srcUser" <> FlagDescription "Source server user.") req
    srcPass <- flagParam (FlagLong "srcPass" <> FlagDescription "Source server pass.") req

    tgtSrv  <- flagParam (FlagLong "tgtSrv" <> FlagDescription "Target server name.") req
    tgtPort <- flagParam (FlagLong "tgtPort" <> FlagDescription "Target server port.") req
    tgtFlags <- flagMany $ flagParam (FlagLong "tgtFlag" <> FlagDescription "Target server flags.") (FlagOptional [] Right)
    tgtUser <- flagParam (FlagLong "tgtUser" <> FlagDescription "Target server user.") req
    tgtPass <- flagParam (FlagLong "tgtPass" <> FlagDescription "Target server pass.") req

    actFlags <- flagMany $ flagParam (FlagLong "actFlag" <> FlagDescription "Action flags.") (FlagOptional [] Right)

    action $ \toParam -> do
        let src = ImapConnection { srvName = fromJust $ toParam srcSrv
                                 , srvPort = fromJust $ toParam srcPort
                                 , srvFlags = map read $ toParam srcFlags
                                 , srvUser = fromJust $ toParam srcUser
                                 , srvPass = fromJust $ toParam srcPass
                                 }
        let tgt = ImapConnection { srvName = fromJust $ toParam tgtSrv
                                 , srvPort = fromJust $ toParam tgtPort
                                 , srvFlags = map read $ toParam tgtFlags
                                 , srvUser = fromJust $ toParam tgtUser
                                 , srvPass = fromJust $ toParam tgtPass
                                 }

        let actionFlags = map read $ toParam actFlags

        putStrLn $ unwords ["Source server:", show src]
        putStrLn $ unwords ["Target server:", show tgt]
        putStrLn $ unwords ["Actions:", show actionFlags]

        rsyncImap src tgt actionFlags
