{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-|
Module : IMAP.Parsers
Description : Support to parse "Data.ByteString" into "IMAP.Types", e.g. 'Message'.

This module is based on "Data.Attoparsec". It provides a variety of parsers to handle input from "IMAP.Connection". The parser results are always types from "IMAP.Types".

Possible ways to read responses, where @createResponse@ comes directly from "System.IO".

@
maybeResult $ parse parseCreateResponse createResponse
@

@
handleError = ..
handleResult = ..

either $ parseOnly parseCreateResponse createResponse $ handleError handleResult
@

-}
module IMAP.Parsers ( Mailbox(..)
                    , parseListResponse
                    , parseSelectResponse
                    , parseCreateResponse
                    , MessageUid(..)
                    , parseUidSearchResponse
                    , Message(..)
                    , parseMessage
                    , MessagePart(..)
                    , parseBodyResponse
                    , parseAppendResponse
                    -- re-export Data.Attoparsec
                    , parse
                    , maybeResult
                    , parseOnly
                    ) where

import           Codec.Binary.QuotedPrintable     (qpDecode)
import           Control.Monad                    (unless, when)
import           Data.Attoparsec.ByteString.Char8 (Parser, anyChar, char,
                                                   endOfLine, isDigit,
                                                   isEndOfLine, many', manyTill,
                                                   maybeResult, parse,
                                                   parseOnly, sepBy, skipSpace,
                                                   string, scan)
import qualified Data.Attoparsec.ByteString.Char8 as AP (choice, count, option,
                                                         take, takeWhile)
import qualified Data.ByteString                  as B (ByteString, empty,
                                                        length, null, concat, last)
import           Data.ByteString.Char8            (pack, unpack)
import Data.Word8 (_backslash)
import           Data.Either
import           Data.List                        (find, intercalate)
import           Data.Maybe                       (fromJust, fromMaybe, isJust)
import           Data.Time                        (ZonedTime, defaultTimeLocale,
                                                   formatTime, parseTimeM)
import           Text.Read                        (readMaybe)
import           Debug.Trace
import           IMAP.Types

tagOK = string "tag OK"

-- | parse response to @tag list "" "*"@
parseListResponse :: Parser [Mailbox]
parseListResponse = do
  res <- many' parseListResult
  tagOK >> eol >> return res where
    parseListResult = do
      string "* LIST ("
      flags <-  AP.takeWhile (/=')') `sepBy` char ' '
      string ") \""
      delim <- anyChar
      string "\" \""
      name <- AP.takeWhile (/='\"')
      char '\"'
      endOfLine
      return Mailbox { mbName = unpack name
                   , mbExists = True
                   , mbDelim = delim
                   , mbFlags = fmap unpack flags
                   }

-- | parse response to @tag uid search ALL@
parseUidSearchResponse :: Parser [MessageUid]
parseUidSearchResponse = do
  string "* SEARCH"
  res <- many' parseUidSearchResult
  endOfLine
  tagOK >> eol >> return res where
    parseUidSearchResult = do
      char ' '
      uid <- AP.takeWhile isDigit
      return $ read $ unpack uid

-- | parse IMAP time, attempting several formats
parseImapTime :: String -> ZonedTime
parseImapTime str = fromJust $ fromJust $ find isJust $ map (parseTime str) imapSendingDateFormats where
    parseTime str fmt = parseTimeM True defaultTimeLocale fmt str
    -- Wed, 7 Dec 2011 15:34:14 +0000 (GMT)
    -- imapSendingDateFormat = "%a, %e %b %Y %T %z (%Z)"
    imapSendingDateFormats = [ -- 18-Nov-2008 16:33:30 +0100
                               "%d-%b-%Y %T %z"
                               -- Wed, 7 Dec 2011 15:34:14 +0000 (GMT)
                               , imapSendingDateFormat -- declared in Types.hs
                               -- Mon, 26 Mar 2012 14:53:14 +0200
                               , "%a, %e %b %Y %T %z"]

-- | parse response to @tag uid fetch 1 FULL@
{-
* 1 FETCH (UID 1 FLAGS (\Seen) INTERNALDATE "15-Jul-2011 13:30:59 +0200" RFC822.SIZE 6195 ENVELOPE ("Fri, 15 Jul 2011 13:30:59 +0200" "Re: DSA Verteiler mit David" (("Robin Schmidt" NIL "robinschmidt" "online.de")) (("Robin Schmidt" NIL "robinschmidt" "online.de")) (("Robin Schmidt" NIL "robinschmidt" "online.de")) (("Ingo Haltermann" NIL "moonstone23" "gmx.de")) (("g.czudnochowski@t-online.de" NIL "g.czudnochowski" "t-online.de")(NIL NIL "JEsser" "gmx.de")(NIL NIL "m.saemisch" "yahoo.de")(NIL NIL "Daniel.Loeckemann" "gmx.de")(NIL NIL "o.klostermann" "gmx.de")(NIL NIL "d.pappenheim" "gmx.de")) NIL "<20110715112939.118730@gmx.net>" "<4E2024F3.4000000@online.de>") BODY ("text" "plain" ("charset" "utf-8") NIL NIL "quoted-printable" 3897 131))
tag OK FETCH completed.
-}
parseMessage :: Parser Message
parseMessage = do
    string "* "
    seqNr <- AP.takeWhile isDigit
    string " FETCH (UID "
    uid <- AP.takeWhile isDigit
    string " FLAGS ("
    flags <- AP.takeWhile (/=')') `sepBy` char ' '
    string ") INTERNALDATE "
    internalDate <- parseQuoted -- 15-Dec-2010 21:53:39 +0100
    string " RFC822.SIZE "
    size <- AP.takeWhile isDigit
    let msgInternal = Message { mSeqNr = read $ unpack seqNr
                              , mUid = read $ unpack uid
                              , mFlags = fmap unpack flags
                              , mIDate = parseImapTime $ unpack internalDate
                              , mSize = read $ unpack size
                              }


    string " ENVELOPE ("
    msg <- parseEnvelope msgInternal

    string ") BODY "
    parts <- parsePartHeaders 0 []

    eol >> tagOK >> eol

    return msg { mBody = parts }

parseEnvelope :: Message -> Parser Message
parseEnvelope msg = do
  envSendingDate <- parseQuoted -- varying format
  space
  envSubject <- parseQuotedOrNil
  space
  envFrom <- parseAddressOrNil
  space
  envSender <- parseAddressOrNil
  space
  envReplyTo <- parseAddressOrNil
  space
  envTo <- parseAddressOrNil
  space
  envCc <- parseAddressOrNil
  space
  envBcc <- parseAddressOrNil
  space
  envReplyingTo <- parseQuotedOrNil
  space
  envMessageId <- parseQuotedOrNil
  return msg { mSDate = parseImapTime $ unpack envSendingDate
             , mSubject = unpack envSubject
             , mFrom = head envFrom
             , mSender = head envSender
             , mReplyTo = head envReplyTo
             , mTo = envTo
             , mCc = envCc
             , mBcc = envBcc
             , mReplyingTo = unpack envReplyingTo
             , mMessageId = unpack envMessageId
             }
    

{-| supported cases:
            1. BODY (onePart)
                => 1st bracket consumed in parseMessageParts
            2. BODY ((1stPart)(2ndPart))
                => 1st bracket consumed in manyBrackets, 2nd in parseMessageParts, closing brackets in reverse order
            3. BODY ((1stPart)(2ndPart) bodyMime)
                => 1st bracket consumer in manyBrackets, 2nd in parseMessageParts, forelast closing brackets in parseMessageParts, last in bodyMime-do-block
            4. BODY (((1stPart)(2ndPart) groupMime)(3rdPart)(4thPart) bodyMime)
                => 1st bracket consumer in manyBrackets, 2nd in parseMessageParts, forelast closing brackets in parseMessageParts, last in bodyMime-do-block
-}
{-
( .. BODY ("quoted-printable" "" NIL NIL NIL "8bit" 2326))

( .. BODY ("text" "plain" ("charset" "utf-8") NIL NIL "quoted-printable" 3897 131))

( .. BODY (
  (
    (\"text\" \"plain\" (\"charset\" \"windows-1252\") NIL NIL \"quoted-printable\" 1358 37)
    (\"text\" \"html\" (\"charset\" \"windows-1252\") NIL NIL \"quoted-printable\" 1641 26)
  \"alternative\")\
  (\"application\" \"pdf\" (\"name\" \"grenzfrei_a4.pdf\") NIL NIL \"base64\" 315686)
  (\"text\" \"plain\" (\"charset\" \"iso-8859-1\") NIL NIL \"7bit\" 181 4) \
\\"mixed\")

) -- end of message

-}
parsePartHeaders :: Int -> [MessagePartOrGroup] -> Parser [MessagePartOrGroup]
parsePartHeaders openedBrackets acc = do
  opBrackets <- count' (=='(')
  groupTrail <- AP.option B.empty $ string " "
  
  let isGroupTrail = not $ B.null groupTrail
  let openedBrackets' = openedBrackets + opBrackets

  -- traceM $ unwords ["parsePartHeaders", show isGroupTrail, show openedBrackets, show opBrackets, show acc]

  if isGroupTrail || openedBrackets' == 0
    then trails openedBrackets'
    else do
    res <- fmap (Left) parseMessagePart
    acc' <- AP.choice [ groupEnd res openedBrackets'
                      , subGroupBegin res
                      , partEnd res
                      ]
    parsePartHeaders (openedBrackets' - (fst acc')) (snd acc')
      where
        trails 0 = return acc
        trails openedBrackets' = do
          -- traceM $ unwords ["trails", show openedBrackets', show acc]
          grMime <- parseQuotedOrNil
          char ')'

          let grp = Right MessagePartGroup { grpParts = reverse acc
                                           , grpMime = unpack grMime
                                           }

          parsePartHeaders (openedBrackets' - 1) [grp]
        groupEnd :: MessagePartOrGroup -> Int -> Parser (Int, [MessagePartOrGroup])
        groupEnd res openedBrackets' = do
          string ") " -- end of last message part
          grMime <- parseQuotedOrNil
          char ')'

          let res' = [res]

          let grpParts' = if openedBrackets' > 0
                then (reverse acc) ++ res'
                else res'
          let acc' = if openedBrackets' > 0
                then []
                else acc
          let grp' = Right MessagePartGroup { grpParts = grpParts'
                                            , grpMime = unpack grMime
                                            }
          return (2, grp' : acc')
        subGroupBegin :: MessagePartOrGroup -> Parser (Int, [MessagePartOrGroup])
        subGroupBegin res = do
          string ")(("
          grp <- parsePartHeaders 2 []
          string ") "
          grMime <- parseQuotedOrNil

          let acc' = (reverse acc) ++ [res] ++ grp
          let grp' = Right MessagePartGroup { grpParts = (reverse acc) ++ [res] ++ grp
                                            , grpMime = unpack grMime
                                            }
          return (1, [grp'])
        partEnd :: MessagePartOrGroup -> Parser (Int, [MessagePartOrGroup])
        partEnd res = do
          char ')'

          let acc' = res : acc
          return (1, acc')

parseMessagePart :: Parser MessagePart
parseMessagePart = do
  mimeClass <- parseQuotedOrNil
  space
  mimeType <- parseQuotedOrNil
  space
  encoding <- parseEncodingParamsOrNil
  space
  parseQuotedOrNil
  space
  parseQuotedOrNil
  space
  contentType <- parseQuotedOrNil
  space
  contentSize <- AP.takeWhile isDigit
  contentLines <- AP.option (pack "-1") $ do
    space
    AP.takeWhile isDigit
  -- message/rfc822
  subMessage <- AP.option Nothing $ do
    char '('
    subEnvelope <- parseEnvelope $ Message { }
    string ") "
    subParts <- parsePartHeaders 0 []

    -- TODO identify this information
    moreData <- AP.takeWhile (/=')')

    return $ Just subEnvelope { mBody = subParts }
  return MessagePart { mpMime = unpack mimeClass ++  if B.null mimeType
                                                     then ""
                                                     else "/" ++ unpack mimeType
                     , mpEncoding = fmap unpack' encoding
                     , mpContentType = unpack contentType
                     , mpContent = "" -- see processBody
                     , mpContentSize = readNum $ unpack contentSize
                     , mpContentLines = readNum $ unpack contentLines
                     }

{-
tag uid fetch 6519 body[1.1]
* 2494 FETCH (UID 6519 BODY[1.1] {1471}
Hy jessek !

...
)
tag OK FETCH completed.
-}
parseBodyResponse :: MessagePart -> Parser MessagePart
parseBodyResponse mp = do
  string "* "
  msgIdx <- AP.takeWhile isDigit
  string " FETCH (UID "
  uid <- AP.takeWhile isDigit
  char ' '
  part <- AP.takeWhile (/=' ')
  string " {"
  bdyIdx <- AP.takeWhile isDigit
  char '}'
  endOfLine

  content <- AP.take (mpContentSize mp)

  eol
  return $ mp {mpContent = decodeQP content}



{-
* FLAGS (\Answered \Flagged \Deleted \Seen \Draft)
* OK [PERMANENTFLAGS (\Answered \Flagged \Deleted \Seen \Draft \*)] Flags permitted.
* 28 EXISTS
* 0 RECENT
* OK [UIDVALIDITY 1494870189] UIDs valid
* OK [UIDNEXT 32] Predicted next UID
* OK [HIGHESTMODSEQ 1] Highest
tag OK [READ-WRITE] Select completed.
-or-
tag OK [READ-WRITE] Ok
-or-
tag OK [READ-WRITE] Select completed (0.000 secs).
-}
parseSelectResponse :: Mailbox -> Parser Mailbox
parseSelectResponse mb = do
    previousClosed <- AP.option B.empty $ do
        string "* OK [CLOSED] Previous mailbox closed."
        endOfLine
        return B.empty
    exists <- AP.option B.empty $ string "* FLAGS ("
    if B.null exists
        then return mb {mbExists = False}
        else do
            let undefined = "-1"

            flags <- AP.option [] $ do
                res <- AP.takeWhile (/=')') `sepBy` char ' '
                char ')'
                endOfLine
                return res

            permanentFlags <- AP.option [] $ do
                string "* OK [PERMANENTFLAGS ("
                res <- AP.takeWhile (/=')') `sepBy` char ' '
                string ")]"
                eol
                return res

            existsCount <- AP.option undefined $ do
                string "* "
                res <- AP.takeWhile isDigit
                string " EXISTS"
                endOfLine
                return res

            recentCount <- AP.option undefined $ do
                string "* "
                res <- AP.takeWhile isDigit
                string " RECENT"
                endOfLine
                return res

            unseenCount <- AP.option undefined $ do
                string "* OK [UNSEEN "
                res <- AP.takeWhile isDigit
                char ']'
                eol
                return res

            uidValidity <- AP.option undefined $ do
                string "* OK [UIDVALIDITY "
                res <- AP.takeWhile isDigit
                char ']'
                eol
                return res

            myRights <- AP.option "" $ do
                string "* OK [MYRIGHTS \""
                res <- AP.takeWhile (/='\"')
                string "\"]"
                eol
                return res

            uidNext <- AP.option undefined $ do
                string "* OK [UIDNEXT "
                res <- AP.takeWhile isDigit
                char ']'
                eol
                return res

            highestModeSeq <- AP.option undefined $ do
                string "* OK [HIGHESTMODSEQ "
                res <- AP.takeWhile isDigit
                char ']'
                eol
                return res

            tagOK >> eol >> return mb { mbExists = True
                                      , mbFlags = fmap unpack flags
                                      , mbPermanentFlags = fmap unpack permanentFlags
                                      , mbExistsCount = read $ unpack existsCount
                                      , mbRecentCount = read $ unpack recentCount
                                      , mbUidValidity = read $ unpack uidValidity
                                      , mbUidNext = read $ unpack uidNext
                                      , mbHighestModeSeq = read $ unpack highestModeSeq
                                      }

{-
tag OK Create completed.
-}
parseCreateResponse = AP.option False $ tagOK >> eol >> return True

{-
* 29 EXISTS
* 1 RECENT
tag OK [APPENDUID 1494870189 32] Append completed.
-}
parseAppendResponse :: Parser (Maybe MessageUid)
parseAppendResponse = AP.option Nothing $ do
  exists <- do
    string "* "
    res <- AP.takeWhile isDigit
    string " EXISTS"
    eol
    return res
  recent <- do
    string "* "
    res <- AP.takeWhile isDigit
    string " RECENT"
    eol
    return res

  tagOK >> string " [APPENDUID "
  seqNr <- AP.takeWhile isDigit
  space
  uid <- AP.takeWhile isDigit
  char ']' >> eol
  return $ Just $ read $ unpack uid

-- helper func

parseEncodingParamsOrNil = AP.choice [parseEncodingNil, parseEncodingParams]
parseEncodingNil = do
  string "NIL"
  return []
parseEncodingParams = do
    char '('
    res <- many' parseEncodingParam
    char ')'
    return res
parseEncodingParam = do
    key <- parseQuoted
    space
    value <- parseQuoted
    AP.option ' ' $ char ' '
    return (key, value)

manyBrackets elemParser= do
    char '('
    res <- many' elemParser
    AP.option ' ' $ char ')'
    return res

parseQuotedOrNil = AP.choice[string "NIL", parseQuoted]
parseQuoted = do
  char '\"'
  res <- parseEscaped  
  char '\"'
  return res
  where
    parseEscaped = do
      res <- AP.takeWhile (/='\"')
      if (isEscaped res)
        then do
        char '\"'
        res2 <- parseEscaped
        return $ B.concat [res, pack "\"", res2]
        else return res
    isEscaped res
      | B.null res = False
      | otherwise = B.last res == _backslash

parseAddressOrNil = AP.choice [parseNoAddress, manyBrackets parseAddress]
parseNoAddress = do
    string "NIL"
    return []
parseAddress = do
    char '('
    adName <- parseQuotedOrNil
    space
    adAdl <- parseQuotedOrNil
    space
    adMailbox <- parseQuotedOrNil
    space
    adHost <- parseQuotedOrNil
    char ')'
    return Address { adName = unpack adName
                   , adAdl = unpack adAdl
                   , adMailbox = unpack adMailbox
                   , adHost = unpack adHost
                   }

-- character utils

eol = do
  AP.takeWhile(/='\r')
  endOfLine

unpack' (x,y) = (unpack x, unpack y)

singleton :: Parser a -> Parser [a]
singleton = fmap (: [])

space = skipSpace
    
count' p = do
  parsed <- AP.takeWhile p
  return $ B.length parsed

decodeQP content = either unpackError unpackResult $ qpDecode content where
  unpackResult = unpack.fst
  unpackError = do
    input <- unpack.snd
    error $ unwords ["Could not decode quoted-parsable:", show input]

readNum :: String -> Int
readNum = fromMaybe 0 . readMaybe
