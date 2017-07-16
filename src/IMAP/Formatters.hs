{-|
Basically converts from "IMAP.Types", e.g. 'Message', to 'ByteString'.
-}
module IMAP.Formatters (formatMessage) where

import           Codec.Binary.QuotedPrintable (qpDecode)
import qualified Data.ByteString              as B
import           Data.ByteString.Char8        (pack)
import           Data.Char                    (isSpace)
import           Data.Either
import           Data.List                    (find, intercalate, isPrefixOf)
import           Data.Maybe
import           Data.Time                    (defaultTimeLocale, formatTime)
import           Data.UUID                    (toString)
import           Data.UUID.V5                 (generateNamed, namespaceX500)
import           IMAP.Types

formatMessage :: Message -> B.ByteString
formatMessage msg = pack $
   unlines' [ dateLine
           , fromLine
           , subjectLine
           , toLine
           , ccLine
           , bccLine
           , replyToLine
           , replyingToLine
           , messageIdLine
           , mimeVersionLine
           , contentTypeLine
           , contents
           ] where
                dateLine = line "Date" $ formatTime defaultTimeLocale imapSendingDateFormat (mSDate msg)
                fromLine = line "From" $ show $ mFrom msg
                subjectLine = line "Subject" $ mSubject msg
                toLine = line "To" $ intercalate "," . fmap show $ mTo msg
                ccLine = line "Cc" $ intercalate "," . fmap show $ mCc msg
                bccLine = line "Bcc" $ intercalate "," . fmap show $ mBcc msg
                replyToLine = line "Reply-To" $ show $ mReplyTo msg
                replyingToLine = line "In-Reply-To" $ mReplyingTo msg
                messageIdLine = line "Message-Id" $ mMessageId msg
                mimeVersionLine = line "MIME-Version" "1.0"

                -- message-level content-type and encoding-params
                contentTypeLine = line "Content-Type" $ mimeType ++ encodingParams
                mimeType
                  | soloPart = mime fstPart
                  | otherwise = "multipart/mixed"

                -- TODO message/rfc822
                boundary
                  | isPrefixOf "multipart" mimeType = Just $ nextUUID (mMessageId msg)
                  | otherwise = Nothing

                encodingParams
                  | isJust boundary = ";boundary=" ++ fromJust boundary
                  | otherwise = either' fstPart (encoding isAny) $ const ""

                soloPart = length (mBody msg) == 1
                fstPart = head $ mBody msg

                contents = appendParts (mBody msg) boundary

-- content expansion

mime :: MessagePartOrGroup -> String
mime = mimeMapped . either mpMime grpMime

isAny (_,_) = True
isContentType (k,_) = k /= "name"
{-|
     disposition := "Content-Disposition" ":"
                    disposition-type
                    *(";" disposition-parm)

     disposition-type := "inline"
                       / "attachment"
                       / extension-token
                       ; values are not case-sensitive

     disposition-parm := filename-parm
                       / creation-date-parm
                       / modification-date-parm
                       / read-date-parm
                       / size-parm
                       / parameter

See <https://www.ietf.org/rfc/rfc2183.txt RFC-2183>.
-}
isDisposition (k,_) = k == "name"

encodingMap keys part = filter keys $ mpEncoding part
encoding keys part = intercalate "; " $ "" : (fmap (\(k,v) -> k ++ "=" ++ v) $ encodingMap keys part)

appendParts :: [MessagePartOrGroup] -> Maybe String -> String
appendParts [] boundary        = ""
appendParts (part:xs) boundary
  | null xs = (appendPart part boundary True) ++ (appendParts xs boundary)
  | otherwise = (appendPart part boundary False) ++ (appendParts xs boundary)
appendPart :: MessagePartOrGroup -> Maybe String -> Bool -> String
appendPart (Left part) boundary last
  | isNothing boundary = "\r\n" ++ partContents
  | otherwise = unlines' [ boundaryStart
                         , partHeaders
                         , "\r\n" ++ partContents ++ "\r\n"
                         , boundaryEnd
                         ]
  where
    partHeaders = unlines' [ partHeaderContentType
                           , partHeaderTransferEncoding
                           , partHeaderDisposition
                           ]
    partHeaderContentType = line "Content-Type" $ (mpMime part) ++ partEncoding
    partHeaderTransferEncoding = line "Content-Transfer-Encoding" (mpContentType part)
    partHeaderDisposition = line "Content-Disposition" $ headOrEmpty $ fmap (\(_,v) -> "attachment; filename=\"" ++ v ++ "\"") partName
    partEncoding = encoding isContentType part
    partContents = mpContent part

    partName = encodingMap isDisposition part

    boundaryStart = "\r\n" ++ boundaryString
    boundaryEnd = if last
                  then boundaryString ++ "\r\n"
                  else ""
    boundaryString = "--" ++ fromJust boundary

appendPart (Right grp) boundary _ = appendParts (grpParts grp) boundary

mimeMapped mime
  | mime == "alternative" = "multipart/alternative"
  | otherwise = mime

-- helper function

unlines' = intercalate "\r\n" . filter (not.null)

line :: String -> String -> String
line name value = if not $ null value
                  then name ++ ": " ++ value
                  else "" -- to be skipped

nextUUID :: String -> String
nextUUID objName = toString $ generateNamed namespaceX500 $ B.unpack $ pack objName

either' e a b = either a b e

headOrEmpty list
  | null list = ""
  | otherwise = head list
