{-|
Module : IMAP.Types
Description : Typed Objects

See <https://tools.ietf.org/html/rfc3501#section-9 RFC-3501 Formal Syntax>.
-}
module IMAP.Types( Mailbox(..)
                 , MessageUid
                 , MessageId
                 , Message(..)
                 , Address(..)
                 , MessagePart(..)
                 , MessagePartGroup(..)
                 , MessagePartOrGroup
                 , MessagePartEncodingMap
                 , imapSendingDateFormat
) where

import           Data.Either
import           Data.Time

{-| mailbox-data = "FLAGS" SP flag-list / "LIST" SP mailbox-list /
                   "LSUB" SP mailbox-list / "SEARCH" *(SP nz-number) /
                   "STATUS" SP mailbox SP "(" [status-att-list] ")" /
                   number SP "EXISTS" / number SP "RECENT"
-}
data Mailbox = Mailbox { mbName           :: String
                       , mbExists         :: Bool
                       , mbDelim          :: Char
                       , mbFlags          :: [String]
                       , mbPermanentFlags :: [String]
                       , mbExistsCount    :: Int
                       , mbRecentCount    :: Int
                       , mbUidValidity    :: Integer
                       , mbUidNext        :: Int
                       , mbHighestModeSeq :: Integer
                       } deriving Show


-- | address = "(" addr-name SP addr-adl SP addr-mailbox SP addr-host ")"

data Address = Address { adName    :: String
                       , adAdl     :: String
                       , adMailbox :: String
                       , adHost    :: String
                       }

instance Show Address where
  show ad
    | adName ad == "NIL" = adMailbox ad ++ "@" ++ adHost ad
    | otherwise = adName ad ++ " <" ++ adMailbox ad ++ "@" ++ adHost ad ++ ">"

imapSendingDateFormat = "%a, %e %b %Y %T %z (%Z)"

type MessagePartOrGroup = Either MessagePart MessagePartGroup

type MessageUid = Integer

type MessageId = String

{-|
msg-att-static  = "ENVELOPE" SP envelope / "INTERNALDATE" SP date-time /
                  "RFC822" [".HEADER" / ".TEXT"] SP nstring /
                  "RFC822.SIZE" SP number /
                  "BODY" ["STRUCTURE"] SP body /
                  "BODY" section ["<" number ">"] SP nstring /
                  "UID" SP uniqueid
                    ; MUST NOT change for a message

envelope = "(" env-date SP env-subject SP env-from SP
                                       env-sender SP env-reply-to SP env-to SP env-cc SP
                                       env-bcc SP env-in-reply-to SP env-message-id ")"
-}
data Message = Message { mSeqNr      :: Int
                       , mUid        :: MessageUid
                       , mFlags      :: [String]
                       , mIDate      :: ZonedTime
                       , mSize       :: Int
                       , mSDate      :: ZonedTime
                       , mSubject    :: String
                       , mFrom       :: Address
                       , mSender     :: Address
                       , mReplyTo    :: Address
                       , mTo         :: [Address]
                       , mCc         :: [Address]
                       , mBcc        :: [Address]
                       , mReplyingTo :: String
                       , mMessageId  :: MessageId
                       , mBody       :: [MessagePartOrGroup]
                       } deriving Show

instance Eq Message where
  x == y = (mMessageId x) == (mMessageId y)

type MessagePartEncodingMap = [(String, String)]

{-|
body-type-1part = (body-type-basic / body-type-msg / body-type-text)
                  [SP body-ext-1part]

body-type-mpart = 1*body SP media-subtype
                  [SP body-ext-mpart]
-}
data MessagePart = MessagePart { mpMime         :: String
                               , mpEncoding     :: MessagePartEncodingMap
                               , mpContentType  :: String
                               , mpContent      :: String
                               , mpContentSize  :: Int
                               , mpContentLines :: Int
                               } deriving Show

{-|
@MessagePartGroup@ contains a group of parts of a message, which would be declared by @multipart/*@ MIME-type.
-}
data MessagePartGroup = MessagePartGroup { -- | parts contained in the group
                                           grpParts :: [MessagePartOrGroup]
                                           -- | mime of the group
                                         , grpMime  :: String
                                         } deriving Show
