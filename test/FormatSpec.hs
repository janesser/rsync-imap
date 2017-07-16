{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module FormatSpec where

import           Data.ByteString.Char8
import           Data.Either
import           Data.Maybe
import           Data.Time
import           IMAP.Formatters
import           IMAP.Types
import           Test.HUnit

testMessage = Message {  mUid = 123
                      , mMessageId = "testMessageId"
                      , mSDate = testTime
                      , mSubject = "testSubject"
                      , mFrom = emptyAddress { adName = "Mustermann, Hans"
                                             , adMailbox = "hans"
                                             , adHost = "mustermann.de"
                                             }
                      , mTo = [ emptyAddress { adName = "Mustermann, Fritz"
                                             , adMailbox = "fritz"
                                             , adHost = "mustermann.de"
                                             }
                              ]
                      , mBcc = []
                      , mCc = []
                      , mReplyTo = emptyAddress { adMailbox = "hans"
                                                , adHost = "mustermann.de"
                                                }
                      , mReplyingTo = nil
                      }

testFormatPlaintext = TestCase $
  assertEqual "Unexpected message contents" "Date: Fri,  1 Jan 2016 00:00:00 +0000 (GMT)\r\n\
                                            \From: Mustermann, Hans <hans@mustermann.de>\r\n\
                                            \Subject: testSubject\r\n\
                                            \To: Mustermann, Fritz <fritz@mustermann.de>\r\n\
                                            \Reply-To: hans@mustermann.de\r\n\
                                            \In-Reply-To: NIL\r\n\
                                            \Message-Id: testMessageId\r\n\
                                            \MIME-Version: 1.0\r\n\
                                            \Content-Type: text/plain; charset=utf-8\r\n\
                                            \\r\n\
                                            \testMessagePart" $ unpack $
    formatMessage testMessage { mBody = [ Left MessagePart { mpMime = "text/plain"
                                                           , mpContentType = "quoted-printable"
                                                           , mpEncoding = [("charset", "utf-8")]
                                                           , mpContent = "testMessagePart"
                                                           }
                                        ]
                              }

testFormatMultipartAlternative = TestCase $
  assertEqual "Unexpected message contents" "Date: Fri,  1 Jan 2016 00:00:00 +0000 (GMT)\r\n\
                                            \From: Mustermann, Hans <hans@mustermann.de>\r\n\
                                            \Subject: testSubject\r\n\
                                            \To: Mustermann, Fritz <fritz@mustermann.de>\r\n\
                                            \Reply-To: hans@mustermann.de\r\n\
                                            \In-Reply-To: NIL\r\n\
                                            \Message-Id: testMessageId\r\n\
                                            \MIME-Version: 1.0\r\n\
                                            \Content-Type: multipart/alternative;boundary=f18ee409-3ad9-5644-b251-92ab9e199a9d\r\n\
                                            \\r\n\
                                            \--f18ee409-3ad9-5644-b251-92ab9e199a9d\r\n\
                                            \Content-Type: text/plain; charset=iso-8859-1\r\n\
                                            \Content-Transfer-Encoding: quoted-printable\r\n\
                                            \\r\n\
                                            \plain text message\r\n\
                                            \\r\n\
                                            \--f18ee409-3ad9-5644-b251-92ab9e199a9d\r\n\
                                            \Content-Type: text/html; charset=iso-8859-1\r\n\
                                            \Content-Transfer-Encoding: quoted-printable\r\n\
                                            \\r\n\
                                            \<html><head /><body><i>html</i> message</body></html>\r\n\
                                            \\r\n\
                                            \--f18ee409-3ad9-5644-b251-92ab9e199a9d\r\n" $ unpack $
    formatMessage testMessage { mBody = [Right MessagePartGroup { grpMime = "multipart/alternative"
                                                               , grpParts = [ Left MessagePart { mpMime = "text/plain"
                                                                                               , mpContentType = "quoted-printable"
                                                                                               , mpEncoding = [("charset", "iso-8859-1")]
                                                                                               , mpContent = "plain text message"
                                                                                               }
                                                                            , Left MessagePart { mpMime = "text/html"
                                                                                               , mpContentType = "quoted-printable"
                                                                                               , mpEncoding = [("charset", "iso-8859-1")]
                                                                                               , mpContent = "<html><head /><body><i>html</i> message</body></html>"
                                                                                               }
                                                                            ]
                                                               }
                                        ]
                              }

testFormatMultipartMixed = TestCase $
  assertEqual "Unexpected message contents" "Date: Fri,  1 Jan 2016 00:00:00 +0000 (GMT)\r\n\
                                            \From: Mustermann, Hans <hans@mustermann.de>\r\n\
                                            \Subject: testSubject\r\n\
                                            \To: Mustermann, Fritz <fritz@mustermann.de>\r\n\
                                            \Reply-To: hans@mustermann.de\r\n\
                                            \In-Reply-To: NIL\r\n\
                                            \Message-Id: testMessageId\r\n\
                                            \MIME-Version: 1.0\r\n\
                                            \Content-Type: multipart/mixed;boundary=f18ee409-3ad9-5644-b251-92ab9e199a9d\r\n\
                                            \\r\n\
                                            \--f18ee409-3ad9-5644-b251-92ab9e199a9d\r\n\
                                            \Content-Type: text/plain; charset=iso-8859-1\r\n\
                                            \Content-Transfer-Encoding: quoted-printable\r\n\
                                            \\r\n\
                                            \plain text message\r\n\
                                            \\r\n\
                                            \--f18ee409-3ad9-5644-b251-92ab9e199a9d\r\n\
                                            \Content-Type: application/pdf\r\n\
                                            \Content-Transfer-Encoding: base64\r\n\
                                            \Content-Disposition: attachment; filename=\"test.pdf\"\r\n\
                                            \\r\n\
                                            \cGRmIGZpbGUgY29udGVudHM=\r\n\
                                            \\r\n\
                                            \--f18ee409-3ad9-5644-b251-92ab9e199a9d\r\n" $ unpack $
    formatMessage testMessage { mBody = [Right MessagePartGroup { grpMime = "multipart/mixed"
                                                               , grpParts = [ Left MessagePart { mpMime = "text/plain"
                                                                                               , mpContentType = "quoted-printable"
                                                                                               , mpEncoding = [("charset", "iso-8859-1")]
                                                                                               , mpContent = "plain text message"
                                                                                               }
                                                                            , Left MessagePart { mpMime = "application/pdf"
                                                                                               , mpContentType = "base64"
                                                                                               , mpEncoding = [("name", "test.pdf")]
                                                                                               -- "pdf file contents"
                                                                                               , mpContent = "cGRmIGZpbGUgY29udGVudHM="
                                                                                               }
                                                                            ]
                                                               }
                                        ]
                              }


-- helpers

testTime :: ZonedTime
testTime = fromJust $ parseTimeM False defaultTimeLocale "%Y-%m-%d %H:%M %z %Z" "2016-01-01 00:00 +0100 GMT"

nil = "NIL"

emptyAddress = Address { adName = nil
                       , adAdl = nil
                       , adMailbox = nil
                       , adHost = nil
                       }
