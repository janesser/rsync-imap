{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module ParsersSpec where

import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString.Char8            hiding (count, head, length,
                                                   null)
import           Data.Either
import           Data.Maybe
import           IMAP.Parsers
import           IMAP.Types
import           Test.HUnit
import           Debug.Trace

testListResponse = TestCase $
  parseTest' "* LIST (\\HasNoChildren) \".\" \"INBOX.Yahoo\"\r\n\
  \* LIST (\\HasNoChildren) \".\" \"INBOX.Work\"\r\n\
  \* LIST (\\HasNoChildren) \".\" \"INBOX.GMX.dsa\"\r\n\
  \* LIST (\\HasNoChildren) \".\" \"INBOX.Drafts\"\r\n\
  \* LIST (\\Marked \\HasChildren) \".\" \"INBOX\"\r\n\
  \* LIST (\\HasNoChildren) \".\" \"INBOX.Archives.2014\"\r\n\
  \* LIST (\\HasNoChildren) \".\" \"INBOX.Sent\"\r\n\
  \* LIST (\\HasChildren) \".\" \"INBOX.Archives\"\r\n\
  \* LIST (\\HasNoChildren) \".\" \"INBOX.Trash\"\r\n\
  \* LIST (\\HasChildren) \".\" \"INBOX.GMX\"\r\n\
  \tag OK LIST completed\r\n" parseListResponse (assertEqual "Should have found 10 mailboxes." 10 . length)

testSelectFail = TestCase $
  parseTest' "tag NO Mailbox doesn't exist: INBOX2" (parseSelectResponse testMailbox) (assertBool "Mailbox should not exist." . not . mbExists)

testSelectSuccess = TestCase $
  parseTest' "* OK [CLOSED] Previous mailbox closed.\r\n\
    \* FLAGS (\\Answered \\Flagged \\Deleted \\Seen \\Draft)\r\n\
    \* OK [PERMANENTFLAGS (\\Answered \\Flagged \\Deleted \\Seen \\Draft \\*)] Flags permitted.\r\n\
    \* 28 EXISTS\r\n\
    \* 0 RECENT\r\n\
    \* OK [UIDVALIDITY 1494870189] UIDs valid\r\n\
    \* OK [UIDNEXT 32] Predicted next UID\r\n\
    \* OK [HIGHESTMODSEQ 1] Highest\r\n\
    \tag OK [READ-WRITE] Select completed.\r\n" (parseSelectResponse testMailbox) (assertBool "Mailbox should exist." . mbExists)

testSelectSuccess2 = TestCase $
  parseTest' "* FLAGS (\\Answered \\Flagged \\Deleted \\Seen \\Draft)\r\n\
    \* OK [PERMANENTFLAGS (\\Answered \\Flagged \\Deleted \\Seen \\Draft \\*)] Flags permitted.\r\n\
    \* 36 EXISTS\r\n\
    \* 0 RECENT\r\n\
    \* OK [UIDVALIDITY 1494870193] UIDs valid\r\n\
    \* OK [UIDNEXT 143] Predicted next UID\r\n\
    \* OK [HIGHESTMODSEQ 153] Highest\r\n\
    \tag OK [READ-WRITE] Select completed (0.000 secs)." (parseSelectResponse testMailbox) (assertBool "Mailbox should exist." . mbExists)

testSearchResponse = TestCase $
  parseTest' "* SEARCH 123 234\r\n\
    \tag OK Search completed (0.030 secs).\r\n" parseUidSearchResponse $ assertBool "Should have found message uids 123 and 234." . (\res -> res == [123, 234])

testSearchNoResponse= TestCase $
  parseTest' "* SEARCH\r\n\
    \tag OK Search completed (0.030 secs).\r\n" parseUidSearchResponse $ assertBool "Empty search result expected." . null

{-
* 323 FETCH (UID 1789 FLAGS (\Seen NonJunk) INTERNALDATE "13-Feb-2009 09:31:11 +0100" RFC822.SIZE 28493 ENVELOPE ("Wed, 10 Dec 2008 00:00:25 +0100" "Re: AW: DSA diese Woche" (("Jan Esser" NIL "JEsser" "gmx.de")) (("Jan Esser" NIL "JEsser" "gmx.de")) (("Jan Esser" NIL "JEsser" "gmx.de")) (("Daniel Loeckemann" NIL "Daniel.Loeckemann" "gmx.de")) (("\"Markus\" =?ISO-8859-1?Q?S=E4misch=22?=" NIL "m.saemisch" "yahoo.de")(NIL NIL "o.klostermann" "gmx.de")(NIL NIL "moonstone23" "gmx.de")(NIL NIL "77.BASSPUNK" "web.de")(NIL NIL "robinschmidt" "online.de")(NIL NIL "g.czudnochowski" "t-online.de")) NIL "<20081209200053.296120@gmx.net>" "<1228863622.4644.5.camel@penelope64.MN>") BODY ("text" "plain" ("charset" "utf-8") NIL NIL "quoted-printable" 27416 816))
tag OK FETCH completed.

-}
testFetchPlaintext = TestCase $
  parseTest' "* 1 FETCH (UID 1 FLAGS (\\Seen) INTERNALDATE \"11-Dec-2011 13:11:05 +0100\" RFC822.SIZE 4003 \
    \ENVELOPE (\"Wed, 07 Dec 2011 13:28:25 +0100\" \"Re: Drachenchronik; =?UTF-8?B?RHJhY2hlbmTDpG1tZXJ1bmc7IEFtIFJhbmQ=?= =?UTF-8?B?ZSBkZXIgVW5zdGVyYmxpY2hrZWl0IElJ?=\" \
        \((\"Oliver Klostermann\" NIL \"o.klostermann\" \"gmx.de\")) \
        \((\"Oliver Klostermann\" NIL \"o.klostermann\" \"gmx.de\")) \
        \((\"Oliver Klostermann\" NIL \"o.klostermann\" \"gmx.de\")) \
        \((\"Ingo Haltermann\" NIL \"moonstone23\" \"gmx.de\")) \
        \((\"Robin Schmidt\" NIL \"robinschmidt\" \"online.de\")(NIL NIL \"Daniel.Loeckemann\" \"gmx.de\")(NIL NIL \"g.czudnochowski\" \"t-online.de\")(NIL NIL \"JEsser\" \"gmx.de\")(\"\\\"Markus\\\" =?ISO-8859-1?Q?S=E4misch=22?=\" NIL \"m.saemisch\" \"yahoo.de\")(NIL NIL \"d.pappenheim\" \"gmx.de\")) \
        \NIL \
        \\"<20111207121958.277030@gmx.net>\" \
        \\"<4EDF5BE9.7060706@gmx.de>\") \
    \BODY (\"text\" \"plain\" (\"charset\" \"utf-8\" \"format\" \"flowed\") NIL NIL \"quoted-printable\" 2330 72))\r\n\
    \tag OK FETCH completed.\r\n" parseMessage $
    assertEqual "Unexpected Message-Id." "<4EDF5BE9.7060706@gmx.de>" . mMessageId

testFetchSimpletext = TestCase $
  parseTest' "* 16 FETCH (UID 20 FLAGS (\\Seen) INTERNALDATE \"07-Dec-2011 13:28:25 +0100\" RFC822.SIZE 3017 \
             \ENVELOPE (\"Wed,  7 Dec 2011 13:28:25 +0100 (+0100)\" \"Re: Drachenchronik; =?UTF-8?B?RHJhY2hlbmTDpG1tZXJ1bmc7IEFtIFJhbmQ=?= =?UTF-8?B?ZSBkZXIgVW5zdGVyYmxpY2hrZWl0IElJ?=\" ((\"Oliver Klostermann\" NIL \"o.klostermann\" \"gmx.de\")) ((\"Oliver Klostermann\" NIL \"o.klostermann\" \"gmx.de\")) ((\"Oliver Klostermann\" NIL \"o.klostermann\" \"gmx.de\")) ((\"Ingo Haltermann\" NIL \"moonstone23\" \"gmx.de\")) ((\"Robin Schmidt\" NIL \"robinschmidt\" \"online.de\")(\"NIL\" NIL \"Daniel.Loeckemann\" \"gmx.de\")(\"NIL\" NIL \"g.czudnochowski\" \"t-online.de\")(\"NIL\" NIL \"JEsser\" \"gmx.de\")(\"NIL\" NIL \"m.saemisch\" \"yahoo.de\")(\"NIL\" NIL \"d.pappenheim\" \"gmx.de\")) NIL \"<20111207121958.277030@gmx.net>\" \"<1498591297.5990.3.camel@gmx.de>\") \
             \BODY (\"quoted-printable\" \"\" NIL NIL NIL \"8bit\" 2326))\r\n\
             \tag OK FETCH completed.\r\n" parseMessage $
  assertEqual "Unexpected MIME type." "quoted-printable" . (either mpMime grpMime) . head . mBody

{-
* 2585 FETCH (UID 6719 FLAGS (\Seen) INTERNALDATE "15-Dec-2010 21:53:39 +0100" RFC822.SIZE 323529 ENVELOPE ("Wed, 1 Dec 2010 00:49:52 +0100" "[Studierende-l] Grenzfrei - Dokumentarfilmabende zu Flucht, Migration und Rassismus (2.12 und 6.12.)" (("Christoph Creutziger" NIL "c_creu01" "uni-muenster.de")) ((NIL NIL "studierende-l-bounces" "listserv.uni-muenster.de")) (("Christoph Creutziger" NIL "c_creu01" "uni-muenster.de")) ((NIL NIL "studierende-l" "listserv.uni-muenster.de")) NIL NIL NIL "<AANLkTimO=Z1+u9gm8=7wMXBNzkP=L4DdK-obQVHPGB9b@mail.gmail.com>")
BODY (
  (
    ("text" "plain" ("charset" "windows-1252") NIL NIL "quoted-printable" 1358 37)
    ("text" "html" ("charset" "windows-1252") NIL NIL "quoted-printable" 1641 26)
  "alternative")
  ("application" "pdf" ("name" "grenzfrei_a4.pdf") NIL NIL "base64" 315686)
  ("text" "plain" ("charset" "iso-8859-1") NIL NIL "7bit" 181 4)
"mixed"))

tag OK FETCH completed.
-}
testFetchMultipartAlternative = TestCase $
  parseTest' "* 2585 FETCH (UID 6719 FLAGS (\\Seen) INTERNALDATE \"15-Dec-2010 21:53:39 +0100\" RFC822.SIZE 323529 \
  \ENVELOPE (\
    \\"Wed, 1 Dec 2010 00:49:52 +0100\" \
    \\"[Studierende-l] Grenzfrei - Dokumentarfilmabende zu Flucht, Migration und Rassismus (2.12 und 6.12.)\" \
    \((\"Christoph Creutziger\" NIL \"c_creu01\" \"uni-muenster.de\")) \
    \((NIL NIL \"studierende-l-bounces\" \"listserv.uni-muenster.de\")) \
    \((\"Christoph Creutziger\" NIL \"c_creu01\" \"uni-muenster.de\")) \
    \((NIL NIL \"studierende-l\" \"listserv.uni-muenster.de\")) NIL NIL NIL \
    \\"<AANLkTimO=Z1+u9gm8=7wMXBNzkP=L4DdK-obQVHPGB9b@mail.gmail.com>\") \
  \BODY (\
    \(\
      \(\"text\" \"plain\" (\"charset\" \"windows-1252\") NIL NIL \"quoted-printable\" 1358 37)\
      \(\"text\" \"html\" (\"charset\" \"windows-1252\") NIL NIL \"quoted-printable\" 1641 26) \
    \\"alternative\")\
    \(\"application\" \"pdf\" (\"name\" \"grenzfrei_a4.pdf\") NIL NIL \"base64\" 315686)\
    \(\"text\" \"plain\" (\"charset\" \"iso-8859-1\") NIL NIL \"7bit\" 181 4) \
  \\"mixed\"))\r\n\
  \tag OK FETCH completed.\r\n" parseMessage $
  assertEqual "Message-Parts should be parsed." 1 . length . mBody
{-
parsePartHeaders 0 [
Right (MessagePartGroup {grpParts = [
  Right (MessagePartGroup {grpParts = [
    Left (MessagePart {mpMime = "text/plain", mpEncoding = [("charset","windows-1252")], mpContentType = "quoted-printable", mpContent = "", mpContentSize = 1358, mpContentLines = 37}),
    Left (MessagePart {mpMime = "text/html", mpEncoding = [("charset","windows-1252")], mpContentType = "quoted-printable", mpContent = "", mpContentSize = 1641, mpContentLines = 26})],
  grpMime = "alternative"}),

  Left (MessagePart {mpMime = "application/pdf", mpEncoding = [("name","grenzfrei_a4.pdf")], mpContentType = "base64", mpContent = "", mpContentSize = 315686, mpContentLines = 0}),
  Left (MessagePart {mpMime = "text/plain", mpEncoding = [("charset","iso-8859-1")], mpContentType = "7bit", mpContent = "", mpContentSize = 181, mpContentLines = 4})],
grpMime = "mixed"})]
-}

testFetchMultipartRelated = TestCase $
  parseTest' "* 72 FETCH (UID 415 FLAGS (\\Seen NotJunk) INTERNALDATE \"13-Feb-2009 08:28:38 +0100\" RFC822.SIZE 642773 \
             \ENVELOPE (\"Fri, 5 Sep 2008 11:48:23 +0200\" \"FW: LexisNexis Party 2008\" ((\"Boehmert, Carsten  (LNG-MUE)\" NIL \"Carsten.Boehmert\" \"lexisnexis.de\")) ((\"Boehmert, Carsten  (LNG-MUE)\" NIL \"Carsten.Boehmert\" \"lexisnexis.de\")) ((\"Boehmert, Carsten  (LNG-MUE)\" NIL \"Carsten.Boehmert\" \"lexisnexis.de\")) ((NIL NIL \"\" NIL)(NIL NIL \"\" NIL)) NIL NIL NIL \"<E74BF4D485115048AF3A7D77FA7A756105379D89@lngmueexcp001.legal.regn.net>\") \
             \BODY (\
               \(\
                 \(\
                   \(\"text\" \"plain\" (\"charset\" \"utf-8\") NIL NIL \"quoted-printable\" 3106 99)\
                   \(\"text\" \"html\" (\"charset\" \"utf-8\") NIL NIL \"quoted-printable\" 24600 818) \"alternative\")\
                   \(\"image\" \"jpeg\" (\"name\" \"image001.jpg\") \"<image001.jpg@01C90F4D.4CA8D040>\" \"image001.jpg\" \"base64\" 4688)\
                   \(\"image\" \"jpeg\" (\"name\" \"image002.jpg\") \"<image002.jpg@01C90F4D.4CA8D040>\" \"image002.jpg\" \"base64\" 6436)\
                 \ \"related\")\
                 \(\"application\" \"octet-stream\" (\"name\" \"FM-Party 2008.pdf\") NIL \"FM-Party 2008.pdf\" \"base64\" 181190)\
                 \(\"application\" \"octet-stream\" (\"name\" \"TuS Saxonia.pdf\") NIL \"TuS Saxonia.pdf\" \"base64\" 419476)\
               \ \"mixed\"))\r\n\
               \tag OK FETCH completed.\r\n" parseMessage $
  assertEqual "Message-Parts should be parsed." 1 . length . mBody
{-
parsePartHeaders 0 [
Right (MessagePartGroup {grpParts = [
  Right (MessagePartGroup {grpParts = [
    Right (MessagePartGroup {grpParts = [
      Left (MessagePart {mpMime = "text/plain", mpEncoding = [("charset","utf-8")], mpContentType = "quoted-printable", mpContent = "", mpContentSize = 3106, mpContentLines = 99}),
      Left (MessagePart {mpMime = "text/html", mpEncoding = [("charset","utf-8")], mpContentType = "quoted-printable", mpContent = "", mpContentSize = 24600, mpContentLines = 818})],
    grpMime = "alternative"}),
    Left (MessagePart {mpMime = "image/jpeg", mpEncoding = [("name","image001.jpg")], mpContentType = "base64", mpContent = "", mpContentSize = 4688, mpContentLines = 0}),
    Left (MessagePart {mpMime = "image/jpeg", mpEncoding = [("name","image002.jpg")], mpContentType = "base64", mpContent = "", mpContentSize = 6436, mpContentLines = 0})],
  grpMime = "related"}),
  Left (MessagePart {mpMime = "application/octet-stream", mpEncoding = [("name","FM-Party 2008.pdf")], mpContentType = "base64", mpContent = "", mpContentSize = 181190, mpContentLines = 0}),
  Left (MessagePart {mpMime = "application/octet-stream", mpEncoding = [("name","TuS Saxonia.pdf")], mpContentType = "base64", mpContent = "", mpContentSize = 419476, mpContentLines = 0})],
grpMime = "mixed"})]
-}

testFetchMultipartMixed = TestCase $
  parseTest' "* 1 FETCH (UID 1 FLAGS (\\Seen) INTERNALDATE \"16-Jun-2015 20:09:46 +0200\" RFC822.SIZE 1204291 \
             \ENVELOPE (\"Mon, 01 Jun 2015 08:23:47 +0200\" \"eci init\" ((\"Jan Esser\" NIL \"JEsser\" \"gmx.de\")) ((\"Jan Esser\" NIL \"JEsser\" \"gmx.de\")) ((\"Jan Esser\" NIL \"JEsser\" \"gmx.de\")) ((NIL NIL \"jan.esser\" \"mgm-tp.com\")) NIL NIL NIL \"<1433139827.3384.0.camel@N5>\") \
             \BODY (\
             \(\"text\" \"plain\" NIL NIL NIL \"7bit\" 14 1)\
             \(\"text\" \"x-log\" (\"name\" \"init5.log\" \"charset\" \"utf-8\") NIL NIL \"7bit\" 236086 1802)\
             \(\"text\" \"x-log\" (\"name\" \"init4.log\" \"charset\" \"utf-8\") NIL NIL \"7bit\" 238317 1812)\
             \(\"text\" \"x-log\" (\"name\" \"init3.log\" \"charset\" \"utf-8\") NIL NIL \"7bit\" 239024 1815)\
             \(\"text\" \"x-log\" (\"name\" \"init2.log\" \"charset\" \"utf-8\") NIL NIL \"7bit\" 234738 1748)\
             \(\"text\" \"x-log\" (\"name\" \"init.log\" \"charset\" \"utf-8\") NIL NIL \"7bit\" 254631 1867) \"mixed\"))\r\n\
             \tag OK FETCH completed.\r\n" parseMessage $
  assertEqual "Message-Parts should be parsed." 1 . length . mBody
{-
parsePartHeaders 0 [
Right (MessagePartGroup {grpParts = [
  Left (MessagePart {mpMime = "text/plain", mpEncoding = [], mpContentType = "7bit", mpContent = "", mpContentSize = 14, mpContentLines = 1}),
  Left (MessagePart {mpMime = "text/x-log", mpEncoding = [("name","init5.log"),("charset","utf-8")], mpContentType = "7bit", mpContent = "", mpContentSize = 236086, mpContentLines = 1802}),
  Left (MessagePart {mpMime = "text/x-log", mpEncoding = [("name","init4.log"),("charset","utf-8")], mpContentType = "7bit", mpContent = "", mpContentSize = 238317, mpContentLines = 1812}),
  Left (MessagePart {mpMime = "text/x-log", mpEncoding = [("name","init3.log"),("charset","utf-8")], mpContentType = "7bit", mpContent = "", mpContentSize = 239024, mpContentLines = 1815}),
  Left (MessagePart {mpMime = "text/x-log", mpEncoding = [("name","init2.log"),("charset","utf-8")], mpContentType = "7bit", mpContent = "", mpContentSize = 234738, mpContentLines = 1748}),
  Left (MessagePart {mpMime = "text/x-log", mpEncoding = [("name","init.log"),("charset","utf-8")], mpContentType = "7bit", mpContent = "", mpContentSize = 254631, mpContentLines = 1867})],
grpMime = "mixed"})]
-}

testFetchMultipartAlternativeRelated = TestCase $
  parseTest' "* 118 FETCH (UID 710 FLAGS (\\Seen) \
\INTERNALDATE \"13-Feb-2009 08:42:06 +0100\" RFC822.SIZE 377654 \
\ENVELOPE (\"Wed, 19 Nov 2008 23:17:14 +0100\" \"=?WINDOWS-1252?Q?[flat_erique_news]_Mi,_26.11._|_Gleis_22|_Konze?=\t=?WINDOWS-1252?Q?rt_|_Belleruche_=28UK=29_|_Flat_Erique_&_The_Be?=\t=?WINDOWS-1252?Q?at_Explorers_|_Bl=FCmel_?=\" ((\"WOLFRAM CATENHUSEN\" NIL \"wolfram.catenhusen\" \"gmx.de\")) ((\"WOLFRAM CATENHUSEN\" NIL \"wolfram.catenhusen\" \"gmx.de\")) ((\"WOLFRAM CATENHUSEN\" NIL \"wolfram.catenhusen\" \"gmx.de\")) ((NIL NIL \"flaterique\" \"luna-bar.org\")) NIL NIL NIL \"<B61A93F4-A7A7-4DA9-8C16-4B9D5C3A1A80@gmx.de>\") \
\BODY \
\(\
  \(\"text\" \"plain\" (\"charset\" \"windows-1252\" \"format\" \"flowed\" \"delsp\" \"yes\") NIL NIL \"quoted-printable\" 2022 62)\
  \(\
    \(\"text\" \"html\" (\"charset\" \"windows-1252\") NIL NIL \"quoted-printable\" 10817 210)\
    \(\"image\" \"jpeg\" (\"x-unix-mode\" \"0664\" \"x-apple-mail-type\" \"stationery\" \"name\" \"top.jpg\") \"<2F80CE4F-138C-4514-B26C-131D9134226C/top.jpg>\" NIL \"base64\" 14028)\
    \(\"image\" \"png\" (\"x-unix-mode\" \"0666\" \"x-apple-mail-type\" \"stationery\" \"name\" \"Photos.png\") \"<2F80CE4F-138C-4514-B26C-131D9134226C/2/Photos>\" NIL \"base64\" 265522)\
    \(\"image\" \"jpeg\" (\"x-unix-mode\" \"0664\" \"x-apple-mail-type\" \"stationery\" \"name\" \"bottom.jpg\") \"<2F80CE4F-138C-4514-B26C-131D9134226C/bottom.jpg>\" NIL \"base64\" 24428)\
    \(\"image\" \"jpeg\" (\"x-unix-mode\" \"0664\" \"x-apple-mail-type\" \"stationery\" \"name\" \"bg_pattern.jpg\") \"<2F80CE4F-138C-4514-B26C-131D9134226C/bg_pattern.jpg>\" NIL \"base64\" 29724)\
    \(\"image\" \"jpeg\" (\"x-unix-mode\" \"0664\" \"x-apple-mail-type\" \"stationery\" \"name\" \"bg_letter.jpg\") \"<2F80CE4F-138C-4514-B26C-131D9134226C/bg_letter.jpg>\" NIL \"base64\" 28036)\
  \ \"related\")\
\ \"alternative\"))\r\n\
\tag OK FETCH completed.\r\n" parseMessage $
    assertEqual "Message-Parts should be parsed" 1 . length . mBody

testFetchEmbeddedMessage = TestCase $
  parseTest' "* 352 FETCH (UID 1914 FLAGS (\\Seen NotJunk) \
             \INTERNALDATE \"13-Feb-2009 09:34:46 +0100\" RFC822.SIZE 4274 \
             \ENVELOPE (\"Mon, 26 Nov 2007 09:30:06 +0000\" \"Bug#432348 closed by =?UTF-8?Q?Rapha=C3=ABl?= Enrici \t<blacknoz@club-internet.fr> (pgadmin3 crashes under particular  conditions.)\" ((\"Debian Bug Tracking System\" NIL \"owner\" \"bugs.debian.org\")) ((\"Debian BTS\" NIL \"debbugs\" \"rietz.debian.org\")) ((NIL NIL \"432348\" \"bugs.debian.org\")) ((\"JE\" NIL \"mod77\" \"gmx.net\")) NIL NIL NIL \"<handler.432348.D432348.119606934428814.notifdone@bugs.debian.org>\") \
             \BODY (\
             \(\"text\" \"plain\" (\"charset\" \"utf-8\") NIL NIL \"quoted-printable\" 555 16)\
             \(\"message\" \"rfc822\" NIL NIL NIL \"7bit\" 1853 \
               \(\"Mon, 26 Nov 2007 10:28:46 +0100\" \"pgadmin3 crashes under particular conditions.\" \
                 \((\"=?ISO-8859-1?Q?Rapha=EBl_Enrici?=\" NIL \"blacknoz\" \"club-internet.fr\")) \
                 \((\"=?ISO-8859-1?Q?Rapha=EBl_Enrici?=\" NIL \"blacknoz\" \"club-internet.fr\")) \
                 \((\"=?ISO-8859-1?Q?Rapha=EBl_Enrici?=\" NIL \"blacknoz\" \"club-internet.fr\")) \
                 \((NIL NIL \"432348-done\" \"bugs.debian.org\")(NIL NIL \"444579-done\" \"bugs.debian.org\")) NIL NIL NIL \"<474A91CE.9000004@club-internet.fr>\") \
                   \(\"text\" \"plain\" (\"charset\" \"iso-8859-1\" \"format\" \"flowed\") NIL NIL \"base64\" 407 7) 35)\
             \ \"mixed\"))\r\n\
\tag OK FETCH completed.\r\n" parseMessage $
  assertEqual "Message-Parts should be parsed" 1 . length . mBody


-- helpers

testMailbox = Mailbox { mbName = "test.INBOX" }

parseTest' :: Show t => String -> Parser t -> (t -> Assertion) -> Assertion
parseTest' input parseFunc assertFunc = do
  let bInput = pack input
  let parsed = parseOnly parseFunc bInput
  -- traceM $ show parsed
  assertFunc' parsed 
    where
      assertFunc' (Right x)  = assertFunc x
      assertFunc' (Left err) = assertFailure err
