import           Test.HUnit

import           FormatSpec
import           ParsersSpec
import           SyncSpec

main = runTestTT tests

tests = TestList [ TestLabel "login and logout from IMAP servers" testRsyncImap
                 , TestLabel "parse - tag list" testListResponse
                 , TestLabel "parse - tag select - non existant" testSelectFail
                 , TestLabel "parse - tag select - mailbox details" testSelectSuccess
                 , TestLabel "parse - tag uid search - found many" testSearchResponse
                 , TestLabel "parse - tag uid search - found none" testSearchNoResponse
                 , TestLabel "parse - tag uid fetch - simple text" testFetchSimpletext
                 , TestLabel "parse - tag uid fetch - plain text" testFetchPlaintext
                 , TestLabel "parse - tag uid fetch - multipart/alternative" testFetchMultipartAlternative
                 , TestLabel "parse - tag uid fetch - multipart/related" testFetchMultipartRelated
                 , TestLabel "parse - tag uid fetch - multipart/mixed" testFetchMultipartMixed
                 , TestLabel "parse - tag uid fetch - multipart/alternative+related" testFetchMultipartAlternativeRelated
                 , TestLabel "parse - tag uid fetch - message/rfc822" testFetchEmbeddedMessage
                 , TestLabel "format message - text/plain" testFormatPlaintext
                 , TestLabel "format message - multipart/alternative" testFormatMultipartAlternative
                 , TestLabel "format message - multipart/mixed" testFormatMultipartMixed
                 ]
