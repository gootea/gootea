module TorrentSpec where

import qualified Data.BEncode as BE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Network.Socket
import Test.HUnit
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Torrent
import Network.Bittorrent.LPMessage

spec :: Spec
spec = do
  describe "Data.Torrent" $ do
    it "should parse a torrent with a single file" testParseSingleFileTorrent
    it "should parse a torrent with multiple files" testParseMultifileTorrent

simpleTorrentBytes = BSC.pack "d4:name14:MySuperTorrente"

simpleTorrentName = BSC.pack "MySuperTorrent"

multifileTorrentBytes =
  BSC.pack
    "d5:filesld4:pathl6:subdir8:file.txteed4:pathl9:file2.logeee4:name9:directorye"

multifileTorrentName = BSC.pack "directory"

multifileTorrentFile1 = [BSC.pack "subdir", BSC.pack "file.txt"]

multifileTorrentFile2 = [BSC.pack "file2.log"]

testParseSingleFileTorrent = torrent `shouldBe` expected
  where
    torrent = BE.decode simpleTorrentBytes
    expected =
      Right $ Metainfo {mName = Just simpleTorrentName, mFiles = Nothing}

testParseMultifileTorrent = torrent `shouldBe` expected
  where
    torrent = BE.decode multifileTorrentBytes
    files = [FileInfo multifileTorrentFile1, FileInfo multifileTorrentFile2]
    expected =
      Right $ Metainfo {mName = Just multifileTorrentName, mFiles = Just files}
