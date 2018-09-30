module TorrentSpec where

import qualified Data.BEncode as BE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Either
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
    it
      "should list file for a single file torrent"
      testListFilesSingleFileTorrent
    it "should list file for a multi file torrent" testListFilesMultiFileTorrent

-- Simple Torrent with a single file
simpleTorrentBytes = BSC.pack "d4:name14:MySuperTorrente"

simpleTorrentName = BSC.pack "MySuperTorrent"

simpleTorrent = Metainfo {mName = Just simpleTorrentName, mFiles = Nothing}

-- Torrent with multiple files
multifileTorrentBytes =
  BSC.pack
    "d5:filesld4:pathl6:subdir8:file.txteed4:pathl9:file2.logeee4:name9:directorye"

multifileTorrentName = BSC.pack "directory"

multifileTorrentFile1 = [BSC.pack "subdir", BSC.pack "file.txt"]

multifileTorrentFile2 = [BSC.pack "file2.log"]

multifileTorrent =
  Metainfo {mName = Just multifileTorrentName, mFiles = Just files}
  where
    files = [FileInfo multifileTorrentFile1, FileInfo multifileTorrentFile2]

testParseSingleFileTorrent = torrent `shouldBe` Right simpleTorrent
  where
    torrent = BE.decode simpleTorrentBytes

testParseMultifileTorrent = torrent `shouldBe` Right multifileTorrent
  where
    torrent = BE.decode multifileTorrentBytes

testListFilesSingleFileTorrent = files `shouldBe` expectedFiles
  where
    files = listFiles simpleTorrent
    expectedFiles = [BSC.pack "MySuperTorrent"]

testListFilesMultiFileTorrent = files `shouldBe` expectedFiles
  where
    files = listFiles multifileTorrent
    expectedFiles =
      [BSC.pack "directory/subdir/file.txt", BSC.pack "directory/file2.log"]
