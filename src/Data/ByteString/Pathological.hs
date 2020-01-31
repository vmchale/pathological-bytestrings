module Data.ByteString.Pathological ( nonstandardRead
                                    , chunksOf
                                    , readLarge
                                    , readSmall
                                    , readRandom
                                    ) where

import           Control.Applicative
import           Control.Monad        ((<=<))
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           System.Random        (randomRIO)

-- | Read a file into a 'BSL.ByteString' using varying chunk size.
nonstandardRead :: FilePath -> IO BSL.ByteString
nonstandardRead fp = do
    bStrict <- BS.readFile fp
    let (h, t) = BS.splitAt (64 * 1024) bStrict
    pure $ BSL.fromChunks [h, t]

-- | Read into a 'BSL.ByteString' of varying chunk sizes (non-lazily).
readRandom :: FilePath -> IO BSL.ByteString
readRandom = fmap BSL.fromChunks . (randomSplit <=< BS.readFile)

randomSplit :: BS.ByteString -> IO [BS.ByteString]
randomSplit b = do
    nextSz <- randomRIO (16 * 1024, 128 * 1024)
    if BS.length b <= nextSz
        then pure [b]
        else
            let (b', b'') = BS.splitAt nextSz b
            in (b' :) <$> randomSplit b''

-- | Read into @16k@ chunks (non-lazily)
readSmall :: FilePath -> IO BSL.ByteString
readSmall = fmap (BSL.fromChunks . chunksOf (16 * 1024)) . BS.readFile

-- | Read into @128k@ chunks (non-lazily)
readLarge :: FilePath -> IO BSL.ByteString
readLarge = fmap (BSL.fromChunks . chunksOf (128 * 1024)) . BS.readFile

chunksOf :: Int -> BS.ByteString -> [BS.ByteString]
chunksOf bSz b =
    if BS.length b <= bSz
        then [b]
        else
            let (b', b'') = BS.splitAt bSz b
            in b' : chunksOf bSz b''
