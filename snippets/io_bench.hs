import System.Random
import Data.List
import Data.Time.Clock
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)

-- 処理時間を測る。特に I/O
-- 1000x1000 個の Int の出力
  -- String -> 3.1s
  -- ByteString -> 2.9s

type RandType = Int

main :: IO ()
main = do
  content <- contentGenBS 1000 1000
  benchIO $ BS.writeFile "o" content

contentGen :: Int -> Int -> IO String
contentGen row col = do
  stdGen <- getStdGen
  let rs = randoms stdGen :: [RandType]
  return . unlines . map unwords . take row . cut col $ map show rs where
    cut :: Int -> [String] -> [[String]]
    cut c = unfoldr (\xs -> case xs of
      [] -> Nothing
      _ -> Just (take c xs, drop c xs))

contentGenBS :: Int -> Int -> IO ByteString
contentGenBS row col = do
  stdGen <- getStdGen
  let rs = randoms stdGen :: [RandType]
  return . BS.unlines . map BS.unwords . take row . cut col $ map (BS.pack . show) rs where
    cut :: Int -> [ByteString] -> [[ByteString]]
    cut c = unfoldr (\xs -> case xs of
      [] -> Nothing
      _ -> Just (take c xs, drop c xs))

benchIO :: IO a -> IO ()
benchIO f = flip diffUTCTime <$> getCurrentTime <* f <*> getCurrentTime >>= print

-- bench :: a -> IO ()
-- bench f = flip diffUTCTime <$> getCurrentTime <* pure f <*> getCurrentTime >>= print
