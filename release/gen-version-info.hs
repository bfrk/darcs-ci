{- cabal:
    build-depends: base, process
-}
import System.Environment ( getArgs )
import System.IO ( hPutStrLn, stderr )
import System.IO.Error ( catchIOError )
import System.Process ( readProcess )

main :: IO ()
main = do
  args <- getArgs
  let version = case args of [] -> "."; arg:_ -> arg
  context <- darcs ["log", "-a", "--context"]
  writeFile "release/distributed-context" $ show context
  patches <- readInt <$> darcs ["log", "-a", "--from-tag", version, "--count"]
  writeFile "release/distributed-version" $ show patches

darcs :: [String] -> IO (Maybe String)
darcs args =
  (Just <$> readProcess "darcs" args "")
  `catchIOError` \e -> do
    hPutStrLn stderr $ "Warning: executing darcs failed: " ++ show e
    return Nothing

readInt :: Maybe String -> Maybe Int
readInt (Just s) | (n, _):_ <- reads s = Just (n - 1)
readInt _ = Nothing
