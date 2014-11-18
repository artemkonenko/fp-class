{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import System.Environment
import System.Directory
import System.FilePath
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative

data AppConfig = AppConfig {
      cfgMaxDepth :: Int
    } deriving (Show)

data AppState = AppState {
      stCurDepth :: Int,
      stCurPath :: FilePath
    } deriving (Show)

type AppLog = [(FilePath, Int)]

newtype MyApp a = MyA {
      runA :: ReaderT AppConfig (StateT AppState (WriterT AppLog IO)) a
    } deriving (Functor, Applicative, Monad,
                MonadIO,
                MonadReader AppConfig,
                MonadWriter [(FilePath, Int)],
                MonadState AppState)

runMyApp :: MyApp a -> Int -> FilePath -> IO (a, AppLog)
runMyApp app maxDepth path =
    let config = AppConfig maxDepth
        state = AppState 0 path
    in runWriterT (evalStateT (runReaderT (runA app) config) state)

listDirectory :: FilePath -> IO [String]
listDirectory = liftM (filter notDots) . getDirectoryContents
    where notDots p = p /= "." && p /= ".."

constrainedCount :: MyApp ()
constrainedCount = do
  maxDepth <- cfgMaxDepth `liftM` ask
  st <- get
  let curDepth = stCurDepth st
  when (curDepth < maxDepth) $ do
    let path = stCurPath st
    contents <- liftIO $ listDirectory $ path
    tell $ [(path, length contents)]
    forM_ contents $ \name -> do
     let newPath = path </> name
     let newDepth = curDepth + 1
     isDir <- liftIO $ doesDirectoryExist newPath
     when isDir $ do
       put $ st {stCurDepth = newDepth, stCurPath = newPath}
       constrainedCount

main = do
  [d, p] <- getArgs
  (_, xs) <- runMyApp constrainedCount (read d) p
  print xs


