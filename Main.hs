{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
import System.OSX.FSEvents
import System.Environment
import System.Process
import System.Directory
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Exception
import System.FilePath
import Data.List (isInfixOf)

main :: IO ()
main = do
    allArgs <- getArgs

    when (null allArgs) $ 
        error $ unlines 
            [ "Usage: hard <command>." 
            , "Watches the current directory/subdirectories and re-runs the <command>."
            , "Ignores files in your .gitignore" 
            , "Create a .hard file with a newline-separated list of .extensions to only"
            , "monitor files with those extensions" ]

    let (cmd:args) = allArgs

    ignorables <- (".git":) . lines <$> readFile ".gitignore"
    extensions <- lines <$> readFile ".hard" `catch` (\(_e::IOException) -> return "")
    when (null extensions) $ 
        putStrLn "No .hard file found. Consider creating one to opt-in to only certain extensions."

    dir <- getCurrentDirectory

    let processImmediate = True
        ignoreSelfEvents = True
        fileLevel = True
    currentProc <- newMVar =<< start cmd args
    eventStreamCreate [dir] 0.1 processImmediate ignoreSelfEvents fileLevel $ \event -> do
        let path = eventPath event
            extension = takeExtension path
            ignorable = or (map (`isInfixOf` path) ignorables)
            includable = or (map (== extension) extensions) || null extensions
        when (not ignorable && includable) $ do
            whenJustM_ (tryTakeMVar currentProc) killProcess
            putMVar currentProc =<< start cmd args
    forever $ threadDelay 1000000

killProcess :: ProcessHandle -> IO ()
killProcess process = whenNothingM_ (getProcessExitCode process) $ do
    interruptProcessGroupOf process 
        `catch` (\e -> print (e :: IOException))
    -- Clean up the handle
    waitForProcess process

whenJustM_ :: Monad m => m (Maybe t) -> (t -> m a) -> m ()
whenJustM_ actionA actionB = actionA >>= \case
    Just x -> actionB x >> return ()
    _      -> return ()

whenNothingM_ :: Monad m => m (Maybe t) -> m a -> m ()
whenNothingM_ actionA actionB = actionA >>= \case
    Nothing -> actionB >> return ()
    _       -> return ()

-- We start the process in a group such that 
-- interruptProcessGroupOf will also terminate any child processes
start :: FilePath -> [String] -> IO ProcessHandle
start cmd args = do
    (_,_,_,p) <- createProcess ((proc cmd args) {delegate_ctlc=False, create_group=True})
    return p