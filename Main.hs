{-# LANGUAGE LambdaCase #-}
import System.OSX.FSEvents
import System.Environment
import System.Process
import System.Directory
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.List (isInfixOf)

main :: IO ()
main = do
    allArgs <- getArgs

    when (null allArgs) $ 
        error "Usage: hard <command>. Watches the current directory/subdirectories and re-runs the <command>. Ignores files in your .gitignore"

    let (cmd:args) = allArgs

    ignorables <- lines <$> readFile ".gitignore"

    dir <- getCurrentDirectory

    let processImmediate = True
        ignoreSelfEvents = True
        fileLevel = True
    currentProc <- newMVar =<< start cmd args
    eventStreamCreate [dir] 0.1 processImmediate ignoreSelfEvents fileLevel $ \event -> do
        let path = eventPath event
        let ignorable = or (map (`isInfixOf` path) ignorables)
        if ignorable 
            then putStrLn $ "Ignoring event: " ++ show path
            else do
                tryTakeMVar currentProc >>= \case
                    -- waitForProcess cleans up the handle
                    Just process -> interruptProcessGroupOf process >> waitForProcess process >> return ()
                    _ -> return () 
                putMVar currentProc =<< start cmd args
    forever $ threadDelay 1000000

-- We start the process in a group such that interruptProcessGroupOf will also terminate any child processes
start :: FilePath -> [String] -> IO ProcessHandle
start cmd args = do
    (_,_,_,p) <- createProcess ((proc cmd args) {delegate_ctlc=False, create_group=True})
    return p