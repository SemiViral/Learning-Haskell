import Data.List
import Control.Arrow
import Control.Monad.Reader
import Control.Exception
import Network
import System.IO
import System.Exit
import Text.Printf

server = "irc6.foonetic.net"
port = 6667
chan = "#testgrounds"
nick = "testing_haskell"

data Bot = Bot { socket :: Handle }
type Net = ReaderT Bot IO

main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop st    = runReaderT run st

connect :: IO Bot
connect = notify $ do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return (Bot h)
  where
    notify a = bracket_
      (printf "Connection to %s..." server >> hFlush stdout)
      (putStrLn "done.")
      a

run :: Net ()
run = do
  write "NICK" nick
  write "USER" (nick ++ " 0 * : Haskell Testbot")
  asks socket >>= listen

listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    io (putStrLn s)
    if ping s
    then pong s
    else if endMotd s
      then do
        write "MODE" "+B"
        write "JOIN" chan
      else eval (clean s)
  where
    forever a = do a; forever a

    clean     = drop 1 . dropWhile (/=':') . drop 1

    ping x = "PING :" `isPrefixOf` x
    pong x = write "PONG" (':' : drop 6 x)
    
    endMotd x = "376" `isInfixOf` x && ":End of" `isInfixOf` x

eval :: String -> Net ()
eval     "!quit"              = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
eval x | "!id" `isPrefixOf` x = privmsg (drop 4 x)
eval _                        = return ()

privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

write :: String -> String -> Net ()
write s t = do
  h <- asks socket
  io $ hPrintf h "%s %s\r\n" s t
  io $ printf    "> %s %s\n" s t

io :: IO a -> Net a
io = liftIO
