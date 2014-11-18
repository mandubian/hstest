module Test1 where

--import Control.Monad.Reader
--import Control.Monad.Writer
--import Control.Monad.State
--import Control.Applicative
--import Data.Maybe
--import Data.String

--import Control.Monad.State.Class
--import Network.HTTP
--import Control.Monad.Fix
--import qualified GHC.IO.Exception as G

--import Control.Monad (unless)
--import Pipes
--import System.IO (isEOF)
--import Control.Exception (try, throwIO)

{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.HTTP.Types                 (status200, hContentType)
import Network.Wai.Handler.Warp           (run)
import Control.Concurrent.MVar
import Control.Exception                  (bracket)
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8 (fromShow)
import Data.Monoid                        ((<>))
import System.IO                  
import Pipes        
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as PB
import qualified Data.ByteString.Char8 as ByteString
import Database.HDBC.PostgreSQL           (Connection, connectPostgreSQL)

import Control.Concurrent (threadDelay)

main :: IO ()
main = run 3000 $ app


-- | Add a delay (in milliseconds) between each element
{-# INLINABLE delay #-}
delay :: Double -> Pipe a a IO r
delay ms = for cat $ \a -> do
  yield a
  lift $ threadDelay (truncate (ms * 1000))


streamFile :: Response
streamFile = responseStream status200 [(hContentType, "text/plain")] $ \send flush ->
  withFile "toto.txt" ReadMode $ \hIn ->
    runEffect $ for (PB.hGet 1 hIn >-> P.map fromByteString >-> delay 50) $ \builder -> lift $ do
      send builder
      flush


app :: Application
app _ respond = respond streamFile

psql :: IO Connection
psql = connectPostgreSQL "jdbc:postgresql://localhost/haskell"      


--application :: MVar Int -> Application
--application cref _ respond = do
--  modifyMVar cref $ \x -> do
--    resp <- respond $ responseLBS status200 [(hContentType, "text/plain")] $ 
--      toLazyByteString $ fromByteString "Hello World" <> fromShow x
--    return (x+1, resp)


--streamFileResp :: IO Response
--streamFileResp =  withFile "toto.txt" ReadMode $ \hIn -> do
--  return $ responseStream status200 [(hContentType, "text/plain")] $ \send flush ->
--    runEffect $ for (PB.fromHandle hIn >-> P.map fromByteString) $ \builder -> lift $ do
--      send builder
--      flush

--streamFileResp2 :: IO Response
--streamFileResp2 =  bracket
--  (openFile "toto.txt" ReadMode)
--  (hClose)
--  (\hIn -> do
--    return $ responseStream status200 [(hContentType, "text/plain")] $ \send flush ->
--      runEffect $ for (PB.fromHandle hIn >-> P.map fromByteString) $ \builder -> lift $ do
--        send builder
--        flush
--  )


--main = run 3000 $ app
--main = do
--  str <- withFile "toto.txt" ReadMode hGetLine
--  putStrLn str

--main = run 3000 $ app

--main = do
--  cref  <- newMVar 0
--  run 3000 $ application cref


--defaultMain :: IO ()
--defaultMain = runEffect $ for (every pair) (lift . print)

-- stdinLn >-> takeN 4 >-> stdoutLn

--defaultMain = do
--  putStrLn "coucou:"
--  name <- getLine
--  putStrLn $ "your name is " ++ name

--loop :: Effect IO ()
--loop = for stdinLn (lift . putStrLn)

--stdinLn :: Producer String IO ()
--stdinLn = do
--  eof <- lift isEOF
--  unless eof $ do
--    str <- lift getLine
--    yield str
--    stdinLn

--each' :: Monad m => [a] -> Producer a m ()
--each' as = mapM_ yield as

--stdoutLn :: Consumer String IO ()
--stdoutLn = do
--  str <- await
--  x <- lift $ try $ putStrLn str
--  case x of
--    Left e@G.IOError { G.ioe_type = t } -> 
--      lift $ unless (t == G.ResourceVanished) $ throwIO e
--    Right () -> stdoutLn

--takeN :: Int -> Pipe a a IO ()
--takeN 0 = lift $ putStrLn "finished"
--takeN n = do
--  x <- await
--  yield x
--  takeN (n-1)

--pair :: ListT IO (Int, Int)
--pair = do
--   x <- Select $ each [1, 2]
--   lift $ putStrLn $ "x = " ++ show x
--   y <- Select $ each [3, 4]
--   lift $ putStrLn $ "y = " ++ show y
--   return (x, y)

--fetchHttp ::IO (String, String)
--fetchHttp = liftA2 (,) m1 m2 where
--  m1 = simpleHTTP (getRequest "http://www.fpcomplete.com/") >>= getResponseBody
--  m2 = simpleHTTP (getRequest "http://www.google.com/") >>= getResponseBody

--stream2 :: Maybe [Int]
--stream2 = mfix $ \xs -> do
--  xs' <- Just (1:xs)
--  return (map negate xs')
  

--type Algebra f a = f a -> a
--type Coalgebra f a = a -> f a
--newtype Fix f = Fix { unFix :: f (Fix f) }

--cata :: Functor f => Algebra f a -> Fix f -> a 
--cata alg = alg . fmap (cata alg) . unFix



-- data F = A | B

--f :: F -> ()
--f x = case x of 
--  A -> ()

----instance Functor [] where
----  fmap f (x:xs) = f x : fmap f _


--g :: Monad m => m a -> (a -> m b) -> m b
--g ma f = ma >>= f


--h = do 
--  a <- Just 5
--  return a

--m = [1, 2, 3, 4]

--data Ctx = Ctx 
--  {
--    foo :: String
--  , bar :: Int
--  } deriving (Show)

--comp :: Reader Ctx (Maybe String)
--comp = do 
--  n <- asks bar
--  x <- asks foo
--  if n > 0
--    then return (Just x)
--    else return Nothing

--runComp :: Maybe String
--runComp = runReader comp $ Ctx "toto" 5

--w :: Writer [Int] String
--w = do
--  tell [1, 2, 3]
--  tell [4, 5, 6]
--  return "foo"

--res = runWriter w


--st :: State Int Int
--st = do
--  put(3)
--  modify(+3)
--  get

--rst :: Int -> (Int, Int)
--rst n = runState st n

--data Expr
--  = Val Int
--  | Add Expr Expr
--  | Var String deriving (Show)

--type Env = [(String, Int)]
--type Eval a = ReaderT Env Maybe a

--eval :: Expr -> Eval Int
--eval e = case e of
--  Val a -> return a

--  Var s -> do
--    env <- ask
--    a   <- lift $ lookup s env
--    return a

--  Add x y -> do
--    a <- eval x
--    b <- eval y
--    return (a + b)

--env = [("x", 1), ("y", 2)]

--ex1 = Add (Var "x") (Var "y")

--evalEx1 = runReaderT (eval ex1) env

--instance Monoid Int where
--  mempty = 0
--  mappend a b = a + b

--doit :: WriterT Int (State Int) Int
--doit = do
--  tell 1
--  a <- get
--  return a


--doit2 :: State Int Int
--doit2 = do
--  a <- get
--  put a
--  return a

--ex2 :: Maybe Int
--ex2 = (+) <$> m1 <*> m2 where
--  m1 = Just 3
--  m2 = Just 4
