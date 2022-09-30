module Lib (repl) where

import Data.Char                            ( isSpace, chr )
import Data.Function                        ( on )
import Data.Map                             ( Map, (!) )
import Data.Stream                          ( Stream(Cons) )
import Data.Word                            ( Word8 )
import System.IO                            ( stdout, hFlush )
import Text.Read                            (readMaybe)

import Control.Monad.Writer                 
    ( runWriter, execWriter, MonadWriter(writer), Writer )
import Text.ParserCombinators.Parsec        
    ( char, between, string, choice, (<|>), many, parse, ParseError )

import qualified Data.Map       as Map
import qualified Data.Stream    as S

repl :: IO ()
repl = putStr "\nBRAINFUCK> "
               >> hFlush stdout         -- Flush buffer 
               >> getLine >>= execute

-- ======================================
-- PROGRAM EXECUTION
-- ======================================
type Program = Writer String BFMachine
type Command = Program -> IO Program

execute :: String -> IO ()
execute s = resolveEither $ do
               parsed <- parseBFExp $ filter (`elem` validSymbols ++ ['[', ']']) s  -- Ignore invalid characters
               let commands = map toCommand parsed
                   res = run makeBFMachine commands
               pure $ res >>= putStr . execWriter -- Display output log

-- Resolve Either ParseError (IO ()) to IO ()
resolveEither :: Either ParseError (IO ()) -> IO ()
resolveEither = \case Left e -> print e; Right s -> s

-- Execute commands sequentially
run :: BFMachine -> [Command] -> IO Program
run tm = foldl (>>=) (pure $ pure tm)

-- Create a map of non-looping symbols to commands
commandMap :: Map Char (BFMachine -> BFMachine)
commandMap = Map.fromList $ zip validSymbols [increment, decrement, moveR, moveL]

-- Convert expression to command
toCommand :: BFExp -> Command
toCommand exp = toCommand' exp . runWriter

toCommand' :: BFExp -> (BFMachine, String) -> IO Program
toCommand' (Stmt ".") (tm, s) = pure $ writer (tm, s <> [chr $ fromIntegral $ get tm])       -- Update log
toCommand' (Stmt ",") (tm, s) = (\x -> writer (tm { get = toWord8 x }, s)) <$> getLine -- Update current valid with input
toCommand' (Stmt [c]) (tm, s) = pure $ writer (commandMap ! c $ tm, s)                       -- Retrieve command from map
toCommand' l@(Loop exps) w@(tm, s) = runLoop f (writer w) (get tm == 0)                      -- Form command with runLoop
                                        where f x = foldl (>>=) x (map toCommand exps)
toCommand' _ w = pure $ writer w

-- Iterate through a loop
runLoop :: (IO Program -> IO Program) -> Program -> Bool -> IO Program
runLoop f program = \case True -> pure program;
                          False -> f (pure program) >>= \x ->
                                   runLoop f x ((==0) . get . fst $ runWriter x)

-- ======================================
-- BRAINFUCK PARSER
-- ======================================

data BFExp = Stmt String | Loop [BFExp] deriving (Show)

-- List of valid brainfuck symbols, excluding loop delimiters
validSymbols :: [Char]
validSymbols = ['+', '-', '>', '<', '.', ',']

-- Parse a given string to a brainfuck program
parseBFExp :: String -> Either ParseError [BFExp]
parseBFExp = parse (many token) ""
                where token = sqBraces <|> terminal
                      terminal = Stmt <$> choice (map (string . pure) validSymbols) -- Parse a terminal statement
                      sqBraces = Loop <$> (between `on` char) '[' ']' (many token) -- Parse a loop

-- ======================================
-- TURING MACHINE
-- ======================================
type BFMachine = TM Word8

-- Turing machine implemented with Stream
data TM a = TM { left :: S.Stream a
               , get  :: a
               , right :: S.Stream a }

-- Show current value and nearest 5 cells on each side
instance Show a => Show (TM a) where
        show (TM l x r) = unwords ["TM ", f l, show x, f r]
                                where f = show . S.take 5

-- Turing Machine Commands
moveL, moveR :: TM a -> TM a
moveL (TM (Cons l' l) v r) = TM l l' (Cons v r)
moveR (TM l v (Cons r' r)) = TM (Cons v l) r' r

-- Increment/Decrement the current value
-- | Values wrap on underflow/overflow
increment, decrement :: (Enum a, Bounded a, Eq a) => TM a -> TM a
increment tm@TM {..} = tm { get = if get == maxBound then minBound else succ get }
decrement tm@TM {..} = tm { get = if get == minBound then maxBound else pred get }

-- Smart Constructor for Turing Machine
makeBFMachine :: BFMachine
makeBFMachine = TM (S.repeat 0) 0 (S.repeat 0)

-- Converting a string to the equivalent Word8 value
toWord8 :: String -> Word8
toWord8 = fromIntegral . fromEnum . head