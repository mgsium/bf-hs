module Interface (repl) where

import Control.Monad            ( forever )
import System.IO

import qualified Lib as L

repl :: IO ()
repl = putStr brainfuckInfo >> forever L.repl

brainfuckInfo :: String
brainfuckInfo = "\nBrainf**ck is an esoteric programming language which models a Turing Machine\n"
                ++ "Find out more at https://github.com/brain-lang/brainfuck/blob/master/brainfuck.md\n"
