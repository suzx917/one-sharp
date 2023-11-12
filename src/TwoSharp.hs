{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- Using Tagless Final approach

module TwoSharp where

import Data.Map as M
import Parser

class Instr r where
  addO :: Integer -> r
  addS :: Integer -> r
  shiftF :: Integer -> r
  shiftB :: Integer -> r
  cases  :: Integer -> r

class Instr (Elem a) => Machine a where
  type Elem a
  load :: [Elem a] -> Register -> a
  shift  :: Integer -> a -> a
  regOut :: a -> Register
  regIn  :: Register -> a -> a
  step :: a -> a
  isHalt :: a -> Bool

type Register = M.Map Integer String

instance Instr (Integer -> String) where
  addO  rn = \_   -> "Add 1 to R" ++ show rn
  addS  rn = \_   -> "Add # to R" ++ show rn
  shiftF i = \idx -> "Foward to line " ++ show (idx + i)
  shiftB i = \idx -> "Back to line " ++ show (idx - i)
  cases rn = \_   -> "Case split on R" ++ show rn

-- How to achieve mutually recursive definition here?
-- Machine = (Queue[Instr],Register)
-- and to implement instance Instr (Machine -> Machine)?

instance Machine m => Instr (m -> m) where
  addO rn m =
    let reg  = regOut m
        reg' = M.adjust (++ "1") rn reg
        m'   = regIn reg' m
     in shift 1 m'
  addS rn m =
    let reg  = regOut m
        reg' = M.adjust (++ "#") rn reg
        m'   = regIn reg' m
     in shift 1 m'
  shiftF i m =
    shift i m
  shiftB i m =
    shift (-i) m
  cases rn m =
    let reg  = regOut m
     in case (M.lookup rn reg) of
          Just ('1':rest) -> let reg' = M.insert rn rest reg
                              in shift 2 (regIn reg' m)
          Just ('#':rest) -> let reg' = M.insert rn rest reg
                              in shift 3 (regIn reg' m)
          _               -> shift 1 m

-- Machine instance
instance Instr a => Machine ([a], [a], Register) where
  type Elem ([a], [a], Register) = a
  
  load prog reg = ([], prog, reg)
  
  shift 0 m = m
  shift 1 (bs, c:cs, reg) = (c:bs, cs, reg)
  shift n m
   | n > 0 = shift (n-1) (shift 1 m)
   | n < 0 = flip $ shift (-n) (flip m)
     where flip (bs, cs, reg) = (cs, bs, reg)

-- pprint
--instance Machine String where
-- it's weird to write `regOut` or `isHalt` here   
