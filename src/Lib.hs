module Lib where

import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Bifunctor


data Button = Character Char
            | Add
            | Sub
            | Mul
            | Div
            | Pow
            | Eq
            | Clear
            | Delete
            deriving (Show, Eq)

initialState = State T.empty (Right 0) (flip const)

data State = State
           { sInput :: T.Text
           , sValue :: Either T.Text Double
           , sOp    :: Double -> Double -> Double
           }

instance Show State where
  show (State i v _) = show i ++ " " ++ show v


showValue :: State -> T.Text
showValue (State i v _) | T.length i == 0 = either id (T.pack . show) v
                        | otherwise       = i
eval :: Button -> State -> State
eval (Character n) s@(State i _ _) = s {sInput = i `T.append` T.singleton n}

eval Delete s@(State i _ _) = s {sInput = newSCharacter}
  where newSCharacter = if i == T.empty
                       then T.empty
                       else T.init i

eval Clear _ = initialState
eval Eq s = eval' (flip const) s
eval Add s = eval' (+) s
eval Sub s = eval' (-) s
eval Mul s = eval' (*) s
eval Div s = eval' (/) s
eval Pow s = eval' (**) s

eval' :: (Double -> Double -> Double) -> State -> State
eval' f s = s { sValue = newSValue, sInput = T.empty, sOp = f}
  where
    newSValue = if sInput s == T.empty
                  then sValue s
                  else do
                         v1 <- sValue s
                         (v2,_) <- first T.pack $ TR.double $ sInput s
                         return $ (sOp s) v1 v2
