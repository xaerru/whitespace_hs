{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

type ErrorMsg = String

type Result = Either ErrorMsg String

data Prog = Prog {_code :: String, _cursor :: Int, _counter :: Int} deriving (Show)

data LangState = LangState {_prog :: Prog, _input :: String, _output :: String, _stack :: [Int], _labels :: Map String Int, _heap :: Map Int Int, _lerror :: String} deriving (Show)

makeLenses ''Prog
makeLenses ''LangState

makeLangState :: String -> String -> LangState
makeLangState c i = LangState {_prog = Prog {_code = filter (\x->x==' '||x=='\t'||x=='\n') c, _cursor = 0, _counter = -1}, _input = i, _output = "", _stack = [], _labels = Map.empty, _heap = Map.empty, _lerror = ""}

cursorAfterNextTerminal :: LangState -> LangState
cursorAfterNextTerminal s = np
  where
    ocur = view (prog . cursor) s
    co = view (prog . code) s
    ncur = '\n' `elemIndex` drop (ocur + 1) co
    np = case ncur of
      Just n -> set (prog . cursor) (n + ocur + 2) s
      Nothing -> set lerror "Error" s

dropBeforeCursor :: LangState -> String
dropBeforeCursor s = drop (s ^. prog . cursor) $ s ^. (prog . code)

moveCur :: LangState -> (Int -> Int) -> LangState
moveCur s f = (prog . cursor) %~ f $ s

decFromBin :: String -> Maybe Int
decFromBin (c : cs)
  | c == ' ' = decFromBin cs
  | c == '\t' = Just (+ (2 ^ fromMaybe 0 ('\n' `elemIndex` cs))) <*> decFromBin cs
decFromBin ('\n' : _) = Just 0
decFromBin (_ : cs) = decFromBin cs
decFromBin [] = Nothing

parseNumber :: LangState -> (Int, LangState)
parseNumber s
  | x == '\t' = (-1 * fst e, snd e)
  | x == ' ' = e
  where
    co = dropBeforeCursor s
    x = head co
    xs = tail co
    r = decFromBin xs
    ns = cursorAfterNextTerminal s
    e = case r of
      Just n -> (n, ns)
      Nothing -> (0, set lerror "NumParseError" ns)
parseNumber (LangState (Prog (_ : _) _ _) _ _ _ _ _ _) = (0, set lerror "NumParseError" (makeLangState "" ""))
parseNumber (LangState (Prog [] _ _) _ _ _ _ _ _) = (0, set lerror "NumParseError" (makeLangState "" ""))

parseNumberMoveCur :: LangState -> (Int -> Int) -> (Int, LangState)
parseNumberMoveCur s f = parseNumber $ moveCur s f

parseLabel :: String -> Maybe String
parseLabel (c : cs)
  | c == '\t' || c == ' ' = Just (c :) <*> parseLabel cs
parseLabel ('\n' : _) = Just []
parseLabel (_ : cs) = parseLabel cs
parseLabel [] = Nothing

impSpace :: LangState -> LangState
impSpace s = ns
  where
    ns = case dropBeforeCursor s of
      ' ' : _ -> stack %~ (++ [n]) $ s1
        where
          (n, s1) = parseNumberMoveCur s (+ 1)
      '\t' : ' ' : _ -> stack %~ (\x -> x ++ [x !! (length x - n - 1)]) $ s1
        where
          (n, s1) = parseNumberMoveCur s (+ 2)
      '\t' : '\n' : _ ->
        if n >= length (view stack s1) || n < 0
          then stack %~ (\x -> [last x]) $ s1
          else stack %~ (\x -> take (length x - n + 1) x) $ s1
        where
          (n, s1) = parseNumberMoveCur s (+ 2)
      '\n' : ' ' : _ -> stack %~ (\x -> x ++ [last x]) $ moveCur s (+ 2)
      '\n' : '\t' : _ -> stack %~ (\x -> take (length x - 2) x ++ [last x, last $ init x]) $ moveCur s (+ 2)
      '\n' : '\n' : _ -> stack %~ init $ moveCur s (+ 2)
      _ : _ -> moveCur s (+1)
      [] -> s

stackBinOp :: (Int -> Int -> Int) -> [Int] -> [Int]
stackBinOp binop x = take (length x - 2) x ++ [binop (x !! (length x - 2)) (x !! (length x - 1))]

impTabSpace :: LangState -> LangState
impTabSpace s = ns
  where
    ns = case dropBeforeCursor s of
      ' ' : ' ' : _ -> stack %~ stackBinOp (+) $ moveCur s (+ 2)
      ' ' : '\t' : _ -> stack %~ stackBinOp (-) $ moveCur s (+ 2)
      ' ' : '\n' : _ -> stack %~ stackBinOp (*) $ moveCur s (+ 2)
      '\t' : ' ' : _ -> stack %~ stackBinOp div $ moveCur s (+ 2) -- Add error
      '\t' : '\t' : _ -> stack %~ stackBinOp mod $ moveCur s (+ 2) -- Add error
      _ : _ -> moveCur s (+1)
      [] -> s

impTabTab :: LangState -> LangState
impTabTab s = ns
  where
    ns = case dropBeforeCursor s of
      ' ' : _ -> stack %~ (\x -> take (length x - 2) x) $ addToMap (s1 ^. stack)
        where
          s1 = moveCur s (+ 1)
          addToMap x = heap %~ Map.insert (x !! (length x - 2)) (x !! (length x - 1)) $ s1
      '\t' : _ -> pushStack $ moveCur s (+ 1)
        where
          pushStack s1 = stack %~ (\x -> init x ++ [Map.findWithDefault 0 (last x) (s1 ^. heap)]) $ s1
      _ : _ -> moveCur s (+1)
      [] -> s

parseNumberFromInput :: LangState -> (Int, LangState)
parseNumberFromInput s = (read (takeWhile (/= '\n') (s ^. input)) :: Int, input %~ tail . dropWhile (/= '\n') $ s)

impTabLineFeed :: LangState -> LangState
impTabLineFeed s = ns
  where
    ns = case dropBeforeCursor s of
      ' ' : ' ' : _ -> stack %~ init $ output %~ (++ [chr $ last (s ^. stack)]) $ moveCur s (+ 2)
      ' ' : '\t' : _ -> stack %~ init $ output %~ (++ (show $ last (s ^. stack))) $ moveCur s (+ 2)
      '\t' : ' ' : _ -> heap %~ Map.insert b a $ stack %~ init $ s1
        where
          a = ord $ head (s ^. input)
          s1 = input %~ tail $ moveCur s (+ 2)
          b = last (s ^. stack)
      '\t' : '\t' : _ -> heap %~ Map.insert b a $ stack %~ init $ s1
        where
          (a, s1) = parseNumberFromInput $ moveCur s (+ 2)
          b = last (s ^. stack)
      _ : _ -> moveCur s (+1)
      [] -> s

impLineFeed :: LangState -> LangState
impLineFeed s = ns
  where
    ns = case dropBeforeCursor s of
      ' ' : ' ' : _ -> nss
        where
          s1 = moveCur s (+ 2)
          l = parseLabel $ dropBeforeCursor s1
          s2 = cursorAfterNextTerminal s1
          nss = case l of
            Just n -> labels %~ Map.insert n (s2 ^. prog . cursor) $ s2
            Nothing -> lerror %~ const "LabelParseError" $ s2
      ' ' : '\t' : _ -> nss
        where
          s1 = moveCur s (+ 2)
          l = parseLabel $ dropBeforeCursor s1
          s2 = cursorAfterNextTerminal s1
          s3 = prog . counter %~ const (s2 ^. prog . cursor) $ s2
          nss = case l of
            Just n -> prog . cursor %~ const (Map.findWithDefault 0 n (s ^. labels)) $ s3
            Nothing -> lerror %~ const "LabelParseError" $ s2
      '\t' : '\n' : _ ->
        if (s ^. prog . counter) == (-1)
          then moveCur s (+ 2)
          else prog . cursor %~ const (-1) $ prog . cursor %~ const (s ^. prog . counter) $ s
      _ : _ -> moveCur s (+1)
      [] -> s

process :: LangState -> LangState
process s = ns
  where
    ns = case dropBeforeCursor s of
      ' ' : _ -> process $ impSpace (moveCur s (+ 1))
      '\n' : _ -> process $ impLineFeed (moveCur s (+ 1))
      '\t' : ' ' : _ -> process $ impTabSpace (moveCur s (+ 2))
      '\t' : '\t' : _ -> process $ impTabTab (moveCur s (+ 2))
      '\t' : '\n' : _ -> process $ impTabLineFeed (moveCur s (+ 2))
      _ : _ -> process $ moveCur s (+1)
      [] -> s

whitespace :: String -> String -> Result
whitespace "" "" = Left "error"
whitespace c i = r
  where
    s = process $ makeLangState c i
    r = case s^.lerror of
          "" -> Right (s^.output)
          _  -> Left (s^.lerror)

main :: IO ()
main = do
  let i = makeLangState "   \t\n   \t \n\t\t    \t\n\t\t\t" ""
  print $ process i
