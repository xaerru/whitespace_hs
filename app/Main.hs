{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.List
import Data.Maybe

type ErrorMsg = String
type Result = Either ErrorMsg String 

data Prog = Prog {_code::String, _cursor::Int} deriving Show
data LangState = LangState {_prog::Prog, _input::String, _output::String, _stack::[Int], _lerror::String} deriving Show
makeLenses ''Prog
makeLenses ''LangState

cursorAfterNextTerminal :: LangState -> LangState
cursorAfterNextTerminal s = np
    where
        ocur = view (prog . cursor) s
        co = view (prog . code) s
        ncur = '\n' `elemIndex` drop (ocur+1) co
        np = case ncur of
            Just n -> set (prog . cursor) (n+ocur+2) s
            Nothing -> set lerror "Error" s

makeLangState :: String -> String -> LangState
makeLangState c i = LangState {_prog=Prog{_code=c,_cursor=0}, _input=i, _output="", _stack=[], _lerror=""}

parseDecFromBin :: String -> Maybe Int
parseDecFromBin (c:cs)
    | c == ' ' = parseDecFromBin cs
    | c == '\t' = Just (+(2^fromMaybe 0 ('\n'`elemIndex`cs))) <*> parseDecFromBin cs
parseDecFromBin ('\n':_) = Just 0
parseDecFromBin (_:cs) = parseDecFromBin cs
parseDecFromBin [] = Nothing

parseNumber :: LangState -> (Int, LangState)
parseNumber s
    | x == '\t' = (-1 * fst e, snd e)
    | x == ' '  = e
    where
        c = view (prog . cursor) s
        co = drop c $ view (prog . code) s
        x = head co
        xs = tail co
        r = parseDecFromBin xs
        ns = cursorAfterNextTerminal s
        e = case r of 
            Just n -> (n, ns)
            Nothing-> (0, set lerror "NumParseError" ns)
parseNumber (LangState (Prog (_:_) _)_ _ _ _) = (0, set lerror "NumParseError" (makeLangState "" ""))
parseNumber (LangState (Prog [] _)_ _ _ _) = (0, set lerror "NumParseError" (makeLangState "" ""))

parseLabel :: String -> Maybe String
parseLabel (c:cs)
    | c == '\t' || c == ' ' = Just (c:) <*> parseLabel cs
parseLabel ('\n':_) = Just []
parseLabel (_:cs) = parseLabel cs
parseLabel [] = Nothing

impSpace :: LangState -> LangState
impSpace s = ns
    where
        c = view (prog . cursor) s
        co = drop c $ view (prog . code) s
        ns = case co of
            ' ':_ ->  nss
                where
                    n = parseNumber (over (prog . cursor) (+1) s)
                    nss = over stack (++[fst n]) (snd n)
            '\t':' ':_ -> nss
                where
                    n = parseNumber (over (prog . cursor) (+1) s)
                    nss = over stack (\x -> x++[x!!(length x - fst n)]) (snd n)


whitespace :: String -> String -> Result
whitespace code input = undefined

main :: IO ()
main = do
    print $ either id id (whitespace "hello"  "hello")
