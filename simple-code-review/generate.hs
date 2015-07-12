{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import System.Exit
import System.IO
import Data.List
import Data.List.Split
import Data.String.Utils
import Data.Time
import Data.Time.LocalTime
import Data.Time.Format

import Text.Format
import Text.Pandoc

-- Basic config

usageText :: String
usageText = "generate\n\
            \ \
            \Usage:\n\
            \  generate <lang> <title> <code> <feedback> <output>\n\
            \ "


-- Data structures

newtype LineNumber = LineNumber Integer deriving (Ord, Eq)
instance Show LineNumber where
    show (LineNumber n) = show n

data Feedback = 
    Feedback {
        fLineno :: LineNumber,
        fText :: String
    } deriving (Show)

data CodeFragment = 
    CodeFragment {
        cLineno :: LineNumber,
        cText :: String
    } deriving (Show)

data Line = 
    Line {
        lineno :: LineNumber,
        code :: String,
        commentary :: String
    } deriving (Show)


-- Parsing feedback file

mungeCodeTags :: String -> String
mungeCodeTags html = replace "\"><code>" "\">" t
    where
        t = replace "<pre class=\"" "<pre><code class=\"language-" html

convertToHtml :: String -> String
convertToHtml text = 
    case readMarkdown def text of
        Left error -> "error"
        Right pandoc -> mungeCodeTags $ writeHtmlString def pandoc

unindent :: String -> String
unindent "" = ""
unindent text = drop 4 text

parseSingle :: [String] -> Feedback
parseSingle (rawLineNo : rest) = 
    Feedback (LineNumber lineNo) html
    where
        lineNo :: Integer
        lineNo = read $ init rawLineNo

        html :: String
        html = convertToHtml $ intercalate "\n" $ map unindent rest 

parseFeedback :: String -> [Feedback]
parseFeedback text = map (parseSingle . getLines) chunks
    where
        chunks :: [String]
        chunks = drop 1 $ splitOn "Line " text

        getLines :: String -> [String]
        getLines text = splitOn "\n" text 


-- Parse code

escapeHtml :: String -> String
escapeHtml html = intercalate "" $ map escape html
    where
        escape :: Char -> String
        escape c = case c of
            '&' -> "&amp;"
            '<' -> "&lt;"
            '>' -> "&gt;"
            c -> [c]

parseCode :: String -> [CodeFragment]
parseCode text = map convertCode $ zip [1..] (splitOn "\n" text)
    where
        convertCode :: (Integer, String) -> CodeFragment
        convertCode (index, "") = CodeFragment (LineNumber index) " "
        convertCode (index, text) = CodeFragment (LineNumber index) (escapeHtml text)


-- Combining

makeLine :: CodeFragment -> Feedback -> Line
makeLine code feedback = Line (cLineno code) (cText code) (fText feedback)

makeSimpleLine :: CodeFragment -> Line
makeSimpleLine code = Line (cLineno code) (cText code) ""

bundle :: [CodeFragment] -> [Feedback] -> [Line]
bundle [] _ = []
bundle (c:cs) [] = (makeSimpleLine c) : (bundle cs [])
bundle (c : cs) (f : fs) 
    | (cLineno c) == (fLineno f) = (makeLine c f) : (bundle cs fs)
    | otherwise                  = (makeSimpleLine c) : (bundle cs (f : fs))

-- HTML conversion

lineTemplate :: String 
lineTemplate = 
    "  <div class=\"line {0}\">\n\
    \    <div class=\"codeline\">\n\
    \      <div class=\"lineno\"><span>{1}</span></div>\n\
    \      <pre><code class=\"language-{2}\">{3}</code></pre>\n\
    \    </div>\n\
    \    <div class=\"commentary\">{4}</div>\n\
    \  </div>"
   
timestamp :: IO String
timestamp = do 
    let tz = hoursToTimeZone (-7)
    time <- getCurrentTime
    return $ formatTime defaultTimeLocale "%c" (utcToZonedTime tz time)

lineToHtml :: String -> Line -> String
lineToHtml lang (Line lineNo code commentary) 
    | commentary == "" = format lineTemplate ("not-annotated" : args)
    | otherwise        = format lineTemplate ("annotated" : args)
    where 
        args :: [String]
        args = [(show lineNo), lang, code, commentary]

linesToHtml :: String -> String -> String -> String -> [Line] -> String
linesToHtml template title lang timestamp lines = 
    format template [title, timestamp, intercalate "\n" (map (lineToHtml lang) lines)]


-- Commands

generate :: String -> String -> FilePath -> FilePath -> FilePath -> IO ()
generate lang title sourcePath feedbackPath outputPath = do
    rawCode <- readFile sourcePath
    rawFeedback <- readFile feedbackPath
    template <- readFile "templates/feedback.html"
    now <- timestamp

    let code = parseCode rawCode
    let feedback = parseFeedback rawFeedback
    let lines = bundle code feedback

    writeFile outputPath (linesToHtml template title lang now lines)

usage :: IO ()
usage = putStrLn usageText

exit :: IO ()
exit = exitWith ExitSuccess


-- Parsing and execution

parse :: [String] -> IO ()
parse [lang, title, source, feedback, output] = 
        generate lang title source feedback output
parse _ = 
    die usageText

main :: IO ()
main = do
    args <- getArgs
    parse args
    exit
