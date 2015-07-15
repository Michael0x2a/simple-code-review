{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import System.Exit
import System.IO
import Data.List
import Data.List.Split
import Data.String
import Data.String.Utils
import Data.Time
import Data.Time.LocalTime
import Data.Time.Format

import Text.Format
import Text.Pandoc (readMarkdown, writeHtmlString, def)


-- Data structures

newtype LineNumber = LineNumber Integer deriving (Ord, Eq)
instance Show LineNumber where show (LineNumber n) = show n

newtype SanitizedHtml = SanitizedHtml String deriving Eq
instance Show SanitizedHtml where show (SanitizedHtml a) = a

newtype Template = Template String
--instance IsString Template where fromString = Template
instance Show Template where show (Template a) = a

newtype Language = Language String deriving Eq
instance Show Language where show (Language a) = a

newtype Title = Title String
instance Show Title where show (Title a) = a

newtype Timestamp = Timestamp String deriving (Ord, Eq)
instance Show Timestamp where show (Timestamp a) = a


data Feedback = 
    Feedback {
        fLineno :: LineNumber,
        fText :: SanitizedHtml 
    } deriving (Show)

data CodeFragment = 
    CodeFragment {
        cLineno :: LineNumber,
        cText :: SanitizedHtml
    } deriving (Show)

data Line = 
    Line {
        lineno :: LineNumber,
        code :: SanitizedHtml,
        commentary :: SanitizedHtml
    } deriving (Show)

data TemplateOptions = 
    TemplateOptions {
        template :: Template,
        language :: Language,
        title :: Title,
        timestamp :: Timestamp
    } deriving (Show)

data SourceOptions = 
    SourceOptions {
        codePath :: FilePath,
        feedbackPath :: FilePath,
        outputPath :: FilePath
    } deriving (Show)

-- Parsing feedback file

mungeCodeTags :: SanitizedHtml -> SanitizedHtml
mungeCodeTags (SanitizedHtml html) = SanitizedHtml $ replace "\"><code>" "\">" temp
    where
        temp = replace "<pre class=\"" "<pre><code class=\"language-" html

convertToHtml :: String -> SanitizedHtml 
convertToHtml text = 
    case readMarkdown def text of
        Left error -> SanitizedHtml "error"
        Right pandoc -> mungeCodeTags $ SanitizedHtml (writeHtmlString def pandoc)

unindent :: String -> String
unindent "" = ""
unindent text = drop 4 text

parseSingle :: [String] -> Feedback
parseSingle (rawLineNo : rest) = 
    Feedback lineNo html
    where
        lineNo :: LineNumber
        lineNo = LineNumber $ read (init rawLineNo)

        html :: SanitizedHtml
        html = convertToHtml $ intercalate "\n" $ map unindent rest 

parseFeedback :: String -> [Feedback]
parseFeedback text = map (parseSingle . getLines) chunks
    where
        chunks :: [String]
        chunks = drop 1 $ splitOn "Line " text

        getLines :: String -> [String]
        getLines text = splitOn "\n" text 


-- Parse code

escapeHtml :: String -> SanitizedHtml
escapeHtml html = SanitizedHtml $ intercalate "" (map escape html)
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
        convertCode (index, "") = CodeFragment (LineNumber index) (SanitizedHtml " ")
        convertCode (index, text) = CodeFragment (LineNumber index) (escapeHtml text)


-- Combining

makeLine :: CodeFragment -> Feedback -> Line
makeLine code feedback = Line (cLineno code) (cText code) (fText feedback)

makeSimpleLine :: CodeFragment -> Line
makeSimpleLine code = Line (cLineno code) (cText code) (SanitizedHtml "")

bundle :: [CodeFragment] -> [Feedback] -> [Line]
bundle [] _ = []
bundle (c:cs) [] = (makeSimpleLine c) : (bundle cs [])
bundle (c : cs) (f : fs) 
    | (cLineno c) == (fLineno f) = (makeLine c f) : (bundle cs fs)
    | otherwise                  = (makeSimpleLine c) : (bundle cs (f : fs))

-- HTML conversion

lineTemplate :: Template 
lineTemplate = Template 
    "  <div class=\"line {0}\">\n\
    \    <div class=\"codeline\">\n\
    \      <div class=\"lineno\"><span>{1}</span></div>\n\
    \      <pre><code class=\"language-{2}\">{3}</code></pre>\n\
    \    </div>\n\
    \    <div class=\"commentary\">{4}</div>\n\
    \  </div>"

formatTemplate :: Template -> [String] -> SanitizedHtml
formatTemplate (Template template) fragments = 
    SanitizedHtml $ format template fragments
   
getTimestamp :: IO Timestamp
getTimestamp = do 
    tz <- getCurrentTimeZone
    time <- getCurrentTime
    let rawTime = formatTime defaultTimeLocale formatStr (utcToZonedTime tz time)
    return $ Timestamp rawTime
    where 
        formatStr :: String
        formatStr = "%A, %B %Y at %l:%M:%S %p, %Z"

isAnnotated :: Line -> Bool
isAnnotated line = (show (commentary line) /= "")

lineToHtml :: Language -> Line -> SanitizedHtml
lineToHtml (Language lang) line@(Line lineNo code commentary) 
    | isAnnotated line = formatTemplate lineTemplate ("not-annotated" : args)
    | otherwise        = formatTemplate lineTemplate ("annotated" : args)
    where 
        args :: [String]
        args = [show lineNo, lang, show code, show commentary]

linesToHtml :: Language -> [Line] -> SanitizedHtml
linesToHtml lang lines = SanitizedHtml $ intercalate "\n" (map show htmlFragments)
    where
        htmlFragments :: [SanitizedHtml]
        htmlFragments = map (lineToHtml lang) lines

generateHtml :: TemplateOptions -> [Line] -> SanitizedHtml
generateHtml (TemplateOptions template language title timestamp) lines =
    formatTemplate template [show title, show timestamp, show innerHtml]
    where 
        innerHtml :: SanitizedHtml
        innerHtml = linesToHtml language lines


-- Commands

generate :: TemplateOptions -> SourceOptions -> IO ()
generate templateOptions sourceOptions = do
    rawCode <- readFile (codePath sourceOptions)
    rawFeedback <- readFile (feedbackPath sourceOptions)

    let code = parseCode rawCode
    let feedback = parseFeedback rawFeedback
    let lines = bundle code feedback
    let output = generateHtml templateOptions lines

    writeFile (outputPath sourceOptions) (show output)

usageText :: String
usageText = "generate\n\
            \ \
            \Usage:\n\
            \  generate <lang> <title> <code> <feedback> <output>\n\
            \ "

usage :: IO ()
usage = putStrLn usageText

exit :: IO ()
exit = exitWith ExitSuccess


-- Parsing and execution

getTemplate :: FilePath -> IO Template
getTemplate path = do
    rawContent <- readFile path
    return $ Template rawContent

parse :: [String] -> IO ()
parse [language, title, source, feedback, output] = do
    template <- getTemplate "templates/feedback.html"
    timestamp <- getTimestamp

    let language' = (Language language)
    let title' = (Title title)

    let templateOptions = (TemplateOptions template language' title' timestamp)
    let sourceOptions = SourceOptions source feedback output

    generate templateOptions sourceOptions
parse _ = 
    die usageText

main :: IO ()
main = do
    args <- getArgs
    parse args
    exit
