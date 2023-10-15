module Args where

import Options.Applicative

data Args = Args Flags Command deriving (Eq, Show)

data Command
  = Doctor
  | Conway
  deriving (Eq, Show)

data Verbosity = Verbose | NotVerbose deriving (Eq, Show)

newtype Flags = Flags Verbosity deriving (Eq, Show)

argsParser :: Parser Args
argsParser = Args <$> flags <*> commands

verbosity :: Parser Verbosity
verbosity =
  flag
    NotVerbose
    Verbose
    (long "verbose" <> short 'v' <> help "Enable verbose output")

flags :: Parser Flags
flags = Flags <$> verbosity

commands :: Parser Command
commands =
  hsubparser
    ( command "doctor" (info (pure Doctor) (progDesc "check the CLI is working")) <>
     command "conway" (info (pure Conway) (progDesc "run conway's game of life"))
    )

withInfo :: Parser a -> String -> ParserInfo a
withInfo p s = info (helper <*> p) (fullDesc <> progDesc s)

parseArgs :: [String] -> ParserResult Args
parseArgs = execParserPure preferences parserInfo
  where
    parserInfo = withInfo argsParser "Comonadic examples"
    preferences = prefs (disambiguate <> showHelpOnEmpty <> showHelpOnError)
