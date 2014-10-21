import System.Environment
import System.Console.GetOpt
import System.IO
import System.Exit
import Control.Monad
import Data.Char
import System.Random



{-
  Все входные данные программы должны передаваться с помощью параметров
  командной строки.
-}

data Options = Options  { optInput      :: IO String
                        , optOutput     :: String -> IO ()
                        , f_WC          :: Bool
                        , f_addToBegin  :: (Bool, String)
                        , f_addToEnd    :: (Bool, String)
                        , f_upper       :: Bool
                        , f_combinewith :: (Bool, [String])
                        , max_lenght    :: Int
                        , max_width     :: Int
                        , f_genrandfile :: Bool
                        }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "i" ["input"]
        (ReqArg
            (\arg opt -> return opt { optInput = readFile arg })
            "FILE")
        "Input file"
 
    , Option "o" ["output"]
        (ReqArg
            (\arg opt -> return opt { optOutput = writeFile arg })
            "FILE")
        "Output file"
 
    , Option "s" ["string"]
        (ReqArg
            (\arg opt -> return opt { optInput = return arg })
            "String")
        "Input string"

    , Option "w" ["wc"]
        (NoArg
            (\opt -> return opt { f_WC = True }))
        "Count of lines in input"

    , Option "a" ["addToBegin"]
        (ReqArg
            (\arg opt -> return opt { f_addToBegin = (True, arg) })
            "String")
        "Add String to begin of output"

    , Option "z" ["addToEnd"]
        (ReqArg
            (\arg opt -> return opt { f_addToEnd = (True, arg) })
            "String")
        "Add String to end of output"

    , Option "U" ["toUpperChars"]
        (NoArg
            (\opt -> return opt { f_upper = True }))
        "Write input in uppercase"
 
{-    , Option "C" ["combineWith"]
        (ReqArg
            (\arg opt -> return opt { f_combinewith = (True, do
                                                              fc <- readFile arg
                                                              return $ lines fc
                                                              )})
            "FILE")
        "Interline input with file"
-}
    , Option "l" ["maxLenght"]
        (ReqArg
            (\arg opt -> return opt { max_lenght = read arg })
            "Number")
        "Max lenght of random file"

    , Option "w" ["maxWidth"]
        (ReqArg
            (\arg opt -> return opt { max_width = read arg })
            "Number")
        "Max with of random file"

    , Option "r" ["genRandomFile"]
        (NoArg
            (\opt -> return opt { f_genrandfile = True }))
        "Generate random file. Max size is `maxLenght` and `maxWidth`"

    , Option "V" ["version"]
        (NoArg
            (\_ -> do
                hPutStrLn stderr "Version 0.01"
                exitWith ExitSuccess))
        "Print version"
 
    , Option "h" ["help"]
        (NoArg
            (\_ -> do
              prg <- getProgName
              hPutStrLn stderr (usageInfo prg options)
              exitWith ExitSuccess))
        "Show help"
    ]

startOptions :: Options
startOptions = Options  { optInput      = getContents
                        , optOutput     = putStrLn
                        , f_WC          = False
                        , f_addToBegin  = (False, "")
                        , f_addToEnd    = (False, "")
                        , f_upper       = False
                        , f_combinewith = (False, [])
                        , max_lenght    = 16
                        , max_width     = 16
                        , f_genrandfile = False
                        }

genRandFile = undefined 

main = do
  args <- getArgs
  let ( actions, nonOptions, errors ) = getOpt RequireOrder options args
  opts <- foldl (>>=) (return startOptions) actions

  let Options { optInput = input
              , optOutput = output 
              , f_WC      = f_WC
              , f_addToBegin  = f_addToBegin
              , f_addToEnd    = f_addToEnd
              , f_upper       = f_upper
              , f_combinewith = (if_combinewith, sf_combinewith)
              , max_lenght    = max_lenght
              , max_width     = max_width
              , f_genrandfile = f_genrandfile  } = opts

  --  Разработайте утилиту со следующими возможностями:

  --  1) подсчёт количества строк в заданном текстовом файле;
  when f_WC ( input >>= output . show . length . lines )

  --  2.a) добавление заданной строки в начало заданного файла;
  when (fst f_addToBegin) ( input >>= output . (++) (snd f_addToBegin) . (++) "\n" )
  --  2.b) добавление заданной строки в конец заданного файла;
  when (fst f_addToEnd) ( input >>= output . flip (++) (snd f_addToEnd) . flip (++) "\n" )

  --  3) преобразование всех буквенных символов заданного файла к верхнему
  --     регистру (результат выводится на консоль);
  when f_upper ( input >>= output . map toUpper )

  --  4) построчное слияние двух заданных файлов (каждая строка первого файла
  --     соединяется с соответствующей строкой второго файла);
  when if_combinewith ( input >>= output . undefined sf_combinewith )

  --  5) генерация случайного текстового файла (случайность должна ограничиваться
  --     максимальным количеством строк в файле и символов в строке).
  when f_genrandfile ( genRandFile )