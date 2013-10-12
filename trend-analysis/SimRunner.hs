import Simulation
import PriceBar
import System.Environment
import System.Exit
import System.Console.GetOpt

main = do
    args <- getArgs
    (opts, nonopts) <- simOpts args
    let debug = debugEnabled opts
    let help = helpRequested opts
    if help then usage else return ()

    let verbose = verboseEnabled opts
    let numPbs = getNumPbs opts "100"
    let extStrength = getStrength opts "3"
    let balance = getBalance opts "100000"
    let dsPath = getFeed opts dataSourcePath
    
    dbgOut debug $ do
        putStrLn ("numPbs=" ++ (show numPbs))
        putStrLn ("extStrength=" ++ (show extStrength))
        putStrLn ("dsPath=" ++ dsPath)
    
    ds <- initDataSource dsPath
    dbgStrLn debug ("length ds = " ++ (show (length ds)))
    dbgStrLn debug ("first bar = " ++ (show (head ds)))

    let s = initSimulation ds numPbs extStrength balance

    let reporter = if verbose then reportVerbose else reportActivity
    runSimulation s reporter verbose


dbgOut :: Bool -> IO () -> IO ()
dbgOut dbg f = if dbg then f else return ()

dbgStrLn :: Bool -> String -> IO ()
dbgStrLn debug s = dbgOut debug (do putStrLn s)

-- TODO: any logged messages that start with ":" will be output
reportActivity :: [String] -> IO ()
reportActivity = putStr . unlines . filter (startsWith ":")
    where startsWith s = (== s) . take (length s)

reportVerbose :: [String] -> IO ()
reportVerbose = putStr . unlines

dataDir = "../data/"
dataSourcePath = dataDir ++ "EURUSD_day.csv"

-- Command line flag handling modeled on
-- example in hackage page for System.Console.GetOpt.
-- TODO The code for working the resulting list of options
-- is mine and is unsatisfying.

-- To add an option:
-- 1) Add a Flag constructor.
-- 2) Add an option description.
-- 3) Add a test in sameOptFlag.
-- 4) Add an accessor function.
data Flag 
 = Debug | Help | Verbose
 | Strength String | NumPriceBars String | Feed String
 | Balance String
   deriving Show

usageHeader = "Usage: SimRunner [OPTION...]"

options :: [OptDescr Flag]
options =
 [ Option ['d']     ["debug"]    (NoArg Debug)                "debug output"
 , Option ['?','h'] ["help"]     (NoArg Help)                 "show help"
 , Option ['v']     ["verbose"]  (NoArg Verbose)              "show verbose output"
 , Option ['b']     ["balance"]  (ReqArg Balance "100000")    "initial account balance"
 , Option ['n']     ["numpbs"]   (ReqArg NumPriceBars "100")  "number of price bars"
 , Option ['s']     ["strength"] (ReqArg Strength "3")        "high/low strength"
 , Option ['f']     ["feed"]     (ReqArg Feed dataSourcePath) "input FILE of price bars"
 ]

simOpts :: [String] -> IO ([Flag], [String])
simOpts argv = 
   case getOpt Permute options argv of
      (o,n,[]  ) -> return (o,n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo usageHeader options))

findOpt :: Flag -> [Flag] -> Maybe Flag
findOpt _ [] = Nothing
findOpt t (f:fs) = if sameOptFlag t f then Just f else findOpt t fs

sameOptFlag :: Flag -> Flag -> Bool
sameOptFlag f1 f2 = case (f1,f2) of
    (Debug, Debug)                   -> True
    (Help,  Help)                    -> True
    (Verbose, Verbose)               -> True
    (NumPriceBars _, NumPriceBars _) -> True
    (Strength _, Strength _)         -> True
    (Balance _, Balance _)           -> True
    (Feed _, Feed _)                 -> True
    otherwise                        -> False

checkBoolFlag :: Flag -> [Flag] -> Bool
checkBoolFlag flag opts = case findOpt flag opts of
                                Just _ -> True
                                otherwise -> False

--TODO will rudely fail on bad int
checkIntFlag :: Flag -> [Flag] -> String -> Int
checkIntFlag flag opts defaultVal  = read s
   where s = case findOpt flag opts of
                   Just (Strength s) -> s
                   Just (NumPriceBars s) -> s
                   otherwise -> defaultVal

--TODO will rudely fail on bad float
checkFloatFlag :: Flag -> [Flag] -> String -> Price
checkFloatFlag flag opts defaultVal  = read s
  where s = case findOpt flag opts of
                  Just (Balance s) -> s
                  otherwise -> defaultVal

checkStringFlag :: Flag -> [Flag] -> String -> String
checkStringFlag flag opts defaultVal = f
   where f = case findOpt flag opts of
                   Just (Feed f) -> f
                   otherwise -> defaultVal

debugEnabled   = checkBoolFlag  Debug
helpRequested  = checkBoolFlag  Help
verboseEnabled = checkBoolFlag  Verbose
getStrength    = checkIntFlag   (Strength "")
getNumPbs      = checkIntFlag   (NumPriceBars "")
getBalance     = checkFloatFlag (Balance "")

--TODO will rudely fail on file error
getFeed        = checkStringFlag (Feed "")

usage :: IO ()
usage = do
    putStrLn (usageInfo usageHeader options)
    exitSuccess
