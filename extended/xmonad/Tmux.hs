{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
-- | Semi-usable tmux sessions prompt for XMonad
--
-- Inspired by Simon Gomizelj (@vodik) - https://github.com/vodik/dotfiles/blob/master/xmonad/lib/XMonad/Util/Tmux.hs
module Tmux where

import           Control.Applicative
import           Control.Monad
import           Data.Array ((!), array, listArray)
import           Data.Foldable (asum, traverse_)
import           Data.Function (on)
import           Data.Ord (comparing)
import           Data.List (isPrefixOf, nubBy, sortBy)
import           Data.Maybe (maybeToList)
import           Data.Monoid (Monoid(..))
import           Data.Map (Map)
import qualified System.Directory as D
import           System.FilePath ((</>))
import           System.Wordexp.Simple (wordexp)
import           Text.Printf (printf)
import           XMonad hiding (spawn)
import           XMonad.Prompt
import qualified XMonad.StackSet as W
import           XMonad.Util.Run
import           XMonad.Util.NamedWindows (getName)

import           Route
import           Spawn (spawn)


type Host = String
type Session = String

data Command
  = Hop Host Command
  | Tmux Session Settings

hop :: Host -> Command -> Command
hop = Hop

tmux :: Session -> Mod Settings -> Command
tmux n (Mod f) = Tmux n (f defaultSettings)

newtype Mod t = Mod (t -> t)

instance Monoid (Mod t) where
  mempty = Mod id
  Mod f `mappend` Mod g = Mod (g . f)

data Settings = Settings
  { _directory :: Maybe FilePath
  , _command   :: Maybe String
  , _env       :: [(String, String)]
  }

directory :: FilePath -> Mod Settings
directory p = Mod $ \s -> s { _directory = Just p }

command :: String -> Mod Settings
command c = Mod $ \s -> s { _command = Just c }

env :: [(String, String)] -> Mod Settings
env xs = Mod $ \s -> s { _env = xs }

infix 1 .=
(.=) :: a -> b -> (a, b)
(.=) = (,)

defaultSettings :: Settings
defaultSettings = Settings { _directory = Nothing, _command = Nothing, _env = [] }


-- | Ask what session user wants to create/attach to
prompt
  :: [String]          -- ^ Candidates patterns
  -> RouteT IO Command -- ^ Routing
  -> XPConfig          -- ^ Prompt theme
  -> X ()
prompt patterns route xpConfig = do
  cs <- currents
  ds <- concatMapM expand patterns
  let as = nubBy ((==) `on` un) $ map ('\'' :) cs ++ ds
  mkXPromptWithReturn (UnfuckedInputPrompt "tmux") xpConfig (compl' as) return
    >>= traverse_ (start cs route)

newtype UnfuckedInputPrompt = UnfuckedInputPrompt String

instance XPrompt UnfuckedInputPrompt  where
  showXPrompt (UnfuckedInputPrompt s) = s ++ ": "
  commandToComplete _ = id
  nextCompletion _ = getNextCompletion

type PromptKeymap = Map (KeyMask, KeySym) (XP ())

-- | Get current active tmux sessions names
currents :: X [String]
currents = io $ lines <$> runProcessWithInput "tmux" ["list-sessions", "-F", "#{session_name}"] ""

-- | Semifuzzy completion function
compl' :: [String] -> ComplFunction
compl' xs s  = return . sortBy status . sortOn (levenshtein s) . filter (\x -> s `isSubsequenceOf` un x) $ xs
 where
  status ('\'' : _) ('\'' : _) = EQ
  status ('\'' : _) _          = LT
  status _          ('\'' : _) = GT
  status _          _          = EQ

-- | Unquote string if it's quoted
un :: String -> String
un ('\'' : s) = s
un         s  = s

-- | A bit smarter tmux sessions search than 'isSuffixOf'
-- Checks that the first string is a subsequence of the second, that is
-- all its characters are present in the second string in the same order
isSubsequenceOf :: String -> String -> Bool
isSubsequenceOf []     _  = True
isSubsequenceOf (x:xs) ys =
  case dropWhile (/= x) ys of
    _ : zs -> xs `isSubsequenceOf` zs
    []     -> False

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = map fst . sortBy (comparing snd) . map (\x -> (x, f x))

levenshtein :: Eq a => [a] -> [a] -> Int
levenshtein xs ys = arr ! (max_i, max_j)
 where
  axs   = listArray (1, max_i) xs
  ays   = listArray (1, max_j) ys
  arr   = array ((0, 0), (max_i, max_j)) [((i, j), go i j) | i <- [0 .. max_i], j <- [0 .. max_j]]
  max_i = length xs
  max_j = length ys
  go i 0 = i
  go 0 j = j
  go i j = minimum [1 + arr ! (i - 1, j), 1 + arr ! (i, j - 1), c + arr ! (i - 1, j - 1)]
   where
    c = if axs ! i == ays ! j then 0 else 2

-- | Safely expands wordexp pattern catching IO errors
expand :: String -> X [FilePath]
expand p = io $ fmap (fmap (replace  '/' ' ')) (wordexp p) `mplus` return []


-- | Start tmux session terminal
-- May either start a new tmux session if it does not exist or connect to existing session
start :: [String] -> RouteT IO Command -> String -> X ()
start runningSessions route (un -> userInput) = do
  term <- asks $ terminal . config
  if userInput `elem` runningSessions
    then do
      let nameWindows = mapM (\w -> fmap (\n -> (show n, w)) (getName w)) . W.integrate' . W.stack
      ws <- gets windowset
      kv <- concatMapM nameWindows (W.workspaces ws)
      case lookup userInput kv of
        Nothing -> spawn $ createOrAttach term userInput
        Just w  -> windows (W.focusWindow w)
    else do
      routed <- io $ Route.run route userInput
      case routed of
        Just command -> create term command
        Nothing      -> spawn $ createOrAttach term userInput
 where
  createOrAttach t e =
    printf "%s -e tmux new-session -AD -s '%s'" t e
  create t c = safeSpawn t ("-e" : compile c)

compile :: Command -> [String]
compile = go where
  go (Hop h xs) = ["ssh", h, "-t"] ++ go xs
  go (Tmux n Settings { _command, _directory, _env }) =
       map (\(k, v) -> k ++ "=" ++ v) _env
    ++ ["tmux", "new-session", "-AD", "-s", n]
    ++ maybe [] (\d -> ["-c", "${HOME}" </> d]) _directory -- change working directory
    ++ maybeToList _command                                -- run this command

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = liftM concat . mapM f

routes :: RouteT IO Command
routes = do
  home <- getHome
  asum $
    [ route "git $repo" $ do
        repo <- arg "repo"
        let dir = "git" </> repo
        mkdir_p dir
        return (tmux dir (directory (home </> dir)))
    , route "svn $repo" $ do
        repo <- arg "repo"
        let dir = "svn" </> repo
        minusd dir <&> \y ->
          tmux dir (directory (if y then home </> dir else home </> "svn"))
    , route "play $bucket" $ do
        bucket <- arg "bucket"
        let dir = home </> "playground" </> bucket
        name <- input
        mkdir_p dir
        return (tmux name (directory dir))
    , route "re ko $dir" $
        arg "dir" <&> \dir -> hop "kolyskovi" (tmux dir (directory ("work" </> dir)))
    , route "re ko" $
        return (hop "kolyskovi" (tmux "main" mempty))
    , route "re slave $id" $
        arg "id" <&> \n -> hop ("slave" ++ show n) (tmux "main" (env ["TERM" .= "screen-256color"]))
    , route "re $host" $
        arg "host" <&> \host -> tmux host (command ("ssh " ++ show host))
    , route "work $session" $
        arg "session" <&> \session ->
          hop "ce837848" (hop "d378e6d3" (tmux (replace '.' '/' session) mempty))
    ]

getHome :: RouteT IO FilePath
getHome = liftIO (D.getHomeDirectory)

mkdir_p :: FilePath -> RouteT IO ()
mkdir_p = liftIO . D.createDirectoryIfMissing True

minusd :: FilePath -> RouteT IO Bool
minusd = liftIO . D.doesDirectoryExist

replace :: (Eq a, Functor f) => a -> a -> f a -> f a
replace y z = fmap (\x -> if x == y then z else x)

infixl 1 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
