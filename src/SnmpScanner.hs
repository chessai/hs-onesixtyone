{-# language
    BangPatterns
  , DuplicateRecordFields
  , LambdaCase
  , NamedFieldPuns
  , NumericUnderscores
  , RecordWildCards
  , ScopedTypeVariables
  , StrictData
  #-}

module SnmpScanner where

import Control.Applicative ((<|>))
import Control.Concurrent
import Control.Exception (SomeException, bracket, try)
import Control.Monad (void)
import Data.Foldable (foldlM)
import Data.Maybe (catMaybes)
import Data.Primitive
import Data.Word
import Language.Asn.Types
import Net.IPv4 (IPv4, IPv4Range)
import Prelude hiding (log)
import Snmp.Client (SnmpException(..), Credentials(..), CredentialsV2(..))
import Snmp.Types ()
import System.Log.FastLogger
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified GHC.Exts as Exts
import qualified Language.Asn.ObjectIdentifier as Oid
import qualified Net.IPv4 as IPv4
import qualified Options.Applicative as O
import qualified Snmp.Client as Snmp

_MAX_COMMUNITIES :: Int
_MAX_COMMUNITIES = 16384

_MAX_HOSTS :: Int
_MAX_HOSTS = 65535

_MAX_COMMUNITY_SIZE :: Int
_MAX_COMMUNITY_SIZE = 32

data Config = Config
  { communities :: ~FilePath
  , hosts :: ~FilePath
  , logs :: ~FilePath
  , quiet :: Bool
  , wait :: Word16
  , port :: Word16
  }

parserConfig :: O.Parser Config
parserConfig = Config
  <$> (O.strOption $ mconcat [O.long "communities", O.short 'c', O.metavar "FILEPATH", O.help "File with community names to try"])
  <*> (O.strOption $ mconcat [O.long "hosts", O.short 'h', O.metavar "FILEPATH", O.help "File with hosts to try. A host is either a single IPv4 address or a subnet (IPv4 with netmask)"])
  <*> (O.strOption $ mconcat [O.long "logs", O.short 'l', O.metavar "FILEPATH", O.help "Output log file"])
  <*> (O.switch $ mconcat [O.long "quiet", O.short 'q', O.help "Quiet mode. Do not dump to stdout."])
  <*> (O.option O.auto $ mconcat [O.long "wait", O.short 'w', O.metavar "Word16", O.help "Wait n milliseconds between sending packets", O.value 10, O.showDefault])
  <*> (O.option O.auto $ mconcat [O.long "port", O.short 'p', O.metavar "PORT", O.help "Specify alternate SNMP port (affects all hosts)", O.value 161, O.showDefault])

data Host
  = Single IPv4
  | Subnet IPv4Range

data Ctx = Ctx
  { communities :: Array Credentials
  , hosts :: Array Host
  , logs :: ~FilePath
  , quiet :: Bool
  , wait :: Word16
  , port :: Word16
  }

configToCtx :: Config -> IO Ctx
configToCtx Config{communities=cs,hosts=hs,..} = do
  communities <- readCommunities cs
  hosts <- readHosts hs
  pure Ctx{..}

splitNewlines :: BL.ByteString -> [BL.ByteString]
splitNewlines = BL.split '\n'

readCommunities :: FilePath -> IO (Array Credentials)
readCommunities cs = do
  x <- BL.readFile cs
  let filterLen b =
        let len = B.length b
        in if len >= 1 && len < 33 then Just b else Nothing
  let filterWS b = TE.encodeUtf8 . T.strip . TE.decodeUtf8 $ b

  -- TODO: fuse this/see if this fuses (doubt it)
  -- TODO: report failures
  let ret = id
        . Exts.fromList
        . catMaybes
        . map (fmap (CredentialsConstructV2 . CredentialsV2))
        . map (fmap filterWS . filterLen . BL.toStrict)
        . take _MAX_COMMUNITIES
        . splitNewlines
  pure $ ret x

readHosts :: FilePath -> IO (Array Host)
readHosts hs = do
  x <- BL.readFile hs
  let decodeHost b = (Subnet <$> IPv4.decodeRange (TE.decodeUtf8 b))
        <|> (Single <$> IPv4.decodeUtf8 b)
  -- TODO: report failures
  let ret = id
        . Exts.fromList
        . catMaybes
        . map (decodeHost . BL.toStrict)
        . take _MAX_HOSTS
        . splitNewlines
  pure $ ret x

modelOid :: ObjectIdentifier
modelOid = Oid.fromList [1,3,6,1,2,1,1,2,0]

withLogger :: Ctx -> (FastLogger -> IO a) -> IO a
withLogger Ctx{logs} = withFastLogger (LogFileNoRotate logs 4080)

poll :: FastLogger -> Ctx -> Snmp.Session -> Host -> IO ()
poll log ctx session = \case
  Single ip -> do
    p <- pollSingle ctx session ip
    log $ toLogStr $ show p
  Subnet rng -> do
    p <- pollSubnet ctx session rng
    log $ toLogStr $ show p

pollSubnet :: Ctx -> Snmp.Session -> IPv4Range -> IO (Either (Maybe SnmpException) ())
pollSubnet _ _ _ = pure (Right ())

pollSingle :: Ctx -> Snmp.Session -> IPv4 -> IO (Either (Maybe SnmpException) ())
pollSingle Ctx{wait,communities,port} session host = do
  let mkCtx = Snmp.Context session (Snmp.Destination host port)
  let sz = length communities
  let go !ix merr = if ix < sz
        then do
          creds <- indexArrayM communities ix
          e <- Snmp.get' (mkCtx creds) modelOid
          case e of
            Left err -> do
              -- TODO: accumulate snmp exceptions?
              threadDelay (fromIntegral wait * 1_000)
              go (ix + 1) (Just err)
            Right _ -> do
              pure (Right ())
        else do
          pure (Left merr)
  go 0 Nothing

main :: IO ()
main = do
  cfg <- O.execParser $ O.info
    (parserConfig O.<**> O.helper)
    O.fullDesc
  run cfg

run :: Config -> IO ()
run cfg = do
  ctx@Ctx{..} <- configToCtx cfg
  e <- withLogger ctx $ \log -> do
    withSession ctx $ \s -> do
      fold (poll log ctx s) hosts
  case e of
    Left err -> do
      putStrLn $ "Encountered error: " ++ show err
    Right () -> do
      putStrLn $ "Completed."

withSession :: Ctx -> (Snmp.Session -> IO a) -> IO a
withSession Ctx{hosts} f = do
  -- threads equal to number of hosts
  let threads = length hosts
  -- timeout after 10 seconds
  let timeout = 10_000_000
  -- number of retries
  let retries = 0
  let cfg = Snmp.Config threads timeout retries

  bracket (Snmp.openSession cfg) (Snmp.closeSession) f

fold :: forall t m a. (Foldable t, Monoid m)
  => (a -> IO m)
  -> t a
  -> IO (Either SomeException m)
fold f xs = do
  var <- newEmptyMVar
  total <- foldlM
    (\ !n a -> do
      void $ forkIO (try (f a) >>= putMVar var)
      pure (n + 1)
    ) 0 xs
  let go2 :: Int -> SomeException -> IO (Either SomeException m)
      go2 !n e = if n < total
        then takeMVar var *> go2 (n + 1) e
        else pure (Left e)
  let go :: Int -> m -> IO (Either SomeException m)
      go !n !m = if n < total
        then takeMVar var >>= \case
          Left r -> go2 (n + 1) r
          Right m' -> go (n + 1) (m <> m')
        else pure (Right m)
  go 0 mempty
