{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Grading.Utils.Tar
    ( ByteString
    , CheckedArchive
    , uncheckedSize
    , checkedSize
    , toBS
    , uncheck
    , tarFolder
    , extractArchive
    , checkArchive
    , checkArchive_
    ) where

import qualified Codec.Archive.Tar                as Tar
import qualified Codec.Archive.Tar.Check          as Tar
import           Codec.Compression.GZip           (compress, decompress)
import           Control.Exception                (Exception (..), SomeException (..), throwIO, try)
import           Control.Monad                    (unless)
import           Control.Monad.IO.Class           (MonadIO (..))
import           Data.Binary                      (Binary)
import           Data.ByteString.Lazy             (ByteString)
import qualified Data.ByteString.Lazy             as BS
import           Data.Coerce                      (coerce)
import           Data.Foldable                    (asum)
import           Database.SQLite.Simple.FromField (FromField)
import           Database.SQLite.Simple.ToField   (ToField)
import           Servant                          (MimeRender, MimeUnrender, OctetStream)
import           System.Directory                 (doesDirectoryExist, makeAbsolute)

import           Grading.Types

newtype CheckedArchive = CheckedArchive UncheckedArchive
    deriving (Show, Read, Eq, Ord, Binary, FromField, ToField, MimeRender OctetStream, MimeUnrender OctetStream)

toBS :: CheckedArchive -> ByteString
toBS = coerce

uncheck :: CheckedArchive -> UncheckedArchive
uncheck = coerce

uncheckedSize :: UncheckedArchive -> Int
uncheckedSize (UncheckedArchive bs) = fromIntegral $ BS.length bs

checkedSize :: CheckedArchive -> Int
checkedSize = uncheckedSize . uncheck

tarFolder :: MonadIO m => FilePath -> m CheckedArchive
tarFolder f = liftIO $ do
    a         <- normFolder f
    unchecked <- UncheckedArchive . compress . Tar.write <$> Tar.pack a ["."]
    echecked  <- checkArchive unchecked
    case echecked of
        Right checked -> return checked
        Left e        -> throwIO e 

extractArchive :: MonadIO m => CheckedArchive -> FilePath -> m ()
extractArchive a f = liftIO $ Tar.unpack f $ Tar.read $ decompress $ toBS a

normFolder :: MonadIO m => FilePath -> m FilePath
normFolder f = liftIO $ do
    b <- doesDirectoryExist f
    unless b $ throwIO $ userError $ "folder " ++ show f ++ " does not exists"
    makeAbsolute f

data TarError =
      DecompressionError !String
    | FormatError !Tar.FormatError
    | TarBombError !Tar.TarBombError
    | PortabilityError !Tar.PortabilityError
    | FileNameError !Tar.FileNameError
    deriving Show

instance Exception TarError where

    toException e@(DecompressionError _) = SomeException e
    toException (FormatError e)          = toException e
    toException (TarBombError e)         = toException e
    toException (PortabilityError e)     = toException e
    toException (FileNameError e)        = toException e

    fromException e = asum $ map ($ e) 
        [ fmap FormatError . fromException
        , fmap TarBombError . fromException
        , fmap PortabilityError . fromException
        , fmap FileNameError . fromException
        ]

toTarError :: Either (Either (Either Tar.FormatError Tar.TarBombError) Tar.PortabilityError) Tar.FileNameError -> TarError
toTarError (Right e)               = FileNameError e
toTarError (Left (Right e))        = PortabilityError e
toTarError (Left (Left (Right e))) = TarBombError e
toTarError (Left (Left (Left e)))  = FormatError e

toEntries :: ByteString -> Tar.Entries TarError
toEntries = fmap toTarError 
          . Tar.checkSecurity 
          . Tar.checkPortability 
          . Tar.checkTarbomb "." 
          . Tar.read 
          . decompress

checkArchive :: MonadIO m => UncheckedArchive -> m (Either TarError CheckedArchive)
checkArchive unchecked@(UncheckedArchive bs) = liftIO $ do
    em <- try $ checkEntries $ toEntries bs
    return $ case em of
        Left (e :: SomeException) -> Left (DecompressionError $ displayException e)
        Right (Just e)            -> Left e
        Right Nothing             -> Right $ CheckedArchive unchecked
  where
    checkEntries :: Tar.Entries TarError -> IO (Maybe TarError)
    checkEntries (Tar.Next _ es) = checkEntries es
    checkEntries Tar.Done        = return Nothing
    checkEntries (Tar.Fail e)    = return $ Just e

checkArchive_ :: MonadIO m => UncheckedArchive -> m CheckedArchive
checkArchive_ unchecked = do
    echecked <- checkArchive unchecked
    case echecked of
        Right checked -> return checked
        Left e        -> liftIO $ ioError $ userError $ displayException e
