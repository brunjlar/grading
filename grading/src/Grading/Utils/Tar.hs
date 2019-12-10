{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Grading.Utils.Tar
    ( ByteString
    , IsChecked (..)
    , Archive
    , archive
    , emptyArchive
    , archivedBytes
    , archiveSize
    , nullArchive
    , uncheck
    , tarFolder
    , extractArchive
    , normFolder
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
import           GHC.Generics                     (Generic)
import           Servant                          (MimeRender, MimeUnrender, OctetStream)
import           System.Directory                 (doesDirectoryExist, makeAbsolute)

data IsChecked = Checked | Unchecked

newtype Archive (c :: IsChecked) = Archive ByteString
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving newtype (MimeRender OctetStream, MimeUnrender OctetStream, FromField, ToField, Binary)

archive :: ByteString -> Archive Unchecked
archive = Archive

emptyArchive :: Archive c
emptyArchive = Archive BS.empty

archivedBytes :: Archive c -> ByteString
archivedBytes = coerce

uncheck :: Archive Checked -> Archive Unchecked
uncheck = coerce

archiveSize :: Archive c -> Int
archiveSize = fromIntegral . BS.length . archivedBytes

nullArchive :: Archive c -> Bool
nullArchive = (== 0) . archiveSize

tarFolder :: MonadIO m => FilePath -> m (Archive Checked)
tarFolder f = liftIO $ do
    a         <- normFolder f
    unchecked <- archive . compress . Tar.write <$> Tar.pack a ["."]
    echecked  <- checkArchive unchecked
    case echecked of
        Right checked -> return checked
        Left e        -> throwIO e 

extractArchive :: MonadIO m => Archive Checked -> FilePath -> m ()
extractArchive a f = liftIO $ Tar.unpack f $ Tar.read $ decompress $ archivedBytes a

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

checkArchive :: MonadIO m => Archive Unchecked -> m (Either TarError (Archive Checked))
checkArchive unchecked
    | nullArchive unchecked = return $ Right emptyArchive
    | otherwise             = liftIO $ do
        em <- try $ checkEntries $ toEntries $ archivedBytes unchecked
        return $ case em of
            Left (e :: SomeException) -> Left (DecompressionError $ displayException e)
            Right (Just e)            -> Left e
            Right Nothing             -> Right (coerce unchecked)
  where
    checkEntries :: Tar.Entries TarError -> IO (Maybe TarError)
    checkEntries (Tar.Next _ es) = checkEntries es
    checkEntries Tar.Done        = return Nothing
    checkEntries (Tar.Fail e)    = return $ Just e

checkArchive_ :: MonadIO m => (Archive Unchecked) -> m (Archive Checked)
checkArchive_ unchecked = do
    echecked <- checkArchive unchecked
    case echecked of
        Right checked -> return checked
        Left e        -> liftIO $ ioError $ userError $ displayException e
