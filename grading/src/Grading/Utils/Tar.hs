{-# LANGUAGE ScopedTypeVariables #-}

module Grading.Utils.Tar
    ( tarFolder
    , checkArchive
    ) where

import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Check as Tar
import           Codec.Compression.GZip  (compress, decompress)
import           Control.Exception       (Exception (..), SomeException (..), throwIO, try)
import           Control.Monad           (unless)
import           Data.ByteString.Lazy    (ByteString)
import           Data.Foldable           (asum)
import           System.Directory        (doesDirectoryExist, makeAbsolute)

tarFolder :: FilePath -> IO ByteString
tarFolder f = do
    a  <- normFolder f
    bs <- compress . Tar.write <$> Tar.pack a ["."]
    m  <- checkArchive bs
    case m of
        Nothing -> return bs
        Just e  -> throwIO e 

normFolder :: FilePath -> IO FilePath
normFolder f = do
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

checkArchive :: ByteString -> IO (Maybe TarError)
checkArchive bs = do
    em <- try $ checkEntries $ toEntries bs
    case em of
        Left (ex :: SomeException) -> return $ Just (DecompressionError $ show ex)
        Right m                    -> return m
  where
    checkEntries :: Tar.Entries TarError -> IO (Maybe TarError)
    checkEntries (Tar.Next _ es) = checkEntries es
    checkEntries Tar.Done        = return Nothing
    checkEntries (Tar.Fail e)    = return $ Just e
