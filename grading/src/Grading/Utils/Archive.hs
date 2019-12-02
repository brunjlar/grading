{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

module src.Grading.Utils.Archive
    ( IsChecked (..)
    , Archive
    , archive
    , archivedBytes
    , archiveSize
    ) where

data IsChecked = Checked | Unchecked

newtype Archive (c :: IsChecked) = Archive ByteString
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving newtype (MimeRender OctetStream, MimeUnrender OctetStream, FromField, ToField, Binary)

archive :: ByteString -> Archive Unchecked
archive = Archive

archivedBytes :: Archive c -> ByteString
archivedBytes = coerce

archiveSize :: Archive c -> Int
archiveSize = fromIntegral . BS.length . archiveBytes
