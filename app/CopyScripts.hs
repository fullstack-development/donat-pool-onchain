{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Exception
import Data.Aeson
import Data.Aeson.Casing (aesonDrop, camelCase)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BL
import Data.List (isSuffixOf)
import Data.Maybe (maybe)
import GHC.Generics (Generic)
import System.Directory

main :: IO ()
main = do
  let sourceDir = "compiled"
  let targetDir = "../offchain/scripts"
  allFiles <- getDirectoryContents sourceDir
  let plutusScripts = filter (isSuffixOf ".plutus") allFiles
  mapM_ (copyScriptWithConvertation sourceDir targetDir) plutusScripts
  print "Done"

copyScriptWithConvertation :: FilePath -> FilePath -> FilePath -> IO ()
copyScriptWithConvertation sourceDir targetDir fileName = do
  scriptBs <- BL.readFile $ sourceDir <> "/" <> fileName
  let mbPlyDescr :: Maybe PlyDescription = decode scriptBs
  plyDescr <- maybe (throwIO PlyScriptDecodingError) pure mbPlyDescr
  let ctlDescr = convertScriptDescription plyDescr
  BL.writeFile (targetDir <> "/" <> fileName) (encodePretty ctlDescr)

data PlyDescription = PlyDescription
  { pdCborHex :: String
  , pdDescription :: String
  , pdVersion :: String
  }
  deriving (Eq, Show, Generic)

instance FromJSON PlyDescription where
  parseJSON = genericParseJSON $ aesonDrop 2 camelCase

instance ToJSON PlyDescription where
  toJSON = genericToJSON $ aesonDrop 2 camelCase

data CtlDescription = CtlDescription
  { cdCborHex :: String
  , cdDescription :: String
  , cdType :: String
  }
  deriving (Eq, Show, Generic)

instance FromJSON CtlDescription where
  parseJSON = genericParseJSON $ aesonDrop 2 camelCase

instance ToJSON CtlDescription where
  toJSON = genericToJSON $ aesonDrop 2 camelCase

convertScriptDescription :: PlyDescription -> CtlDescription
convertScriptDescription PlyDescription {..} =
  CtlDescription
    { cdCborHex = pdCborHex
    , cdDescription = pdDescription
    , cdType = "Plutus" <> pdVersion
    }

data PlyScriptDecodingError = PlyScriptDecodingError
  deriving (Show)

instance Exception PlyScriptDecodingError
