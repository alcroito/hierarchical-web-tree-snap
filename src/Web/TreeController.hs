
{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies  #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell, GADTs, QuasiQuotes, ScopedTypeVariables, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -w -fno-warn-unused-imports -fno-warn-type-defaults #-}
module TreeController(runApp) where

import qualified Paths_hierarchical_web_tree_snap as P

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Either
import Control.Monad.Logger
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Maybe
import Data.Monoid
import System.Directory
import Data.Char (toLower)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B

import Database.Persist.Sqlite
import Data.Pool (Pool)
import GHC.Int (Int64)
import TreeModel

import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server

import Heist
import Heist.Interpreted
import Blaze.ByteString.Builder
import qualified  Text.XmlHtml as X

connectionCount = 2

data AppState m = AppState {
  _sqlPool :: Pool Connection,
  _heistState :: HeistState m
}

makeAppState p s = AppState p s

askPool = do
  (AppState pool _) <- ask
  return pool

askHeistState = do
  (AppState _ heistState) <- ask
  return heistState

--runApp :: IO ()
runApp = 
  withSqlitePool "myhaskdb.db" connectionCount $ \pool -> do
    dataDir <- P.getDataDir
    setCurrentDirectory dataDir
    heistState <- initHeistState
    runDB pool $ do
        setupDB
        deleteFromTreeTable
        insertTestData
    quickHttpServe $ runReaderT appRouter $ makeAppState pool heistState


--appRouter:: (MonadReader (AppState) f, MonadSnap f) => f ()
appRouter =
    ifTop homepage <|>
    route [ ("add/:nid", insertNewNode)
          , ("delete/:nid", deleteSelectedNodeAndChildren)
          , ("", (serveDirectory "."))
          ]

--runDB :: MonadBaseControl IO m => Pool Connection -> SqlPersistT (NoLoggingT (ResourceT m)) a -> m a
runDB pool action = runResourceT . runNoLoggingT $ runSqlPool action pool 

--echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param

--insertNewNode :: (MonadReader (AppState IO) m, MonadSnap m) => m ()
insertNewNode = do
    pool <- askPool
    pid_bytestring <- getParam "nid"
    let pid = fromIntegral $ fst $ fromJust $ B.readInt $ fromJust pid_bytestring
    tree <- liftIO $ do 
      runDB pool $ insertIntoTreeTable "additional" 10 pid
      runDB pool getTreeFromDB
    makeTemplateResponse tree

-- deleteSelectedNodeAndChildren :: (MonadReader (AppState IO) m, MonadSnap m) => m ()
deleteSelectedNodeAndChildren = do
    pool <- askPool
    nid_bytestring <- getParam "nid"
    let nid = fromIntegral $ fst $ fromJust $ B.readInt $ fromJust nid_bytestring
    tree <- liftIO $ do
      when (nid /= 1) $ do
        old_tree <- runDB pool getTreeFromDB
        runDB pool $ deleteNodeFromTreeTable nid old_tree
      runDB pool getTreeFromDB
    makeTemplateResponse tree

--homepage :: (MonadReader (AppState) m, MonadSnap m) => m ()
homepage = do
    pool <- askPool
    tree <- liftIO $ runDB pool getTreeFromDB
    makeTemplateResponse tree

--makeTemplateResponse :: Tree TreeInfo -> m ()
makeTemplateResponse tree = do
  myHeistState <- askHeistState
  let new_state = bindStrings [("sitetitle", "Snap trees"), ("headtitle", "Snap trees"), ("keywords", "snap, esqueleto, persistent, sqlite")] myHeistState
  let new_state2 = bindSplice "tree" (renderTreeSplice tree) new_state
  result <- liftIO $ renderHeistTemplate new_state2 "tree"
  writeLBS result

--renderTreeSplice' :: [Node] -> Tree TreeInfo -> HeistT n n Template
renderTreeSplice' _ EmptyTree = textSplice "temporary stop"
renderTreeSplice' treeNodeChildrenTags tree@(Node (TreeInfo text nid pid) children) = do
    localHS (bindSplices splices) (runNodeList treeNodeChildrenTags)
  where
    splices = [("nodevalue", textSplice $ T.pack text),
               ("nid", textSplice $ T.pack $ show nid),
               ("deletelink", deleteLinkSplice nid),
               ("debug_tree", textSplice $ T.pack $ show tree),
               ("children", mapSplices (renderTreeSplice' treeNodeChildrenTags) children)]

deleteLinkSplice nid = case nid of
  1 -> return []
  otherwise -> return [deleteLink nid]
deleteLink nid = X.Element "a" [("href", T.pack ("/delete/" ++ (show nid)))]  [deleteLinkSpan]
deleteLinkSpan = X.Element "span" [("class", "glyphicon glyphicon-minus-sign")] [X.TextNode ""]

-- renderTreeSplice :: (Monad m, Functor m) => Tree TreeInfo -> HeistT m m Template
renderTreeSplice tree = do
    treeNodeChildrenTags <- X.childNodes <$> getParamNode
    renderTreeSplice' treeNodeChildrenTags tree

--initHeistState :: IO (HeistState IO)
initHeistState = do
  ehs <- runEitherT $ initHeist myHeistConfig
  let (Right ts) = ehs
  return ts

--myHeistConfig :: MonadIO m => HeistConfig m
myHeistConfig = mempty
    { hcInterpretedSplices = defaultInterpretedSplices
    , hcTemplateLocations = [loadTemplates "templates"]
    }

renderHeistTemplate heistState templateName = do 
  result <- renderTemplate heistState templateName
  case result of
         Nothing -> error "Error finding template"
         Just (builderM, mimeType) ->
             return (toLazyByteString builderM)

