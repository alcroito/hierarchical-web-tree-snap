{-# OPTIONS_GHC -w -fno-warn-unused-imports -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts, GADTs, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, ScopedTypeVariables #-}

module TreeModel(
	Tree(..),
	Mytree(..),
	TreeInfo(..),
	main2,
	setupDB,
	deleteFromTreeTable,
	deleteNodeFromTreeTable,
	insertTestData,
	insertIntoTreeTable,
	getTreeFromDB,
	selectListOfElements,
	migrateAll
	)
where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Monad.Logger
import qualified Data.Foldable as F
import Data.Monoid
import GHC.Int (Int64)
import Database.Esqueleto
import Database.Persist.TH
import Database.Persist.Types
import Database.Persist.Sqlite hiding ((==.))
import Language.Haskell.TH


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
TreeObject
     value String
     nid Int
     parent TreeObjectId Maybe
     UniqueNid nid
     deriving Eq Show
|]

data TreeInfo = TreeInfo {text :: String, nid :: Int, pid :: Int} deriving (Show, Eq, Read)
data Tree a = EmptyTree | Node {value :: a, children :: [Tree a]} deriving (Show, Eq, Read)
type Mytree = Tree TreeInfo

instance F.Foldable Tree where
    foldMap f (Node node children) = f (node) `mappend` F.foldMap (F.foldMap f) children

main2 = runSqlite "myhaskdb.db" $ do
	setupDB
	deleteFromTreeTable
	--insertIntoTreeTable $ Just 5
	insertTestData
	trees <- selectListOfElements
	let mylist = mapFromListOfTreeObjectsToTree $ getTreeValues trees
	liftIO $ print mylist
	liftIO $ print "List of tree values"
	liftIO $ mapM_ print $ getTreeValues trees

setupDB :: SqlPersistT (NoLoggingT (ResourceT IO)) ()
setupDB = do
	runMigration migrateAll
	--rawExecute "COMMIT; PRAGMA foreign_keys = ON; BEGIN;" []

getTreeValues :: [Entity (TreeObjectGeneric backend)] -> [(String, Int, Maybe (KeyBackend backend (TreeObjectGeneric backend)))]
getTreeValues = map $ \entity -> (treeObjectValue $ entityVal entity, treeObjectNid $ entityVal entity, treeObjectParent $ entityVal entity)

computeKey :: Int64 -> Maybe (KeyBackend backend entity)
computeKey x = Just $ Key (PersistInt64 x)

getTreeFromDB :: SqlPersistT (NoLoggingT (ResourceT IO)) (Mytree)
getTreeFromDB = do 
	db_trees <- selectListOfElements
	let tree = mapFromListOfTreeObjectsToTree $ getTreeValues db_trees
	return tree

insertIntoTreeTable ::
     String
     -> Int
     -> Int64
     -> SqlPersistT (NoLoggingT (ResourceT IO)) (Key (TreeObjectGeneric SqlBackend))
insertIntoTreeTable text nid pid = do
	list_of_nids <- getMaxNidFromDB
	let (Value nid) = head list_of_nids
	insert $ TreeObject text (nid + 1) $ computeKey pid

insertTestData :: SqlPersistT (NoLoggingT (ResourceT IO)) ()
insertTestData = mapM_ insertOne [("v1", 1, 0), ("v2", 2, 1), ("v3", 3, 1), ("v4", 4, 1), ("v5", 5, 2), ("v6", 6, 3)]
				 where insertOne (text, nid, pid) = insert $ TreeObject text nid $ computeKey pid

deleteFromTreeTable :: SqlPersistT (NoLoggingT (ResourceT IO)) ()
deleteFromTreeTable = do
	Database.Esqueleto.delete $
		from $ \(t :: SqlExpr (Entity TreeObject)) -> 
			return ()

deleteNodeFromTreeTable :: Int -> Mytree -> SqlPersistT (NoLoggingT (ResourceT IO)) ()
deleteNodeFromTreeTable nid tree = do
	let ids_to_delete = getTreeChildren $ findElementInTree2 nid tree
	Database.Esqueleto.delete $
		from $ \t -> do
			where_ (t ^. TreeObjectNid ==. val nid)
	Database.Esqueleto.delete $
		from $ \t -> do
			where_ (t ^. TreeObjectNid `in_` valList ids_to_delete)

getMaxNidFromDB :: SqlPersistT (NoLoggingT (ResourceT IO)) [Value Int]
getMaxNidFromDB = do
	select $
		from $ \t -> do
			return $ max_ (t ^. TreeObjectNid)

treeFilter :: (Monad m, F.Foldable t, Monoid (m a)) => (a -> Bool) -> t a -> m a
treeFilter p = F.foldMap (\a -> if p a then return a else mempty)

annotateTree :: Tree a -> Tree (Tree a)
annotateTree (Node node children) = Node (Node node children) (map annotateTree children)

findElementInTree2 :: Int -> Tree TreeInfo -> Tree TreeInfo
findElementInTree2 fnid tree = 
	case filter_result of 
		[] -> EmptyTree
		otherwise -> head filter_result
	where 
		filter_result = treeFilter predicate $ annotateTree tree 
		predicate (Node (TreeInfo _ nid _) _) = fnid == nid

findElementInTree fnid EmptyTree = EmptyTree
findElementInTree fnid (Node (TreeInfo text nid pid) children)
  | fnid == nid = Node (TreeInfo text nid pid) children
  | fnid /= nid = foldl fold_function EmptyTree children
  where fold_function acc subtree = let result = findElementInTree fnid subtree 
  									in if result == EmptyTree then acc else result

getTreeChildren EmptyTree = []
getTreeChildren (Node (TreeInfo _ _ _) []) = []
getTreeChildren (Node (TreeInfo _ _ _) children) =
	foldl fold_function [] children
	where fold_function acc (Node (TreeInfo text child_nid parent) child_children) = 
		  child_nid : (getTreeChildren (Node (TreeInfo text child_nid parent) child_children)) ++ acc

selectListOfElements  :: SqlPersistT (NoLoggingT (ResourceT IO)) [Entity (TreeObjectGeneric SqlBackend)]
selectListOfElements = do
	select $
		from $ \treeobject -> do
			return treeobject

mapFromListOfTreeObjectsToTree :: [(String, Int, Maybe (KeyBackend backend entity))] -> Mytree
mapFromListOfTreeObjectsToTree entities = foldl insertTreeObjectIntoTree EmptyTree entities

insertTreeObjectIntoTree :: Mytree-> (String, Int, Maybe (KeyBackend backend entity)) -> Mytree
insertTreeObjectIntoTree tree (text, nid, parent_id_wrapped) = insertElem tree text nid $ keyToInt parent_id_wrapped

insertElem :: Mytree -> String -> Int -> Int -> Mytree
insertElem EmptyTree text nid parent = Node (TreeInfo text nid parent) []
insertElem (Node (TreeInfo p_text p_nid p_pid) p_children) text nid parent
	| p_nid == parent = Node (TreeInfo p_text p_nid p_pid) (Node (TreeInfo text nid parent) [] : p_children)
	| p_nid /= parent = Node (TreeInfo p_text p_nid p_nid) $ map (\child -> insertElem child text nid parent) p_children


keyToInt :: Num b => Maybe (KeyBackend backend entity) -> b
keyToInt (Just complex_id) = persIntToInt $ unKey complex_id

persIntToInt :: Num b => PersistValue -> b
persIntToInt (PersistInt64 p) = fromIntegral p
