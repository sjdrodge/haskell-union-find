module Lib
  ( DisjointForest,
    fromList,
    toDebugList,
    toList,
    empty,
    insert,
    find,
    union,
  )
where

import Control.Monad
import Control.Monad.ST (ST)
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef)

-- TODO: make a decision about Map
newtype DisjointForest s a = DisjointForest {unDisjointForest :: STRef s (Map a (Entry s a))}

type Entry s a = STRef s (EntryData s a)

data EntryData s a
  = Root {entryValue :: a, entryRank :: Int}
  | Node {entryValue :: a, entryParent :: Entry s a}

fromList :: Ord a => [a] -> ST s (DisjointForest s a)
fromList xs = do
  d <- empty
  traverse_ (`insert` d) xs
  pure d

toDebugList :: DisjointForest s a -> ST s [(a, Int, a)]
toDebugList = readSTRef . unDisjointForest >=> traverse (readSTRef >=> entryToTriple) . M.elems
  where
    entryToTriple Root {entryValue = v, entryRank = n} = pure (v, n, v)
    entryToTriple Node {entryValue = v, entryParent = p} = (,,) v 0 . entryValue <$> readSTRef p

toList :: DisjointForest s a -> ST s [a]
toList (DisjointForest d) = M.keys <$> readSTRef d

empty :: ST s (DisjointForest s a)
empty = DisjointForest <$> newSTRef M.empty

insert :: Ord a => a -> DisjointForest s a -> ST s ()
insert x d = do
  y <- find x d
  case y of
    Nothing -> do
      e <- newSTRef (Root {entryValue = x, entryRank = 0})
      modifySTRef' (unDisjointForest d) $ M.insert x e
    Just _ -> pure ()

find' :: Ord a => Entry s a -> ST s (Entry s a)
find' e = do
  e_d <- readSTRef e
  case e_d of
    Root {} -> pure e
    Node {entryParent = e'} -> (let r = find' e' in (r >>= modifySTRef' e . parentReplace >> r))
      where
        parentReplace p e_d'@Node {} = e_d' {entryParent = p}
        parentReplace p Root {} = error "parentReplace should never be called on a Root"

find :: Ord a => a -> DisjointForest s a -> ST s (Maybe a)
find x d = do
  m_e <- M.lookup x <$> readSTRef (unDisjointForest d)
  case m_e of
    Nothing -> pure Nothing
    Just e -> do
      e' <- find' e
      e_d' <- readSTRef e'
      case e_d' of
        Root {entryValue = v} -> pure $ Just v
        Node {} -> error "find' should always return a Root"

union :: Ord a => a -> a -> DisjointForest s a -> ST s ()
union x y d = do
  insert x d
  insert y d
  m <- readSTRef (unDisjointForest d)
  -- fromJust cannot fail since we just inserted
  e_x <- find' <$> fromJust $ M.lookup x m
  e_y <- find' <$> fromJust $ M.lookup y m
  ed_x <- readSTRef e_x
  ed_y <- readSTRef e_y
  if entryRank ed_x < entryRank ed_y
    then modifySTRef' e_x (newParent e_y) >> modifySTRef' e_y incrRank
    else modifySTRef' e_x incrRank >> modifySTRef' e_y (newParent e_x)
  where
    newParent p Root {entryValue = v} = Node {entryValue = v, entryParent = p}
    newParent _ Node {} = error "newParent should only ever be called on a Root"
    incrRank r@Root {entryRank = n} = r {entryRank = n + 1}
    incrRank Node {} = error "incrRank should only ever be called on a Root"