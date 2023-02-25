{-# LANGUAGE LambdaCase #-}

module UnionFind.Internal where

import Control.Monad.ST.Class
import Data.STRef

-- | A 'Node' is either the root/representative node of a set, or a child of another node.
--
-- Warning: the 'Eq' instance is _pointer equality_, it doesn't check if two 'Node's are part of the same set.
-- For that, use 'equivalent'.
newtype Node s a = Node (STRef s (Member s a))
  deriving (Eq)

data Member s a
  = Root a
  | Child (Node s a)

{-# INLINE readNode #-}
readNode :: MonadST m => Node (World m) a -> m (Member (World m) a)
readNode (Node n) = liftST $ readSTRef n

{-# INLINE writeNode #-}
writeNode :: MonadST m => Node (World m) a -> Member (World m) a -> m ()
writeNode (Node n) m = liftST $ writeSTRef n m

-- | Finds the root member of a set, and its value.
-- This is internal, not because it's unsafe (it's not), but because the user should never have to care what the representative member is.
{-# INLINE findRoot #-}
findRoot :: MonadST m => Node (World m) a -> m (Node (World m) a, a)
findRoot n =
  readNode n >>= \case
    Root a -> pure (n, a)
    Child n' -> do
      r'@(rep, _) <- findRoot n'
      writeNode n (Child rep)
      pure r'
