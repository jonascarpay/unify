-- | This module provides a flexible interface to the disjoint-set/union-find data structure.
--
-- The simplest ways of unifying are through 'unify'/'unifyErr'.
-- These should be sufficient for simple applications, such as certain graph algorithms.
-- Once you have recursive unification however, i.e. unification of sets that refer to other sets, it is common to need more elaborate logic, in which case you might want to look at 'unsafeUnifyM' or 'preUnifyM'.
--
-- Because all functions are generic in the underlying monad, they can be used on top of both ST- and IO-based monad transformer stacks.
module UnionFind
  ( Node,
    fresh,
    descriptor,
    unify,
    unifyErr,
    equivalent,
    setDescriptor,
    modifyDescriptor,
    unsafeUnifyM,
    preUnifyM,
    MonadST (..),
  )
where

import Control.Monad (liftM2, unless)
import Control.Monad.ST.Class
import Control.Monad.Trans.Except
import Data.Function (on)
import Data.STRef
import UnionFind.Internal

fresh :: MonadST m => a -> m (Node (World m) a)
fresh a = liftST $ Node <$> newSTRef (Root a)

descriptor :: MonadST m => Node (World m) a -> m a
descriptor = fmap snd . findRoot

-- | Test if two nodes are part of the same set.
equivalent :: MonadST m => Node (World m) a -> Node (World m) a -> m Bool
equivalent a b = liftM2 ((==) `on` fst) (findRoot a) (findRoot b)

-- | Unify two sets.
-- This is a noop if the two nodes belong to the same set.
{-# INLINE unify #-}
unify :: MonadST m => (a -> a -> a) -> Node (World m) a -> Node (World m) a -> m ()
unify f na nb = liftST $ unsafeUnifyM (\a b -> pure (f a b)) na nb

-- | Unify two sets, potentially throwing an error.
-- This is a noop if the two nodes belong to the same set.
{-# INLINE unifyErr #-}
unifyErr :: MonadST m => (a -> a -> Either e a) -> Node (World m) a -> Node (World m) a -> m (Maybe e)
unifyErr f na nb = fmap (either Just (const Nothing)) $ runExceptT $ unsafeUnifyM k na nb
  where
    k a b = case f a b of
      Left err -> throwE err
      Right r -> pure r

{-# INLINE setDescriptor #-}
setDescriptor :: MonadST m => Node (World m) a -> a -> m ()
setDescriptor p r = modifyDescriptor p (const r)

{-# INLINE modifyDescriptor #-}
modifyDescriptor :: MonadST m => Node (World m) a -> (a -> a) -> m ()
modifyDescriptor p f = do
  (rep, r) <- findRoot p
  writeNode rep (Root $ f r)

-- | Unifies two non-equivalent sets.
-- If they're already 'equivalent', this is a noop.
-- If they're not equivalent, it does what the type signature suggests:
-- the continuation is called on the set's respective descriptors, and after unification the resulting set's descriptor is set to the continuation's return value.
--
-- The reason this is unsafe is that if, within the continuation, you unify either of the sets with a third set you get undefined behavior.
-- Modifying either descriptor is safe, it will just get overwritten when the continuation exits.
--
-- Recursive unification is a tricky, and there is no catch-all default way of handling it.
-- So, this trusts the user to either not do anything effectful, or add their own logic for making sure sets aren't reused in the inner calculation.
-- Here are two example ways you could implement an [occurs check](https://en.wikipedia.org/wiki/Occurs_check):
--
--   1. Keep track of the sets you are trying to unify in a Reader environment. Before unifying, see if either of the sets are in the list of pending environments.
--   2. Use 'setDescriptor' to flag nodes as visited/dirty/pending at the start of the continuation. If you encounter a flagged node, you have infinite recursion.
{-# INLINE unsafeUnifyM #-}
unsafeUnifyM ::
  MonadST m =>
  (a -> a -> m a) ->
  Node (World m) a ->
  Node (World m) a ->
  m ()
unsafeUnifyM f na nb = do
  (ra, a) <- findRoot na
  (rb, b) <- findRoot nb
  unless (ra == rb) $ do
    a' <- f a b
    writeNode ra (Root a')
    writeNode rb (Child ra)

-- | Unifies two non-equivalent sets.
-- If they're already 'equivalent', this is a noop.
-- If they're not equivalent, it first unifies the sets, and then calls the continuation on the respective descriptors.
--
-- There are two main differences w.r.t. 'unsafeUnifyM':
--
--   1. The sets are unified _before_ the continuation is called.
--      The advantage is that unifying them within the continuation is no longer undefined.
--      The disadvantage is that throwing an error in the continuation no longer aborts unification.
--      For example, for this reason 'unifyErr' cannot be implemented in terms of 'preUnifyM'.
--   2. It is up to the user to update the unified set's descriptor.
--      You should assume that until you do, it is undefined.
--      In practice, it just gets the first argument's descriptor.
--
-- If you're not interested in recovering from errors, this is probably what you want to use.
-- It makes it so you don't have to consider both sets when unifying recursively.
{-# INLINE preUnifyM #-}
preUnifyM ::
  MonadST m =>
  (Node (World m) a -> a -> a -> m ()) ->
  Node (World m) a ->
  Node (World m) a ->
  m ()
preUnifyM f na nb = do
  (ra, a) <- findRoot na
  (rb, b) <- findRoot nb
  unless (ra == rb) $ do
    writeNode rb (Child ra)
    f ra a b
