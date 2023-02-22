{-# LANGUAGE LambdaCase #-}

module UnionFind
  ( Point,
    fresh,
    repr,
    unsafeUnify,
    equivalent,
    update,
  )
where

import Control.Monad (unless)
import Control.Monad.ST.Class
import Data.STRef

-- | Warning: the Eq instance is _pointer equality_, it doesn't check if two Point are part of the same set.
-- For that, use 'equivalent'.
newtype Point s a = Point {unPoint :: STRef s (Link s a)}
  deriving (Eq)

data Link s a
  = Rep a
  | Link (Point s a)

fresh :: MonadST m => a -> m (Point (World m) a)
fresh a = liftST $ Point <$> newSTRef (Rep a)

repr :: MonadST m => Point (World m) a -> m a
repr = fmap snd . rep

equivalent :: MonadST m => Point (World m) a -> Point (World m) a -> m Bool
equivalent a b = do
  (ra, _) <- rep a
  (rb, _) <- rep b
  pure $ ra == rb

{-# INLINE rep #-}
rep :: MonadST m => Point (World m) a -> m (Point (World m) a, a)
rep (Point p) =
  liftST (readSTRef p) >>= \case
    Rep a -> pure (Point p, a)
    Link p' -> do
      r' <- rep p'
      liftST (writeSTRef p $ Link p')
      pure r'

-- | Unify two sets.
-- This is a noop if the two points belong to the same set.
-- It's unsafe because reusing the 'Point's in the inner action is undefined behavior.
-- Unfortunately, there's not really a good, safe default, so I leave it up to the user to not shoot themselves in the foot.
{-# INLINE unsafeUnify #-}
unsafeUnify :: MonadST m => (a -> a -> m a) -> Point (World m) a -> Point (World m) a -> m ()
unsafeUnify f pa pb = do
  (ra, a) <- rep pa
  (rb, b) <- rep pb
  unless (ra == rb) $ do
    r <- f a b
    liftST $ do
      writeSTRef (unPoint ra) $ Rep r
      writeSTRef (unPoint rb) $ Link pa

update :: MonadST m => a -> Point (World m) a -> m ()
update a' p = do
  (ra, _) <- rep p
  liftST $ writeSTRef (unPoint ra) (Rep a')
