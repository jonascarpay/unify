{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module OnionFind
  ( UnifyIT,
    UnifyT,
    Unify,
    UnifyI,
    Point,
    fresh,
    unify,
    unifyWith,
    unifyErr,
    unifyGeneric,
    runUnifyT,
    runUnify,
    writePoint,
    readPoint,
    rootElems,
    rootKeys,
    roots,
    unPoint,
    bind,
    fuse,
    morph,
    morphM,
    morphT,
    unsafeMorph,
  )
where

import Control.Monad
import Control.Monad.Trans.Class
import Data.Coerce (coerce)
import Data.Functor.Identity
import Data.IntMap.Strict
import qualified Data.IntMap.Strict as IntMap

-- | Using this as a transformer is technically unsafe.
-- If the underlying monad can branch, there is no guarantuee that a 'Point' from one brancy doesn't bleed into another, where it is invalid.
newtype UnifyIT s i o m a = UnifyT
  { unUnify :: forall r. (a -> IntMap Int -> IntMap o -> Int -> m r) -> IntMap Int -> IntMap i -> Int -> m r
  }
  deriving stock (Functor)

instance MonadTrans (UnifyIT s t t) where
  lift m = UnifyT $ \ok l r c -> m >>= (\a -> ok a l r c)

type UnifyT s t = UnifyIT s t t

type UnifyI s i o = UnifyIT s i o Identity

type Unify s t = UnifyIT s t t Identity

runUnifyT :: Applicative m => (forall s. UnifyIT s i o m a) -> m a
runUnifyT (UnifyT k) = k (\a _ _ _ -> pure a) mempty mempty 0

runUnify :: (forall s. UnifyI s i o a) -> a
runUnify k = runIdentity $ runUnifyT k

instance Applicative (UnifyIT s t t m) where
  pure a = UnifyT $ \ok -> ok a
  (<*>) = ap

instance Monad (UnifyIT s t t m) where
  UnifyT k >>= f = UnifyT $ \ok -> k (\a -> (unUnify $ f a) ok)

rootElems :: UnifyT s t m [t]
rootElems = IntMap.elems <$> rootsRaw

rootKeys :: UnifyT s t m [Point s]
rootKeys = coerce . IntMap.keys <$> rootsRaw

roots :: UnifyT s t m [(Point s, t)]
roots = coerce . IntMap.toList <$> rootsRaw

rootsRaw :: UnifyT s t m (IntMap t)
rootsRaw = UnifyT $ \ok l r -> ok r l r

find :: Int -> IntMap Int -> (Int, IntMap Int)
find n l = case IntMap.lookup n l of
  Nothing -> (n, l)
  Just n' ->
    let (rep, l') = find n' l
     in ( rep,
          if n' == rep then IntMap.insert n rep l' else l'
        )

newtype Point s = Point {unPoint :: Int}
  deriving newtype (Eq, Ord, Show)

writePoint :: Point s -> t -> UnifyT s t m ()
writePoint (Point n) t = UnifyT $ \ok l r ->
  let (rep, l') = find n l
   in ok () l' (IntMap.insert rep t r)

readPoint :: Point s -> UnifyT s t m t
readPoint (Point p) = UnifyT $ \ok l r c ->
  let (rep, l') = find p l
   in ok (r IntMap.! rep) l' r c

{-# INLINE fresh #-}
fresh :: t -> UnifyT s t m (Point s)
fresh t = UnifyT $ \ok link root count -> ok (Point count) link (IntMap.insert count t root) (count + 1)

{-# INLINE bind #-}
bind :: UnifyIT s i h m a -> (a -> UnifyIT s h o m b) -> UnifyIT s i o m b
bind (UnifyT i) f = UnifyT $ \ok -> i (\a -> (unUnify $ f a) ok)

{-# INLINE fuse #-}
fuse :: (a -> b -> c) -> UnifyIT s i h m a -> UnifyIT s h o m b -> UnifyIT s i o m c
fuse f (UnifyT i) (UnifyT o) = UnifyT $ \ok -> i (\a -> o (ok . f a))

morph :: (i -> o) -> UnifyIT s i o m ()
morph f = unsafeMorph (fmap f)

{-# ANN morphT "HLint: ignore Eta reduce" #-}
morphT :: (forall f. Traversable f => f i -> f o) -> UnifyIT s i o m ()
morphT f = unsafeMorph f

morphM :: (IntMap i -> i -> o) -> UnifyIT s i o m ()
morphM f = unsafeMorph (\i -> f i <$> i)

local :: UnifyIT s t t' m () -> UnifyIT s t' t m () -> UnifyT s t' m a -> UnifyT s t m a
local scope unscope inner = fuse (const id) scope $ fuse const inner unscope

{-# INLINE unsafeMorph #-}
unsafeMorph :: (IntMap i -> IntMap o) -> UnifyIT s i o m ()
unsafeMorph f = UnifyT $ \ok l r -> ok () l (f r)

unify :: Point s -> Point s -> UnifyT s t m ()
unify (Point p1) (Point p2) = UnifyT $ \ok l r n ->
  let (r1, l1) = find p1 l
      (r2, l2) = find p2 l1
   in ok () (IntMap.insert r2 r1 l2) (IntMap.delete r2 r) n

unifyWith :: (t -> t -> t) -> Point s -> Point s -> UnifyT s t m ()
unifyWith f p1 p2 = void $ unifyGeneric (\a b -> pure ((), f a b)) p1 p2

unifyErr :: (t -> t -> Either e t) -> Point s -> Point s -> UnifyT s t m (Maybe e)
unifyErr f p1 p2 = either pure (const Nothing) <$> unifyGeneric f' p1 p2
  where
    f' a b = case f a b of
      Left e -> Left e
      Right r -> Right ((), r)

{-# INLINE unifyGeneric #-}
unifyGeneric :: (t -> t -> Either e (info, t)) -> Point s -> Point s -> UnifyT s t m (Either e info)
unifyGeneric f (Point p1) (Point p2) = UnifyT $ \ok l r n ->
  let (r1, l1) = find p1 l
      (r2, l2) = find p2 l1
      t1 = r IntMap.! r1
      t2 = r IntMap.! r2
   in case f t1 t2 of
        Left e -> ok (Left e) l r n
        Right (info, t) -> ok (Right info) (IntMap.insert r2 r1 l2) (IntMap.insert r1 t $ IntMap.delete r2 r) n
