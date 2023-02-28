{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module OnionFind
  ( IUnify,
    Unify,
    Point,
    fresh,
    unify,
    unifyWith,
    unifyErr,
    unifyGeneric,
    runUnify,
    writePoint,
    readPoint,
    rootElems,
    rootKeys,
    roots,
    unPoint,
    bind,
    fuse,
    fuseAp,
    morph,
    morphM,
    morphT,
    unsafeMorph,
  )
where

import Control.Monad
import Data.Coerce (coerce)
import Data.IntMap.Strict
import qualified Data.IntMap.Strict as IntMap

newtype IUnify s i o a = IUnify
  { unUnify :: forall r. (a -> IntMap Int -> IntMap o -> Int -> r) -> IntMap Int -> IntMap i -> Int -> r
  }
  deriving stock (Functor)

type Unify s t = IUnify s t t

runUnify :: (forall s. IUnify s i o a) -> a
runUnify (IUnify k) = k (\a _ _ _ -> a) mempty mempty 0

instance Applicative (IUnify s t t) where
  pure a = IUnify $ \ok -> ok a
  (<*>) = ap

instance Monad (IUnify s t t) where
  IUnify k >>= f = IUnify $ \ok -> k (\a -> (unUnify $ f a) ok)

rootElems :: Unify s t [t]
rootElems = IntMap.elems <$> rootsRaw

rootKeys :: Unify s t [Point s]
rootKeys = coerce . IntMap.keys <$> rootsRaw

roots :: Unify s t [(Point s, t)]
roots = coerce . IntMap.toList <$> rootsRaw

rootsRaw :: Unify s t (IntMap t)
rootsRaw = IUnify $ \ok l r -> ok r l r

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

writePoint :: Point s -> t -> Unify s t ()
writePoint (Point n) t = IUnify $ \ok l r ->
  let (rep, l') = find n l
   in ok () l' (IntMap.insert rep t r)

readPoint :: Point s -> Unify s t t
readPoint (Point p) = IUnify $ \ok l r c ->
  let (rep, l') = find p l
   in ok (r IntMap.! rep) l' r c

fresh :: t -> Unify s t (Point s)
fresh t = IUnify $ \ok link root count -> ok (Point count) link (IntMap.insert count t root) (count + 1)

bind :: IUnify s i h a -> (a -> IUnify s h o b) -> IUnify s i o b
bind (IUnify i) f = IUnify $ \ok l r n -> i (\a l' r' n' -> (unUnify $ f a) ok l' r' n') l r n

fuse :: (a -> b -> c) -> IUnify s i h a -> IUnify s h o b -> IUnify s i o c
fuse f (IUnify i) (IUnify o) = IUnify $ \ok l r n -> i (\a l' r' n' -> o (ok . f a) l' r' n') l r n

fuseAp :: IUnify s i h (a -> b) -> IUnify s h o a -> IUnify s i o b
fuseAp (IUnify i) (IUnify o) = IUnify $ \ok -> i (\f -> o (ok . f))

morph :: (i -> o) -> IUnify s i o ()
morph f = unsafeMorph (fmap f)

morphT :: (forall f. Traversable f => f i -> f o) -> IUnify s i o ()
morphT f = unsafeMorph f

morphM :: (IntMap i -> i -> o) -> IUnify s i o ()
morphM f = unsafeMorph (\i -> f i <$> i)

{-# INLINE unsafeMorph #-}
unsafeMorph :: (IntMap i -> IntMap o) -> IUnify s i o ()
unsafeMorph f = IUnify $ \ok l r -> ok () l (f r)

unify :: Point s -> Point s -> IUnify s t t ()
unify (Point p1) (Point p2) = IUnify $ \ok l r n ->
  let (r1, l1) = find p1 l
      (r2, l2) = find p2 l1
   in ok () (IntMap.insert r2 r1 l2) (IntMap.delete r2 r) n

unifyWith :: (t -> t -> t) -> Point s -> Point s -> IUnify s t t ()
unifyWith f (Point p1) (Point p2) = IUnify $ \ok l r n ->
  let (r1, l1) = find p1 l
      (r2, l2) = find p2 l1
      t1 = r IntMap.! r1
      t2 = r IntMap.! r2
   in ok () (IntMap.insert r2 r1 l2) (IntMap.insert r1 (f t1 t2) $ IntMap.delete r2 r) n

unifyErr :: (t -> t -> Either e t) -> Point s -> Point s -> IUnify s t t (Maybe e)
unifyErr f (Point p1) (Point p2) = IUnify $ \ok l r n ->
  let (r1, l1) = find p1 l
      (r2, l2) = find p2 l1
      t1 = r IntMap.! r1
      t2 = r IntMap.! r2
   in case f t1 t2 of
        Left e -> ok (Just e) l r n
        Right t -> ok Nothing (IntMap.insert r2 r1 l2) (IntMap.insert r1 t $ IntMap.delete r2 r) n

unifyGeneric :: (t -> t -> Either e (info, t)) -> Point s -> Point s -> IUnify s t t (Either e info)
unifyGeneric f (Point p1) (Point p2) = IUnify $ \ok l r n ->
  let (r1, l1) = find p1 l
      (r2, l2) = find p2 l1
      t1 = r IntMap.! r1
      t2 = r IntMap.! r2
   in case f t1 t2 of
        Left e -> ok (Left e) l r n
        Right (info, t) -> ok (Right info) (IntMap.insert r2 r1 l2) (IntMap.insert r1 t $ IntMap.delete r2 r) n
