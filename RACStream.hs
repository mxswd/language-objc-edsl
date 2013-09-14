{-# LANGUAGE DataKinds, RankNTypes, TypeOperators, GADTs, KindSignatures, FlexibleContexts, FlexibleInstances, InstanceSigs, MultiParamTypeClasses, TypeFamilies #-}
module RACStream where

import RAC
import qualified Prelude as P


-- the length of a view
sizeSig :: RACSignal FKSize
sizeSig = undefined

-- take length and turn into square rectangle
squareFun :: RACSignal FKSize -> RACSignal FKRect
squareFun = undefined

-- you can check using those functions together give the correct type
f :: RACSignal (FKTuple FKSize FKRect)
f = take 4 (zipWith sizeSig (map (squareFun) sizeSig))

undefined = P.undefined

data family Lift :: FK -> *

-- `self` is always the last argument

empty :: RACSignal a
empty = undefined

return :: RACSignal a -> RACSignal a
return = P.id

-- TODO
bind :: ((Lift a) -> ()) -> RACSignal a -> RACSignal a
bind = undefined

-- TODO
concat :: RACSignal a -> RACSignal a -> RACSignal a
concat = undefined

-- TODO
zipWith :: RACSignal a -> RACSignal b -> RACSignal (FKTuple a b)
zipWith = undefined

-- TODO
-- flattenMap :: RACSignal a -> RACSignal ((Lift a) ~> (Lift b)) -> RACSignal b
-- flattenMap = undefined

-- TODO
flatten :: RACSignal a -> RACSignal a
flatten = undefined

-- TODO
map :: (RACSignal a -> RACSignal b) -> RACSignal a -> RACSignal b
map = undefined

-- TODO
mapReplace :: RACSignal a -> (Lift b) -> RACSignal b
mapReplace = undefined

-- TODO
filter :: ((Lift a) -> P.Bool) -> RACSignal a -> RACSignal a
filter = undefined

-- TODO
ignore :: P.Eq (Lift a) => RACSignal a -> RACSignal a
ignore = undefined -- P.filter

-- TODO
reduceEach = undefined
startWith = undefined

-- TODO
skip :: P.Int -> RACSignal a -> RACSignal a
skip = undefined

-- TODO
take :: P.Int -> RACSignal a -> RACSignal a
take = undefined --  P.take

-- TODO
zip = undefined
zipReduce = undefined
-- concat = undefined
scanWithStartReduce = undefined
-- combinePreviousWithStartReduceWith

-- TODO
takeUntilBlock :: ((Lift a) -> P.Bool) -> RACSignal a -> RACSignal a
takeUntilBlock = undefined

-- TODO
takeWhileBlock :: ((Lift a) -> P.Bool) -> RACSignal a -> RACSignal a
takeWhileBlock = undefined

-- TODO
skipUntilBlock :: ((Lift a) -> P.Bool) -> RACSignal a -> RACSignal a
skipUntilBlock = undefined

-- TODO
skipWhileBlock :: ((Lift a) -> P.Bool) -> RACSignal a -> RACSignal a
skipWhileBlock = undefined
