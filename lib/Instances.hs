{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- Note that the iteration-limit is just increased until it works
{-# OPTIONS_GHC -fno-warn-orphans -fconstraint-solver-iterations=12 #-}

{-

Tuple instances for Groundhog. This is experimental.

It has been moved out from Database since it is cleaner this
way, and because the use of TypeFamilies caused some code
that was compiling to error out (presumably it just needed
type annotations).

-}

module Instances where

import Database.Groundhog.Core (Projection(..))
import Database.Groundhog.Instances ()

-- | It looks like Groundhog limits itself to tuples with only 5 members;
--   but we can do more, even if it isn't sensible.
--
instance (Projection a1 a1'
         , Projection a2 a2'
         , Projection a3 a3'
         , Projection a4 a4'
         , Projection a5 a5'
         , Projection a6 a6')
         => Projection (a1, a2, a3, a4, a5, a6)
                       (a1', a2', a3', a4', a5', a6') where
  
  type ProjectionDb (a1, a2, a3, a4, a5, a6) db =
    (ProjectionDb (a1, a2, a3, a4, a5) db
    , ProjectionDb a6 db)
    
  type ProjectionRestriction (a1, a2, a3, a4, a5, a6) r =
    (ProjectionRestriction (a1, a2, a3, a4, a5) r
    , ProjectionRestriction a6 r)
    
  projectionExprs (a1, a2, a3, a4, a5, a6) =
    projectionExprs a1 . projectionExprs a2 . projectionExprs a3 .
    projectionExprs a4 . projectionExprs a5 . projectionExprs a6
  
  projectionResult (a', b', c', d', e', f') xs = do
    (a, rest0) <- projectionResult a' xs
    (b, rest1) <- projectionResult b' rest0
    (c, rest2) <- projectionResult c' rest1
    (d, rest3) <- projectionResult d' rest2
    (e, rest4) <- projectionResult e' rest3
    (f, rest5) <- projectionResult f' rest4
    return ((a, b, c, d, e, f), rest5)


instance (Projection a1 a1'
         , Projection a2 a2'
         , Projection a3 a3'
         , Projection a4 a4'
         , Projection a5 a5'
         , Projection a6 a6'
         , Projection a7 a7')
         => Projection (a1, a2, a3, a4, a5, a6, a7)
                       (a1', a2', a3', a4', a5', a6', a7') where
  
  type ProjectionDb (a1, a2, a3, a4, a5, a6, a7) db =
    (ProjectionDb (a1, a2, a3, a4, a5, a6) db
    , ProjectionDb a7 db)
    
  type ProjectionRestriction (a1, a2, a3, a4, a5, a6, a7) r =
    (ProjectionRestriction (a1, a2, a3, a4, a5, a6) r
    , ProjectionRestriction a7 r)
    
  projectionExprs (a1, a2, a3, a4, a5, a6, a7) =
    projectionExprs a1 . projectionExprs a2 . projectionExprs a3 .
    projectionExprs a4 . projectionExprs a5 . projectionExprs a6 .
    projectionExprs a7
  
  projectionResult (a', b', c', d', e', f', g') xs = do
    (a, rest0) <- projectionResult a' xs
    (b, rest1) <- projectionResult b' rest0
    (c, rest2) <- projectionResult c' rest1
    (d, rest3) <- projectionResult d' rest2
    (e, rest4) <- projectionResult e' rest3
    (f, rest5) <- projectionResult f' rest4
    (g, rest6) <- projectionResult g' rest5
    return ((a, b, c, d, e, f, g), rest6)


instance (Projection a1 a1'
         , Projection a2 a2'
         , Projection a3 a3'
         , Projection a4 a4'
         , Projection a5 a5'
         , Projection a6 a6'
         , Projection a7 a7'
         , Projection a8 a8')
         => Projection (a1, a2, a3, a4, a5, a6, a7, a8)
                       (a1', a2', a3', a4', a5', a6', a7', a8') where
  
  type ProjectionDb (a1, a2, a3, a4, a5, a6, a7, a8) db =
    (ProjectionDb (a1, a2, a3, a4, a5, a6, a7) db
    , ProjectionDb a8 db)
    
  type ProjectionRestriction (a1, a2, a3, a4, a5, a6, a7, a8) r =
    (ProjectionRestriction (a1, a2, a3, a4, a5, a6, a7) r
    , ProjectionRestriction a8 r)
    
  projectionExprs (a1, a2, a3, a4, a5, a6, a7, a8) =
    projectionExprs a1 . projectionExprs a2 . projectionExprs a3 .
    projectionExprs a4 . projectionExprs a5 . projectionExprs a6 .
    projectionExprs a7 . projectionExprs a8
  
  projectionResult (a', b', c', d', e', f', g', h') xs = do
    (a, rest0) <- projectionResult a' xs
    (b, rest1) <- projectionResult b' rest0
    (c, rest2) <- projectionResult c' rest1
    (d, rest3) <- projectionResult d' rest2
    (e, rest4) <- projectionResult e' rest3
    (f, rest5) <- projectionResult f' rest4
    (g, rest6) <- projectionResult g' rest5
    (h, rest7) <- projectionResult h' rest6
    return ((a, b, c, d, e, f, g, h), rest7)


instance (Projection a1 a1'
         , Projection a2 a2'
         , Projection a3 a3'
         , Projection a4 a4'
         , Projection a5 a5'
         , Projection a6 a6'
         , Projection a7 a7'
         , Projection a8 a8'
         , Projection a9 a9')
         => Projection (a1, a2, a3, a4, a5, a6, a7, a8, a9)
                       (a1', a2', a3', a4', a5', a6', a7', a8', a9') where
  
  type ProjectionDb (a1, a2, a3, a4, a5, a6, a7, a8, a9) db =
    (ProjectionDb (a1, a2, a3, a4, a5, a6, a7, a8) db
    , ProjectionDb a9 db)
    
  type ProjectionRestriction (a1, a2, a3, a4, a5, a6, a7, a8, a9) r =
    (ProjectionRestriction (a1, a2, a3, a4, a5, a6, a7, a8) r
    , ProjectionRestriction a9 r)
    
  projectionExprs (a1, a2, a3, a4, a5, a6, a7, a8, a9) =
    projectionExprs a1 . projectionExprs a2 . projectionExprs a3 .
    projectionExprs a4 . projectionExprs a5 . projectionExprs a6 .
    projectionExprs a7 . projectionExprs a8 . projectionExprs a9
  
  projectionResult (a', b', c', d', e', f', g', h', i') xs = do
    (a, rest0) <- projectionResult a' xs
    (b, rest1) <- projectionResult b' rest0
    (c, rest2) <- projectionResult c' rest1
    (d, rest3) <- projectionResult d' rest2
    (e, rest4) <- projectionResult e' rest3
    (f, rest5) <- projectionResult f' rest4
    (g, rest6) <- projectionResult g' rest5
    (h, rest7) <- projectionResult h' rest6
    (i, rest8) <- projectionResult i' rest7
    return ((a, b, c, d, e, f, g, h, i), rest8)


instance (Projection a1 a1'
         , Projection a2 a2'
         , Projection a3 a3'
         , Projection a4 a4'
         , Projection a5 a5'
         , Projection a6 a6'
         , Projection a7 a7'
         , Projection a8 a8'
         , Projection a9 a9'
         , Projection a10 a10')
         => Projection (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
                       (a1', a2', a3', a4', a5', a6', a7', a8', a9'
                       , a10') where
  
  type ProjectionDb (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) db =
    (ProjectionDb (a1, a2, a3, a4, a5, a6, a7, a8, a9) db
    , ProjectionDb a10 db)
    
  type ProjectionRestriction (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) r =
    (ProjectionRestriction (a1, a2, a3, a4, a5, a6, a7, a8, a9) r
    , ProjectionRestriction a10 r)
    
  projectionExprs (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) =
    projectionExprs a1 . projectionExprs a2 . projectionExprs a3 .
    projectionExprs a4 . projectionExprs a5 . projectionExprs a6 .
    projectionExprs a7 . projectionExprs a8 . projectionExprs a9 .
    projectionExprs a10
  
  projectionResult (a', b', c', d', e', f', g', h', i', j') xs = do
    (a, rest0) <- projectionResult a' xs
    (b, rest1) <- projectionResult b' rest0
    (c, rest2) <- projectionResult c' rest1
    (d, rest3) <- projectionResult d' rest2
    (e, rest4) <- projectionResult e' rest3
    (f, rest5) <- projectionResult f' rest4
    (g, rest6) <- projectionResult g' rest5
    (h, rest7) <- projectionResult h' rest6
    (i, rest8) <- projectionResult i' rest7
    (j, rest9) <- projectionResult j' rest8
    return ((a, b, c, d, e, f, g, h, i, j), rest9)


instance (Projection a1 a1'
         , Projection a2 a2'
         , Projection a3 a3'
         , Projection a4 a4'
         , Projection a5 a5'
         , Projection a6 a6'
         , Projection a7 a7'
         , Projection a8 a8'
         , Projection a9 a9'
         , Projection a10 a10'
         , Projection a11 a11')
         => Projection (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
                       (a1', a2', a3', a4', a5', a6', a7', a8', a9'
                       , a10', a11') where
  
  type ProjectionDb (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) db =
    (ProjectionDb (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) db
    , ProjectionDb a11 db)
    
  type ProjectionRestriction (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) r =
    (ProjectionRestriction (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) r
    , ProjectionRestriction a11 r)
    
  projectionExprs (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) =
    projectionExprs a1 . projectionExprs a2 . projectionExprs a3 .
    projectionExprs a4 . projectionExprs a5 . projectionExprs a6 .
    projectionExprs a7 . projectionExprs a8 . projectionExprs a9 .
    projectionExprs a10 . projectionExprs a11
  
  projectionResult (a', b', c', d', e', f', g', h', i', j', k') xs = do
    (a, rest0) <- projectionResult a' xs
    (b, rest1) <- projectionResult b' rest0
    (c, rest2) <- projectionResult c' rest1
    (d, rest3) <- projectionResult d' rest2
    (e, rest4) <- projectionResult e' rest3
    (f, rest5) <- projectionResult f' rest4
    (g, rest6) <- projectionResult g' rest5
    (h, rest7) <- projectionResult h' rest6
    (i, rest8) <- projectionResult i' rest7
    (j, rest9) <- projectionResult j' rest8
    (k, rest10) <- projectionResult k' rest9
    return ((a, b, c, d, e, f, g, h, i, j, k), rest10)


instance (Projection a1 a1'
         , Projection a2 a2'
         , Projection a3 a3'
         , Projection a4 a4'
         , Projection a5 a5'
         , Projection a6 a6'
         , Projection a7 a7'
         , Projection a8 a8'
         , Projection a9 a9'
         , Projection a10 a10'
         , Projection a11 a11'
         , Projection a12 a12')
         => Projection (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
                       (a1', a2', a3', a4', a5', a6', a7', a8', a9'
                       , a10', a11', a12') where
  
  type ProjectionDb (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11
                    , a12) db =
    (ProjectionDb (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) db
    , ProjectionDb a12 db)
    
  type ProjectionRestriction (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10
                             , a11, a12) r =
    (ProjectionRestriction (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) r
    , ProjectionRestriction a12 r)
    
  projectionExprs (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) =
    projectionExprs a1 . projectionExprs a2 . projectionExprs a3 .
    projectionExprs a4 . projectionExprs a5 . projectionExprs a6 .
    projectionExprs a7 . projectionExprs a8 . projectionExprs a9 .
    projectionExprs a10 . projectionExprs a11 . projectionExprs a12
  
  projectionResult (a', b', c', d', e', f', g', h', i', j', k', l') xs = do
    (a, rest0) <- projectionResult a' xs
    (b, rest1) <- projectionResult b' rest0
    (c, rest2) <- projectionResult c' rest1
    (d, rest3) <- projectionResult d' rest2
    (e, rest4) <- projectionResult e' rest3
    (f, rest5) <- projectionResult f' rest4
    (g, rest6) <- projectionResult g' rest5
    (h, rest7) <- projectionResult h' rest6
    (i, rest8) <- projectionResult i' rest7
    (j, rest9) <- projectionResult j' rest8
    (k, rest10) <- projectionResult k' rest9
    (l, rest11) <- projectionResult l' rest10
    return ((a, b, c, d, e, f, g, h, i, j, k, l), rest11)


instance (Projection a1 a1'
         , Projection a2 a2'
         , Projection a3 a3'
         , Projection a4 a4'
         , Projection a5 a5'
         , Projection a6 a6'
         , Projection a7 a7'
         , Projection a8 a8'
         , Projection a9 a9'
         , Projection a10 a10'
         , Projection a11 a11'
         , Projection a12 a12'
         , Projection a13 a13')
         => Projection (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11
                       , a12, a13)
                       (a1', a2', a3', a4', a5', a6', a7', a8', a9'
                       , a10', a11', a12', a13') where
  
  type ProjectionDb (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11
                    , a12, a13) db =
    (ProjectionDb (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) db
    , ProjectionDb a13 db)
    
  type ProjectionRestriction (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10
                             , a11, a12, a13) r =
    (ProjectionRestriction (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10
                           , a11, a12) r
    , ProjectionRestriction a13 r)
    
  projectionExprs (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) =
    projectionExprs a1 . projectionExprs a2 . projectionExprs a3 .
    projectionExprs a4 . projectionExprs a5 . projectionExprs a6 .
    projectionExprs a7 . projectionExprs a8 . projectionExprs a9 .
    projectionExprs a10 . projectionExprs a11 . projectionExprs a12 .
    projectionExprs a13
  
  projectionResult
    (a', b', c', d', e', f', g', h', i', j', k', l', m') xs = do
    (a, rest0) <- projectionResult a' xs
    (b, rest1) <- projectionResult b' rest0
    (c, rest2) <- projectionResult c' rest1
    (d, rest3) <- projectionResult d' rest2
    (e, rest4) <- projectionResult e' rest3
    (f, rest5) <- projectionResult f' rest4
    (g, rest6) <- projectionResult g' rest5
    (h, rest7) <- projectionResult h' rest6
    (i, rest8) <- projectionResult i' rest7
    (j, rest9) <- projectionResult j' rest8
    (k, rest10) <- projectionResult k' rest9
    (l, rest11) <- projectionResult l' rest10
    (m, rest12) <- projectionResult m' rest11
    return ((a, b, c, d, e, f, g, h, i, j, k, l, m), rest12)


instance (Projection a1 a1'
         , Projection a2 a2'
         , Projection a3 a3'
         , Projection a4 a4'
         , Projection a5 a5'
         , Projection a6 a6'
         , Projection a7 a7'
         , Projection a8 a8'
         , Projection a9 a9'
         , Projection a10 a10'
         , Projection a11 a11'
         , Projection a12 a12'
         , Projection a13 a13'
         , Projection a14 a14')
         => Projection (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11
                       , a12, a13, a14)
                       (a1', a2', a3', a4', a5', a6', a7', a8', a9'
                       , a10', a11', a12', a13', a14') where
  
  type ProjectionDb (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11
                    , a12, a13, a14) db =
    (ProjectionDb (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) db
    , ProjectionDb a14 db)
    
  type ProjectionRestriction (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10
                             , a11, a12, a13, a14) r =
    (ProjectionRestriction (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10
                           , a11, a12, a13) r
    , ProjectionRestriction a14 r)
    
  projectionExprs (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12
                  , a13, a14) =
    projectionExprs a1 . projectionExprs a2 . projectionExprs a3 .
    projectionExprs a4 . projectionExprs a5 . projectionExprs a6 .
    projectionExprs a7 . projectionExprs a8 . projectionExprs a9 .
    projectionExprs a10 . projectionExprs a11 . projectionExprs a12 .
    projectionExprs a13 . projectionExprs a14
  
  projectionResult
    (a', b', c', d', e', f', g', h', i', j', k', l', m', n') xs = do
    (a, rest0) <- projectionResult a' xs
    (b, rest1) <- projectionResult b' rest0
    (c, rest2) <- projectionResult c' rest1
    (d, rest3) <- projectionResult d' rest2
    (e, rest4) <- projectionResult e' rest3
    (f, rest5) <- projectionResult f' rest4
    (g, rest6) <- projectionResult g' rest5
    (h, rest7) <- projectionResult h' rest6
    (i, rest8) <- projectionResult i' rest7
    (j, rest9) <- projectionResult j' rest8
    (k, rest10) <- projectionResult k' rest9
    (l, rest11) <- projectionResult l' rest10
    (m, rest12) <- projectionResult m' rest11
    (n, rest13) <- projectionResult n' rest12
    return ((a, b, c, d, e, f, g, h, i, j, k, l, m, n), rest13)


instance (Projection a1 a1'
         , Projection a2 a2'
         , Projection a3 a3'
         , Projection a4 a4'
         , Projection a5 a5'
         , Projection a6 a6'
         , Projection a7 a7'
         , Projection a8 a8'
         , Projection a9 a9'
         , Projection a10 a10'
         , Projection a11 a11'
         , Projection a12 a12'
         , Projection a13 a13'
         , Projection a14 a14'
         , Projection a15 a15')
         => Projection (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11
                       , a12, a13, a14, a15)
                       (a1', a2', a3', a4', a5', a6', a7', a8', a9'
                       , a10', a11', a12', a13', a14', a15') where
  
  type ProjectionDb (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11
                    , a12, a13, a14, a15) db =
    (ProjectionDb (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12
                  , a13, a14) db
    , ProjectionDb a15 db)
    
  type ProjectionRestriction (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10
                             , a11, a12, a13, a14, a15) r =
    (ProjectionRestriction (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10
                           , a11, a12, a13, a14) r
    , ProjectionRestriction a15 r)
    
  projectionExprs (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12
                  , a13, a14, a15) =
    projectionExprs a1 . projectionExprs a2 . projectionExprs a3 .
    projectionExprs a4 . projectionExprs a5 . projectionExprs a6 .
    projectionExprs a7 . projectionExprs a8 . projectionExprs a9 .
    projectionExprs a10 . projectionExprs a11 . projectionExprs a12 .
    projectionExprs a13 . projectionExprs a14 . projectionExprs a15
  
  projectionResult
    (a', b', c', d', e', f', g', h', i', j', k', l', m', n', o') xs = do
    (a, rest0) <- projectionResult a' xs
    (b, rest1) <- projectionResult b' rest0
    (c, rest2) <- projectionResult c' rest1
    (d, rest3) <- projectionResult d' rest2
    (e, rest4) <- projectionResult e' rest3
    (f, rest5) <- projectionResult f' rest4
    (g, rest6) <- projectionResult g' rest5
    (h, rest7) <- projectionResult h' rest6
    (i, rest8) <- projectionResult i' rest7
    (j, rest9) <- projectionResult j' rest8
    (k, rest10) <- projectionResult k' rest9
    (l, rest11) <- projectionResult l' rest10
    (m, rest12) <- projectionResult m' rest11
    (n, rest13) <- projectionResult n' rest12
    (o, rest14) <- projectionResult o' rest13
    return ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o), rest14)
