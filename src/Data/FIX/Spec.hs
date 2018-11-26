{-# LANGUAGE CPP               #-}
{-# LANGUAGE EmptyCase         #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}

module Data.FIX.Spec where

import Data.FIX.Message (FIXSpec(fsVersion))

#ifdef __FIX40__
import Data.FIX.Spec.FIX40 (fix40)
#endif
#ifdef __FIX41__
import Data.FIX.Spec.FIX41 (fix41)
#endif
#ifdef __FIX42__
import Data.FIX.Spec.FIX42 (fix42)
#endif
#ifdef __FIX43__
import Data.FIX.Spec.FIX43 (fix43)
#endif
#ifdef __FIX44__
import Data.FIX.Spec.FIX44 (fix44)
#endif

data FIXVersion where
#ifdef __FIX40__
  FIX40 :: FIXVersion
#endif
#ifdef __FIX41__
  FIX41 :: FIXVersion
#endif
#ifdef __FIX42__
  FIX42 :: FIXVersion
#endif
#ifdef __FIX43__
  FIX43 :: FIXVersion
#endif
#ifdef __FIX44__
  FIX44 :: FIXVersion
#endif
#ifdef __HAS_FIX__
  deriving (Eq, Ord, Show)
#endif

fixSpec :: FIXVersion -> FIXSpec
fixSpec = \case
#ifdef __FIX40__
  FIX40 -> fix40
#endif
#ifdef __FIX41__
  FIX41 -> fix41
#endif
#ifdef __FIX42__
  FIX42 -> fix42
#endif
#ifdef __FIX43__
  FIX43 -> fix43
#endif
#ifdef __FIX44__
  FIX44 -> fix44
#endif

fixVersions :: [FIXVersion]
fixVersions
  = []
#ifdef __FIX40__
  ++ [FIX40]
#endif
#ifdef __FIX41__
  ++ [FIX41]
#endif
#ifdef __FIX42__
  ++ [FIX42]
#endif
#ifdef __FIX43__
  ++ [FIX43]
#endif
#ifdef __FIX44__
  ++ [FIX44]
#endif

versionMapping :: [(String, FIXVersion)]
versionMapping =
  [(fsVersion (fixSpec v), v) | v <- fixVersions]

findVersion :: String -> Maybe FIXVersion
findVersion v = lookup v versionMapping
