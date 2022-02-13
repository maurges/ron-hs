{-# LANGUAGE KindSignatures, TypeApplications #-}
module Data.Ron.Class.Internal
    ( ProductSize (productSize)
    ) where

import GHC.Generics ((:*:)(..), M1)
import Data.Proxy (Proxy (Proxy))
import Data.Kind (Type)


class ProductSize (f :: Type -> Type) where
    productSize :: Proxy f -> Int

instance ProductSize (M1 c s f) where
    productSize _ = 1
    {-# INLINE productSize #-}

instance (ProductSize fl, ProductSize fr) => ProductSize (fl :*: fr) where
    productSize _ = productSize (Proxy @fl) + productSize (Proxy @fr)
    {-# INLINE productSize #-}
