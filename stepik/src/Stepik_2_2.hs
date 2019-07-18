{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module Stepik_2_2 where

import           Control.Applicative (Alternative (..), ZipList (..),
                                      getZipList, liftA2, (<**>))
import           Data.Char           (isDigit)
import           Data.Function       ((&))
