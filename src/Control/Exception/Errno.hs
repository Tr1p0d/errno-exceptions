{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}

------------------------------------------------------------------
-- |
-- Module      :  Every errno has its Exception wrapper here
-- Copyright   :  (C) Marek Kidon 2016
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Marek 'Tr1p0d' Kidon <marek.kidon@itcommunity.cz>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Purpose of all these wrappers is to make particular exceptions
-- raised in your code distinguishable by their type signatures.
----------------------------------------------------------------------
module Control.Exception.Errno
where

import Control.Exception (Exception)
import Data.Typeable (Typeable)
import System.IO.Error (IOError)
import Text.Show (Show)

-- | @Errno@ 4, the @Interupted system call@ exception.
newtype CallInterupted a = CallInterupted a
    deriving (Show, Typeable)

type CallInteruptedError = CallInterupted IOError

-- | @Errno@ 5, the @I/O error@ exception.
newtype InputOutput a = InputOutput a
    deriving (Show, Typeable)

type InputOutputError = InputOutput IOError

-- | @Errno@ 9, the @Bad file number@ exception.
newtype BadFileDescriptor a = BadFileDescriptor a
    deriving (Show, Typeable)

type BadFileDescriptorError = BadFileDescriptor IOError
--
-- | @Errno@ 32, the @Broken pipe@ exception.
newtype BrokenPipe a = BrokenPipe a
    deriving (Show, Typeable)

type BrokenPipeError = BrokenPipe IOError

-- | @Errno@ 111, the @Connection refused@ exception.
newtype ConnectionRefused a = ConnectionRefused a
    deriving (Show, Typeable)

type ConnectionRefusedError = ConnectionRefused IOError

-- | @Errno@ 113, the @No route to host@ exception.
newtype HostUnreachable a = HostUnreachable a
    deriving (Show, Typeable)

type HostUnreachableError = HostUnreachable IOError

-- | Exception instances
instance Exception CallInteruptedError
instance Exception InputOutputError
instance Exception BadFileDescriptorError
instance Exception BrokenPipeError
instance Exception ConnectionRefusedError
instance Exception HostUnreachableError
