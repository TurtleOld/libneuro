{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Neuronet.Flags
where

import Foreign.C
import Foreign.Ptr

import Control.Monad

import Network.Neuronet.Core


nnet_SetSendPacketSize :: NNET_MASTER a -> IO Int
nnet_SetSendPacketSize a =
	liftM (fromIntegral) (nnet_setsendpacketsize a)

foreign import ccall unsafe "neuro/network.h NNet_SetSendPacketSize" nnet_setsendpacketsize :: Ptr a -> IO CInt

nnet_SetQuitFlag :: NNET_MASTER a -> IO ()
nnet_SetQuitFlag = nnet_setquitflag

foreign import ccall unsafe "neuro/network.h NNet_SetQuitFlag" nnet_setquitflag :: Ptr a -> IO ()
