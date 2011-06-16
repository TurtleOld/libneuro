{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Neuronet.Core
where

import Foreign.C
import Foreign.Ptr

data Connection = TypeClient | TypeServer

type NNET_MASTER a = Ptr a

type NNET_SLAVE a = Ptr a

type NNET_STATUS a = Ptr a

nnet_Create :: Connection -> IO (NNET_MASTER a)
nnet_Create TypeClient = nnet_create 1
nnet_Create TypeServer = nnet_create 0

foreign import ccall unsafe "neuro/network.h NNet_Create" nnet_create :: CInt -> IO (Ptr a)

nnet_Destroy :: NNET_MASTER a -> IO ()
nnet_Destroy = nnet_destroy

foreign import ccall unsafe "neuro/network.h NNet_Destroy" nnet_destroy :: Ptr a -> IO ()

nnet_Poll :: NNET_MASTER a -> IO (NNET_STATUS b)
nnet_Poll = nnet_poll

foreign import ccall unsafe "neuro/network.h NNet_Poll" nnet_poll :: Ptr a -> IO (Ptr b)

nnet_SetDebugFilter :: String -> IO ()
nnet_SetDebugFilter input = 
	withCString input (nnet_setdebugfilter)

foreign import ccall unsafe "neuro/network.h NNet_SetDebugFilter" nnet_setdebugfilter :: Ptr a -> IO ()
