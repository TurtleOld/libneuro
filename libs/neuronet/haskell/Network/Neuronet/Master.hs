{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Neuronet.Master
where

import Foreign.C
import Foreign.Ptr

import Network.Neuronet.Core

nnet_Listen :: NNET_MASTER a -> String -> Int -> IO (NNET_SLAVE b)
nnet_Listen master listen_addr port =
	withCString listen_addr $ \cstr -> nnet_listen master cstr (fromIntegral port)

foreign import ccall unsafe "neuro/network.h NNet_Listen" nnet_listen :: Ptr a -> CString -> CInt -> IO (Ptr b)
