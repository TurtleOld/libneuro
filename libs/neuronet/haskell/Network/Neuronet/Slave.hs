{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Neuronet.Slave
where

import Foreign.C
import Foreign.Ptr

import Control.Monad

import qualified Data.ByteString as B

import Network.Neuronet.Core
import Network.Neuronet.Packet

-- for connecting to a server
nnet_Connect :: NNET_MASTER a -> String -> Int -> IO (NNET_SLAVE b)
nnet_Connect master host port =
	withCString host (\str -> nnet_connect master str (fromIntegral port))

foreign import ccall unsafe "neuro/network.h NNet_Connect" nnet_connect :: Ptr a -> CString -> CInt -> IO (Ptr b)

nnet_SetTimeOut :: NNET_SLAVE a -> Int -> IO ()
nnet_SetTimeOut slv ts = nnet_settimeout slv (fromIntegral ts)

foreign import ccall unsafe "neuro/network.h NNet_SetTimeOut" nnet_settimeout :: Ptr a -> CInt -> IO ()

nnet_Send :: NNET_SLAVE a -> Packet b -> IO Int
nnet_Send slave packet =
	-- see foreign import in Packet.hs
	pkt_GetBuffer packet >>= \raw_pkt ->
	pkt_GetLen packet >>= \len ->
	liftM fromIntegral (nnet_send slave raw_pkt len)

nnet_Send2 :: NNET_SLAVE a -> B.ByteString -> IO Int
nnet_Send2 slave bs =
	let len = fromIntegral (B.length bs) :: CInt in
	B.useAsCString bs (\cstr -> liftM fromIntegral (nnet_send slave cstr len))

foreign import ccall unsafe "neuro/network.h NNet_Send" nnet_send :: Ptr a -> Ptr b -> CInt -> IO CInt

nnet_GetIP :: NNET_SLAVE a -> IO String
nnet_GetIP slave = nnet_getip slave >>= peekCString 

foreign import ccall unsafe "neuro/network.h NNet_GetIP" nnet_getip :: Ptr a -> IO CString

-- set and get user defined data

nnet_SetData :: NNET_SLAVE a -> Ptr b -> IO ()
nnet_SetData = nnet_setdata

foreign import ccall unsafe "neuro/network.h NNet_SetData" nnet_setdata :: Ptr a -> Ptr b -> IO ()

nnet_GetData :: NNET_SLAVE a -> IO (Ptr b)
nnet_GetData = nnet_getdata

foreign import ccall unsafe "neuro/network.h NNet_GetData" nnet_getdata :: Ptr a -> IO (Ptr b)

-- Slave functions available to server instances only

nnet_DisconnectClient :: NNET_SLAVE a -> IO ()
nnet_DisconnectClient = nnet_disconnectclient

foreign import ccall unsafe "neuro/network.h NNet_DisconnectClient" nnet_disconnectclient :: Ptr a -> IO ()
