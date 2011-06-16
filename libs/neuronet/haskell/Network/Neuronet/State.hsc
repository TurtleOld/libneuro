{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Neuronet.State
where

import Foreign.C
import Foreign.Ptr

import Control.Monad

import Network.Neuronet.Core
import Network.Neuronet.Packet

#include <hsneuronet.h>

data NNETState = 
	StateNoData |
	StateDataAvail |
	StateDisconnect |
	StateNewClient |
	StateClientDisconnect

convState :: NNETState -> Int
convState StateNoData = (#const State_NoData)
convState StateDataAvail = (#const State_DataAvail)
convState StateDisconnect = (#const State_Disconnect)
convState StateNewClient = (#const State_NewClient)
convState StateClientDisconnect = (#const State_ClientDisconnect)

convState' :: Int -> NNETState
convState' (#const State_NoData) = StateNoData
convState' (#const State_DataAvail) = StateDataAvail
convState' (#const State_Disconnect) = StateDisconnect
convState' (#const State_NewClient) = StateNewClient
convState' (#const State_ClientDisconnect) = StateClientDisconnect
convState' _ = StateNoData

nnet_GetState :: NNET_STATUS a -> IO NNETState
nnet_GetState slave = 
	liftM (convState' . fromIntegral) (nnet_status slave)

foreign import ccall unsafe "hsneuronet.h NNet_GetStatus" nnet_status :: Ptr a -> IO CInt

nnet_GetPacket :: NNET_STATUS a -> Packet b -> IO Int
nnet_GetPacket slave pkt =
	nnet_getpacketlen slave >>= \len ->
	-- see the foreign import from Packet.hs
	nnet_getpacket slave >>= \raw_pkt ->
	liftM fromIntegral (pkt_Set2 pkt raw_pkt (fromIntegral len))

foreign import ccall unsafe "hsneuronet.h NNet_GetPacket" nnet_getpacket :: Ptr a -> IO (Ptr b)

-- see nnet_GetPacket
foreign import ccall unsafe "hsneuronet.h NNet_GetPacketLen" nnet_getpacketlen :: Ptr a -> IO CInt

nnet_GetSlave :: NNET_STATUS a -> IO (NNET_SLAVE b)
nnet_GetSlave = nnet_getslave

foreign import ccall unsafe "hsneuronet.h NNet_GetSlave" nnet_getslave :: Ptr a -> IO (Ptr b)
