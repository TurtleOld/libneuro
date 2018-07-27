{-# LANGUAGE ForeignFunctionInterface, MultiParamTypeClasses, FlexibleInstances #-}

module Network.Neuronet.Packet
where

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

import qualified Data.ByteString as A
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Unsafe as UB
import qualified Data.Text as T
--import qualified Data.ByteString.Lazy.Char8 as C

import Data.Word
import Foreign.Marshal.Array

import Control.Monad

type Packet a = Ptr a

class Pkt a b where
	packetPush :: Packet a -> b -> IO Int
	packetPushStream :: Packet a -> Int -> b -> IO Int -- Supports Unicode
	packetPushAStream :: Packet a -> Int -> b -> IO Int -- Raw Version of stream
	packetPop :: Packet a -> IO b
	packetAPop :: Packet a -> IO b

	packetPushStream _ _ _ = return 0
	packetPushAStream _ _ _ = return 0
	packetAPop _ = return undefined

instance Pkt a Double where -- 64 bits
	packetPush pkt num = liftM fromIntegral (pkt_Push64 pkt ((fromRational . toRational) num))
	packetPop pkt = liftM (fromRational . toRational) (pkt_Pop64 pkt)

instance Pkt a Int where -- 32 bits
	packetPush pkt num = liftM fromIntegral (pkt_Push32 pkt (fromIntegral num))
	packetPop pkt = liftM (fromIntegral) (pkt_Pop32 pkt)

instance Pkt a CShort where -- 16 bits
	packetPush pkt num = liftM fromIntegral (pkt_Push16 pkt (fromIntegral num))
	packetPop pkt = liftM (fromIntegral) (pkt_Pop16 pkt)

instance Pkt a CChar where -- 8 bits
	packetPush pkt num = liftM fromIntegral (pkt_Push8 pkt (fromIntegral num))
	packetPop pkt = liftM (fromIntegral) (pkt_Pop8 pkt)

instance Pkt a [Char] where
	packetPush pkt str = liftM fromIntegral $ withStringToC2 str $ \str' -> pkt_PushString pkt (fromIntegral $ (length str) + 1) str'
	packetPushStream pkt len str = liftM fromIntegral $ withStringToC2 str $ pkt_PushString pkt (fromIntegral len)
	--packetPushAStream pkt len str = liftM fromIntegral $ withCAString str $ pkt_PushString pkt (fromIntegral len)
	packetPushAStream pkt len str = liftM fromIntegral $ withStringToC str $ pkt_PushString pkt (fromIntegral len)
	packetPop pkt = pkt_PopString pkt >>= peekCString
	packetAPop pkt = pkt_PopString pkt >>= peekCAString

instance Pkt a A.ByteString where
	packetPush pkt str = liftM fromIntegral $ withCUStringFromB str $ \str' -> pkt_PushString pkt (fromIntegral (A.length str)) str'
	--packetPush pkt str = liftM fromIntegral $ A.useAsCStringLen str $ \(str', len) -> pkt_PushString pkt (fromIntegral len) str'
	packetPushStream pkt len str = liftM fromIntegral $ withCUStringFromB str $ \str' -> pkt_PushString pkt (fromIntegral len) str'
	packetPushAStream pkt len str = liftM fromIntegral $ withCUStringFromB str $ \str' -> pkt_PushString pkt (fromIntegral len) str'
	--packetPushStream pkt len str = liftM fromIntegral $ A.useAsCString str $ pkt_PushString pkt $ fromIntegral len
	--packetPushAStream pkt len str = liftM fromIntegral $ UB.unsafeUseAsCString str $ pkt_PushString pkt $ fromIntegral len
	packetPop pkt = pkt_PopString pkt >>= A.packCString

instance Pkt a B.ByteString where
	packetPush pkt str = 
		packetPush pkt $ foldl (\a b -> a `A.append` b ) A.empty $ B.toChunks str
		--liftM fromIntegral $ withCStringE (B.unpack str) $ pkt_PushString pkt $ fromIntegral $ B.length str
	packetPushStream pkt len str = 
		packetPushStream pkt len $ foldl (\a b -> a `A.append` b ) A.empty $ B.toChunks str
	packetPushAStream pkt len str = 
		packetPushAStream pkt len $ foldl (\a b -> a `A.append` b ) A.empty $ B.toChunks str
	packetPop pkt = 
		packetPop pkt >>= return . B.fromChunks . (: [])
		--pkt_PopString pkt >>= peekCStringToByteString

withCStringE :: Integral a => [a] -> (Ptr CChar -> IO b) -> IO b
withCStringE = withArray0 (0 :: CChar) . map (fromInteger . fromIntegral)

withStringToC :: String -> (Ptr CUChar -> IO b) -> IO b
withStringToC str f = allocaBytes (length str) $ \ptr ->
	cpStr str ptr >> f ptr
	where	cpStr [] _ = return ()
		cpStr (x:xs) ptr = do
			poke ptr $ castCharToCUChar x
			cpStr xs $ ptr `plusPtr` 1

withStringToC2 :: String -> (Ptr CUChar -> IO b) -> IO b
withStringToC2 = withArray . map castCharToCUChar

withCUStringFromB :: A.ByteString -> (Ptr CUChar -> IO b) -> IO b
withCUStringFromB = withArray . map (castCharToCUChar . toEnum . fromEnum) . A.unpack

peekCStringToByteString :: CString -> IO B.ByteString
peekCStringToByteString cs = peekArray0 (0 :: CChar) cs >>= return . B.pack . castCCharToWord8
	where	castCCharToWord8 :: [CChar] -> [Word8]
		castCCharToWord8 = map (toEnum . fromEnum)

instance Pkt a T.Text where
	packetPush pkt str = undefined
	packetPop pkt = pkt_PopString pkt >>= undefined

packet_Push64 :: Packet a -> Double -> IO Int
packet_Push64 pkt num = 
	liftM fromIntegral (pkt_Push64 pkt ((fromRational . toRational) num))

foreign import ccall unsafe "neuro/nnet/packet.h Packet_Push64" pkt_Push64 :: Ptr a -> CDouble -> IO CInt

packet_Push32 :: Packet a -> Int -> IO Int
packet_Push32 pkt num = 
	liftM fromIntegral (pkt_Push32 pkt (fromIntegral num))

foreign import ccall unsafe "neuro/nnet/packet.h Packet_Push32" pkt_Push32 :: Ptr a -> CInt -> IO CInt

packet_Push16 :: Packet a -> Int -> IO Int
packet_Push16 pkt num = 
	liftM fromIntegral (pkt_Push16 pkt (fromIntegral num))

foreign import ccall unsafe "neuro/nnet/packet.h Packet_Push16" pkt_Push16 :: Ptr a -> CShort -> IO CInt

packet_Push8 :: Packet a -> Int -> IO Int
packet_Push8 pkt num = 
	liftM fromIntegral (pkt_Push8 pkt (fromIntegral num))

foreign import ccall unsafe "neuro/nnet/packet.h Packet_Push8" pkt_Push8 :: Ptr a -> CChar -> IO CInt

packet_PushString :: Packet a -> String -> IO Int
packet_PushString pkt string = undefined
	--liftM fromIntegral (withCString (string) (\str -> pkt_PushString pkt (fromIntegral ((length string + 1))) str))

foreign import ccall unsafe "neuro/nnet/packet.h Packet_PushString" pkt_PushString :: Ptr a -> CUInt -> Ptr CUChar -> IO CInt

packet_Create :: IO (Packet a)
packet_Create =
	pkt_Create

foreign import ccall unsafe "neuro/nnet/packet.h Packet_Create" pkt_Create :: IO (Ptr a)

packet_Destroy :: Packet a -> IO ()
packet_Destroy = pkt_Destroy

foreign import ccall unsafe "neuro/nnet/packet.h Packet_Destroy" pkt_Destroy :: Ptr a -> IO ()

packet_GetLen :: Packet a -> IO Int
packet_GetLen pkt =
	liftM fromIntegral (pkt_GetLen pkt)

foreign import ccall unsafe "neuro/nnet/packet.h Packet_GetLen" pkt_GetLen :: Ptr a -> IO CInt

packet_GetBuffer :: Packet a -> IO String
packet_GetBuffer pkt =
	(pkt_GetBuffer pkt) >>= \str ->
	pkt_GetLen pkt >>= \len ->
	--peekCStringLen (str, fromIntegral len)
	liftM (map castCUCharToChar) $ peekArray (fromIntegral len) str 

packet_GetBufferA :: Packet a -> IO String
packet_GetBufferA pkt =
	(pkt_GetBuffer pkt) >>= \str ->
	pkt_GetLen pkt >>= \len ->
	--peekCAStringLen (str, fromIntegral len)
	liftM (map castCUCharToChar) $ peekArray (fromIntegral len) str

foreign import ccall unsafe "neuro/nnet/packet.h Packet_GetBuffer" pkt_GetBuffer :: Ptr a -> IO (Ptr CUChar)

packet_Pop64 :: Packet a -> IO Double
packet_Pop64 pkt =
	liftM (fromRational . toRational) (pkt_Pop64 pkt)

foreign import ccall unsafe "neuro/nnet/packet.h Packet_Pop64" pkt_Pop64 :: Ptr a -> IO CDouble

packet_Pop32 :: Packet a -> IO Int
packet_Pop32 pkt =
	liftM (fromIntegral) (pkt_Pop32 pkt)

foreign import ccall unsafe "neuro/nnet/packet.h Packet_Pop32" pkt_Pop32 :: Ptr a -> IO CInt

packet_Pop16 :: Packet a -> IO Int
packet_Pop16 pkt =
	liftM (fromIntegral) (pkt_Pop16 pkt)

foreign import ccall unsafe "neuro/nnet/packet.h Packet_Pop16" pkt_Pop16 :: Ptr a -> IO CUShort

packet_Pop8 :: Packet a -> IO Int
packet_Pop8 pkt =
	liftM (fromIntegral) (pkt_Pop8 pkt)

foreign import ccall unsafe "neuro/nnet/packet.h Packet_Pop8" pkt_Pop8 :: Ptr a -> IO CUChar

packet_PopString :: Packet a -> IO String
packet_PopString pkt =
	pkt_PopString pkt >>= peekCString

foreign import ccall unsafe "neuro/nnet/packet.h Packet_PopString" pkt_PopString :: Ptr a -> IO CString

packet_PopData :: Packet a -> Int -> IO String
packet_PopData pkt len =
	pkt_PopData pkt (fromIntegral len) >>= \str -> peekCStringLen (str, len)

packet_PopData2 :: Packet a -> Int -> IO B.ByteString
packet_PopData2 pkt len =
	pkt_PopData pkt (fromIntegral len) >>= \str -> A.packCStringLen (str, len) >>= return . B.fromChunks . (: [])

foreign import ccall unsafe "neuro/nnet/packet.h Packet_PopData" pkt_PopData :: Ptr a -> CUInt -> IO CString

packet_Reset :: Packet a -> IO ()
packet_Reset = pkt_Reset

foreign import ccall unsafe "neuro/nnet/packet.h Packet_Reset" pkt_Reset :: Ptr a -> IO ()

packet_Set :: String -> Int -> IO (Packet a)
packet_Set string len =
	withCString string (\str -> pkt_Set str (fromIntegral len))

foreign import ccall unsafe "neuro/nnet/packet.h Packet_Set" pkt_Set :: CString -> CUInt -> IO (Ptr a)

packet_Set2 :: Packet a -> String -> Int -> IO (Int)
packet_Set2 pkt string len =
	liftM fromIntegral (withCString string (\str -> pkt_Set2 pkt str (fromIntegral len)))

packet_Set3 :: Packet a -> B.ByteString -> Int -> IO (Int)
packet_Set3 pkt string len = do
	let string' = foldl (\a b -> a `A.append` b ) A.empty $ B.toChunks string
	liftM fromIntegral (A.useAsCString string' (\str -> pkt_Set2 pkt str (fromIntegral len)))

foreign import ccall unsafe "neuro/nnet/packet.h Packet_Set2" pkt_Set2 :: Ptr a -> CString -> CUInt -> IO CInt
