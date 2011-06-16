{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Neuronet.Epoll (
	Epoll,
	EpollEvent (..),
	EpollEventType (..),
	EpollCtlMod (..),
	epollCtl,
	epollWait,
	withEpoll,
) where

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable

import Control.Monad
import Control.Monad.Trans

import Data.Bits
import Data.List

#include <hsneuronet.h>

type Epoll a = Ptr a

foreign import ccall unsafe "hsneuronet.h Epoll_Create" epoll_create :: CInt -> IO (Ptr a)

foreign import ccall unsafe "hsneuronet.h Epoll_Destroy" epoll_destroy :: Ptr a -> IO ()

{-
typedef union epoll_data 
{
	void *ptr;
	int fd;
	__uint32_t u32;
	__uint32_t u64;
} epoll_data_t;

struct epoll_event
{
	__uint32_t events;
	epoll_data_t data;
}
-}

data EpollEvent a = EpollEvent [EpollEventType] (Ptr a)
	deriving (Show, Eq)

epollEventToC :: EpollEvent a -> (Ptr b -> IO c) -> IO c
epollEventToC (EpollEvent types ua) f =
	allocaBytes (#size EPOLL_EVENT) $ \buf -> do
		let dataBuf = (#ptr EPOLL_EVENT, data) buf

		(#poke EPOLL_EVENT, events) buf (eetsToEpollEvent types)

--		temp <- mallocBytes (#size epoll_data_t)

--		if ua == nullPtr
--			then return ()
--			else poke temp ua

		if ua == nullPtr
			then return ()
			else (#poke epoll_data_t, ptr) dataBuf ua
		rv <- f buf
--		free temp
		return rv
	where
		eetsToEpollEvent :: [EpollEventType] -> CInt
		eetsToEpollEvent eets = foldl' (.|.) 0 $ map cov eets
			where
				cov :: EpollEventType -> CInt
				cov EpollIn = (#const EPOLLIN)
				cov EpollOut = (#const EPOLLOUT)
				cov EpollPri = (#const EPOLLPRI)
				cov EpollErr = (#const EPOLLERR)
				cov EpollHup = (#const EPOLLHUP)
				cov EpollET = (#const EPOLLET)
				cov _ = 0

epollWait :: Epoll a -> Int -> IO (Int, [EpollEvent a])
epollWait ev timeout = do
	alloca $ \tmp -> do
		rawResult <- epoll_wait ev (fromIntegral timeout) tmp
		amount <- peek tmp
		if amount < 0
			then return (fromIntegral amount, [])
			else do
				let amount' = fromIntegral amount

				convEventPoll' rawResult amount' (amount', [])

foreign import ccall unsafe "hsneuronet.h Epoll_Wait" epoll_wait :: Ptr a -> CInt -> Ptr CInt -> IO (Ptr a)

convEventPoll' :: Ptr a -> Int -> (Int, [EpollEvent a]) -> IO (Int, [EpollEvent a])
convEventPoll' _ 0 result = return result
convEventPoll' ev num (amount, result) = do

	current <- toEpollEvent ev

	let ev' = current `seq` plusPtr ev (#size EPOLL_EVENT)

	convEventPoll' ev' (num - 1) (amount, result ++ [current])

	where
		toEpollEvent :: Ptr a -> IO (EpollEvent a)
		toEpollEvent cur = do
			rawEvents <- (#peek EPOLL_EVENT, events) cur
			
			let tmp = (#ptr EPOLL_EVENT, data) cur

			let userDefined = (#ptr epoll_data_t, ptr) tmp

			uD <- peek userDefined
	
			return $ EpollEvent (bundleType rawEvents) uD

		
		bundleType :: CInt -> [EpollEventType]
		bundleType value =
			let
				valList = [
					(#const EPOLLIN),
					(#const EPOLLOUT),
					(#const EPOLLPRI),
					(#const EPOLLERR),
					(#const EPOLLHUP),
					(#const EPOLLET)]
			in
				filter (/= EpollNone) $ map (\e -> if e .&. value == e then covType e else EpollNone) valList

		covType :: CInt -> EpollEventType
		covType (#const EPOLLIN) = EpollIn
		covType (#const EPOLLOUT) = EpollOut
		covType (#const EPOLLPRI) = EpollPri
		covType (#const EPOLLERR) = EpollErr
		covType (#const EPOLLHUP) = EpollHup
		covType (#const EPOLLET) = EpollET
		covType _ = EpollNone

data EpollEventType = EpollIn | EpollOut | EpollPri | EpollErr | EpollHup | EpollET | EpollNone
	deriving (Show, Eq)


data EpollCtlMod = EpollCtlAdd | EpollCtlDel | EpollCtlMod
	deriving (Show, Eq)

epollCtl :: Epoll a -> EpollCtlMod -> Int -> EpollEvent b -> IO Int
epollCtl ep ctlMod fd ev =
	epollEventToC ev $ \a -> 
		liftM fromIntegral $ epoll_ctl ep (cov ctlMod) (fromIntegral fd) a
	where
		cov :: EpollCtlMod -> CInt
		cov EpollCtlAdd = (#const EPOLL_CTL_ADD)
		cov EpollCtlDel = (#const EPOLL_CTL_DEL)
		cov EpollCtlMod = (#const EPOLL_CTL_MOD)
	

foreign import ccall unsafe "hsneuronet.h Epoll_Ctl" epoll_ctl :: Ptr a -> CInt -> CInt -> Ptr a -> IO CInt


withEpoll :: MonadIO m => Int -> (Epoll a -> m b) -> m b
withEpoll initial f = do
	ep <- liftIO $ epoll_create (fromIntegral initial)
	rv <- f ep
	liftIO $ epoll_destroy ep
	return rv
