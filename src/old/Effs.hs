{-# LANGUAGE FlexibleContexts #-}

-- Эксперименты с эффектами

module Effs where

import Control.Monad.Trans.Cont (runContT, ContT)
import Control.Monad.Trans.State (runStateT, StateT)
import Control.Monad.Trans.Except (runExceptT, ExceptT)

-- ========== Setup effects ========== --

import Control.Monad.Cont.Class
import Control.Monad.State.Class
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad (when)


getCC :: MonadCont m => m (m a)
getCC = callCC $ \t -> let g = t g in return g

extractExcept :: Monad m => (e -> m a) -> ExceptT e m a -> m a
extractExcept h em = runExceptT em >>= \ex -> case ex of
  Left e -> h e
  Right a -> return a


-- ======= End of setup effects ======= --

data Ctx = Ctx {getIter :: Int} deriving Show

data ErrType = RE | NPE | ArrOutBds deriving Show

data Err = Err {getErrType :: ErrType, getMesg :: String} deriving Show



comp :: (MonadCont m, MonadState Ctx m, MonadError Err m, MonadIO m) => m ()
comp = do
         label <- getCC                               -- while
         i <- gets getIter                              -- i = ... (read i) 
         callCC $ \exit -> do                           -- init break
           when (i >= 10) $ exit ()                     -- if (i>10) break
           liftIO $ putStrLn ("i=" ++ show i)           -- print i
           when (i == 7) $ throwError $ Err RE "bad value: i = 7"
           modify $ \ctx -> ctx {getIter = i + 2}       -- i = i + 2
           label                                      -- end while
         liftIO $ putStrLn "Success"                  -- print "Success"; return
       `catchError` \e -> do
         liftIO $ putStrLn ("Error catched: " ++ show e ++ " rethrowing")
         throwError e
           
           
execComp :: Int -> IO ()
execComp i = let
               compImpl :: ExceptT Err (ContT () (StateT Ctx IO)) ()
               compImpl = comp
               errHandler e = liftIO $ putStrLn ("Error: " ++ show e)
               unwrapExcept = extractExcept errHandler compImpl
               unwrapCont  = runContT unwrapExcept return
               s0 = Ctx i
               unwrapState = fst <$> runStateT unwrapCont s0
             in 
               unwrapState

