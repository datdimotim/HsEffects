{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

-- Эксперименты с эффектами

module Effs where

import Control.Monad.Trans.Cont (runContT, ContT)
import Control.Monad.Trans.State (runStateT, StateT)
import Control.Monad.Trans.Except (runExceptT, ExceptT)
import Control.Lens

-- ========== Setup effects ========== --

import Control.Monad.Cont.Class
import Control.Monad.State.Class
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad (when)


getCC :: MonadCont m => m (m a)
getCC = callCC $ \t -> let g = t g in return g

extractExcept :: Monad m => (e -> m a) -> ExceptT e m a -> m a
extractExcept h em = runExceptT em >>= \case
                                         Left e -> h e
                                         Right a -> return a

type LoopLabel m = (m (), m ())

continue :: (m (), m ()) -> m ()
continue = fst

breakLoop :: (m (), m ()) -> m ()
breakLoop = snd

while :: MonadCont m => (LoopLabel m -> m a) -> m ()
while body = do
  label <- getCC
  callCC $ \exit -> do
    body (label, exit ())
    label


for :: (MonadCont m, MonadState s m) => m () -> m Bool -> m () -> (LoopLabel m -> m a) -> m ()
for mi mc mn body = do
  mi
  while $ \label -> do
    b <- mc
    when (not b) $ breakLoop label
    body label
    mn

-- ======= End of setup effects ======= --

data Ctx = Ctx {getIter :: Int} deriving Show

data ErrType = RE | NPE | ArrOutBds deriving Show

data Err = Err {getErrType :: ErrType, getMesg :: String} deriving Show



comp :: (MonadCont m, MonadState Ctx m, MonadError Err m, MonadIO m) => m ()
comp = do
         liftIO $ putStrLn "start"
         while $ \label -> do
           i <- gets getIter                          -- i = ... (read i)
           when (i >= 10) $ breakLoop label                    -- if (i>10) break
           liftIO $ putStrLn ("i=" ++ show i)         -- print i
           when (i == 7) $ throwError $ Err RE "bad value: i = 7"
           modify $ \ctx -> ctx {getIter = i + 2}       -- i = i + 2
           continue label
           liftIO $ putStrLn "this line never print"
         liftIO $ putStrLn "Success"                  -- print "Success"; return
       `catchError` \e -> do
         liftIO $ putStrLn ("Error catched: " ++ show e ++ " rethrowing")
         throwError e


comp2 :: (MonadCont m, MonadState (Int, Int) m, MonadIO m) => m ()
comp2 = do
          liftIO $ putStrLn "start"
          while $ \outer -> do
            i <- gets fst
            when (i >= 10) $ breakLoop outer
            liftIO $ putStrLn ("i=" ++ show i)
            modify $ \(i, j) -> (i, 0)
            while $ \inner -> do
              j <- gets snd
              when (j >= 10) $ breakLoop inner
              liftIO $ putStrLn ("  j=" ++ show j)
              when (i == 3 && j == 4) $ do
                liftIO $ putStrLn "solution found"
                breakLoop outer
              modify $ \(i, j) -> (i, j+1)
            modify $ \(i, j) -> (i+1, j)
          liftIO $ putStrLn "Success"

data St = St {_iv :: Int, _jv :: Int}
makeLenses ''St

comp3 :: (MonadCont m, MonadState St m, MonadIO m) => m ()
comp3 = do
          liftIO $ putStrLn "start"
          for (iv %= id) (uses iv (<10)) (iv += 1) $ \outer -> do
            i <- use iv
            liftIO $ putStrLn ("i=" ++ show i)
            for (jv .= 0) (uses jv (<10)) (jv += 1) $ \inner -> do
              j <- use jv
              liftIO $ putStrLn ("  j=" ++ show j)
              when (i == 3 && j == 4) $ do
                liftIO $ putStrLn "solution found"
                breakLoop outer
          liftIO $ putStrLn "Success"


execEffs :: ExceptT Err (ContT () (StateT s IO)) () -> s -> IO ()
execEffs m s0 = let
                    errHandler e = liftIO $ putStrLn ("Error: " ++ show e)
                    unwrapExcept = extractExcept errHandler m
                    unwrapCont  = runContT unwrapExcept return
                    unwrapState = fst <$> runStateT unwrapCont s0
                  in
                    unwrapState

execComp :: Int -> IO ()
execComp i = execEffs comp (Ctx i)

execComp2 :: IO ()
execComp2 = execEffs comp2 (0, 0)

execComp3 :: IO ()
execComp3 = execEffs comp3 (St 0 0)
