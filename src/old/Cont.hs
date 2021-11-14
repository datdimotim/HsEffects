{-# LANGUAGE FlexibleContexts #-}

-- Пишем ContT вручную

module Cont where


import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.State
import Control.Monad.State.Class (MonadState)
import qualified Control.Monad.State.Class as S
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Cont.Class (MonadCont)
import qualified Control.Monad.Cont.Class as C

newtype Cont r a = Cont {runCont :: ((a -> r) -> r)}


instance Functor (Cont r) where
  -- f :: a -> b
  -- c :: (a -> r) -> r
  -- t :: b -> r
  fmap f (Cont c)  = Cont $ \t -> c (t . f)
  
  
instance Applicative (Cont r) where
  pure a = Cont $ \t -> t a
  -- f :: ((b -> c) -> r) -> r
  -- g :: (b -> r) -> r
  -- t :: c -> r
  (Cont f) <*> (Cont g) = Cont $ \t -> f (\bc -> g (t . bc))
  
  
instance Monad (Cont r) where
  return = pure
  -- k :: a -> Cont r b
  -- t :: b -> r
  (Cont arr) >>= k = Cont $ \t -> arr (\a -> runCont (k a) t)
  
  
  
callCC' :: ((a -> Cont r b) -> Cont r a) -> Cont r a
  -- t :: a -> r 
callCC' c = Cont $ \t -> runCont (c $ \a -> Cont (\_ -> t a)) t



getCC' :: a -> Cont r (a, a -> Cont r a)
  -- t :: (a -> Cont r b) -> Cont r c
getCC' a0 = callCC' $ \t -> let g a = t (a, g) in return (a0, g)



newtype ContT r m a = ContT {runContT :: ((a -> m r) -> m r)}


instance Monad m => Applicative (ContT r m) where
  pure = return
  cf <*> ac = cf >>= \f -> ac >>= \a -> return (f a) 

instance Monad m => Functor (ContT r m) where
  fmap f m = m >>= \a -> return (f a) 


instance Monad m => Monad (ContT r m) where
  return a = ContT $ \t -> t a
  -- k :: a -> ContT m r b
  -- t :: b -> m r
  (ContT arr) >>= k = ContT $ \t -> arr (\a -> runContT (k a) $ t)
  
  
instance MonadTrans (ContT r) where
  -- lift :: Monad m => m a -> t m a
  lift ma = ContT (\t -> ma >>= t)
  
callCC :: Monad m => ((a -> ContT r m b) -> ContT r m a) -> ContT r m a
  -- t :: a -> r 
callCC c = ContT $ \t -> runContT (c $ \a -> ContT (\_ -> t a)) t


getCC :: Monad m => a -> ContT r m (a, a -> ContT r m a)
  -- t :: (a -> Cont r b) -> Cont r c
getCC a0 = callCC $ \t -> let g a = t (a, g) in return (a0, g)


getCC_ :: Monad m => ContT r m (ContT r m a)
getCC_ = callCC $ \t -> let g = t g in return g

getCCS :: MonadCont m => m (m a)
getCCS = C.callCC $ \t -> let g = t g in return g

gotoExample :: ContT () (StateT Int IO) ()
gotoExample = do
                  lift $ put 1
                  label <- getCC_
                  i <-lift $ get
                  lift $ lift $ putStrLn ("i=" ++ show i)
                  if i < 1000000
                  then do
                    lift $ put (i + 1)
                    label
                  else return ()
                  

-- fst  <$> runStateT (C.runContT gotoExampleMtl (\u -> return ())) 0
gotoExampleMtl :: (MonadCont m, MonadIO m, MonadState Int m) => m () 
gotoExampleMtl = do
                  S.put 1
                  label <- getCCS
                  i <- S.get
                  liftIO $ putStrLn ("i=" ++ show i)
                  if i < 10
                  then do
                    S.put (i + 1)
                    label
                  else return ()       
                  
                             
  

