module Utils where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class

-- | State monad transformer.
--
-- The Parser type uses this transformer under the hood
-- to define parser combinators.
newtype StateT s m a =
  StateT
    { runStateT :: s -> m (a, s)
    }

instance Monad m => Functor (StateT s m) where
  fmap f sT =
    StateT $ \s -> do
      (a, s') <- runStateT sT s
      pure (f a, s')

instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  sT <*> s'T =
    StateT $ \s -> do
      (f, s') <- runStateT sT s
      (a, s'') <- runStateT s'T s'
      pure (f a, s'')

instance Monad m => Monad (StateT s m) where
  return = pure
  sT >>= f =
    StateT $ \s -> do
      (a, s') <- runStateT sT s
      (b, s'') <- runStateT (f a) s'
      pure (b, s'')

instance MonadTrans (StateT s) where
  lift m =
    StateT $ \s -> do
      a <- m
      pure (a, s)

instance Monoid e => Alternative (Either e) where
  empty = Left mempty
  Left e1 <|> Left e2 = Left $ mappend e1 e2
  Right a <|> _ = Right a
  Left _ <|> Right a = Right a

instance Monoid e => MonadPlus (Either e)

instance MonadPlus m => Alternative (StateT s m) where
  empty = StateT $ const mzero
  StateT m <|> StateT n = StateT $ \s -> m s `mplus` n s

getState :: Monad m => StateT s m s
getState = StateT $ \s -> pure (s, s)

setState :: Monad m => s -> StateT s m ()
setState s = StateT $ \_ -> pure ((), s)

newtype EitherT e m a =
  EitherT
    { runEitherT :: m (Either e a)
    }

instance Monad m => Functor (EitherT e m) where
  fmap f (EitherT m) = EitherT $ fmap f <$> m

instance Monad m => Applicative (EitherT e m) where
  pure = EitherT . pure . pure
  (EitherT m1) <*> (EitherT m2) =
    EitherT $ do
      ef <- m1
      ea <- m2
      pure $ ef <*> ea

instance Monad m => Monad (EitherT e m) where
  (EitherT m1) >>= f =
    EitherT $ do
      eith <- m1
      case eith of
        Left e -> pure $ Left e
        Right a -> runEitherT $ f a

instance MonadTrans (EitherT e) where
  lift = EitherT . fmap pure

lift2 :: (MonadTrans u, MonadTrans t, Monad m, Monad (t m)) => m a -> u (t m) a
lift2 = lift . lift
