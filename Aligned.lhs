<style>

body {
  padding: 0.5em 1em 3em;
  text-align: center;
}

main {
  display: inline-block;
  text-align: left;
  border-top: 1px solid #333;
  margin-top: 2em;
  padding: 1em 1em 0;
}

</style>

<h1>Lined-up Haskell types</h1>

<div>
Got more to add?
Submit a <a href="https://github.com/chris-martin/haskell-aligned">GitHub</a> issue
or talk to me on <a href="https://twitter.com/chris__martin">Twitter</a>.
</div>

<div style="display: none;">

> module Aligned where
>
> import Prelude (Applicative, Bool, Functor, Monad, Monoid, Traversable)
> import Control.Monad (MonadPlus)
>
> import qualified Prelude
> import qualified Control.Applicative
> import qualified Control.Monad
> import qualified Data.Foldable
> import qualified Data.Functor
> import qualified Data.List
> import qualified Data.Traversable

</div>

<main>

<h3>Function application</h3>

> ($)      ::                                     (a ->   b) ->   a ->      b
> (<$>)    ::  Functor     f                 =>   (a ->   b) -> f a -> f    b
> (<*>)    ::  Applicative f                 => f (a ->   b) -> f a -> f    b
> liftA    ::  Applicative f                 =>   (a ->   b) -> f a -> f    b
> liftM    ::  Monad       f                 =>   (a ->   b) -> f a -> f    b
> (=<<)    ::  Monad       f                 =>   (a -> f b) -> f a -> f    b
> mapM     :: (Monad       f, Traversable t) =>   (a -> f b) -> t a -> f (t b)
> traverse :: (Applicative f, Traversable t) =>   (a -> f b) -> t a -> f (t b)
> foldMap  :: (Monoid      b, Traversable t) =>   (a ->   b) -> t a ->      b

<h3>Function composition</h3>

> (.)   ::            (b ->   c) -> (a ->   b) -> a ->   c
> (<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c

<h3>Filtering</h3>

> filter  ::                (a ->   Bool) ->  [a] ->   [a]
> filterM :: Monad     m => (a -> m Bool) ->  [a] -> m [a]
> mfilter :: MonadPlus m => (a ->   Bool) -> m a  -> m  a

<h3>Discarding one of two values</h3>

> const ::                    a ->   b ->   a
> (<$)  :: Functor     f =>   a -> f b -> f a
> (<*)  :: Applicative f => f a -> f b -> f a
> ($>)  :: Functor     f => f a ->   b -> f b
> (*>)  :: Applicative f => f a -> f b -> f b

<h3>Restructuring</h3>

> sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
> sequence  :: (Monad       f, Traversable t) => t (f a) -> f (t a)
> join      ::  Monad       f                 => f (f a) -> f    a

</main>

<div style="display: none;">

> (.)       = (Prelude..)
> ($)       = (Prelude.$)
> const     = (Prelude.const)
>
> (<*>)     = (Control.Applicative.<*>)
> (*>)      = (Control.Applicative.*>)
> (<*)      = (Control.Applicative.<*)
> liftA     = Control.Applicative.liftA
>
> (=<<)     = (Control.Monad.=<<)
> (<=<)     = (Control.Monad.<=<)
> filterM   = Control.Monad.filterM
> join      = Control.Monad.join
> liftM     = Control.Monad.liftM
> mapM      = Control.Monad.mapM
> mfilter   = Control.Monad.mfilter
> sequence  = Control.Monad.sequence
>
> foldMap   = Data.Foldable.foldMap
>
> (<$>)     = (Data.Functor.<$>)
> (<$)      = (Data.Functor.<$)
> ($>)      = (Data.Functor.$>)
>
> filter    = Data.List.filter
>
> sequenceA = Data.Traversable.sequenceA
> traverse  = Data.Traversable.traverse

</div>
