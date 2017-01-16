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

.hs-definition, .hs-varop {
  color: blue;
}

.hs-conid {
  color: green;
}

.hs-layout, .hs-keyglyph {
  color: #a57145;
}

</style>

<h1>Lined-up Haskell types</h1>

<div>
Got more to add?
Submit a <a href="https://github.com/chris-martin/haskell-aligned">GitHub</a> issue
or talk to me on <a href="https://twitter.com/chris__martin">Twitter</a>.
</div>

<div style="display: none;">

\begin{code}
module Aligned where

import Prelude (Applicative, Bool, Functor, IO, Maybe, Monad, Monoid, Traversable)
import Control.Monad (MonadPlus)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.List (ListT)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Foldable (Foldable)

import qualified Prelude
import qualified Control.Applicative
import qualified Control.Monad
import qualified Control.Monad.IO.Class
import qualified Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Identity
import qualified Control.Monad.Trans.List
import qualified Control.Monad.Trans.Maybe
import qualified Data.Foldable
import qualified Data.Functor
import qualified Data.List
import qualified Data.Monoid
import qualified Data.Traversable
\end{code}

</div>

<main>

<h3>Function application</h3>

\begin{code}
($)      ::                                     (a ->   b) ->   a ->      b
(<$>)    ::  Functor     f                 =>   (a ->   b) -> f a -> f    b
(<*>)    ::  Applicative f                 => f (a ->   b) -> f a -> f    b
(=<<)    ::  Monad       f                 =>   (a -> f b) -> f a -> f    b
mapM     :: (Monad       f, Traversable t) =>   (a -> f b) -> t a -> f (t b)
traverse :: (Applicative f, Traversable t) =>   (a -> f b) -> t a -> f (t b)
foldMap  :: (Monoid      b, Traversable t) =>   (a ->   b) -> t a ->      b
\end{code}

<h3>Applicative lifting</h3>

\begin{code}
liftA  :: Applicative f => (a -> b)           -> f a -> f b
liftA2 :: Applicative f => (a -> b -> c)      -> f a -> f b -> f c
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
\end{code}

<h3>Function composition</h3>

\begin{code}
(.)   ::            (b ->   c) -> (a ->   b) -> a ->   c
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
\end{code}

<h3>Filtering</h3>

\begin{code}
filter  ::                (a ->   Bool) ->  [a] ->   [a]
filterM :: Monad     m => (a -> m Bool) ->  [a] -> m [a]
mfilter :: MonadPlus m => (a ->   Bool) -> m a  -> m  a
\end{code}

<h3>Monoidal folding</h3>

\begin{code}
fold    :: (Monoid m, Foldable t) => t m   -> m
mconcat ::  Monoid m              =>  [m]  -> m
concat  ::                           [[a]] -> [a]
\end{code}

<h3>Discarding one of two values</h3>

\begin{code}
const ::                    a ->   b ->   a
(<$)  :: Functor     f =>   a -> f b -> f a
(<*)  :: Applicative f => f a -> f b -> f a
($>)  :: Functor     f => f a ->   b -> f b
(*>)  :: Applicative f => f a -> f b -> f b
(>>)  :: Monad       f => f a -> f b -> f b
\end{code}

<h3>Restructuring</h3>

\begin{code}
sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
sequence  :: (Monad       f, Traversable t) => t (f a) -> f (t a)
join      ::  Monad       f                 => f (f a) -> f    a
\end{code}

<h3>Lifting</h3>

\begin{code}
id     ::                                     a ->     a
pure   ::  Applicative f                =>    a ->   f a
return ::  Monad       m                =>    a ->   m a
liftIO ::  MonadIO     m                => IO a ->   m a
lift   :: (Monad       m, MonadTrans t) => m  a -> t m a
\end{code}

<h3>Running transformers</h3>

\begin{code}
runIdentityT :: IdentityT m a -> m        a
runListT     :: ListT     m a -> m       [a]
runMaybeT    :: MaybeT    m a -> m (Maybe a)
\end{code}

<h3>Monads are monoidal functors</h3>

\begin{code}
mappend :: Monoid m => m -> m    -> m   ; mempty  :: Monoid m =>      m
join'   :: Monad  m => m   (m a) -> m a ; return' :: Monad  m => a -> m a
\end{code}

</main>

<div style="display: none;">

\begin{code}
(.)       = (Prelude..)
($)       = (Prelude.$)
const     = Prelude.const
id        = Prelude.id

(<*>)     = (Control.Applicative.<*>)
(*>)      = (Control.Applicative.*>)
(<*)      = (Control.Applicative.<*)
liftA     = Control.Applicative.liftA
liftA2    = Control.Applicative.liftA2
liftA3    = Control.Applicative.liftA3
pure      = Control.Applicative.pure

(=<<)     = (Control.Monad.=<<)
(>>)      = (Control.Monad.>>)
(<=<)     = (Control.Monad.<=<)
filterM   = Control.Monad.filterM
join      = Control.Monad.join
join'     = Control.Monad.join
mapM      = Control.Monad.mapM
mfilter   = Control.Monad.mfilter
return    = Control.Monad.return
return'   = Control.Monad.return
sequence  = Control.Monad.sequence

liftIO       = Control.Monad.IO.Class.liftIO
lift         = Control.Monad.Trans.Class.lift
runIdentityT = Control.Monad.Trans.Identity.runIdentityT
runListT     = Control.Monad.Trans.List.runListT
runMaybeT    = Control.Monad.Trans.Maybe.runMaybeT

fold      = Data.Foldable.fold
foldMap   = Data.Foldable.foldMap

(<$>)     = (Data.Functor.<$>)
(<$)      = (Data.Functor.<$)
($>)      = (Data.Functor.$>)

concat    = Data.List.concat
filter    = Data.List.filter

mappend   = Data.Monoid.mappend
mconcat   = Data.Monoid.mconcat
mempty    = Data.Monoid.mempty

sequenceA = Data.Traversable.sequenceA
traverse  = Data.Traversable.traverse
\end{code}

</div>
