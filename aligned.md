<html>
<head>
<title>Haskell Aligned</title>

<style>
body {
  padding: 3.5em 1em 3em;
  text-align: center;
}
main {
  display: inline-block;
  text-align: left;
  border-top: 1px solid #333;
  margin-top: 2em;
  padding: 1em 1em 0;
}
.haskell {
  color: #3c2410;
}
.haskell .dt {
  color: green;
}
</style>

</head>

<body>

# Lined-up Haskell types

Got more to add? Submit a [GitHub](https://github.com/chris-martin/haskell-aligned) issue or talk to me on [Twitter](https://twitter.com/chris__martin).

<main>

## Function application

```haskell
($)      ::                                     (a ->   b) ->   a ->      b
(<$>)    ::  Functor     f                 =>   (a ->   b) -> f a -> f    b
(<*>)    ::  Applicative f                 => f (a ->   b) -> f a -> f    b
(=<<)    ::  Monad       f                 =>   (a -> f b) -> f a -> f    b
mapM     :: (Monad       f, Traversable t) =>   (a -> f b) -> t a -> f (t b)
traverse :: (Applicative f, Traversable t) =>   (a -> f b) -> t a -> f (t b)
foldMap  :: (Monoid      b, Traversable t) =>   (a ->   b) -> t a ->      b
```

## Applicative lifting

```haskell
pure   :: Applicative f =>  a                 -> f a
liftA  :: Applicative f => (a -> b)           -> f a -> f b
liftA2 :: Applicative f => (a -> b -> c)      -> f a -> f b -> f c
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
```

## Function composition

```haskell
(.)   ::            (b ->   c) -> (a ->   b) -> a ->   c
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
```

## Filtering

```haskell
filter  ::                (a ->   Bool) ->  [a] ->   [a]
filterM :: Monad     m => (a -> m Bool) ->  [a] -> m [a]
mfilter :: MonadPlus m => (a ->   Bool) -> m a  -> m  a
```

## Monoidal folding

```haskell
fold    :: (Monoid m, Foldable t) => t m   -> m
mconcat ::  Monoid m              =>  [m]  -> m
concat  ::                           [[a]] -> [a]
```

## Monoidal folding with(out) mapping

```haskell
fold      :: (Monoid m, Foldable t) =>               t m   ->  m
foldMap   :: (Monoid m, Foldable t) => (a ->  m)  -> t a   ->  m
concat    ::                                         [[a]] -> [a]
concatMap ::            Foldable t  => (a -> [b]) -> t a   -> [b]
```

## Causality with(out) mapping

```haskell
join  :: Monad process => process           (process result) -> process result
(>>=) :: Monad process => process a -> (a -> process result) -> process result
```

## Discarding one of two values

```haskell
const ::                    a ->   b ->   a
(<$)  :: Functor     f =>   a -> f b -> f a
(<*)  :: Applicative f => f a -> f b -> f a
($>)  :: Functor     f => f a ->   b -> f b
(*>)  :: Applicative f => f a -> f b -> f b
(>>)  :: Monad       f => f a -> f b -> f b
```

## Restructuring

```haskell
sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
sequence  :: (Monad       f, Traversable t) => t (f a) -> f (t a)
join      ::  Monad       f                 => f (f a) -> f    a
```

## Traversal with(out) mapping and with(out) result accumulation

```haskell
sequenceA  :: (Traversable t, Applicative f) =>               t (f a) -> f (t a)
traverse   :: (Traversable t, Applicative f) => (a -> f b) -> t    a  -> f (t b)
sequenceA_ :: (Foldable    t, Applicative f) =>               t (f a) -> f ()
traverse_  :: (Foldable    t, Applicative f) => (a -> f b) -> t    a  -> f ()
```

## Folding with(out) effects and with(out) result accumulation

```haskell
foldl  ::                          (b -> a ->   b) -> b -> t a ->   b
foldM  :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldM_ :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m ()
```

## Lifting

```haskell
id     ::                                     a ->     a
pure   ::  Applicative f                =>    a ->   f a
return ::  Monad       m                =>    a ->   m a
liftIO ::  MonadIO     m                => IO a ->   m a
lift   :: (Monad       m, MonadTrans t) => m  a -> t m a
```

## Running transformers

```haskell
runIdentityT :: IdentityT m a -> m        a
runListT     :: ListT     m a -> m       [a]
runMaybeT    :: MaybeT    m a -> m (Maybe a)
```

## The monoid in Monad

```haskell
mappend :: Monoid m => m -> m    -> m
join    :: Monad  m => m   (m a) -> m a

mempty  :: Monoid m =>      m
return  :: Monad  m => a -> m a
```

</main></body></html>
