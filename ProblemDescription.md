# Description of the problems I wish to solve

## The GraphDSL architecture

The main challenge of the GraphDSL architecture
is to combine to separate monadic DSLs in to one
monadic DSL without monad transformers. Either
because a monad transformer may not make sense
for one monad, or simply because monad transformers
make for really ugly code if you want to go a few
layers deep.

### Abstract notion of the problem

We have some monadic DSLs, represented by types:
```haskell
data DSL1 name a = ...
instance Monad (DSL1 name) where ...

data DSL2 name = ...
instance Monad (DSL2 name) where ...
```

We wish to construct the DSL
```haskell
data DSL3 name a = DSL1 name :+: DSL2 name

instance Monad (DSL3 name) where ...
```

Furthermore each of `DSL1` and `DSL2` have a notion
of compilation:
```haskell
compileDSL1 :: DSL1 name a -> Output1

compileDSL2 :: DSL2 name a -> Output2
```
But there exists some form of interdependence
between the DSLs:
```haskell
dependence :: DSL1 name a -> DSL2 name b -> (DSL1 name a, DSL2 name b)
```

This seems to have been partially (or completely?) solved
by [Datatypes á la Carte](http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf).

### Relating this to GraphDSL

The two DSLs are `GraphDSL`, which in my imagined world
ranges over `names` (which is `String` in my example),
and some form of monadic `HaskelzincMonad`
(which should only be a state monad on top of haskelzinc, realistically).
We can compile each down to their separate structures,
an `fgl` graph in the case of `GraphDSL` and a `String` to
be sent to minizinc in the `HaskelzincMonad` case.
The dependence comes from the fact that we may want
constraints from our constraint model reflected in our
`fgl` graph, and we also want to automatically construct
our constraint model, which depends on the graph,
when we compile to minizinc.

## The semantics of Causal Loop Diagrams

The QPN formalism from Wellman works well
for abstracting influences. However I have
yet to find a notion of observation in the
literature. The idea that we can "observe"
plus or minus in a node perplexes me.

Introducing a notion of qualitative time to
QPN:s would give rise to a semantics of
CLD:s.

It would be nice to completely work out the
types and typeclasses involved in QPN:s and
their ultimate probability space (?) semantics.
What we could do for finite (or maybe countable?)
domains is to use some Haskell logic programming
library to construct candiates for the probability
distributions from a QPN. This could be done in a
straight forward way as all a QPN is is a series
of constraints on the probability distribution
(specifically the "First order stochastic dominance"
relation).
