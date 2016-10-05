# DAT235 project description

## Project summary
An area of interest in the interaction between the social sciences and computer science / mathematics is the notion
of qualitative, or imprecise, reasoning. The goal of this project is to develop domain specific languages
for modeling and decision making in Climate Resilient Urban Design using qualitative reasoning.
There are two major problems in designing domain specific languages for wide and imprecise
domains like Climate Resilient Urban Design. The first is to define a precise semantics
of qualitative concepts. The second problem is the decomposition of the domain. For large and complicated
domains an attractive approach to DSL design is to design several small domain specific languages
for different sub domains and some how combine these small DSLs at will to reason about larger subsets of the original domain.

The work will focus on the second problem. Combining several domain specific languages to reason about
large, composite, domains. The aim is to decompose the problem of Climate Resilient Urban Design
in to a number of domain specific languages for e.g. reasoning about spatial limitations,
stakeholder preferences, causality, etc. and show how these languages can be combined
to reason about larger problems in Climate Resilient Urban Design.

The goal is to use free monads[1] combined with the techniques of Axelsson. et. al.[2, 3] to combine multiple code generating
domain specific languages that will target the constraint programming language MiniZinc[4] using the haskelzinc(SIC)[5] library.

\[1\] [Data types à la Carte](http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf)

\[2\] [Combining Deep and Shallow Embedding for EDSL](http://www.cse.chalmers.se/~emax/documents/svenningsson2013combining.pdf)

\[3\] [Generic Monadic Constructs for Embedded Languages](http://www.cse.chalmers.se/~josefs/publications/paper21_cameraready.pdf)

\[4\] [MiniZinc](https://www.minizinc.org)

\[5\] [The haskelzinc library](https://github.com/GRACeFUL-project/haskelzinc)

## Builds on courses
Advanced Functional Programming, Domain Specific Languages of Mathematics, Discrete Mathematics, Probability Theory
