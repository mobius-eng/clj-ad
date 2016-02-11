# clj-ad

This is a library for automatic differentiation in Clojure. It's based
and initiall ported from [R6RS-AD](https://github.com/qobi/R6RS-AD)
and [`clj-auto-diff`](https://github.com/log0ymxm/clj-auto-diff).

## Usage

At the moment library is not in Clojars (yet). So, you will need to
fork the repository and do `lein install` in the repo directory. After
this the project can be referenced using

    [clj-ad "0.2.0"]

in dependencies. More info on using Leiningen with local repository
can be found
[here](http://www.spacjer.com/blog/2015/03/23/leiningen-working-with-local-repository/).


## What is "automatic (algorithmic) differentiation (AD)"?

AD rerers to the method of calculating numerical derivatives
analytically, but not symbolically. A general idea is to exploit the
chain rule. For this the whole arithmetic of the language is replaced
with slightly more generic arithmetic operations: instead of operating
on numbers only, these operations can accept *dual numbers* or *tapes*
and produce the derivative of the operation as well as the main
result.

In essence, *dual numbers* are the extension of numbers with
*perturbations*, their "derivatives". Consider function `f(x)`. If we
know it's derivative `df(x)`, then given a dual number `[z dz]`, the
result of application of `f(x)` to it is a dual number
`[f(z) df(z)*dz]`. This result can become an input to the next
function `g(x)`, producing `[g(f(z)) dg(z)*df(z)*dz]`. And so on. To
begin the process, all we need is to set `dz=1`. This is called
forward derivative.

TODO: reverse derivative.

## Current state
Forward derivatives are implemented in full. Reverse derivatives still
need to be implemented.

Next step: implement full Jacobian calculation.

## Code examples

### Single value functions
TODO
### Vector value functions
TODO
### Advanced use
TODO



## License

Copyright © 2014 Paul English
Copyright © 2016 Alexey Cherkaev (mobius-eng)

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
