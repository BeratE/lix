# LiX

LiX - or Lychee, like the fruit - is a purely functional Lisp like toy language based on an interpreted, extended Lambda Calculus.
The name comes from the fact that LiX is some mix of a Lisp and a Lambda interpreter (Linp).

## Features
This project includes: 
* Parser that constructs a complex lambda term (i.e. terms with additional syntactic sugar) from a given Lisp expression.
* Translation rules to turn complex terms into simple terms (desugar syntax).
* Interpreter for lambda terms based on symbol substituion (beta-reduction).
* Extended with constants & delta-rules for fixpoint computation, arithmetic, etc. (delta-reduction)
* Weak Head Normal Order (WHNO) reduction to mimic a lazy evaluation strategy (confluence is ensured).
* Lisp primitives `ATOM`, `EQ`, `CAR`, `CDR`, `CONS`, `IF` realised as delta rules. 
* Lisp primitive `QUOTE` as extension of evaluation strategy, i.e. no further evaluation of arguments.
* Delta rules for fixpoint evaluation and recursive let declarations.


## Syntax
LiX has a very simple Lisp-like syntax. Every expression is one of the following:
- An Atom, which is either a Symbol or a Literal.
- An S-Expression, which is a list of zero or more expressions surrounded by round brackets.
Furthermore:
- A Symbol is a case-insensitve alphanumerical identifier or a string of special characters, e.g. <*>.
- A Literal is an integer number or a character string enclosed in double-quotes.

The EBNF used by the parser looks somewhat like this:
```
Expr := Symbol | Lit | SExpr
Lit := Number | String
SExpr := `(` Expr* `)`
```

### Syntactical Sugar
To make life a little bit easier, we introduce some special shorthand constructs.

Quote and lambda notation are lexical constructs that have to be parsed into valid S-Expressions. 

The other constructs are already considered valid S-Expressions and are regarded to be some sort of complex lambda terms.
Complex terms are translated to compositions of simple lambda terms before evaluation.

#### Lexical
##### QUOTE 'x
A `QUOTE` expression `(QUOTE expr)` can be abbreviated as `'expr`

##### LAMBDA (\x.y)
A `LAMBDA` expression `(LAMBDA X expr)` can be written as `(\x.expr)`, where x denotes the bound variable in the body expr.
Using this notation, one can express a nested lambda term with multiple bound variables by writing more than one symbol before the dot.
So for example `(\x y.y)` will be parsed into `(LAMBDA X (LAMBDA Y Y))`. 
Internally, only lambda abstractions with one single bound variable are regarded valid lambda terms.

#### Complex terms
##### COND (cond ((p1 e1) .. (pn en)))
The `COND` expression, as known from Lisp, appears in the form `(COND ((p1 e1) .. (pn en)))`. The predicates p1 to pn are evaluated up until the first one that returns a non-nil value, lets say pi. The corresponding expression ei is then evaluated and returned as the value of the whole `COND` expression. If no predicate evaluates to a non-nil value, the empty list `NIL` is returned. The `COND` expression is internally translated to a sequence of nested `IF` expressions.

##### LET (let a expr expr')

TODO..

## Semantics / Evaluation
The empty list `()` is evaluated to the special symbol `NIL`. 

The symbol `BOT` (bottom) is used to denote an error.

TODO..

## Example
The fibonacci function in LiX looks like this:
```lisp
;; The favorite function of every functional programmer
(let fib (lambda x
           (cond (((eq x 1) 1)
                  ((eq x 2) 1)
                  (1
                   (+ (fib (- x 1))
                      (fib (- x 2)))))))
     (fib 9))
```
