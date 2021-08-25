# LiX

LiX - or Lychee, like the fruit - is a purely functional Lisp like toy language based on an interpreted (extended) Lambda Calculus.
The name stems from the project being a mix of a Lisp and a Lambda interpreter (Linp).

## Features
This project includes: 
* Parser that constructs a complex lambda term (i.e. terms with additional syntactic sugar) from a given Lisp expression.
* Translation rules to turn complex terms into simple terms (desugar syntax), e.g.
* Interpreter for lambda terms based on symbol substitution with constants & delta-rules (beta-delta-reduction).
* Weak Head Normal Order (WHNO) reduction to mimic a lazy evaluation strategy.
* Lisp primitives `ATOM`, `EQ`, `CAR`, `CDR`, `CONS`, `IF` realised as delta rules. 
* Lisp primitive `QUOTE` as extension of evaluation strategy.
* Delta rules for fixpoint evaluation and recursive let declarations.


## Syntax
LiX has a very simple Lisp-like syntax. Every expression is one of the following:
- An Atom, which is either a Symbol (alphanumerical identifier) or a Literal (integer number or string)
- An S-Expression, which is a List of zero or more expressions surrounded by round brackets

Currently only integer numbers are supported. 
Strings are enclosed in quotes, as usual.

The EBNF used by the parser looks like follows:
```
Expr := Symbol | Lit | SExpr
Lit := Number | String
SExpr := `(` Expr* `)`
```

To make life little bit easier, we introduce some syntactical constructs that are parsed to regular S-Expressions.

A Lambda abstraction can be written as `(\args.expr)`, where `args` consists of one or more symbols denoting the bound variables in the expression `expr` following the dot. This construct will be pased into the lambda S-Expr `(LAMBDA (args) expr)`. Internally, only lambda abstractions with a single bound variable are valid lambda terms. If a lambda S-Expr has a list of symbols in its arguments, the expression will be translated into nested lambda terms with single bound variables each.

The `COND` expression as known from Lisp can be written as `(COND conds)`, where `conds` is a list of one or more conditions of the form `(pred expr)`. The `pred` part of each condition is evaluated in order until one evaluates to a non Nil value. The corresponding `expr` of that non Nil `pred` is then evaluated which is the value of the whole `COND` expression. The `COND` expression is internally translated to nested `IF` expressions.
