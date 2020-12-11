# Lambda calculator

Calculator based on the theory of lambda calculus

## Install

Make sure the commands `git --version` `cabal --version` work correctly on your computer,
then run

```bash
git clone https://github.com/Glebanister/lambda-calculator
cabal build
```

## Run

To execute calculator run

```bash
cabal run
Up to date
Welcome to lambda calculator!
λ>
```

## Use

Give some function to lambda calculator and it will return type and context of this function:

```text
λ> \x y -> x
() => (a0) -> ((a2) -> (a0))
λ> \x -> x y
(y :: Free.0) => ((Free.0) -> (a1)) -> (a1)
λ> \x -> x x
Typecheck Error: Can't unify (a0) with ((a0) -> (a1))!
```
