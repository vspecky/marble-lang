# Marble
A simple, interpreted, toy programming language in Haskell.

My interest in parsing and parser combinators is probably clear from some of my previous
projects. I wanted to explore the same in haskell with monadic parser combinators and go
the extra mile by also implementing an interpreter for a programming language.

I'm planning on extending this language further as I go and work on different projects. A
tentative roadmap can be found below.

### Usage
Using cabal
```
cabal run marble -- ./examples/proto_09-10-2022/fib.mrbl
```

### Roadmap
- [X] Monadic parser
- [X] Monadic interpreter
- [X] Make interpreter executable to run programs
- [ ] Add location context to both parsing and interpretation for better error messages.
Plus improving error messages
- [ ] Separate out the parser combinators into their own parser library
- [ ] Add container types (Lists and Maps)
- [ ] Add garbage collection
- [ ] Add static types and a static type checker
- [ ] Turn this into a compiler that compiles to some bytecode
