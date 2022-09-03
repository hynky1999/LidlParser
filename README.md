# LidlParser

### Features
- Types
    - integer
    - null (Not in pascal but I found it useful)
    - boolean

- Operators
    - `+`
    - `-`
    - `*`
    - `/`
    - `==`
    - `!=`
    - `>`
    - `<`
    - `>=`
    - `<=`
    - `mod`

- BuiltinFunctions
    - WriteLn (x : integer) : null
    - ReadLn () : integer

- Statements
    - `if then (else?)`
    - `while`

Grammar taken from [https://www.cs.utexas.edu/users/novak/grammar.html]

### Examples of valid programs in test{i}.pc

## Be aware that it is very pedantic about `;` !!!!
There is one change from the grammar above and that is that all statement in begin end must end with a ;.
This is true for if, while also. I believe that I could let ; parsing to be parsed as spaces(optional) but I decided to follow grammar.


### How to run
```
cabal run lidl-parser filename
```

### How to run tests
```
cabal test
``` 
