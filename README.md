# calima

Another (unfinished) attempt at building a compiler.

## Language design

Calima was supposed to be a functional language comparable to the Standard ML core language,
but designed for efficient runtime evaluation. The major feature difference was supposed to be region-based memory management,
with unique/linear types planned for later, enabling the language to bascially be a more functional version of Rust.

Linear/unique types were also planned as an alternative to Haskell-style Monads, in the hope that a purely functional language
without monads would be easier to optimize. 

A typeclass/trait system was also considered but never even made it to the syntax stage.

### Examples

More examples can be found in the `examples` folder.

Factorial:

```
let rec fac = fun i ->
    if i == 0 then
        1
    else
        fac (i - 1)
    end
public let main = fun () -> println (fac 5)
```

List type:

```
type List a =
    | Node (List a, a)
    | Empty
let head = fun list -> case list of
    | Empty -> None
    | Node (_, e) -> Some e
end
let (t1: List Int) = Node (Empty, 2)
public let main = fun () -> let x = head t1 in ()
```

List type with regions:

```
import List
let new = fun _ -> let a = @a Empty
    in a
public let main = fun () ->
    region test
    let (t1: @test List Int) = Node (Empty, 2)
    let h = head t1
    in ()
```

## Compiler

The compiler was written in Rust, containing a hand-written lexer, a [LALRPOP](https://github.com/lalrpop/lalrpop) parser and a basic typechecker for the core language (regions support was never finished).
Error handling infrastructure based on [codespan](https://github.com/brendanzab/codespan) was also implemented and used for gathering multiple errors during the typechecking/type inference phase, while errors
were propagated upwards to avoid cascading.

The typechecker was again based on the Hindley-Milner algorithm.
A basic module/import system was started and large parts of the infrastructure are present but never completed due to complications.

The largest challenge encountered was, again, lifetime handling in Rust, especially when related to multiple modules and references between them.
As a solution, the last implemented feature were `symbols` that used reference-counted, interned strings in the hope of simplifying and solving these issues.

### Incompleteness

- Calima has no backend (because I never got around to it)
- Calima has no module system (it was never finished)
- It turned out that my envisioned region memory management idea would only work for very few deliberately designed programs and be too restrictive or inefficient otherwise

### Lessons learned

- When using Rust, use refcounted strings from the start [quetta](https://github.com/SpacialCircumstances/quetta)
- Don't try to build the entire frontend before doing any backend work
- Be more conservative about memory management