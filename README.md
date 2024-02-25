# ron-hs

This is an implementation of [RON data format](https://github.com/ron-rs/ron)
in haskell. This project is still early in its life, but has been tried in some
of my webservers like [re](https://github.com/maurges/re) and
[dwierz](https://git.morj.men/morj/dwierz)

RON is a human-readable structured format. The name means "rusty object
notation": it takes inspiration from rust language, but is not its subset at
all. Ron is well-suited for haskell because it supports algebraic datatypes.

For usage info and examples, see the API docs.

### Example of RON data

    // RON supports comments
    /* even multiline comments */
    /* even /* embedded /* multiline */ comments */ /* */ */
    TopLevelRecord (
        field: Compound ("this", "is", "a", "compound", 123),

        all_datatypes: (
            int: 1_337_322_228,
            float: 2.718281828,
            char: 'ы',
            string: "hello string\n",
            raw_string: r##"raw "string", still unicode"##,
            list: ["homogenous", "elements"],
            map: {
                "value": "as keys",
                "also": "homogenous",
            },
            tuple: SomeTuple(0, OtherTuple("with", "stuff")),
            record: SomeRecord (
                like: "this one you're looking at",
                not_homogenous: 2022,
            ),
            unit_values: [
                (), // regular unit
                NamedUnit,
                NamedUnitWithBraces(),
            ]
        )
    )

## Differences from ron-rs

1. No support for extensions (and not planned)

2. Support `'` in identifiers, like in haskell (so not at the start of a word)

3. `true` and `false` are not special, treated as other unit values. Also
   haskell's Bool can decode both `true`, `false` and `True`, `False`
   (but encodes to the lowercase version for ron-rs compatability)

4. Support for toplevel lists without outer `[]` like this:
   ```
    "ron-rs document",
    "with a toplevel list",
    "denoted by just elements with commas",
   ```

5. Support for [toplevel records](https://github.com/ron-rs/ron/issues/297)
   like this:
   ```
    field1: SomeValue(),
    field2: "other value",
   ```

6. Intended and unintended differences in floating point handling. Inteded is
   that ron-rs supports `-0` and `-inf` and ron-hs doesn't yet. Unintended is
   that ron-hs uses Scientific and so is infinitely precise.

7. Difference in internal representation.  
   In ron-rs there's a distinction between enums and structs, even though
   enums completely subsume structs. This is gone.  
   Also according to the (admittedly informal) specification, there is a
   distinction between `Unit` and `Unit()`. This is gone as well.  
   Also booleans are not distinct from units. They are just another algebraic
   type after all, with the same encoded representation as some identifier.

## Future work

- [ ] Benchmarks and optimizations. For now the performance is similar to
  aeson, but is 10 times worse than ron-rs
- [x] Generic FromRon and ToRon instances.
- [x] Better serializer with configurable options.
- [ ] Do I really need Map and Vector? I just copied that part from aeson
  * Map makes it very convenient to decode records, vector is probably more
    efficient in encoding tuples. Although there is a low-hanging optimization
    fruit for tuple encoding that I've not taken yet: preallocate the vector
    and put elements by index
  * Another good thing about (ordered) map is that it normalizes field order
- [x] Allow for generic deriving to have options with DerivingVia. Can
  implement the ron-rs's pragmas this way.
- [ ] Option to treat record as a tuple in encoding
  * [ ] A more difficult task is an option to treat as any of those in decoding
- [ ] Add flags to disable some heavy features like TemplateHaskell,
  DerivingVia and Generics for minimal installations

Other than that, it's really ready to use, as I tried on myself.
