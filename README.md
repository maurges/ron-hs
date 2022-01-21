# ron-hs

This is a reference implementation of [RON data
format](https://github.com/ron-rs/ron) in haskell. At this early stage the API
is pretty unstable, the convenience is not present and the speed and
correctness are unknown. But I'm going to use RON in my phd and nothing will
stop me!

RON is a human-readable (sort-of) self-describing format. The name means "rusty
object notation": it takes inspiration from rust language, but is not its
subset at all. Ron is well-suited for haskell because it supports algebraic
datatypes.

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

6. Difference in internal representation.  
   So in ron-rs there's a distinction between enums and structs, even though
   enums completely subsume structs. This is gone.  
   Also according to the (admittedly informal) specification, there is a
   distinction between `Unit` and `Unit()`. This is gone as well.  
   Also booleans are not distinct from units. They are just another algebraic
   type after all, with the same encoded representation as some identifier.

## Future work

- [ ] Benchmark and see if it's completely bad. The quickcheck tests are
  unbearable at size=50 already...
  * Preliminary benchmarks show it's slightly worse than aeson, and a lot worse
    than rust. I really don't known how to profile, so this needs more
    learning.
- [ ] Generic FromRon and ToRon instances. A good opportunity for me to learn
  ghc generics.
- [ ] Better serializer with configurable options.
- [x] Maybe switch to bytestring for encoder? The bytestring builder is a lot
  more advanced than text one.
  * Did and the ergonomics are a lot better
- [x] Maybe switch to bytestring for decoder? The specification is unclear
  whether raw sting may contain invalid unicode, and attoparsec for text is as
  good as for bytestring.
  * Done. Performance is basically the same, but correctness is improved.
- [ ] Do I really need Map and Vector? I just copied that part from aeson
- [ ] See what else good there is in aeson and yank that. Loading from files
  maybe?
- [ ] Should expose attoparsec's chunk-based parse so that this could be used
  with conduit or pipes.
