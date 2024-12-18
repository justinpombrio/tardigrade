Tardigrade is a _prototype_. It's meant to explore possible language designs. This purpose underlies
many of the design decisions documented here.

### Performance

Tardigrade is designed to _be able to be made_ performant, but whenever there's a tradeoff between
implementation clarity and performance it chooses clarity. Expect it to be slow!

Though with compile time evaluation and lack of garbage collection and aliasing, it should be
possible to make it extremely fast one day.

### Memory: Value enum

If you look at the implementation of `Value`s, you'll notice a curious design choice:

- `Value` is a (tagged) enum which knows which variant it is.
- The interface on `Value` hides this information: there's no way to check which variant the `Value`
  is, and if you attempt to access a `Value` as (say) an integer and it's not an integer, it will
  panic.

This is the worst of both worlds! Why would anyone design things this way?

There are two things I wish to achieve that together lead to this otherwise strange design:

- A production version of Tardigrade would have to deal with actual memory, which will not be
  tagged. Thus it is important that the interpreter (later: compiler) not rely on being able to tell
  which variant a `Value` is, because that information will not be available in the future.
- However, if Tardigrade writes an integer but later attempts to read that memory as a float, that's
  a bug and should be caught. This is why `Value` needs to know which variant it is: for detecting
  and reporting this bug.

### Memory: Refcounting

Tardigrade will pass small `Value`s by value, and refcount large values. This refcounting is not meant
to be how Tardigrade ultimately does memory management: it can handle its own memory just fine with
mutable value semantics and does not need refcounts. The refcounting exists solely for detecting
bugs: Tardigrade will check that the refcount is exactly 1 when a value is passed by ownership or
dropped, and panic if it's not.

### Bugs vs. Errors

There's an extremely important distinction between two things that are often both glossed as
"errors":

- **Bug:** There's a mistake in the _program_.
- **Error:** There's a mistake in the _input to the program_.

Awkwardly, the input to Tardigrade is itself a program. So "bug" here means that there's a bug in
Tardigrade's Rust code, while "error" means that there's a mistake in the user's Tardigrade code.

(Let me know if you have a better term for "error" to distinguish it from "bug". Note that this is
not about "recoverable" vs. "unrecoverable" errors: a (non-bug) error could be either recoverable or
unrecoverable.)

Tardigrade will _panic_ on bugs because it's not safe (and not always even feasible) to continue
running code when the code's assumptions have been violated. The panic messages are written for the
person developing Tardigrade itself (me), and may not be particularly friendly to the person who
wrote the Tardigrade code (e.g. it will have to talk about language internals). To be clear though:
if Tardigrade is implemented correctly, it will not panic under any circumstances! Every call to
`.unwrap()` etc. aims to be provably impossible to trigger from any possible input.

In contrast, Tardigrade will return a _Result_ on errors. The error messages will be user friendly,
because they're written to explain to the programmer what went wrong in their Tardigrade program.
Some contexts (such as parsing or type checking) may show multiple errors at once. Whenever
possible, the error message will highlight the relevant part of the input program (see
`src/error.rs`).

For a much more thorough explanation of bugs vs. errors, see BurntSushi's
[Unwrap post](https://blog.burntsushi.net/unwrap/).

### Testing

Tardigrade uses data driven test cases that look like this:

```
TEST addition
    2 + 3
EXPECT
    5
END
```

The TEST section contains source code that is run. The result is printed and compared to the EXPECT
section.

There are several kinds of result: parse errors, type checking errors, runtime errors, and success
values. Whichever of these is produced, it's simply serialized into a string and compared to the
EXPECT section. The test case doesn't say which kind of result it's expecting, but it's easy to tell
as a human (e.g. if it starts with "Parse Error:" then it's probably a parse error).

I view this as the big insight of data driven test cases: make your tests produce strings, because
strings are (i) easy to write in a test case, and (ii) easy to show a diff for if the test case
fails.

For a much more thorough explanation of data driven tests, see Russ Cox's
[Go Testing By Example](https://research.swtch.com/testing).
