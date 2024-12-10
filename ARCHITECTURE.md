### Performance

Tardigrade is purposefully a _prototype_, meant to explore possible language designes. It therefore
doesn't emphasize being performant. Though it is very much designed to _be able to be made_
performant.

### Errors

Errors that could be caused by bad input programs (e.g. parse errors, type errors, division by zero)
are represented with `Result`s. Errors that could not be caused by bad input programs, that is bugs,
will panic (e.g. memory errors).

### Memory

FILL
