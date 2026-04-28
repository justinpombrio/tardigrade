fn main() {
    use panfix::{Position, Source, Span};
    use tardigrade::{Error, Logger, RenderOptions, Verbosity, log};

    let message = "Hello\nWorld";
    let source = Source::new("[test]", "2 + + 3\n".to_owned());
    let span = Span {
        start: Position {
            line: 0,
            col: 7,
            utf8_col: 7,
        },
        end: Position {
            line: 0,
            col: 8,
            utf8_col: 8,
        },
    };
    let error = Error::new(
        "Parse Error",
        &source,
        span,
        "expected expression",
        "Missing expression.",
    );
    let mut log = Logger::new(Verbosity::Info, RenderOptions::default());
    log!(log, Info, "main", {
        log!(log, Info, message);
        log!(log, Info, "SomeLabel");
        log!(log, Info, "error", ("{}", source.source().trim()), {
            log!(log, Info, &error);
        });
    });
}
