// Hackily copied from the panfix crate.

use panfix::{Source, Span};
use std::error;
use std::fmt;
use std::fmt::Write;

/// An error message that contains source highlighting, like the following:
///
/// ```text
/// Parse Error: Missing expression.
///  --> addition:1:8
///   |
/// 1 |    2 + + 3
///   |       ^ expected expression
/// ```
///
/// In this example:
///
/// - `kind` is "Parse Error"
/// - `source` is the whole source program, whose first line is `2 + + 3`.
/// - `span` ranges from 1:8 to 1:9.
/// - `label` is "expected expression"
/// - `message` is "Missing expression."
#[derive(Debug)]
pub struct Error<'s> {
    pub kind: &'static str,
    pub source: &'s Source,
    pub span: Span,
    pub label: String,
    pub message: String,
}

impl<'s> Error<'s> {
    pub fn new(
        kind: &'static str,
        source: &'s Source,
        span: Span,
        label: &str,
        message: &str,
    ) -> Error<'s> {
        Error {
            kind,
            source,
            span,
            label: label.to_owned(),
            message: message.to_owned(),
        }
    }

    /// The regular `fmt::Display` implementation attempts to infer whether to print with color.
    /// Use this method to manually set whether to print with color.
    pub fn display_with_color_override(&self, use_color: bool) -> impl fmt::Display + '_ {
        /// Wrapper struct for recording whether to print with color.
        struct DisplayError<'a> {
            error: &'a Error<'a>,
            use_color: bool,
        }

        impl fmt::Display for DisplayError<'_> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                colored::control::set_override(self.use_color);
                write!(f, "{}", self.error)?;
                colored::control::unset_override();
                Ok(())
            }
        }

        DisplayError {
            error: self,
            use_color,
        }
    }
}

impl fmt::Display for Error<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        show_error_with_loc(
            f,
            self.source,
            self.kind,
            self.span,
            &self.label,
            &self.message,
        )
    }
}

impl error::Error for Error<'_> {}

struct LineInfo<'a> {
    num: String,
    contents: &'a str,
    carets_start: usize,
    carets_len: usize,
}

fn show_error_with_loc(
    buffer: &mut impl Write,
    source: &Source,
    error_kind: &str,
    span: Span,
    label: &str,
    message: &str,
) -> fmt::Result {
    use colored::Colorize;

    let start = span.start;
    let end = span.end;
    let (first_line, last_line, margin, ellided) = {
        if start.line == end.line {
            let first_line = LineInfo {
                num: format!("{}", start.line + 1),
                contents: source.line_contents(start.line),
                carets_start: start.col as usize,
                carets_len: (end.col - start.col).max(1) as usize,
            };
            let margin = first_line.num.len() + 1;
            (first_line, None, margin, false)
        } else {
            let first_line = LineInfo {
                num: format!("{}", start.line + 1),
                contents: source.line_contents(start.line),
                carets_start: start.col as usize,
                carets_len: source.line_contents(start.line).chars().count() - start.col as usize,
            };
            let last_line = LineInfo {
                num: format!("{}", end.line + 1),
                contents: source.line_contents(end.line),
                carets_start: 0,
                carets_len: end.col.max(1) as usize,
            };
            let margin = first_line.num.len().max(last_line.num.len()) + 1;
            let ellided = end.line - start.line >= 2;
            (first_line, Some(last_line), margin, ellided)
        }
    };

    writeln!(
        buffer,
        "{}{} {}",
        error_kind.red().bold(),
        ":".bold(),
        message.bold(),
    )?;

    writeln!(
        buffer,
        "{:margin$}{} {}:{}:{}",
        "",
        "-->".blue().bold(),
        source.filename(),
        start.line + 1,
        start.col + 1,
        margin = margin - 1,
    )?;

    if let Some(last_line) = last_line {
        show_line(buffer, margin, "", "")?;
        show_line(buffer, margin, &first_line.num, first_line.contents)?;
        let carets = show_carets(first_line.carets_start, first_line.carets_len, "");
        show_line(buffer, margin, "", &carets)?;
        if ellided {
            writeln!(buffer, "{}", "...".blue().bold())?;
        }
        show_line(buffer, margin, &last_line.num, last_line.contents)?;
        let carets = show_carets(last_line.carets_start, last_line.carets_len, label);
        show_line(buffer, margin, "", &carets)
    } else {
        show_line(buffer, margin, "", "")?;
        show_line(buffer, margin, &first_line.num, first_line.contents)?;
        let carets = show_carets(first_line.carets_start, first_line.carets_len, label);
        show_line(buffer, margin, "", &carets)
    }
}

fn show_carets(carets_start: usize, carets_len: usize, label: &str) -> String {
    use colored::Colorize;

    if label.is_empty() {
        format!(
            "{:padding$}{}",
            "",
            "^".repeat(carets_len).red().bold(),
            padding = carets_start
        )
    } else {
        format!(
            "{:padding$}{} {}",
            "",
            "^".repeat(carets_len).red().bold(),
            label,
            padding = carets_start
        )
    }
}

fn show_line(
    buffer: &mut impl Write,
    margin: usize,
    line_num: &str,
    contents: &str,
) -> fmt::Result {
    use colored::Colorize;

    writeln!(
        buffer,
        "{:<margin$}{}{}",
        line_num.blue().bold(),
        "|".blue().bold(),
        contents,
        margin = margin,
    )
}
