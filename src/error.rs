use crate::render::{Color, Render, Renderer};
use panfix::{Source, Span};

/// An error message that contains source highlighting, like the following:
///
/// ```text
/// Parse Error: Missing expression.
///  --> addition:1:8
///   |
/// 1 | 2 + + 3
///   |       ^ expected expression
/// ```
///
/// In this example:
///
/// - `kind` is "Parse Error"
/// - `source` is the whole source program, whose first line is `2 + + 3`.
/// - `span` ranges from 0:7 to 0:8.
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
}

impl Render for Error<'_> {
    fn render(&self, r: &mut Renderer) {
        show_error_with_loc(
            r,
            self.source,
            self.kind,
            self.span,
            &self.label,
            &self.message,
        )
    }
}

struct LineInfo<'a> {
    num: String,
    contents: &'a str,
    carets_start: usize,
    carets_len: usize,
}

fn show_error_with_loc(
    r: &mut Renderer,
    source: &Source,
    error_kind: &str,
    span: Span,
    label: &str,
    message: &str,
) {
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

    r.boldly_colored(error_kind, Color::Red);
    r.boldly_colored(": ", Color::White);
    r.boldly_colored(message, Color::White);
    r.newline();

    r.spaces(margin - 1);
    r.boldly_colored("-->", Color::Blue);
    r.raw_str(&format!(
        " {}:{}:{}",
        source.filename(),
        start.line + 1,
        start.col + 1
    ));
    r.newline();

    if let Some(last_line) = last_line {
        show_line(r, margin, "", "");
        show_line(r, margin, &first_line.num, first_line.contents);
        show_line_prefix(r, margin, "");
        show_carets(r, first_line.carets_start, first_line.carets_len, "");
        if ellided {
            r.boldly_colored("...", Color::Blue);
            r.newline();
        }
        show_line(r, margin, &last_line.num, last_line.contents);
        show_line_prefix(r, margin, "");
        show_carets(r, last_line.carets_start, last_line.carets_len, label);
    } else {
        show_line(r, margin, "", "");
        show_line(r, margin, &first_line.num, first_line.contents);
        show_line_prefix(r, margin, "");
        show_carets(r, first_line.carets_start, first_line.carets_len, label);
    }
}

fn show_carets(r: &mut Renderer, carets_start: usize, carets_len: usize, label: &str) {
    let contents = if label.is_empty() {
        "^".repeat(carets_len)
    } else {
        format!("{} {}", "^".repeat(carets_len), label)
    };
    r.spaces(carets_start);
    r.boldly_colored(&contents, Color::Red);
    r.newline();
}

fn show_line(r: &mut Renderer, margin: usize, line_num: &str, contents: &str) {
    show_line_prefix(r, margin, line_num);
    r.raw_str(" ");
    r.raw_str(contents);
    r.newline();
}

fn show_line_prefix(r: &mut Renderer, margin: usize, line_num: &str) {
    r.boldly_colored(line_num, Color::Blue);
    r.spaces(margin - line_num.len());
    r.boldly_colored("|", Color::Blue);
}
