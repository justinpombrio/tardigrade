use crate::ast::Prec;
use crate::format::{Format, INDENT_WIDTH};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Verbosity {
    Required,
    Info,
    Trace,
}

pub struct Logger {
    verbosity: Verbosity,
    indentation: u32,
}

impl Logger {
    pub fn new(verbosity: Verbosity) -> Logger {
        Logger {
            verbosity,
            indentation: 0,
        }
    }

    pub fn msg(&mut self, key: &str, message: Option<String>) {
        use colored::Colorize;

        let indent = (self.indentation * INDENT_WIDTH) as usize;
        let key = key.green().bold();
        if let Some(message) = message {
            let message = message.yellow();
            eprintln!("{:indent$}{} {}", "", key, message, indent = indent);
        } else {
            eprintln!("{:indent$}{}", "", key, indent = indent);
        }
    }

    pub fn span_msg(&mut self, key: &str, message: Option<String>) {
        use colored::Colorize;

        let indent = (self.indentation * INDENT_WIDTH) as usize;
        let key = key.blue().bold();
        if let Some(message) = message {
            let message = message.magenta();
            eprintln!("{:indent$}{} {}", "", key, message, indent = indent);
        } else {
            eprintln!("{:indent$}{}", "", key, indent = indent);
        }
    }

    pub fn log(&mut self, format_obj: &impl Format) {
        let indent = (self.indentation * INDENT_WIDTH) as usize;
        let mut buffer = String::new();
        format_obj
            .format(&mut buffer, self.indentation, Prec::MAX)
            .expect("Failed to log");
        eprintln!("{:indent$}{}", "", buffer, indent = indent);
    }

    pub fn start_span(&mut self) {
        self.indentation += 1;
    }

    pub fn end_span(&mut self) {
        self.indentation -= 1;
    }

    #[inline]
    pub fn enabled(&self, level: Verbosity) -> bool {
        self.verbosity >= level
    }
}

#[macro_export]
macro_rules! log {
    ($logger:expr, $verbosity:ident, $key:literal) => {
        if $logger.enabled($crate::Verbosity::$verbosity) {
            $logger.msg($key, None);
        }
    };
    ($logger:expr, $verbosity:ident, $key:expr, ($fmt_str:literal, $($args:expr),+)) => {
        if $logger.enabled($crate::Verbosity::$verbosity) {
            $logger.msg($key, Some(format!($fmt_str, $($args),+)));
        }
    };
    ($logger:expr, $verbosity:ident, $fmt_obj:expr) => {
        if $logger.enabled($crate::Verbosity::$verbosity) {
            $logger.log($fmt_obj);
        }
    }
}

#[macro_export]
macro_rules! span {
    ($logger:expr, $verbosity:ident, $key:expr, $body:block) => {{
        if $logger.enabled($crate::Verbosity::$verbosity) {
            $logger.span_msg($key, None);
            $logger.start_span();
            #[allow(clippy::redundant_closure_call)]
            let result = (|| $body)();
            $logger.end_span();
            $logger.span_msg("end", None);
            result
        } else {
            $body
        }
    }};
    ($logger:expr, $verbosity:ident, $key:expr, ($fmt_str:literal, $($args:expr),+), $body:block) => {{
        if $logger.enabled($crate::Verbosity::$verbosity) {
            $logger.span_msg($key, Some(format!($fmt_str, $($args),*)));
            $logger.start_span();
            #[allow(clippy::redundant_closure_call)]
            let result = (|| $body)();
            $logger.end_span();
            $logger.span_msg("end", None);
            result
        } else {
            $body
        }
    }};
}
