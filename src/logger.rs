use crate::render::{Color, Render, Renderer};

pub use crate::render::RenderOptions;

#[allow(unused)]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Verbosity {
    Notice,
    Info,
    Debug,
}

pub struct Logger {
    verbosity: Verbosity,
    renderer: Renderer,
}

impl Logger {
    pub fn new(verbosity: Verbosity, options: RenderOptions) -> Logger {
        Logger {
            verbosity,
            renderer: Renderer::new(options),
        }
    }

    pub fn indent(&mut self) {
        self.renderer.indent();
    }

    pub fn dedent(&mut self) {
        self.renderer.dedent();
    }

    pub fn render(&mut self, renderable: &(impl Render + ?Sized)) {
        self.renderer.raw_indent();
        renderable.render(&mut self.renderer);
        self.renderer.raw_newline();
    }

    pub fn item(&mut self, key: &str, message: Option<String>) {
        self.renderer.raw_indent();
        self.renderer.boldly_colored(key, Color::Green);
        if let Some(message) = message {
            self.renderer.raw_str(" ");
            self.renderer.colored(&message, Color::Yellow);
        }
        self.renderer.raw_newline();
    }

    pub fn group(&mut self, key: &str, message: Option<String>) {
        self.renderer.raw_indent();
        self.renderer.boldly_colored(key, Color::Blue);
        if let Some(message) = message {
            self.renderer.raw_str(" ");
            self.renderer.colored(&message, Color::Magenta);
        }
        self.renderer.raw_newline();
    }

    #[inline]
    pub fn enabled(&self, level: Verbosity) -> bool {
        self.verbosity >= level
    }
}

#[macro_export]
macro_rules! log {
    ($logger:expr, $verbosity:ident, $key:literal) => {
        if $logger.enabled($crate::logger::Verbosity::$verbosity) {
            $logger.item($key, None);
        }
    };
    ($logger:expr, $verbosity:ident, $key:expr, ($fmt_str:literal, $($args:expr),+)) => {
        if $logger.enabled($crate::logger::Verbosity::$verbosity) {
            $logger.item($key, Some(format!($fmt_str, $($args),+)));
        }
    };
    ($logger:expr, $verbosity:ident, $fmt_obj:expr) => {
        if $logger.enabled($crate::logger::Verbosity::$verbosity) {
            $logger.render($fmt_obj);
        }
    };
    ($logger:expr, $verbosity:ident, $key:expr, $body:block) => {{
        if $logger.enabled($crate::logger::Verbosity::$verbosity) {
            $logger.group($key, None);
            $logger.indent();
            #[allow(clippy::redundant_closure_call)]
            let result = (|| $body)();
            $logger.dedent();
            $logger.group("end", None);
            result
        } else {
            $body
        }
    }};
    ($logger:expr, $verbosity:ident, $key:expr, ($fmt_str:literal, $($args:expr),+), $body:block) => {{
        if $logger.enabled($crate::logger::Verbosity::$verbosity) {
            $logger.group($key, Some(format!($fmt_str, $($args),*)));
            $logger.indent();
            #[allow(clippy::redundant_closure_call)]
            let result = (|| $body)();
            $logger.dedent();
            $logger.group("end", None);
            result
        } else {
            $body
        }
    }};
}
