use std::io;

/*****************
 * RenderOptions *
 *****************/

const DEFAULT_INDENT_WIDTH: usize = 4;

pub struct RenderOptions {
    indent_width: usize,
    use_color: UseColor,
}

#[allow(unused)]
pub enum UseColor {
    Always,
    Never,
    Auto,
}

impl RenderOptions {
    pub fn default() -> RenderOptions {
        RenderOptions {
            indent_width: DEFAULT_INDENT_WIDTH,
            use_color: UseColor::Auto,
        }
    }
}

impl UseColor {
    pub fn enabled(&self) -> bool {
        use io::IsTerminal;

        match self {
            UseColor::Always => true,
            UseColor::Never => false,
            UseColor::Auto => std::io::stderr().is_terminal(),
        }
    }
}

/*********
 * Color *
 *********/

const RESET_ANSI_CODE: &str = "0";
const BOLD_ANSI_CODE: &str = "1";

#[allow(unused)]
pub enum Color {
    White,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
}

impl Color {
    fn ansi_code(&self) -> &'static str {
        use Color::*;

        match self {
            White => "37",
            Red => "31",
            Green => "32",
            Yellow => "33",
            Blue => "34",
            Magenta => "35",
            Cyan => "36",
        }
    }
}

/************
 * Renderer *
 ************/

pub struct Renderer {
    indent_width: usize,
    color_enabled: bool,
    indent: usize,
}

impl Renderer {
    pub fn new(options: RenderOptions) -> Renderer {
        Renderer {
            indent_width: options.indent_width,
            color_enabled: options.use_color.enabled(),
            indent: 0,
        }
    }

    pub fn indent(&mut self) {
        self.indent += self.indent_width;
    }

    pub fn dedent(&mut self) {
        self.indent -= self.indent_width;
    }

    pub fn spaces(&mut self, num_spaces: usize) {
        eprint!("{:spaces$}", "", spaces = num_spaces);
    }

    pub fn raw_indent(&mut self) {
        self.spaces(self.indent);
    }

    pub fn raw_newline(&mut self) {
        eprintln!();
    }

    pub fn raw_str(&mut self, s: &str) {
        eprint!("{}", s);
    }

    pub fn newline(&mut self) {
        self.raw_newline();
        self.raw_indent();
    }

    pub fn colored(&mut self, s: &str, color: Color) {
        if self.color_enabled {
            self.ansi_code(color.ansi_code());
            eprint!("{}", s);
            self.ansi_code(RESET_ANSI_CODE);
        } else {
            eprint!("{}", s);
        }
    }

    pub fn boldly_colored(&mut self, s: &str, color: Color) {
        if self.color_enabled {
            self.ansi_code(BOLD_ANSI_CODE);
            self.ansi_code(color.ansi_code());
            eprint!("{}", s);
            self.ansi_code(RESET_ANSI_CODE);
        } else {
            eprint!("{}", s);
        }
    }

    #[inline]
    fn ansi_code(&mut self, code: &'static str) {
        eprint!("\x1b[{}m", code);
    }
}

/* Not really needed now. Maybe someday?
impl fmt::Write for Renderer {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for (i, line) in s.split('\n').enumerate() {
            if i != 0 {
                eprint!("\n{:indent$}", "", indent = self.indent)
            }
            eprint!("{}", line);
        }
        Ok(())
    }
}
*/

/****************
 * Render Trait *
 ****************/

pub trait Render {
    fn render(&self, r: &mut Renderer);
}

impl Render for str {
    fn render(&self, r: &mut Renderer) {
        for (i, line) in self.split('\n').enumerate() {
            if i != 0 {
                r.newline();
            }
            r.raw_str(line);
        }
    }
}
