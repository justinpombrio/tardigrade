//! A stack that supports lexically nested functions.
//!
//! I thought of this independently, but it's an old idea. See:
//! https://en.wikipedia.org/wiki/Call_stack#Lexically_nested_routines
//!
//! Stack frames look like this:
//!
//! ```text
//!     Local 2
//!     Local 1
//!   > Local 0
//!     *Previous frame link
//!     *Parent frame link
//!     Function arg 2
//!     Function arg 1
//!     Function arg 0
//! ```
//!
//! "Previous frame link" is a pointer to the frame just before this one, and "Parent frame link"
//! is a pointer to the topmost frame that lexically (in the source) encloses this frame.

use std::fmt::{self, Debug, Display};

const PREVIOUS_FRAME_OFFSET: isize = -1;
const PARENT_FRAME_OFFSET: isize = -2;

#[derive(Debug)]
pub struct Stack<T: Debug + Display> {
    entries: Vec<StackEntry<T>>,
    /// The base (excluding the Link) of the topmost stack frame.
    frame: usize,
}

#[derive(Debug)]
enum StackEntry<T: Debug + Display> {
    /// Reference to another part of the stack
    Link(usize),
    Item(T),
}

impl<T: Debug + Display> Stack<T> {
    pub fn new() -> Stack<T> {
        Stack {
            entries: Vec::new(),
            frame: 0,
        }
    }

    pub fn start_block(&mut self) -> usize {
        self.entries.len()
    }

    pub fn end_block(&mut self, start_of_block: usize) {
        self.entries.truncate(start_of_block);
    }

    pub fn start_frame(&mut self, depth: usize) {
        let prev_frame = self.frame;
        let mut enclosing_frame = self.frame;
        for _ in 0..depth {
            enclosing_frame = self.enclosing_frame(enclosing_frame);
        }
        self.entries.push(StackEntry::Link(enclosing_frame));
        self.entries.push(StackEntry::Link(prev_frame));
        self.frame = self.entries.len();
    }

    pub fn end_frame(&mut self, num_args: usize) {
        let prev_frame = self.previous_frame(self.frame);
        self.entries.truncate(self.frame - num_args - 2);
        self.frame = prev_frame;
    }

    pub fn push(&mut self, item: T) {
        self.entries.push(StackEntry::Item(item));
    }

    /// Find the referenced item in the stack.
    #[track_caller]
    pub fn lookup(&mut self, depth: usize, offset: isize) -> &T {
        let mut frame = self.frame;
        for _ in 0..depth {
            frame = self.enclosing_frame(frame);
        }
        self.get_item(frame, offset)
    }

    #[track_caller]
    pub fn lookup_mut(&mut self, depth: usize, offset: isize) -> &mut T {
        let mut frame = self.frame;
        for _ in 0..depth {
            frame = self.enclosing_frame(frame);
        }
        self.get_item_mut(frame, offset)
    }

    pub fn verify_empty(&self) {
        if !self.entries.is_empty() {
            eprintln!("{}", self);
            panic!("Stack: unmatched start_frame");
        }
    }

    #[track_caller]
    fn enclosing_frame(&self, index: usize) -> usize {
        let index = (index as isize + PARENT_FRAME_OFFSET) as usize;
        match &self.entries[index] {
            StackEntry::Link(link) => *link,
            StackEntry::Item(_) => panic!("Stack: expected Link, found Item"),
        }
    }

    #[track_caller]
    fn previous_frame(&self, index: usize) -> usize {
        let index = (index as isize + PREVIOUS_FRAME_OFFSET) as usize;
        match &self.entries[index] {
            StackEntry::Link(link) => *link,
            StackEntry::Item(_) => panic!("Stack: expected Link, found Item"),
        }
    }

    #[track_caller]
    fn get_item(&self, index: usize, offset: isize) -> &T {
        let index = (index as isize + offset) as usize;
        match &self.entries[index] {
            StackEntry::Item(value) => value,
            StackEntry::Link(_) => panic!("Stack: expected Item, found Link"),
        }
    }

    #[track_caller]
    fn get_item_mut(&mut self, index: usize, offset: isize) -> &mut T {
        let index = (index as isize + offset) as usize;
        match &mut self.entries[index] {
            StackEntry::Item(value) => value,
            StackEntry::Link(_) => panic!("Stack: expected Item, found Link"),
        }
    }
}

impl<T: Debug + Display> Display for Stack<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use StackEntry::*;

        writeln!(f, "STACK")?;
        for (i, entry) in self.entries.iter().enumerate().rev() {
            if i == self.frame {
                write!(f, "*")?;
            }
            match entry {
                Link(link) => writeln!(f, "  {:#x}", link)?,
                Item(item) => writeln!(f, "  {}", item)?,
            }
        }
        Ok(())
    }
}
