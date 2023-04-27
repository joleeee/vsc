use std::io::Write;

pub enum Helper {
    Label(String),
    Jmp(String),
    Emit(String),
}

impl Helper {
    pub fn compile<W: Write>(&self, out: &mut W) {
        let s = match self {
            Self::Label(name) => format!("    {}:\n", name),
            Self::Jmp(location) => format!("    jmp {}\n", location),
            Self::Emit(code) => format!("    {}\n", code),
        };

        out.write_all(s.as_bytes()).unwrap();
    }
}

#[macro_export]
macro_rules! call {
    ($($arg:tt)*) => {
        Helper::Emit("call ".to_string() + &format!($($arg)*))
    };
}

#[macro_export]
macro_rules! leaq {
    ($($arg:tt)*) => {
        Helper::Emit("leaq ".to_string() + &format!($($arg)*))
    };
}

#[macro_export]
macro_rules! xorq {
    ($($arg:tt)*) => {
        Helper::Emit("xorg ".to_string() + &format!($($arg)*))
    };
}

#[macro_export]
macro_rules! movq {
    ($($arg:tt)*) => {
        Helper::Emit("movq ".to_string() + &format!($($arg)*))
    };
}

#[macro_export]
macro_rules! emit {
    ($($arg:tt)*) => {
        Helper::Emit(format!($($arg)*))
    };
}

#[macro_export]
macro_rules! jmp {
    ($($arg:tt)*) => {
        Helper::Jmp(format!($($arg)*))
    };
}

#[macro_export]
macro_rules! label {
    ($($arg:tt)*) => {
        Helper::Label(format!($($arg)*))
    };
}
