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
