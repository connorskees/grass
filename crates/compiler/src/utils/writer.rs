use std::io;
use std::io::Write;

pub(crate) trait StrWrite {
    fn write_ch(&mut self, ch: u8) -> io::Result<()>;
}

impl<W: Write> StrWrite for W {
    fn write_ch(&mut self, ch: u8) -> io::Result<()> {
        self.write_all(&[ch])
    }
}
