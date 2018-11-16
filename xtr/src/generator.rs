use super::Message;
use std::io::prelude::*;
use std::path::Path;

pub fn generate<P: AsRef<Path>>(output: P, messages: Vec<Message>) -> ::std::io::Result<()> {
    let mut output = std::fs::File::create(output)?;

    for m in messages {
        writeln!(output)?;
        if let Some(c) = m.comments {
            for c in c.split('\n') {
                writeln!(output, "#. {}", c)?;
            }
        }
        if !m.locations.is_empty() {
            write!(output, "#:")?;
            for l in m.locations {
                write!(output, " {}:{}", l.file.to_string_lossy(), l.line)?;
            }
            writeln!(output)?;
        }
        if let Some(c) = m.msgctxt {
            writeln!(output, "msgctxt {}", escape(&c))?;
        }

        writeln!(output, "msgid {}", escape(&m.msgid))?;

        if let Some(c) = m.plural {
            writeln!(output, "msgid_plural {}", escape(&c))?;
        }

        writeln!(output, "msgstr {}", escape(&m.msgid))?;
    }
    Ok(())
}

fn escape(s: &str) -> String {
    format!(
        "\"{}\"",
        s.replace('\\', "\\\\")
            .replace('\"', "\\\"")
            .replace('\n', "\\n\"\n\"")
    )
}
