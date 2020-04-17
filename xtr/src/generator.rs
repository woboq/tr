/* Copyright (C) 2018 Olivier Goffart <ogoffart@woboq.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

use super::{AddLocation, Message, OutputDetails};
use chrono::prelude::*;
use std::io::prelude::*;
use std::path::Path;

pub fn generate<'a, P: AsRef<Path>>(
    output: P,
    output_details: OutputDetails,
    messages: impl IntoIterator<Item = &'a Message>,
) -> ::std::io::Result<()> {
    let mut output = std::fs::File::create(output)?;

    if !output_details.omit_header {
        let package = output_details
            .package_name
            .as_ref()
            .map(|x| x.as_ref())
            .unwrap_or("PACKAGE");
        write!(
            output,
            r#"# SOME DESCRIPTIVE TITLE.
# Copyright (C) YEAR {copyright}
# This file is distributed under the same license as the {package} package.
# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.
#
#, fuzzy
msgid ""
msgstr ""
"Project-Id-Version: {package} {version}\n"
"Report-Msgid-Bugs-To: {address}\n"
"POT-Creation-Date: {date}\n"
"PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\n"
"Last-Translator: FULL NAME <EMAIL@ADDRESS>\n"
"Language-Team: LANGUAGE <LL@li.org>\n"
"Language: \n"
"MIME-Version: 1.0\n"
"Content-Type: text/plain; charset={charset}\n"
"Content-Transfer-Encoding: 8bit\n"
"#,
            package = package,
            version = output_details
                .package_version
                .as_ref()
                .map(|x| x.as_ref())
                .unwrap_or("VERSION"),
            copyright = output_details
                .copyright_holder
                .unwrap_or_else(|| format!("THE {}'S COPYRIGHT HOLDER", package)),
            date = Utc::now().format("%Y-%m-%d %H:%M%z").to_string(),
            address = output_details.bugs_address.unwrap_or_default(),
            charset = output_details.charset,
        )?;
    }

    for m in messages {
        writeln!(output)?;
        if let Some(ref c) = m.comments {
            for c in c.split('\n') {
                writeln!(output, "#. {}", c)?;
            }
        }
        if !m.locations.is_empty() && (output_details.add_location != AddLocation::Never) {
            write!(output, "#:")?;
            for l in &m.locations {
                match output_details.add_location {
                    AddLocation::Full => {
                        write!(output, " {}:{}", l.file.to_string_lossy(), l.line)?;
                    }
                    AddLocation::File => {
                        write!(output, " {}", l.file.to_string_lossy())?;
                    }
                    _ => panic!(format!(
                        "unsupported add-location option {0:?}",
                        output_details.add_location
                    )),
                }
            }
            writeln!(output)?;
        }
        if let Some(ref c) = m.msgctxt {
            writeln!(output, "msgctxt {}", escape(&c))?;
        }

        writeln!(output, "msgid {}", escape(&m.msgid))?;

        if let Some(ref c) = m.plural {
            writeln!(output, "msgid_plural {}", escape(&c))?;
        }

        //writeln!(output, "msgstr {}", escape(&m.msgid))?;
        writeln!(output, "msgstr \"\"")?;
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
