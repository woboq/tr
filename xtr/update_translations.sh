#!/bin/sh

# Run the script, translate, run the script again

RUSTFLAGS='--cfg procmacro2_semver_exempt' cargo run src/main.rs --package-name=xtr -d xtr -o xtr.pot

for po in lang/*/LC_MESSAGES
    do msgmerge $po/xtr.po xtr.pot -o $po/xtr.po
    msgfmt $po/xtr.po -o $po/xtr.mo
done
