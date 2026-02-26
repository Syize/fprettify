# fprettify2

**This is a self-used fork of [fprettify](https://github.com/fortran-lang/fprettify). I split the huge `__init__.py` into multiple files, and made some changes.**

---

## What's changed?

- Split huge `__init__.py` into multiple files.
- Change build system to meson.
- Always add indentation to all `if` and `do` block.
- Always change keywords to lowercase.

## Install

```bash
uv add "fprettify @ git+https://github.com/Syize/fprettify"
```
