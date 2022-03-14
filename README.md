This is a frontend for my lang. It uses [chumsky](https://github.com/zesterer/chumsky) for parsing and it also implements some very simple type inference, mainly following [this](https://eli.thegreenplace.net/2018/type-inference/) approach.

This crate is used by [lang-lsp](https://github.com/Sagrel/lang-lsp) (a simple lsp for this lang) and [lang-llvm](https://github.com/Sagrel/lang-llvm) (a simple llvm backend for the lang)