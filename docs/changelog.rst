Changelog
=========

0.2.6
------
- Updates Ocaml to 4.13.1 to support building on arm64

0.2.5
------
- Add support for parsing Swift concurrency syntax

0.2.4
-----

- Fix dependencies and bump version to publish to OPAM and Homebrew

0.2.3
-----

- Support for child and descendant operators (PR#4)
- Parse empty strings and escaped double quotes in rules (PR#7)
- Add support for dumping ASTs as JSON

0.2.2
-----

- Fix rule file capturing during parsing (used for relative paths in rule tests)
- Fix tests being execute per matcher instead of per rule
- Add support for Swift 3.2 keypath syntax
- Fix crash when there's a config file deeper than other files to be analysed

0.2.1
-----

- Add support for sibling patterns through conditional variables
- Add support for "exclude" globs in the configuration file
- Fix bug where the same configuration file was being considered multiple files
- Fix rule tests targeting Python

0.2.0
-----

- Add support for importing rules from other files
- Add support for testing rules
- New command line interface
- Minor fixes in the Swift parser
- New testing infrastructure

0.1.6
-----

- Fix stats - number of violations should be counted after filtering
- Add a simple progress indicator when running from the terminal

0.1.5
-----

- Proper grammar for disabling rules via comment directives
- (Temporary) Support for SwiftLint directives
- Several performance improvements
- Add runtime flag for dumping GC statistics

0.1.4
-----

- Fix parsing of Swift functions that have a newline after the arrow in the result type
- Improve wstring heuristic (`_0` shouldn't be treated as `_`)
- Fix duplicate comments showing up in the Swift ast
- Initial support for disabling rules with comments

0.1.3
-----

- Fix precedence of logical operators
- Exit 1 when any rules fail
- Fix rule not being found when a path to a file was passed to `--rules`
- Add --strict flag to treat warnings as errors
- Parse errors don't exit 1 anymore, unless explicitly requested via --strict-parse
- Add --filelist option to the CLI

0.1.2
-----

- Better regular expressions
- Support for comments in rules
- Support for linting comments in Swift
- Rule names are now identifiers instead of strings
- Python node names and properties are now capitalised
- Separate reporting from evaluation

0.1.1
-----

- Initial docs release
