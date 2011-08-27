# Erlang JSON-LD (ejsonld)

ejsonld is a JSON-LD framework written in erlang to implement the standard interface defined by the JSON-LD [current draft](http://json-ld.org/spec/latest) .

It allows you to do the following operations on a JSON-LD document:
- expand
- compact
- normalize
- frame

Every operation requires a JSON-LD document expressed in [EEP0018 format](http://www.erlang.org/eeps/eep-0018.html).

The test bed is actually using [JSX](https://github.com/talentdeficit/jsx) to convert a binary document into a EEP0018 term.
