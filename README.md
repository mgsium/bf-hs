### brainf**k interpreter

---

Brainf**ck is an esoteric programming language written by Urban Müller in 1993.

This implementation - written using the parsec parser-combinator library - is based on the specification at https://github.com/brain-lang/brainfuck/blob/master/brainfuck.md.

#### Quick Start

After installing the [dependencies](#dependencies), the following command builds and runs the program.

```sh
$ stack run // Builds and runs the interpreter
```

The program exposes a read-evaluate-print-loop to run brainf**k programs:

```
$ stack run 
BRAINFUCK> ++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.
Hello World!

BRAINFUCK> ...
```

---

#### Dependencies

Install `stack >= 2.7` using the instructions at https://docs.haskellstack.org/en/stable/ or https://www.haskell.org/downloads/.

Stack can be used to install `ghc >= 7.10.2`.
