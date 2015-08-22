---
title: Contracts
---

EDEN makes some assumptions with respect to how you structure your solutions, how your makefiles work and how your programs work.
This pages explains how you can make sure that EDEN knows what to do with your solutions.

### Environment variables
EDEN sets the environment variable `EDEN_ROOT` to the absolute path of your EDEN repository during its execution.
You can use it to keep your makefiles tidy.

### Directory structure

Here is an example of how an EDEN repository would look:

```
$EDEN_ROOT
 |- 001
   |- c
   | |- solution.c
   | |- test.cpp
   |- haskell
     |- solution.hs
     |- test.hs
 |- 002
 |- 003
 |- ...
 |- build
   |- c
     |- makefile
     |- makefile2
   |- haskell
     |- makefile
 |- lib
   |- c
     |- makefile
     |- abc.c
     |- abc.h
   |- haskell
     |- makefile
     |- Primes.hs
 |- test
   |- c
     |- makefile
     |- main.cpp
     |- abc_test.cpp
   |- haskell
     |- makefile
     |- AbcTest.hs
 |- writeup
   |- main.tex
```


#### Problem directories

Problems are numbered, luckily. Problem directories' are the $3$-padded string versions of the problems they represent.
`001` for problem 1, `016` for problem 16 and `512` for problem 512.
This ensures that alphabetically sorting the directories makes them appear in the right order.

In any problem directory, there can be multiple language directories.
This allows you to solve a single problem with multiple languages, if you wish.

You then put the source code of your solution in `<problem>/<language>/`.
The name of your source code's file does not matter, but being consistent is important for building and testing your solutions.
I suggest you name your file `solution.c` (for solutions in c).
You can also have multiple solutions in the same directory.

For writeups, the problem directory should contain `<problem>/explanation.tex`.
This will ensure that your explanation of your solution is later included in your writeups.

`eden generate solution <problem> <language>` will generate this structure for you.

#### Program I/O

A solution's input can be provided in the problem directory with an `input.txt` file.
The expected output can be provided in the problem directory with an `output.txt` file.
`eden test solution <problem> <language>` will test whether the solution's output equals the contents of the `output.txt` file, as well as checking that `make test` terminates successfully.

#### The `build` directory

The `build` directory contains a directory for every language: `build/<language>/`.

In these directories, you put the files required for building your solutions.
You will need to put a makefile in this directory: `build/<language>/makefile`.

The default make rule should build a `solution.bin` executable.
This holds for any language and ensures that `eden run` knows what to do.

The `test` make rule should build the unit tests and test the solution.
`make test` should exit with a non-zero exist code if any test fails and with an exit code of zero otherwise.

`eden generate build <language>` will generate this structure for you.

#### The `lib` directory

The `lib` directory contains a directory for every language: `lib/<language>/`.

In these directories, you put the source code for any shared libraries you may want to use in your solutions.
Project euler often handles the same topic in different problems so it comes in handy to be able to share code among your solutions.

Each `lib/<language>` directory should contain a makefile as well.
The default make rule should build any libraries you may want to use.
This holds for any language and ensures that `eden build` knows what to do.

`eden generate library <language>` will generate this structure for you.

#### The `test` directory

The `test` directory contains, you guessed it, a directory for every language: `test/<language>/`.

In these directories, you put the source code for tests of the shared libraries you've built.

Each `test/<language>` directory should contain a makefile as well.
The default make rule should build the test suite(s) and the `test` make rule should execute the test suite.
`make test` should exit with a non-zero exist code if any test fails and with an exit code of zero otherwise.
This holds for any language and ensures that `eden test` knows what to do.

`eden generate tests <language>` will generate this structure for you.

#### The `writeup` directory

Writing about your solutions ensures that you understand what you did later on.
You could write explanations in the comment sections of your source code, but this being Project Euler, you will probably want to use LaTeX.
EDEN has built-in support for this!

EDEN assumes that you use the `subfiles` package to modularise your LaTeX code.
EDEN uses `latexmk` to compile the writeup source files into a nice pdf file.

In `writeup` directory should contain `main.tex` and that should use the subfiles package.
When you run `eden publish all`, EDEN generates a file `explanations.tex` in the `writeup` directory.
If you put `\input{explanations.tex}` in your `main.tex`

`eden generate writeups` will generate this structure for you.
