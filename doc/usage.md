---
title: Usage
---

You can always run `eden -h` to find out about usage.

### Init
`eden init` initialises an EDEN repository.
This ensures that EDEN knows where to look for files.

### Generate
EDEN can generate most of the required directory structure for you.
Try out `eden generate -h` to see what can be generated.

### Build
EDEN can build executables for your solutions.
`eden build <target>` builds the executable.
It will show you an error if anything goes wrong.
Try running `eden build -h` to see what else is possible.

### Test
EDEN can run tests on your programs.
`eden test <target>` tests the entire target.
`eden test -h` shows you all the posibilities.
In particular, `eden test solution <problem> <language>` tests a specific solution.

### Run
EDEN can also run your programs.
`eden run <target>` runs the executable that was built by `eden build <target>`.
Run `eden run -h` to see the other possibilities.

### Publish
`eden publish problem <problem>` compiles the `explanation.tex` file for a single problem into a pdf.
`eden publish part <part>` compiles a single `.tex` file in the `writeup` directory.
`eden publish library <file>` compiles a single `.tex` file in the `lib` directory.
`eden publish all` compiles all the writeups into a single pdf for you.
