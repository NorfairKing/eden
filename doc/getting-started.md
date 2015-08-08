---
title: Getting started
---

First things first.
You need a directory to keep your solutions in.
(You'll probably want to make a git repository out of this.)

```
$ mkdir pe
$ cd pe
```

Next, you need to tell `eden` that this is your repository of solutions.

```
pe $ eden init
```

If you look at the contents of your repository you will see that there is a new directory `.eden`.

```
pe $ ls -a
.  ..  .eden
```

EDEN has a specific command to make it easy to get started.
Let's say you want to write solutions in c:

```
pe $ eden generate getting-started
What language would you like to use to solve the problems?
You can use more than one language later, but what will you start with?
Please use only lower case letters and no spaces.
Language > c
```

EDEN just generated most of the directory structure you need and, in the case of c, a starter makefile.

```
pe $ tree
.
├── 001
│   └── c
├── build
│   └── c
│       └── makefile
├── lib
│   └── c
└── test
    └── c
```

Now we can start solving the first problem:

```
pe $ cat > 001/c/solution.c
#include <stdio.h>
int main () {
    printf("No solutions will be spoiled.\n");
}
```

EDEN makes it very easy to compile your solution.
From any directory in your EDEN repository, you can build any solution:

```
pe $ eden build 1 c
```

Running a solution is also easy:

```
pe $ eden run 1 c
```


