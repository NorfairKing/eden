BIN_NAME = solution.bin
SRC_NAME = Solution.java
NAME     = Solution

all: $(BIN_NAME)

STUB_NAME = stub.sh
define STUB
#!/bin/sh
MYSELF=`which "$$0" 2>/dev/null`
[ $$? -gt 0 -a -f "$$0" ] && MYSELF="./$$0"
java=java
if test -n "$$JAVA_HOME"; then
    java="$$JAVA_HOME/bin/java"
fi
exec "$$java" $$java_args -jar $$MYSELF "$$@"
exit 1 
endef
export STUB

$(BIN_NAME): $(SRC_NAME)
	javac $(SRC_NAME)
	echo Main-Class: $(NAME) > MANIFEST.MF
	jar -cvmf MANIFEST.MF $(NAME).jar $(NAME).class 
	echo "$$STUB" > $(STUB_NAME)
	cat $(STUB_NAME) $(NAME).jar > $(BIN_NAME)
	chmod 775 $(BIN_NAME)

test:
	@echo placeholder

love:
	@echo not war
