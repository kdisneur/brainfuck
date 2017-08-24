C=ghc
I=ghci
OUTPUT_DIR=bin
SOURCE_DIR=src

all: compile

clean:
	[ -f bin/bf ] && rm bin/bf; \
	ls -F | grep '*' | sed -E 's/.?$$//' | xargs rm

compile:
	$(C) -o ${OUTPUT_DIR}/bf -i$(SOURCE_DIR) $(SOURCE_DIR)/Main.hs

console:
	$(I) -i$(SOURCE_DIR) $(SOURCE_DIR)/Main.hs
