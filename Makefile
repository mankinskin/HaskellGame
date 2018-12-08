#!/bin/bash

BUILD=build
CC=ghc -dynamic -i.:src -outputdir $(BUILD)
SRC=$(shell ls src | grep -e '\.hs')
OBJ=$(SRC:hs=o)

	
main: $(BUILD)
	$(CC) -o Main src/Main.hs

$(BUILD):
	@echo "SRC=$(SRC)"
	@echo "OBJ=$(OBJ)"
	@echo "BUILD=$(BUILD)"
	mkdir $(BUILD)

run: main
	./Main

clean:
	rm -rf build
	rm -f Main

.PHONY: run clean
