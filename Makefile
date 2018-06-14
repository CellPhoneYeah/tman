SHELL_DIR := $(shell pwd)
EBIN_DIR := $(SHELL_DIR)/ebin
SRC_DIR := $(SHELL_DIR)/src
INCLUDE_DIR := $(SHELL_DIR)/include

ERLC := erlc -o $(EBIN_DIR) -I $(INCLUDE_DIR) -pa $(EBIN_DIR)

define trav
	$(wildcard $(1)/*.erl)
endef

all:dir compile cp

ALLFILES := $(call trav, $(SRC_DIR))

NEEDFILES := $(patsubst %.erl, %.beam, $(ALLFILES))

dir:
	mkdir -p $(EBIN_DIR)

compile:$(NEEDFILES)

cp:
	cp -a $(SRC_DIR)/tman.app.src $(EBIN_DIR)/tman.app

%.beam:%.erl
	${ERLC} $<
