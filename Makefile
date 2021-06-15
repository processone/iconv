REBAR ?= rebar

all: src

src:
	$(REBAR) get-deps
	$(REBAR) compile

clean:
	$(REBAR) clean

.PHONY: clean src
