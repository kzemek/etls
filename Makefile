##
## Rebar targets
##

.PHONY: all
all: deps compile

.PHONY: deps
deps:
	./rebar get-deps

.PHONY: compile
compile:
	./rebar compile

.PHONY: clean
clean:
	./rebar clean

.PHONY: test
test: compile
	./rebar eunit

.PHONY: doc
doc: compile
	./rebar doc

##
## Dialyzer targets local
##

PLT ?= .dialyzer.plt

# Builds dialyzer's Persistent Lookup Table file.
.PHONY: plt
plt:
	dialyzer --check_plt --plt ${PLT}; \
	if [ $$? != 0 ]; then \
		dialyzer --build_plt --output_plt ${PLT} --apps kernel stdlib \
			compiler crypto erts; \
	fi; exit 0

# Dialyzes the project.
.PHONY: dialyzer
dialyzer: plt
	dialyzer ./ebin --plt ${PLT} -Werror_handling -Wrace_conditions --fullpath
