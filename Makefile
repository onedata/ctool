LIB_DIR = _build/default/lib
GIT_URL := $(shell git config --get remote.origin.url | sed -e 's/\(\/[^/]*\)$$//g')
GIT_URL := $(shell if [ "${GIT_URL}" = "file:/" ]; then echo 'ssh://git@git.onedata.org:7999/vfs'; else echo ${GIT_URL}; fi)
ONEDATA_GIT_URL := $(shell if [ "${ONEDATA_GIT_URL}" = "" ]; then echo ${GIT_URL}; else echo ${ONEDATA_GIT_URL}; fi)
export ONEDATA_GIT_URL

.PHONY: upgrade

all: compile

upgrade:
	@./rebar3 upgrade

compile:
	@./rebar3 compile

clean:
	@./rebar3 clean

distclean:
	@./rebar3 clean --all

##
## Submodules
##

submodules:
	git submodule sync --recursive ${submodule}
	git submodule update --init --recursive ${submodule}


##
## Dialyzer targets local
##

# Dialyzes the project.
dialyzer:
	@./rebar3 dialyzer

##
## Testing
##

eunit:
	./rebar3 eunit skip_deps=true --suite=${SUITES}
## Rename all tests in order to remove duplicated names (add _(++i) suffix to each test)
	@for tout in `find test -name "TEST-*.xml"`; do awk '/testcase/{gsub("_[0-9]+\"", "_" ++i "\"")}1' $$tout > $$tout.tmp; mv $$tout.tmp $$tout; done
