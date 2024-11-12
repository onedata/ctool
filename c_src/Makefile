CXX ?= g++

ERTS_INCLUDE_DIR ?= $(shell erl -noshell -eval "io:format(\"~ts/erts-~ts/include/\", [code:root_dir(), erlang:system_info(version)])." -s erlang halt)

LDFLAGS += -shared
CXXFLAGS += -fPIC -std=c++17 -O3 -Wall -isystem $(ERTS_INCLUDE_DIR)

all: ../priv/hex_utils.so

../priv/hex_utils.so: hex/hex_utils.o
	$(CXX) $? -o $@ $(LDFLAGS)

%.o: %.cc
	$(CXX) $(CXXFLAGS) $(CFLAGS) -c $< -o $@
