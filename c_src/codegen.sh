#!/bin/bash
# Rebar explicitly escapes Erlang includes to _not_ break Windows.
# See commit
# https://github.com/rebar/rebar/commit/4b49dc0727c59ebb4f07d048283021ab2f0fc1e0
#
# Unfortunately this doesn't play well with (at leaast) Clang 6.0.
# So we simply remove the quotes from $ERL_CFLAGS :)
export ERL_CFLAGS=${ERL_CFLAGS//\"}
$CC $ERL_CFLAGS -P -D BITNESS=12 -E priv/ecirca.c > c_src/ecirca_small.c \
&&
$CC $ERL_CFLAGS -P -D BITNESS=28 -E priv/ecirca.c > c_src/ecirca_medium.c \
&&
$CC $ERL_CFLAGS -P -D BITNESS=60 -E priv/ecirca.c > c_src/ecirca_large.c
