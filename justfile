default:
    @just --list

import 'home-units/mod.just'

CABAL_REPL_ARGS:="--enable-multi-repl $MULTI_REPL_LIBRARIES"
NOTEBOOK:="./example"

# Run hoogle
docs:
    echo http://127.0.0.1:8888
    hoogle serve -p 8888 --local

# Run cabal repl
repl *ARGS:
    cabal repl {{ CABAL_REPL_ARGS }} {{ ARGS }}

# Run ghcid -- auto-recompile and run `main` function
run:
    ghcid -T Main.main -c 'cabal repl {{ CABAL_REPL_ARGS }}' --setup ":set args {{ NOTEBOOK }}"

resetdb:
    rm -rf ./packages/imako/state
