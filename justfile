default:
    @just --list

CABAL_REPL_ARGS:="--enable-multi-repl imako"

# Run hoogle
docs:
    echo http://127.0.0.1:8888
    hoogle serve -p 8888 --local

# Run cabal repl
repl *ARGS:
    cabal repl {{ CABAL_REPL_ARGS }} {{ ARGS }}

# Run ghcid -- auto-recompile and run `main` function
run:
    ghcid -T Main.main -c 'cabal repl {{ CABAL_REPL_ARGS }}'
