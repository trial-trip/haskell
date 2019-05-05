# stepik

stack build --file-watch --exec stepil-exe

stack build --ghc-options O0 --copy-bins && bench "~/.local/bin/stepik-exe"
stack build --ghc-options O0 --copy-bins && time "~/.local/bin/stepik-exe"

ghci: :set +s