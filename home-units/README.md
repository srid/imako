# What is "home-units" directory?

https://well-typed.com/blog/2023/03/cabal-multi-unit/

https://x.com/sridca/status/1901283945779544362

Allows ghcid to auto reload when these libraries change, all the while the Nix build will use the reproducible version from flake.lock.

How to use? Just `git clone` it here, and update `mod.just` accordingly. Be sure to keep the `flake.lock` version in sync, and (for reproducibility) try not to rely on the nixpkgs index.
