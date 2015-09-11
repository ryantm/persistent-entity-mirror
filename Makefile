NIXSHELL_COMMAND=nix-shell \
    --pure \
    -j 4 \
    --show-trace \
    -I nixpkgs=/home/ryantm/p/nixpkgs \
    --option extra-binary-caches https://hydra.nixos.org \
    --option extra-binary-caches https://cache.nixos.org \
    --option extra-binary-caches http://hydra.cryp.to \
    --fallback \
    -p haskellPackages.stack haskellPackages.ghc

shell :
	$(NIXSHELL_COMMAND)
