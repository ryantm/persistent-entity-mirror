NIX_PATH=nixpkgs=https://nixos.org/channels/nixpkgs-unstable/nixexprs.tar.xz
NIXSHELL_COMMAND=nix-shell \
    --pure \
    -j 4 \
    -I $(NIX_PATH) \
    --show-trace \
    --option extra-binary-caches https://hydra.nixos.org \
    --option extra-binary-caches https://cache.nixos.org \
    --fallback

shell :
	$(NIXSHELL_COMMAND)
