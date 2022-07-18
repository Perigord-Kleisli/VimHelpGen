{
  description = "A Markdown to Vim Help Text converter";
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            # This overlay adds our project to pkgs
            vimdowner =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc902";
                shell.tools = {
                  cabal = { };
                  hlint = { };
                  haskell-language-server = { };
                };

                shell.buildInputs = with pkgs; [
                  nixpkgs-fmt
                ];


              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.vimdowner.flake {};
      in
      flake // {
      });
}
