{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem (system:
    let
      overlay = self: super:
        let
          inherit (super) lib;
          ghcOverride = input: ovl: input.override (old: {
            overrides = lib.composeExtensions (old.overrides or (_: _: { })) ovl;
          });
          overrideVersion = overlay: version: {
            ${version} = ghcOverride super.haskell.packages.${version} overlay;
          };
          overrideVersions = overlay: versions: lib.foldl' (x: y: x // y) { }
            (builtins.map (overrideVersion overlay) versions);
          haskellOverlay = hself: hsuper: {
            kemel = hsuper.callCabal2nix "kemel"
              (super.nix-gitignore.gitignoreSource [ ] ./.)
              { };
          };
        in
        {
          haskell = super.haskell // {
            packages = super.haskell.packages // overrideVersions haskellOverlay [ "ghc901" "ghc921" ];
          };
        };

      pkgs = import nixpkgs {
        inherit system;
        overlays = [ overlay ];
      };

      hpkgs = pkgs.haskell.packages.ghc901;
    in
    {
      defaultOverlay = self.overlays.${system}.kemel-overlay;

      defaultPackage = self.packages.${system}.kemel;

      overlays.kemel-overlay = overlay;

      packages.kemel = hpkgs.kemel;

      devShell = hpkgs.shellFor {
        packages = ps: with ps; [ kemel ];
        buildInputs = (with pkgs; [
          haskell-language-server
        ]) ++ (with hpkgs; [
          cabal-install
        ]);
      };
    });
}
