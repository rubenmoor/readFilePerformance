let
  easy-hls-src = pkgs.fetchFromGitHub {
    owner = "jkachmar";
    repo = "easy-hls-nix";
    rev = "7c123399ef8a67dc0e505d9cf7f2c7f64f1cd847";
    sha256 = "0402ih4jla62l59g80f21fmgklj7rv0hmn82347qzms18lffbjpx";
  };
  easy-hls = pkgs.callPackage easy-hls-src { ghcVersions = [ "9.0.1" ]; };

  # cabal2nix has to come from a different package set
  # to avoid a cyclic dependency on bytestring
  callCabal2nix = pkgs.haskell.packages.ghc884.callCabal2nix;
  callHackage = pkgs.haskell.packages.ghc884.callHackage;
  compiler = "ghc901";
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = self: super: {
              # overrides
              #bytestring = callHackage "bytestring" "0.11.2.0" {};
            };
          };
        };
      };
    };
    # allow Broken = true;
  };
  # pkgs = import <nixpkgs> { inherit config; };
  pkgs = import <nixos-unstable> { inherit config; };
  drv = pkgs.haskell.packages."${compiler}".callCabal2nix "readFilePerformance" ./. { };
in
  {
    env =
      # don't know why, but the haskell-language doesn't seem to
      # be a build tool, but a native build input
      #
      # with pkgs.haskell.lib;
      # addBuildTools drv (
      #   with pkgs.haskellPackages;
      #   [ haskell-language-server ]
      # );
      with pkgs.haskellPackages;
      drv.env.overrideAttrs ( oldAttrs: rec {
        nativeBuildInputs =
          oldAttrs.nativeBuildInputs ++ [
            cabal-install
            cabal-plan
            easy-hls
          ];
      });
    exec = drv;
  }
