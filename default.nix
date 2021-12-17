let
  easy-hls-src = pkgs.fetchFromGitHub {
    owner = "jkachmar";
    repo = "easy-hls-nix";
    rev = "7c123399ef8a67dc0e505d9cf7f2c7f64f1cd847";
    sha256 = "0402ih4jla62l59g80f21fmgklj7rv0hmn82347qzms18lffbjpx";
  };
  easy-hls = pkgs.callPackage easy-hls-src { ghcVersions = [ "9.0.1" ]; };

  bytestring-src = pkgs.fetchFromGitHub {
    owner = "haskell";
    repo = "bytestring";
    rev = "b701111cab9cebafcd42e9d8093a49db010b3885";
    sha256 = "0r87vwvbw0ipvj5mbw96jrmx3niq5a4b426pqq3aj8pdk0ij27g1";
  };

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
              #bytestring = callCabal2nix "bytestring" bytestring-src {};
              bytestring = callHackage "bytestring" "0.11.2.0" {};

              binary = haskell.lib.dontCheck (self.callHackage "binary" "0.8.8.0" {});
              directory = self.callHackage "directory" "1.3.6.1" {};
              ghc-boot-th = self.callHackage "ghc-boot" "9.0.1" {};
              text = self.callHackage "text" "1.2.4.1" {};
              unix = self.callHackage "unix" "2.7.2.2" {};
              Cabal = haskell.lib.overrideCabal (self.callHackage "Cabal" "3.4.0.0" {}) (oldAttrs: {
                preCompileBuildDriver = ''
                  setupCompileFlags="$setupCompileFlags -package bytestring-0.11.2.0"
                  '';
              });
              doctest = super.doctest.override {
                ghc = self.ghc901;
              };
              ghc901 = haskell.lib.enableCabalFlag super.ghc901 "buildable";
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
