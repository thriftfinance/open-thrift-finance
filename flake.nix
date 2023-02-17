{
  description = "thrift-finance";
  nixConfig.bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]nix-develop \\[\\e[0;1m\\]thrift-finance \\[\\e[0;93m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";

  inputs = {

    nixpkgs.follows = "plutarch/nixpkgs";
    haskell-nix-onchain.follows = "plutarch/haskell-nix";
    haskell-nix-offchain.follows = "plutip/haskell-nix";
    haskell-nix-extra-hackage.follows = "plutarch/haskell-nix-extra-hackage";

    plutarch.url = "github:Plutonomicon/plutarch/staging";
    plutip.url = "github:mlabs-haskell/plutip/d24b98162bcbcbfb4ca403ee62fdb890f2059f47";

    ply = {
      url = "github:mlabs-haskell/ply";
      inputs = {
        haskell-nix.follows = "plutarch/haskell-nix";
        nixpkgs.follows = "plutarch/nixpkgs";
      };
    };

    # FIXME: build failing with partiat TX 
    #    partialTx.url = "github:mlabs-haskell/plutus-partial-tx/vasil";
  };

  outputs =
    inputs@{ self
    , haskell-nix-extra-hackage
    , haskell-nix-offchain
    , haskell-nix-onchain
    , nixpkgs
      #    , partialTx
    , plutarch
    , plutip
    , ply
    , ...
    }:
    let
      # GENERAL
      supportedSystems = with nixpkgs.lib.systems.supported; tier1 ++ tier2 ++ tier3;
      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      onchainNixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [ haskell-nix-onchain.overlay (import "${plutarch.inputs.iohk-nix}/overlays/crypto") ];
      };

      offchainNixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [ haskell-nix-offchain.overlay (import "${plutip.inputs.iohk-nix}/overlays/crypto") ];
      };

      pureNixpkgsFor = system: import nixpkgs { inherit system; };

      fourmoluFor = system: (pureNixpkgsFor system).haskell.packages.ghc921.fourmolu;

      formatCheckFor = system:
        let
          pkgs' = pureNixpkgsFor system;
        in
        pkgs'.runCommand "format-check"
          {
            nativeBuildInputs = [
              pkgs'.git
              pkgs'.fd
              pkgs'.haskellPackages.cabal-fmt
              pkgs'.nixpkgs-fmt
              (fourmoluFor system)
            ];
          } ''
          export LC_CTYPE=C.UTF-8
          export LC_ALL=C.UTF-8
          export LANG=C.UTF-8
          cd ${self}
          make format_check
          mkdir $out
        ''
      ;

      deferPluginErrors = true;

      # ONCHAIN / Plutarch

      onchain =
        rec {
          ghcVersion = "ghc923";
          myhackages = system: compiler-nix-name: haskell-nix-extra-hackage.mkHackagesFor system compiler-nix-name (
            [
              "${plutarch}"
              "${plutarch}/plutarch-extra"
              "${plutarch}/plutarch-test"
              "${ply}/ply-plutarch"
              "${ply}/ply-core"
            ]
          );
          projectFor = system:
            let
              pkgs = onchainNixpkgsFor system;
              pkgs' = pureNixpkgsFor system;
              hackages = myhackages pkgs.system ghcVersion;
            in
            pkgs.haskell-nix.cabalProject' (plutarch.applyPlutarchDep pkgs {
              src = ./.;
              compiler-nix-name = ghcVersion;
              inherit (hackages) extra-hackages extra-hackage-tarballs modules;
              cabalProjectLocal = "";
              cabalProjectFileName = "onchain-cabal.project";
              shell = {
                withHoogle = true;
                exactDeps = true;
                nativeBuildInputs = [
                  pkgs'.cabal-install
                  pkgs'.fd
                  pkgs'.hlint
                  pkgs'.haskellPackages.cabal-fmt
                  pkgs'.nixpkgs-fmt
                  plutarch.project.${system}.hsPkgs.hspec-discover.components.exes.hspec-discover
                  (fourmoluFor system)
                  (plutarch.hlsFor ghcVersion system)
                ];
                shellHook = ''
                  export  NIX_SHELL_TARGET="onchain"
                  ln -fs onchain-cabal.project cabal.project
                '';
              };
            });
        };

      # OFFCHAIN / Testnet, Cardano, ...

      offchain =
        rec {
          ghcVersion = "ghc8107";

          projectFor = system:
            let
              pkgs = offchainNixpkgsFor system;
              pkgs' = pureNixpkgsFor system;
              plutipin = inputs.plutip.inputs;
              fourmolu = pkgs.haskell-nix.tool "ghc921" "fourmolu" { };
              project = pkgs.haskell-nix.cabalProject' {
                src = ./.;
                cabalProjectFileName = "offchain-cabal.project";
                compiler-nix-name = ghcVersion;
                inherit (plutip) cabalProjectLocal;
                extraSources = plutip.extraSources ++ [
                  {
                    src = "${plutip}";
                    subdirs = [ "." ];
                  }
                  {
                    src = "${ply}";
                    subdirs = [ "ply-core" ];
                  }
                  #                  {
                  #                    src = "${partialTx}";
                  #                    subdirs = [ "." ];
                  #                  }
                ];
                modules = [
                  ({ config, ... }: {
                    packages.thrift-finance-offchain.components.tests.thrift-finance-offchain-test.build-tools = [
                      project.hsPkgs.cardano-cli.components.exes.cardano-cli
                      project.hsPkgs.cardano-node.components.exes.cardano-node
                    ];

                  })
                ] ++ plutip.haskellModules;

                shell = {
                  withHoogle = true;

                  exactDeps = true;

                  # We use the ones from Nixpkgs, since they are cached reliably.
                  # Eventually we will probably want to build these with haskell.nix.
                  nativeBuildInputs = [
                    pkgs'.cabal-install
                    pkgs'.fd
                    pkgs'.haskellPackages.apply-refact
                    pkgs'.haskellPackages.cabal-fmt
                    pkgs'.hlint
                    pkgs'.nixpkgs-fmt

                    project.hsPkgs.cardano-cli.components.exes.cardano-cli
                    project.hsPkgs.cardano-node.components.exes.cardano-node

                    (fourmoluFor system)
                  ];

                  tools.haskell-language-server = { };

                  additional = ps: [
                    ps.plutip
                    ps.ply-core
                    #                    ps.plutus-partial-tx
                  ];

                  shellHook = ''
                    export NIX_SHELL_TARGET="offchain"
                    ln -fs offchain-cabal.project cabal.project
                  '';

                };
              };
            in
            project;
        };
    in
    {
      inherit onchainNixpkgsFor;

      onchain = {
        project = perSystem onchain.projectFor;
        flake = perSystem (system: (onchain.projectFor system).flake { });
      };

      offchain = {
        project = perSystem offchain.projectFor;
        flake = perSystem (system: (offchain.projectFor system).flake { });
      };

      packages = perSystem (system:
        self.onchain.flake.${system}.packages
        // self.offchain.flake.${system}.packages
      );
      checks = perSystem (system:
        self.onchain.flake.${system}.checks
        // self.offchain.flake.${system}.checks
        // {
          formatCheck = formatCheckFor system;
        }
      );
      check = perSystem (system:
        (pureNixpkgsFor system).runCommand "combined-test"
          {
            checksss =
              builtins.attrValues self.checks.${system}
              ++ builtins.attrValues self.packages.${system}
              ++ [
                self.devShells.${system}.onchain.inputDerivation
                self.devShells.${system}.offchain.inputDerivation
              ];
          } ''
          echo $checksss
          touch $out
        ''
      );

      devShells = perSystem (system: {
        onchain = self.onchain.flake.${system}.devShell;
        offchain = self.offchain.flake.${system}.devShell;
      });

      apps = perSystem (system: {
        serialise-scripts = {
          type = "app";
          program =
            let dir = self.packages.${system}."thrift-finance:exe:thrift-finance-exe";
            in "${dir}/bin/thrift-finance-exe";
        };
      });
    };
}


