# Thrift Finance

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Thrift Finance](#thrift-finance)
    - [Opening a shell](#opening-a-shell)
    - [Running](#running)
    - [Script Compilation and Loading](#script-compilation-and-loading)
    - [Nix cache](#nix-cache)
    - [Cachix](#cachix)

<!-- markdown-toc end -->


## Opening a shell

- `nix develop .#offchain`
- `nix develop .#onchain`
- `cabal repl --repl-options -Wwarn`

## Running
To get everything set up, copy the contents of this repo to your new project.
You will then need to run the setup.sh script. This will walk you through the
various pieces of information you need to provide.

## Script Compilation and Loading
Due to the repo split, it might seem non-trivial to obtain the validators and
minting policies from the onchain segment, into the offchain segment. However,
all you have to do is compile the Plutarch scripts using `Plutarch.compile` and
write the resulting script into a file - ideally using something like
[`writeFileTextEnvelope`](https://input-output-hk.github.io/cardano-node/cardano-api/lib/Cardano-Api-SerialiseTextEnvelope.html#v:writeFileTextEnvelope).
You can then later read the resulting file in your offchain project using
[`readFileTextEnvelope`](https://input-output-hk.github.io/cardano-node/cardano-api/lib/Cardano-Api-SerialiseTextEnvelope.html#v:writeFileTextEnvelope).

However, `cardano-api` is not available in the onchain project, so you can't
actually use `writeFileTextEnvelope`. Thus, the scaffold includes a
monomorphized implementation of `writeFileTextEnvelope` in
`Scripts.V1.Serialize`.

```hs
writePlutusValidator :: String -> FilePath -> Validator -> IO ()

writePlutusMintingPolicy :: String -> FilePath -> MintingPolicy -> IO ()

writePlutusScript :: String -> FilePath -> Script -> IO ()
```

You can use these functions to write your scripts to the filesystem. Finally, in
your offchain project, you should use `readFileTextEnvelope (AsPlutusScript
AsPlutusScriptV1)` to read the file and deserialize it as a
[`Script`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Scripts.html#t:Script),
then you can wrap it into a
[`Validator`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Scripts.html#t:Validator)
or
[`MintingPolicy`](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Scripts.html#t:MintingPolicy)
as necessary.

This is also facilitated by the offchain scaffold. In particular, you can use
the functions in `Scripts.V1.Deserialize` in the offchain project to load up
these scripts.

```hs
readPlutusValidator :: FilePath -> IO Validator

readPlutusMintingPolicy :: FilePath -> IO MintingPolicy

readPlutusScript :: FilePath -> IO Script
```

**NOTE**: You might need to add the compiled scripts filepaths to `extra-source-files` in your offchain project's cabal file to be able to access it.

## Nix cache

You must have the following in your nix.conf:
```
substituters = https://public-plutonomicon.cachix.org https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= public-plutonomicon.cachix.org-1:3AKJMhCLn32gri1drGuaZmFrmnue+KkKrhhubQk/CWc=
```

## Cachix
- Assuming you have a Cachix auth key, and are using github actions, add your
  auth token as a secret to the repo under the name `CACHIX_AUTH_TOKEN`, and
  your Cachix name as `CACHIX_NAME`

- Then, if you wish to utilize Cachix locally, run the following:
  ```
  cachix authtoken [your auth token]
  cachix use [your cachix name]
  ```
