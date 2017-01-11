{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }:

with pkgs;

haskell.lib.buildStackProject {
  name = "haskell-aligned";
  inherit ghc;

  buildInputs = [ ];

  LANG = "en_US.UTF-8";
}
