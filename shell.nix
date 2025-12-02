with import <nixpkgs> { };
pkgs.mkShell {
  buildInputs = [
    (ghc.withPackages (hPkg: with hPkg; [
      haskell-language-server
    ]))
  ];
}

