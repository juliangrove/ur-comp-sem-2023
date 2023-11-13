with import (fetchTarball https://github.com/NixOS/nixpkgs/archive/nixos-22.11.tar.gz) { };
stdenv.mkDerivation {
  name = "docsEnv";
  shellHook = ''
    export LANG=en_US.UTF-8
  '';
  buildInputs = [
    biber
    (texlive.combine {
      inherit (texlive)
        adjustbox
        appendix
        biblatex
        bussproofs
        cleveref
        doublestroke
        enumitem
        eso-pic
        expex
        forest
        gitinfo2
        inconsolata
        libertine
        # mathptmx
        mathtools
        newtx
        newunicodechar
        pgf
        relsize
        scheme-basic
        scheme-tetex
        sectsty
        stackengine
        stmaryrd
        tcolorbox
        tikz-qtree
        tipa
        titlesec
        titling
        txfonts
        ulem
        upquote
        wasysym
        xkeyval
        xstring
        ;
    })
  ];
}
