with import (fetchTarball https://github.com/NixOS/nixpkgs/archive/nixos-23.05.tar.gz) { };
let
  orgEmacs = emacsWithPackages (with emacsPackagesNg; [ org-ref ]);
  orgEmacsConfig = writeText "default.el" ''
    (add-to-list 'load-path "${emacsPackages.org-ref}")
    (require 'org-ref)
    (require 'oc-biblatex)
    (with-eval-after-load 'ox-beamer
       (add-to-list 'org-latex-classes
          '("slides"
           "\\documentclass[presentation]{beamer}
       \\usepackage[T1]{fontenc}
       \\setbeamerfont{frametitle}{size=\\large}
       \\setbeamertemplate{footline}{
       \\begin{flushright}
       \\insertframenumber/\\inserttotalframenumber\\hspace{2mm}\\mbox{}
       \\end{flushright}}
       \\usepackage{libertine}
       \\usepackage[varqu]{zi4}
       \\usepackage[libertine]{newtxmath}
       %%\\usepackage[small]{eulervm}
       %%\\usepackage[{libertinust1math}
       \\usepackage[T1]{fontenc}
       \\renewcommand*\\familydefault{\\sfdefault}
       \\usepackage{tipa}
       \\usepackage{adjustbox}
       \\usepackage{multirow}
       \\usepackage{multicol}
       \\usepackage[backend=bibtex,uniquename=false,dashed=false,date=year,url=false,isbn=false]{biblatex}
       \\usepackage{expex}
       \\usepackage{stmaryrd}
       \\usepackage{stackrel}
       \\usepackage{stackengine}
       \\usepackage{relsize}
       \\usepackage{amsmath}
       \\usepackage{mathtools}
       \\usepackage{dsfont}
       \\usepackage{fixltx2e}
       \\usepackage{graphicx}
       \\usepackage{xcolor}
       \\usepackage[normalem]{ulem}
       [NO-DEFAULT-PACKAGES]
       [PACKAGES]
       [EXTRA]"
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
  '';
in
stdenv.mkDerivation {
  name = "docsEnv";
  shellHook = ''
    export LANG=en_US.UTF-8
    export MYEMACSLOAD=${orgEmacsConfig}
  '';
  # eval $(egrep ^export ${ghc}/bin/ghc)
  buildInputs = [
    biber
    orgEmacs
    (texlive.combine {
      inherit (texlive)
        adjustbox
        beamer
        beamertheme-metropolis
        biblatex
        collectbox
        doublestroke
        expex
        forest
        inconsolata
        libertine
        mathtools
        multirow
        newtx
        newunicodechar
        pgf
        relsize
        scheme-basic
        scheme-tetex
        stackengine
        stmaryrd
        tikz-qtree
        tikzsymbols
        tipa
        ulem
        upquote
        ;
    })
  ];
}
