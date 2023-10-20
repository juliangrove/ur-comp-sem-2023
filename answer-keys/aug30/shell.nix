with import (fetchTarball https://github.com/NixOS/nixpkgs/archive/nixos-22.05.tar.gz) { };
let
  orgEmacs = emacsWithPackages (with emacsPackagesNg; [ org-ref ]);
  orgEmacsConfig = writeText "default.el" ''
    (add-to-list 'load-path "${emacsPackages.org-ref}")
    (require 'org-ref)
    (require 'oc-biblatex)
    (with-eval-after-load 'ox-latex
       (add-to-list 'org-latex-classes
          '("manuscript"
            "\\documentclass[11pt,letterpaper,twoside]{article}
       \\usepackage[margin=1.4in]{geometry}
       \\usepackage{etoolbox}
       \\ifundef{\\abstract}{}{\\patchcmd{\\abstract}%
           {\\quotation}{\\quotation\\noindent\\ignorespaces\\textbf{Abstract}\\hspace{1mm}}{}{}}
       \\renewcommand{\\abstractname}{\\vspace{-1.4\\baselineskip}}
       \\usepackage{lscape}
       \\usepackage{titling}
       \\usepackage{graphicx}
       \\usepackage{amssymb}
       \\usepackage{mathtools}
       \\usepackage{amsthm}
       \\theoremstyle{definition}
       \\newtheorem{thm}{Theorem}
       \\usepackage[inline]{enumitem}
       \\usepackage{xcolor}
       \\pagestyle{plain}
       \\usepackage[T1]{fontenc}
       \\usepackage{libertine}
       \\usepackage[varqu]{zi4}
       \\usepackage[libertine]{newtxmath}
       %% \\linespread{1.2}
       \\usepackage{tipa}
       %% \\usepackage{natbib} % use biblatex instead
       \\usepackage{stmaryrd}
       \\usepackage{dsfont}
       \\usepackage{fancyhdr}
       \\pagestyle{fancy}
       \\usepackage{sectsty}
       \\usepackage{expex}
       \\usepackage{stackrel}
       \\usepackage{stackengine}
       \\usepackage{relsize}
       \\usepackage{fixltx2e}
       \\raggedbottom
       \\allsectionsfont{\\normalsize}
       \\usepackage[colorlinks=true,urlcolor=blue!60!black!80,citecolor=blue!60!black!80,linkcolor=black,bookmarks,bookmarksopen,bookmarksdepth=2]{hyperref}
       \\renewcommand\\UrlFont{\\normalsize}
       \\usepackage[normalem]{ulem}
       \\usepackage{appendix}
       \\fancyfoot{}
       \\fancyhead[LO]{\\footnotesize\\thetitle}
       \\fancyhead[RE]{\\footnotesize\\theauthor}
       \\fancyhead[LE,RO]{\\footnotesize\\thepage}
       \\renewcommand{\\headrulewidth}{0pt}
       \\pretitle{\\Large\\bfseries\\noindent}\\posttitle{\\par\\vskip 1em}
       \\newcommand{\\affil}{\\vspace{-1em}}
       \\preauthor{\\normalsize\\noindent\\lineskip 0.5em}\\postauthor{\\\\\\affil\\par\\vskip 1em}
       \\predate{\\normalsize\\noindent}\\postdate{\\par\\vskip 1em}
       \\bibliographystyle{li}
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
    orgEmacs
    aspell
    biber
    (texlive.combine {
      inherit (texlive)
        adjustbox
        appendix
        biblatex
        bussproofs
        doublestroke
        enumitem
        eso-pic
        expex
        forest
        gitinfo2
        inconsolata
        libertine
        # mathpazo
        mathtools
        newtx
        newunicodechar
        pgf
        relsize
        scheme-basic
        sectsty
        stackengine
        stmaryrd
        tikz-qtree
        tipa
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
