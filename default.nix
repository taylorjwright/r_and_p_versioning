let
 pkgs = import (fetchTarball "https://github.com/rstats-on-nix/nixpkgs/archive/2025-01-27.tar.gz") {};
 rpkgs = builtins.attrValues {
  inherit (pkgs.rPackages) knitr dplyr tidyr stringr purrr ggplot2 flextable quarto targets tarchetypes;
};
  tex = (pkgs.texlive.combine {
  inherit (pkgs.texlive) scheme-small amsmath booktabs setspace lineno cochineal tex-gyre framed multirow wrapfig tcolorbox environ tikzfill pdfcol;
});
 system_packages = builtins.attrValues {
  inherit (pkgs) R quarto pandoc;
};
in
 pkgs.mkShell {
  buildInputs = [  rpkgs tex system_packages  ];
 }
