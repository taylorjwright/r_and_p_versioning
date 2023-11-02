let
 pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/976fa3369d722e76f37c77493d99829540d43845.tar.gz") {};
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
