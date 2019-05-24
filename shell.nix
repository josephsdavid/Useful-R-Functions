let
    pkgs = import <nixpkgs> {};
in
  pkgs.mkShell {
    name = "handyR";
    buildInputs = with pkgs; [
      R
      rPackages.tidyverse
      rPackages.cowplot
      rPackages.dplyr
      rPackages.tidyr
      rPackages.purrr
      rPackages.RColorBrewer
      rPackages.ggplot2
      rPackages.gplots
      rPackages.corrplot
      rPackages.plyr
    ];
    
  }
