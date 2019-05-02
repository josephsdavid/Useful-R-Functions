 let
   pkgs = import <nixpkgs> {}; 
   stdenv = pkgs.stdenv;
 in with pkgs; {
   myProject = stdenv.mkDerivation {
     name = "handyR";
     version = "2";
     buildInputs =  [
       R
      rPackages.cowplot
      rPackages.dplyr
      rPackages.tidyr
      rPackages.purrr
      rPackages.RColorBrewer
      rPackages.ggplot2
      rPackages.gplots
      rPackages.corrpot
      rPackages.plyr
    ];
  };
}
