{ melpaBuild, gnu-smalltalk }:
let
  pname = "gnu-smalltalk-mode"; 
  version = "0.1.0.0";
in
melpaBuild {

  inherit pname version; 
  src = ./.; 

  preConfigure = ''
    inpdir="${gnu-smalltalk}/share/emacs/site-lisp"
    outdir="$out/share/emacs/site-lisp/elpa/${pname}-${version}"

    mkdir -p $outdir
      
    link () {
      [ ! -f $inpdir/$1 ] &&
        echo ERROR: file $1 does not exist. && exit 1 ||
        ln -s $inpdir/$1 $outdir/$1
    }

    link "smalltalk-mode.elc"
    link "gst-mode.elc"
  ''; 

}
