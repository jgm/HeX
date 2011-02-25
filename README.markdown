HeX - a flexible text macro system
==================================

Good things about TeX/LaTeX:

* Beautiful output
* Extensible using macros

Bad things about TeX/LaTeX:

* Output limited to PDF/PS; difficult to convert to HTML or other formats
* Ugly language for writing macros

HeX:

* Provides a basic macro system that can be customized by writing
  functions in Haskell
* The macros can define output in multiple formats, for example, LaTeX
  and HTML
* References and labels are handled in a consistent way across both formats.
* Math works in both formats (using MathML in HTML output, though this
  is configurable).
* Eventually, citeproc-hs will be used for references.  Producing a
  PDF from HeX's LaTeX output will only require a single run of pdflatex,
  and no packages will be required other than amsmath and amssymb.

HeX is under development, but beyond the core not much has been
implemented yet. If you want to play with it,
you can install HeX in the standard way for Haskell packages:

    cabal install

For an example of a HeX document, see `test/test.hex`.
To convert it to LaTeX and HTML, respectively, change
to the `test` directory and do:

    runghc test.hs latex < test.hex
    runghc test.hs html < test.hex

The file `test.hs` functions like the preamble of a LaTeX document,
defining the commands to be used.  In this case, MathML is used
for math in HTML output.

