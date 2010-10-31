HeX - an extensible text macro language targetting multiple formats
===================================================================

Good things about TeX/LaTeX:

* Beautiful output
* Extensible using macros

Bad things about TeX/LaTeX:

* Output limited to PDF/PS; difficult to convert to HTML or other formats
* Ugly language for writing macros

HeX:

* Provides a basic macro system that can be customized by writing
  functions in Haskell
* These functions can be included in the document (using bird-style
  literate Haskell) or imported from a module
* The macros can define output in multiple formats, for example, TeX
  and HTML

You can install HeX in the standard way for Haskell packages:

    cabal install

For an example of a HeX document, see `examples/simple.lhs`.
To convert it to plain TeX and HTML, respectively:

    hexto tex examples/simple.lhs
    hexto html examples/simple.lhs

At this stage, this is not much more than an idea.

