# H2V - a Haskell to Verilog Compiler
H2V is intended to facilitate the creation of Verilog modules for accelerating parallel
computations. It is a proof of concept and not a completed tool, but is quite capable and easily extended. It is compatible with
existing Haskell compilers, and supports a proper subset of the language. Refer to ```docs/Final Report/Final Report.tex``` for more details.

This was developed for honours thesis project, and is no longer being developed.

## Compilation

    make -C H2V

Requires GHC 7.6.3 or later.
Note that all files compiled should import ```H2V/include/H2V_Compat.hs```, and that the resulting modules may depend on ```H2V/include/include.v```.

Documentation may be compiled by running ```latexmk -pdf``` in directories containing LaTeX files.
PDFs of the final report and presentation are included for convenience.

## Usage

    $ ./H2V --help
    H2V 1.0
    Written by Reuben D'Netto 2014

    Usage: H2V [OPTION] FILE
        -h | --help        Display usage information.
        -a | --ast         Display AST for file. Printed to stdout.
        -d | --dfd         Display DFD for file. Printed to stdout.
        -g | --graphviz    Convert DFD to Graphviz file. Saved as FILE.gv.
        -v | --verilog     Convert file to Verilog. Saved as FILE.v.

## License
H2V is published under the GNU GPLv2.
