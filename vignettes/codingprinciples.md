# Coding Principles for the U.T.-lity Package

## File Structure

* Strictly internal and mere functions (i.e. not being S3 methods) are kept in `utils.R`.
* Test files shall reflect the file structure of the code, except...
  * Tests may be split across several files. So, tests for one code file may be split across several test files.


## S3 Objects

S3 classes from this package carry the specific class (i.e. `ci` or `defectgrid`) and the extra class `U.T.lity` to identify the class from this package.

* One code file contains the basic definition with ...
  * a constructor function `*_new()` which also describes the class.
  * a function `is.*` to check if an object is from class `*` and inherited from the U.T.-lity package.
  * Optionally a printing method `print.*`.
* Additional methods in other files.

## Plotting Functions

Plotting functions ideally support `ggplot2` and `graphics` plots. If `ggplot` is available prefer this plot, unless the user explicitly requests a `graphics` plot.
