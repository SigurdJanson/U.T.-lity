# Coding Rules & Tipps for the U.T.-lity Package

## File Structure

* Strictly internal, not directly related to the purpose of the package and mere functions (i.e. not being S3 methods) are kept in `utils.R`.
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


## Localisation

Use translatable string with `gettext("id string")`.



## Names

### Functions

* Computations based on basic theoretical formulas start with `get...()`. Functions calculating sample characteristics also start with `get...()`.
* Estimating values from data samples start with `estimate...()`.
* Most functions take a problem-by-participant matrix to compute a result. Sometimes a more basic function is available. It requires that the user can provide specific parameters while the more general function derives those parameters from the problem-by-participant matrix. These specific functions have the same name with a leading `.`. Example: `ndark(dg, ...)` and `.ndark(...)`.


### Argument Names

Use these argument names consistently to identify the corresponding objects.

| Meaning | Name |
|---|---|
| an object of various types/classes | `x` |
| Sample size | `n` |
| Defect grid | `dg` |
| Visibility  | `p.occ` |
| Chance of observing | `p.obs` |
|| |
|| |
|| |



