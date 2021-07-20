## Version 1.0.1
Fixes to ensure that network errors do not trigger check or test warnings/errors.

## Test environments
* local Windows 10 x64, R 4.0.5
* Ubuntu 18.04 (on GitHub Actions), R 4.1
* Ubuntu 18.04 (on GitHub Actions), R 3.6
* Ubuntu 18.04 (on GitHub Actions), r80639
* Mac OS X 10.15.7 (on GitHub Actions), R 4.1.0
* Windows x64, i386 (on win-builder), R 4.1.0

## R CMD check results
No ERRORs, or WARNINGs

1 NOTE (on win-builder only):

Days since last update: 4

Uses the non-portable package: 'R2WinBUGS'

This update addresses a concern identified during CRAN checking. The final entry relates to a dependent package and has been acknowledged and cleared in previous submissions.

## Downstream dependencies
There are currently no downstream dependencies for this package