## Test environments
* local Windows 10 x64, R 4.0.5
* Ubuntu 18.04 (on GitHub Actions), R 4.1
* Ubuntu 18.04 (on GitHub Actions), R 3.6
* Ubuntu 18.04 (on GitHub Actions), r80460
* Mac OS X 10.15.7 (on GitHub Actions), R 4.1.0
* Windows x64, i386 (on win-builder), R 4.1.0

## R CMD check results
No ERRORs, or WARNINGs

1 NOTE (on win-builder only):

New submission

Possibly mis-spelled words in DESCRIPTION:
  al (12:27)
  et (12:24)
  wiDB (7:14, 8:9)

Uses the non-portable package: 'R2WinBUGS'

This is the first CRAN submission of this package. All spelling has been 
verified. The final entry relates to a dependent package; I haven't been
able to identify the cause of this NOTE but have verified that our
package's functionality is not affected across a wide range of platforms
and configurations.

## Downstream dependencies
There are currently no downstream dependencies for this package

## Revision comments
* Added () to function references in DESCRIPTION.
* Added url for API protocol in DESCRIPTION.
* Checked all Rd files for missing \value tags: the referenced file is the master man page for the package, and does not represent an exported method.
* Checked all functions for modification of options, par, and working directory. Protected one instance of modified par setting by adding an immediate call to on.exit().
* Checked all examples and vignettes for code modifying par settings. Fixed four instances in the vignette to either eliminate the modification or restore user settings.