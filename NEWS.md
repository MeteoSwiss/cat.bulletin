# cat.bulletin 1.4.0

CATS-583: Add convert_to_image helper function to convert pdfs to png or jpgs

_fma;2026_06_10_

# cat.bulletin 1.3.0

CATS-482 separate monthly bulletin into clim.bull package

* The code for creating the monthly climate bulletin is now separated in the clim.bull package. cat.bulletin only contains generic code for creating bulletins.
* Switched to logger package for logging. See [Logging with logger (for package developing)](https://meteoswiss.atlassian.net/wiki/x/pQFBU)

# cat.bulletin 1.2.3

CATS-504

* Bugfix: Fix `meno` (for precip >150 %) in ital. leadtext
* Update Jenkinsfile

_por;2026_02_27_

# cat.bulletin 1.2.0

CATS-414 

* Allow cat.bulletin to be used from another package: Rmd-elements can now be located in a different package or directory.
* Possibility to customize email-address within pdf report.

_fma;2025_09_23_

# cat.bulletin 1.1.0

* CATS-387 Fix issue in teaser image caption: caption and source can now be specified separately.
* Teaser-Texts for bulletin monthly should now contain 2 lines (first: caption, second: source)

_fma;2025_09_09_

# cat.bulletin 1.0.6

* Fixed combining pdfs from shorties when several shorties lists are present in bulletin.

_fma;2025_06_02_
