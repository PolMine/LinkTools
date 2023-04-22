V0.0.1.9005
* replaced `\code{}` tags in the documentation
* reduced verbosity of intermediate steps
* introduced a check if the external dataset contains NA values in significant columns before fuzzy matching
* introduced messages from the `cli` package

[2023-04-19]
* addressed a quite comprehensive issue in `external_attribute_to_region_matrix()` that potentially obscured speakers which were not matched, making them unavailable for both the fuzzy matching and manual inspection (issue #14)
* introduced tests
* modified vignette, uses GERMAPARLMINI as sample data and the `btmp` package for linking
* removed data and data-raw which were containing the external data now provided by the `btmp` package
* made the addition more robust for subcorpora

[2023-04-12]
* starting rework to address more diverging dataset-text data-combination
* adding the `additional_attributes` argument to `create_attribute_region_datatable()` to make manual inspection more meaningful
* made `check_and_add_missing_values()` more flexible by passing `check_for_groups` as a list and adding the `negate` argument
* added capability to use more than one fuzzy matched variable in `fuzzy_join_missing_values()`
* made more explicit use of fuzzyjoin::stringdist_join()

[2023-02-07]
# v0.0.1.9003
* Added `Depends: R (>= 3.5.0)` to DESCRIPTION to avoid warning when build the package #2.
* Added to DESCRIPTION: `Roxygen: list(markdown = TRUE)` so that markdown syntax can be 
used when writing documentation.
* Renamed this file (NEWS) to NEWS.md for nice rendering on GitHub.
* Added file .Rbuildignore to the repo to exclude data-dir from being included in pkg tarball.
* Moved README.md to README.Rmd to that README.md is created from *.Rmd file.
* Preliminary installation instructions in README.
* Added tags @docType and @keywords to documentation objects.

[2023-02-06] 
# v0.0.1.9002
* moved to GitHub from GitLab
* added excerpt of Stammdaten to data

[2022-02-14]
# v.0.0.1.9001
* initial draft
