
<!-- badges: start -->

[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![R build
status](https://github.com/PolMine/LinkTools/workflows/R-CMD-check/badge.svg)](https://github.com/PolMine/LinkTools/actions)
[![codecov](https://codecov.io/gh/PolMine/LinkTools/branch/main/graph/badge.svg)](https://codecov.io/gh/PolMine/LinkTools/branch/main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-red.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

# LinkTools

## About LinkTools

### Motivation

`LinkTools` facilitates the linkage of datasets by providing accessible
approaches to two tasks: Record Linkage and Entity Linking. It is
developed in the measure [Linking Textual
Data](https://www.konsortswd.de/en/konsortswd/the-consortium/services/linking-textual-data/)
in [KonsortSWD](https://www.konsortswd.de/en/) within the [National
Research Data Infrastructure Germany](https://www.nfdi.de/?lang=en)
(NFDI) and has the goal to make linking textual data more accessible.

### Purpose

Once finalized, four steps should be integrated into this R package:

- the preparation of datasets which should be linked, i.e. the
  transformation into a comparable format and the assignment of new
  information, in particular shared unique identifiers
- the merge of datasets based on these identifiers
- the encoding or enrichment of the data with different output formats
  (data.table, XML or CWB)
- the package includes a wrapper for the Named Entity Linking of textual
  data based on DBpedia Spotlight

A major focus of this package is the provision of an intuitive workflow
with transparency and robustness. Documentation, validity and user
experience as well as training and education are at the heart of this
development. Consequently, the processes of linkage and linking should
be designed as approachable as possible - using GUIs and feedback - as
well as robust and repeatable.

## Current Status

The Record Linkage functionality is maturing and the current state is
documented in the package vignette. Record Linkage with CWB corpora is
currently implemented. Entity Linking using [DBpedia
Spotlight](https://www.dbpedia-spotlight.org/) as the backend is
currently in development.

## Core functionality of LinkTools

``` r
library(LinkTools)
```

### Record Linkage

The `LTDataset` class, an R6 class, is the main driver of the Record
Linkage process within the `LinkTools` package. Aside from the wrangling
of the textual data input, its core functionality includes the merge of
metadata found in text corpora and external datasets. To this end,
first, a strict merge of exact matches is performed. Thereafter, if
observations could not be joined directly, a fuzzy matching approach is
possible in which `LinkTools` suggests matches based on different
measures of string distance. In this stage, the manual inspection of
these suggestions and the addition of missing values is possible.

The vignette shows this for larger data and a real external dataset. In
the following a short artificial example is provided. It is assumed that
the following two resources should be linked:

### Text corpus

``` r
library(polmineR)
use("polmineR")
```

    ## ✔ corpus loaded: GERMAPARLMINI (version: 0.1.0 | build date: 2023-04-16)

GermaParlMini is a sample corpus of the larger GermaParl corpus of
parliamentary debates. It is provided by the `polmineR` R package. It
contains a number of metadata such as the speaker name and the party of
a speaker. To show the capabilities of the tool, a small sample of this
dataset is used.

``` r
germaparlmini_session <- polmineR::corpus("GERMAPARLMINI") |>
  polmineR::subset(date == "2009-11-12")
```

The metadata used for linking looks like the following:

| speaker                | party           |
|:-----------------------|:----------------|
| Alexander Bonde        | B90_DIE_GRUENEN |
| Barbara Hendricks      | SPD             |
| Birgitt Bender         | B90_DIE_GRUENEN |
| Carl-Ludwig Thiele     | FDP             |
| Carola Reimann         | SPD             |
| Elisabeth Scharfenberg | B90_DIE_GRUENEN |
| Elke Ferner            | SPD             |
| Gerda Hasselfeldt      | NA              |
| Gesine Lötzsch         | DIE_LINKE       |
| Hermann Otto Solms     | NA              |
| Jens Spahn             | CDU_CSU         |
| Joachim Poß            | SPD             |
| Norbert Lammert        | NA              |
| Philipp Rösler         | FDP             |
| Rolf Koschorrek        | CDU_CSU         |
| Ulrike Flach           | FDP             |
| Wolfgang Schäuble      | CDU_CSU         |
| Wolfgang Zöller        | CDU_CSU         |

### External Data

To show the (fuzzy) matching possibilities of the package, an artificial
dataset is created on the spot. It is created by modifying the speaker
data found in the textual data by introducing some deviation. In
consequence, it contains most of the speakers in the textual data shown
above as well as the same metadata plus a variable called “ID” which
represents the additional information we want to add to the textual
data. To showcase the fuzzy matching, this artificial dataset also
contains some typos in the names of the speakers as well as a
differently named column for the speaker names.

For a real dataset, please see the package vignette.

| name                   | party           | id    |
|:-----------------------|:----------------|:------|
| Alexander Bodne        | B90_DIE_GRUENEN | ID_1  |
| Barbara Hendricks      | SPD             | ID_2  |
| Birgitt Bender         | B90_DIE_GRUENEN | ID_3  |
| Carl-Ludwig Thiele     | FDP             | ID_4  |
| Carola Reimann         | SPD             | ID_5  |
| Elisabeth Scharfenberg | B90_DIE_GRUENEN | ID_6  |
| Gerda Hassefleldt      | NA              | ID_7  |
| Gesine Lötzsch         | DIE_LINKE       | ID_8  |
| Hermann Otto Somls     | NA              | ID_9  |
| Jens Sphan             | CDU_CSU         | ID_10 |
| Joachim Poß            | SPD             | ID_11 |
| Norbert Lamemrt        | NA              | ID_12 |
| Philipp Rösler         | FDP             | ID_13 |
| Rolf Koschorrek        | CDU_CSU         | ID_14 |
| Ulrike Flcah           | FDP             | ID_15 |
| Wolfgang Schäuble      | CDU_CSU         | ID_16 |
| Wolfgang Zöller        | CDU_CSU         | ID_17 |

### Step 1: Instantiating the `LTDataset` class

The `LTDataset` class is instantiated with the names of the two
resources and some additional information. The package vignette shows
some more arguments. Also see `?LTDataset` for more in-depth
documentation of these arguments.

``` r
LTD <- LTDataset$new(textual_data = germaparlmini_session,
                     textual_data_type = "cwb",
                     external_resource = artificial_id_data,
                     attr_to_add = c("id_in_corpus" = "id"),
                     match_by = c("speaker" = "name",
                                  "party" = "party"),
                     forced_encoding = "UTF-8")
```

### Step 2: Join strictly

With the shared attributes provided in the `match_by` argument, the two
datasets are joined.

``` r
LTD$join_textual_and_external_data()
```

    ## ... decoding s_attribute speaker

    ## ... decoding s_attribute party

    ## ℹ Preparing region matrix for encoding.

    ## ✔ joined textual and external data.

After this join, a data.table object can be created. This can be used to
inspect the results of the join directly. Each row in which the ID
column is “NA” was not matched.

``` r
LTD$create_attribute_region_datatable()
```

``` r
LTD$attrs_by_region_dt |>
  unique() |>
  data.table::setorder(speaker) |>
  knitr::kable(format = "markdown")
```

| speaker                | party           | id_in_corpus |
|:-----------------------|:----------------|:-------------|
| Alexander Bonde        | B90_DIE_GRUENEN | NA           |
| Barbara Hendricks      | SPD             | ID_2         |
| Birgitt Bender         | B90_DIE_GRUENEN | ID_3         |
| Carl-Ludwig Thiele     | FDP             | ID_4         |
| Carola Reimann         | SPD             | ID_5         |
| Elisabeth Scharfenberg | B90_DIE_GRUENEN | ID_6         |
| Elke Ferner            | SPD             | NA           |
| Gerda Hasselfeldt      | NA              | NA           |
| Gesine Lötzsch         | DIE_LINKE       | ID_8         |
| Hermann Otto Solms     | NA              | NA           |
| Jens Spahn             | CDU_CSU         | NA           |
| Joachim Poß            | SPD             | ID_11        |
| Norbert Lammert        | NA              | NA           |
| Philipp Rösler         | FDP             | ID_13        |
| Rolf Koschorrek        | CDU_CSU         | ID_14        |
| Ulrike Flach           | FDP             | NA           |
| Wolfgang Schäuble      | CDU_CSU         | ID_16        |
| Wolfgang Zöller        | CDU_CSU         | ID_17        |

### Step 3: Check and add missing values

If there are cases in which a match was not possible - for example
because of slight differences in the two datasets - manual annotation
supported by fuzzy matching is possible.

In an interactive process, realized with `shiny`, suggestions based on
the fuzzy matching of attributes are provided and can be checked
manually. Remaining missing attributes can be added to the metadata of
the corpus.

``` r
LTD$check_and_add_missing_values(modify = TRUE,
                                 match_fuzzily_by = "speaker",
                                 doc_dir = tempdir())
```

The screenshot below should serve as an illustration of this interactive
element.

![Interactive and manual control of suggestions in LinkTools
v0.0.1.9005](LinkTools_interactive_matching_gui_README.png)

A final functionality allows to write the new attribute back into the
CWB corpus.

## LinkTools and other Resources

The purpose of `LinkTools` is to provide an integrated user experience
for interested persons wanting to link textual data with other types of
data. The package thus will handle different data types which are
important in the realm of textual data, taking care of preprocessing and
the enrichment of the data in a robust and transparent manner. It
provides access to different existing approaches of linking and linkage,
lowering barriers to make use of available resources.

Focusing on textual data, the code base of the [PolMine
project](https://polmine.github.io/) is at the core of `LinkTools`. In
particular, the R package
[polmineR](https://cran.r-project.org/package=polmineR) is used to
manage large CWB corpora. In the future, a broader scope of input will
be covered. The strict merging in the record linkage workflow is mainly
realized with
[data.table](https://cran.r-project.org/package=data.table). The fuzzy -
or probabilistic - joins are mainly realized using the R packages
[fuzzyjoin](https://cran.r-project.org/package=fuzzyjoin) and
[stringdist](https://cran.r-project.org/package=stringdist).

## Dependencies

Running the Vignette requires the availability of the GermaParlMini CWB
corpus and the `btmp` data package which provides the external data for
linking.

## Installation

``` r
remotes::install_github("PolMine/LinkTools")
```
