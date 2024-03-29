% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LTDataset.R
\name{LTDataset}
\alias{LTDataset}
\title{LTDataset}
\format{
\code{\link{R6Class}} object.
}
\description{
LTDataset

LTDataset
}
\details{
This class facilitates the linkage of datasets via shared unique
identifiers. Three steps are realized by this class: a) the preparation
of datasets which should be linked, i.e. the transformation into a comparable
format and the assignment of shared unique identifiers, b) the merge of
datasets based on these identifiers, c) the encoding or enrichment of the
data with three output formats (data.table, XML or CWB).

\code{additional_attributes} might be useful when information of a
dataset is added which only covers a part of the corpus such as as
specific period of time or specific groups of speakers.

The defaults were changed. "lv" is now the default method,
max_dist is 4 and ignore_case is now TRUE.
}
\section{Usage}{
 LTDataset <- LTDataset$new()
}

\section{Arguments}{
 \describe{
\item{\code{textual_data}}{a character vector of the corpus the IDs should be added to.},
\item{\code{textual_data_type}}{a character vector indicating the type of
textual data (cwb or xml)}
\item{\code{external_resource}}{a data.frame of an external dataset the IDs are coming
from.}
\item{\code{attr_to_add}}{a character vector indicating the ID that should be
added. If named, the name indicates the desired name of the ID as an
structural attribute in the corpus data. The value of the character vector
indicates the column of the external dataset the ID is stored in.}
\item{\code{split_by}}{a character vector indicating a s-attribute the corpus data
should be split by to reduce memory usage.}
\item{\code{match_by}}{a character vector indicating the metadata used to
match the corpus data and the external dataset. If named, the names of the
character vector indicate the names of the s-attributes in the corpus data
and the values indicate the corresponding column names in the external
dataset.}
\item{\code{forced_encoding}}{The desired output encoding of the textual data.
This might be useful when the original encoding and the locale differ.}
\item{\code{verbose}}{boolean, whether or not to print messages. Defaults to FALSE.}
}
}

\section{Methods}{
 \describe{
\item{\code{initialize()}}{initialize a new object of class \code{LTDataset}}
\item{\code{join_textual_and_external_data()}}{perform the actual merge of the data},
\item{\code{external_attribute_to_region_matrix()}}{transform the matched data to
a matrix for encoding.}
\item{\code{check_and_add_missing_values()}}{check the completeness of the merging
operation and add missing values interactively.}
\item{\code{add_missing_attributes_via_shiny()}}{called by \code{check_and_add_missing_values()}
to add missing values interactively.}
\item{\code{create_attribute_region_datatable()}}{returns a data.table of matched
attributes. Can be used for further manual checks.}
\item{\code{encode_new_s_attribute()}}{encodes the new attribute
as a structural attribute (for CWB)}
}
}

\references{
When formulating the data.table join functions in
\code{join_textual_and_external_data()}, the following Stack Overflow links
were useful:
https://stackoverflow.com/questions/45043600/merging-all-column-by-reference-in-a-data-table
and https://stackoverflow.com/questions/44433451/r-data-table-update-join

For fuzzy matching \code{fuzzyjoin} is a crucial dependency:

David Robinson (2020). fuzzyjoin: Join Tables Together on
Inexact Matching. R package version 0.1.6.
https://CRAN.R-project.org/package=fuzzyjoin

Using \code{stringdist_join} in combination with \code{fuzzy_join} in
\code{fuzzy_join_missing_values()} was inspired by the second answer here:
https://stackoverflow.com/questions/48008903/combined-fuzzy-and-exact-matching

Using \code{formals} to reset the default values of
\code{stringdist_join} in \code{fuzzy_join_missing_values()} was inspired by the
second answer here:
https://stackoverflow.com/questions/27673415/store-function-arguments-inside-the-function-and-apply-them-for-future-use-in-r

Large parts of \code{add_missing_attributes_via_shiny()} are taken
from the implementation of rhandsontable and shinyWidgets in: Blaette,
Andreas (2023). polmineR: Verbs and Nouns for Corpus Analysis. R
package version v0.8.8.
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{textual_data}}{a \verb{character vector} of the corpus the IDs
should be added to.}

\item{\code{textual_data_type}}{a \verb{character vector} indicating the type of
textual data (cwb or xml).}

\item{\code{external_resource}}{a \code{data.frame} or \code{data.table} of an external
dataset the IDs are coming from.}

\item{\code{attr_to_add}}{a \verb{character vector} indicating the ID that
should be added. If named, the name indicates the desired name of the
ID as an structural attribute in the corpus data. The value of the
character vector indicates the column of the external dataset the ID is
stored in.}

\item{\code{split_by}}{a \verb{character vector} indicating a s-attribute the
corpus data should be split by to reduce memory usage.}

\item{\code{match_by}}{a \verb{character vector} indicating the metadata used to
match the corpus data and the external dataset. If named, the names of
the character vector indicate the names of the s-attributes in the
corpus data and the values indicate the corresponding column names in
the external dataset.}

\item{\code{forced_encoding}}{a \verb{character vector} of the desired output
encoding of the textual data. This might be useful when the original
encoding and the locale differ.}

\item{\code{attrs_by_region_dt}}{a \code{data.table} of regions that should be
matched in the merge.}

\item{\code{verbose}}{\code{logical} whether or not to print messages. Defaults
to FALSE.}

\item{\code{attribute_name_in_corpus}}{a \code{character} the desired name of the
attribute to add in the text data.}

\item{\code{attribute_in_external_resource}}{a \code{character} the name of the
attribute to add in the external resource.}

\item{\code{text_dt}}{a \code{data.table} in case of a cwb corpus, this
contains the decoded corpus.}

\item{\code{encoding_method}}{a \verb{character vector} of the encoding method
used to create the corpus (R or CWB).}

\item{\code{values}}{a \verb{character vector} of values to encode after the
merge.}

\item{\code{region_matrix}}{a \code{matrix} of corpus positions corresponding to
the values in the value field.}

\item{\code{cpos_left}}{a \code{integer} of the left cpos boundary of the corpus
object.}

\item{\code{cpos_right}}{a \code{integer} of the right cpos boundary of the
corpus object.}

\item{\code{missing_after_check}}{a \code{data.table} of observations still
missing after the manual check}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-LTDataset-new}{\code{LTDataset$new()}}
\item \href{#method-LTDataset-print}{\code{LTDataset$print()}}
\item \href{#method-LTDataset-join_textual_and_external_data}{\code{LTDataset$join_textual_and_external_data()}}
\item \href{#method-LTDataset-external_attribute_to_region_matrix}{\code{LTDataset$external_attribute_to_region_matrix()}}
\item \href{#method-LTDataset-encode_new_s_attribute}{\code{LTDataset$encode_new_s_attribute()}}
\item \href{#method-LTDataset-create_attribute_region_datatable}{\code{LTDataset$create_attribute_region_datatable()}}
\item \href{#method-LTDataset-check_and_add_missing_values}{\code{LTDataset$check_and_add_missing_values()}}
\item \href{#method-LTDataset-add_missing_attributes_via_shiny}{\code{LTDataset$add_missing_attributes_via_shiny()}}
\item \href{#method-LTDataset-fuzzy_join_missing_values}{\code{LTDataset$fuzzy_join_missing_values()}}
\item \href{#method-LTDataset-stringdist_join_match_fun}{\code{LTDataset$stringdist_join_match_fun()}}
\item \href{#method-LTDataset-add_missing_regions}{\code{LTDataset$add_missing_regions()}}
\item \href{#method-LTDataset-clone}{\code{LTDataset$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LTDataset-new"></a>}}
\if{latex}{\out{\hypertarget{method-LTDataset-new}{}}}
\subsection{Method \code{new()}}{
initialize a new object of class \code{LTDataset}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LTDataset$new(
  textual_data = NULL,
  textual_data_type = NULL,
  external_resource = NULL,
  attr_to_add = NULL,
  match_by = NULL,
  split_by = NULL,
  verbose = TRUE,
  forced_encoding = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{textual_data}}{a character vector of the corpus the IDs should be
added to.}

\item{\code{textual_data_type}}{a character vector indicating the type of
textual data (cwb or xml).}

\item{\code{external_resource}}{a data.frame of an external dataset the IDs are
coming from.}

\item{\code{attr_to_add}}{a character vector indicating the ID that should be
added. If named, the name indicates the desired name of the ID as an
structural attribute in the corpus data. The value of the character
vector indicates the column of the external dataset the ID is stored
in.}

\item{\code{match_by}}{a character vector indicating the metadata used to match
the corpus data and the external dataset. If named, the names of the
character vector indicate the names of the s-attributes in the corpus
data and the values indicate the corresponding column names in the
external dataset.}

\item{\code{split_by}}{a character vector indicating a s-attribute the corpus
data should be split by to reduce memory usage.}

\item{\code{verbose}}{logical, whether or not to print messages. Defaults to FALSE.}

\item{\code{forced_encoding}}{The desired output encoding of the textual data.
This might be useful when the original encoding and the locale differ.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{LTDataset} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LTDataset-print"></a>}}
\if{latex}{\out{\hypertarget{method-LTDataset-print}{}}}
\subsection{Method \code{print()}}{
print class \code{LTDataset}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LTDataset$print()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LTDataset-join_textual_and_external_data"></a>}}
\if{latex}{\out{\hypertarget{method-LTDataset-join_textual_and_external_data}{}}}
\subsection{Method \code{join_textual_and_external_data()}}{
perform the actual merge of the data.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LTDataset$join_textual_and_external_data(na_value = "NA")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{na_value}}{a \verb{character vector} indicating which value
attributes should have that aren't merged.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LTDataset-external_attribute_to_region_matrix"></a>}}
\if{latex}{\out{\hypertarget{method-LTDataset-external_attribute_to_region_matrix}{}}}
\subsection{Method \code{external_attribute_to_region_matrix()}}{
transform the matched data to a matrix for encoding.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LTDataset$external_attribute_to_region_matrix()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LTDataset-encode_new_s_attribute"></a>}}
\if{latex}{\out{\hypertarget{method-LTDataset-encode_new_s_attribute}{}}}
\subsection{Method \code{encode_new_s_attribute()}}{
encodes the new attribute as a structural attribute (for CWB).
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LTDataset$encode_new_s_attribute()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A new \code{LTDataset} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LTDataset-create_attribute_region_datatable"></a>}}
\if{latex}{\out{\hypertarget{method-LTDataset-create_attribute_region_datatable}{}}}
\subsection{Method \code{create_attribute_region_datatable()}}{
returns a data.table of matched attributes. Can be used for
further manual checks.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LTDataset$create_attribute_region_datatable(additional_attributes = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{additional_attributes}}{\verb{a character vector} of additional
structural attributes which should be considered when evaluating the
results of the linkage.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LTDataset-check_and_add_missing_values"></a>}}
\if{latex}{\out{\hypertarget{method-LTDataset-check_and_add_missing_values}{}}}
\subsection{Method \code{check_and_add_missing_values()}}{
check the completeness of the merging operation and add
missing values interactively.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LTDataset$check_and_add_missing_values(
  check_for_groups = NULL,
  negate = FALSE,
  modify = FALSE,
  match_fuzzily_by = NULL,
  doc_dir = NULL,
  ignore_case = TRUE,
  dist_method = "lv",
  max_dist = 4L,
  verbose = TRUE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{check_for_groups}}{a named \code{list}; in case not all elements are
expected to have been matched, select which elements should be checked.}

\item{\code{negate}}{\code{logical}; whether the selection in \code{check_for_groups}
should be negated, i.e. if these elements should be filtered instead of
selected.}

\item{\code{modify}}{\code{logical}; whether missing values should not only be
inspected but also modified interactively.}

\item{\code{match_fuzzily_by}}{a \verb{character vector}; if not NULL, a fuzzy
match will be performed on the column indicated.}

\item{\code{doc_dir}}{a \verb{character vector}; Indicating a directory in which
a text file is created which documents manual changes to the merge.}

\item{\code{ignore_case}}{\code{logical} whether to match the case. This is a
argument of \code{stringdist_join} which is used in
\code{fuzzy_join_missing_values}}

\item{\code{dist_method}}{A \code{character} The measurement of distance used by
\code{stringdist_join}. See \code{stringdist-metrics} in the \code{stringdist} package
which is used in \code{fuzzy_join_missing_values}.}

\item{\code{max_dist}}{An \code{integer} value indicating the maximum distance
between the two input vectors. This is a argument of \code{stringdist_join}
which is used in \code{fuzzy_join_missing_values}.}

\item{\code{verbose}}{\code{logical}; whether to print more comprehensive
messages.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LTDataset-add_missing_attributes_via_shiny"></a>}}
\if{latex}{\out{\hypertarget{method-LTDataset-add_missing_attributes_via_shiny}{}}}
\subsection{Method \code{add_missing_attributes_via_shiny()}}{
Add missing values with shiny and rhandsontable. Called by
\code{check_and_add_missing_values()}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LTDataset$add_missing_attributes_via_shiny(y, doc_dir)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{y}}{a \code{data.table} containing incomplete rows after the merge.}

\item{\code{doc_dir}}{a \verb{character vector}; Indicating a directory in which
a text file is created which documents manual changes to the merge.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LTDataset-fuzzy_join_missing_values"></a>}}
\if{latex}{\out{\hypertarget{method-LTDataset-fuzzy_join_missing_values}{}}}
\subsection{Method \code{fuzzy_join_missing_values()}}{
Add missing values using a fuzzy join. Called by
\code{check_and_add_missing_values()}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LTDataset$fuzzy_join_missing_values(
  attrs_by_region_dt_min,
  match_fuzzily_by,
  ignore_case = TRUE,
  dist_method = "lv",
  max_dist = 4L
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{attrs_by_region_dt_min}}{a \code{data.table} containing incomplete rows
after the merge.}

\item{\code{match_fuzzily_by}}{a \verb{character vector}; if not NULL, a fuzzy match
will be performed on the column indicated.}

\item{\code{ignore_case}}{\code{logical} whether to match the case. This is a
argument of \code{stringdist_join}.}

\item{\code{dist_method}}{A \code{character} The measurement of distance used by
\code{stringdist_join}. See \code{stringdist-metrics} in the \code{stringdist}
package.}

\item{\code{max_dist}}{An \code{integer} value indicating the maximum distance
between the two input vectors. This is a argument of \code{stringdist_join}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LTDataset-stringdist_join_match_fun"></a>}}
\if{latex}{\out{\hypertarget{method-LTDataset-stringdist_join_match_fun}{}}}
\subsection{Method \code{stringdist_join_match_fun()}}{
The function used for those attributes which are matched
fuzzily. Called by \code{fuzzy_join_missing_values()}. This function is
identical to the "match_fun" of \code{stringdist_join()} of the \code{fuzzyjoin}
package with changed defaults.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LTDataset$stringdist_join_match_fun(
  v1,
  v2,
  max_dist = 4L,
  method = c("lv", "osa", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw",
    "soundex"),
  ignore_case = TRUE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{v1, v2}}{\verb{character vectors} to be compared fuzzily.}

\item{\code{max_dist}}{An \code{integer} value indicating the maximum distance
between the two input vectors. This is a argument of \code{stringdist_join}.}

\item{\code{method}}{A \code{character} The measurement of distance used by
\code{stringdist_join}. See \code{stringdist-metrics} in the \code{stringdist}
package.}

\item{\code{ignore_case}}{\code{logical} whether to match the case. This is a
argument of \code{stringdist_join}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LTDataset-add_missing_regions"></a>}}
\if{latex}{\out{\hypertarget{method-LTDataset-add_missing_regions}{}}}
\subsection{Method \code{add_missing_regions()}}{
The function checks if the entire corpus is covered by the
region matrix. If not, the missing regions of the corpus are added to
the region matrix and the value vector. This is needed to encode the
new data.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LTDataset$add_missing_regions()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LTDataset-clone"></a>}}
\if{latex}{\out{\hypertarget{method-LTDataset-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LTDataset$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
