#' LTDataset
#'
#' This class facilitates the linkage of datasets via shared unique
#' identifiers. Three steps are realized by this class: a) the preparation
#' of datasets which should be linked, i.e. the transformation into a comparable
#' format and the assignment of shared unique identifiers, b) the merge of
#' datasets based on these identifiers, c) the encoding or enrichment of the
#' data with three output formats (data.table, XML or CWB).
#'
#' @section Usage: LTDataset <- LTDataset$new()
#' 
#' @format \code{\link{R6Class}} object.
#' @section Arguments: \describe{
#'    \item{`textual_data`}{a character vector of the corpus the IDs should be added to.},
#'    \item{`textual_data_type`}{a character vector indicating the type of 
#'    textual data (cwb or xml)}
#'    \item{`external_resource`}{a data.frame of an external dataset the IDs are coming
#'   from.}
#'   \item{`attr_to_add`}{a character vector indicating the ID that should be
#'   added. If named, the name indicates the desired name of the ID as an
#'   structural attribute in the corpus data. The value of the character vector
#'   indicates the column of the external dataset the ID is stored in.}
#'   \item{`split_by`}{a character vector indicating a s-attribute the corpus data
#'   should be split by to reduce memory usage.}
#'   \item{`match_by`}{a character vector indicating the metadata used to
#'   match the corpus data and the external dataset. If named, the names of the
#'   character vector indicate the names of the s-attributes in the corpus data
#'   and the values indicate the corresponding column names in the external
#'   dataset.}
#'   \item{`forced_encoding`}{The desired output encoding of the textual data.
#'   This might be useful when the original encoding and the locale differ.}
#'   \item{`verbose`}{boolean, whether or not to print messages. Defaults to FALSE.}
#'   }
#' @section Methods: \describe{
#'    \item{`initialize()`}{initialize a new object of class `LTDataset`}
#'   \item{`join_textual_and_external_data()`}{perform the actual merge of the data},
#'   \item{`external_attribute_to_region_matrix()`}{transform the matched data to
#'   a matrix for encoding.}
#'   \item{`check_and_add_missing_values()`}{check the completeness of the merging
#'   operation and add missing values interactively.}
#'   \item{`add_missing_attributes_via_shiny()`}{called by `check_and_add_missing_values()`
#'   to add missing values interactively.}
#'   \item{`create_attribute_region_datatable()`}{returns a data.table of matched
#'   attributes. Can be used for further manual checks.}
#'   \item{`encode_new_s_attribute()`}{encodes the new attribute
#'   as a structural attribute (for CWB)}
#' }
#' @export LTDataset
#' @importFrom magrittr %>%
#' @importFrom R6 R6Class
#' @importFrom data.table data.table := is.data.table setnames setorder rleid rleidv
#' @importFrom polmineR size s_attributes corpus decode
#' @importFrom cwbtools s_attribute_encode registry_file_parse
#' @importFrom RcppCWB cl_cpos2struc cl_struc2str
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel 
#' @importFrom shiny fillRow reactiveValues reactive stopApp observeEvent runGadget paneViewer
#' @importFrom rhandsontable rHandsontableOutput renderRHandsontable hot_to_r hot_col hot_cols hot_table
#' @importFrom stringr str_to_lower str_length
#' @importFrom stringdist stringdist
#' @importFrom fuzzyjoin fuzzy_join
LTDataset <- R6Class(
  
  "LTDataset",
  
  public = list(
    
    # fields
    
    #' @field textual_data a \code{character vector} of the corpus the IDs
    #'   should be added to.
    #' @field textual_data_type a \code{character vector} indicating the type of
    #'   textual data (cwb or xml).
    #' @field external_resource a \code{data.frame or data.table} of an external
    #'   dataset the IDs are coming from.
    #' @field attr_to_add a \code{character vector} indicating the ID that
    #'   should be added. If named, the name indicates the desired name of the
    #'   ID as an structural attribute in the corpus data. The value of the
    #'   character vector indicates the column of the external dataset the ID is
    #'   stored in.
    #' @field split_by a \code{character vector} indicating a s-attribute the
    #'   corpus data should be split by to reduce memory usage.
    #' @field match_by a \code{character vector} indicating the metadata used to
    #'   match the corpus data and the external dataset. If named, the names of
    #'   the character vector indicate the names of the s-attributes in the
    #'   corpus data and the values indicate the corresponding column names in
    #'   the external dataset.
    #' @field forced_encoding a \code{character vector} of the desired output
    #'   encoding of the textual data. This might be useful when the original
    #'   encoding and the locale differ.
    #' @field attrs_by_region_dt a \code{data.table} of regions that should be
    #'   matched in the merge.
    #' @field verbose \code{logical} whether or not to print messages. Defaults
    #'   to FALSE.
    #' @field attribute_name_in_corpus a \code{character} the desired name of the
    #'   attribute to add in the text data.
    #' @field attribute_in_external_resource a \code{character} the name of the
    #'   attribute to add in the external resource.
    #' @field text_dt a \code{data.table} in case of a cwb corpus, this
    #'   contains the decoded corpus.
    #' @field encoding_method a \code{character vector} of the encoding method
    #'   used to create the corpus (R or CWB).
    #' @field values a \code{character vector} of values to encode after the
    #'   merge.
    #' @field region_matrix a \code{matrix} of corpus positions corresponding to
    #'   the values in the value field.
    #' @field cpos_left a \code{integer} of the left cpos boundary of the corpus
    #'   object.
    #' @field cpos_right a \code{integer} of the right cpos boundary of the
    #'   corpus object.
    #' @field missing_after_check a \code{data.table} of observations still
    #'   missing after the manual check
    textual_data = NULL,
    textual_data_type = NULL,
    external_resource = NULL,
    attr_to_add = NULL,
    split_by = NULL,
    match_by = NULL,
    forced_encoding = NULL,
    attrs_by_region_dt = NULL,
    verbose = TRUE,

    attribute_name_in_corpus = NULL,
    attribute_in_external_resource = NULL,
    
    text_dt = NULL,
    encoding_method = "R",
    values = NULL,
    region_matrix = NULL,
    cpos_left = NULL,
    cpos_right = NULL,
    missing_after_check = NULL,
    
    # methods
    # R6 documentation with Roxygen
    
    #' @description initialize a new object of class \code{LTDataset}.
    #' @param textual_data a character vector of the corpus the IDs should be
    #'   added to.
    #' @param textual_data_type a character vector indicating the type of
    #'   textual data (cwb or xml).
    #' @param external_resource a data.frame of an external dataset the IDs are
    #'   coming from.
    #' @param attr_to_add a character vector indicating the ID that should be
    #'   added. If named, the name indicates the desired name of the ID as an
    #'   structural attribute in the corpus data. The value of the character
    #'   vector indicates the column of the external dataset the ID is stored
    #'   in.
    #' @param match_by a character vector indicating the metadata used to match
    #'   the corpus data and the external dataset. If named, the names of the
    #'   character vector indicate the names of the s-attributes in the corpus
    #'   data and the values indicate the corresponding column names in the
    #'   external dataset.
    #' @param split_by a character vector indicating a s-attribute the corpus
    #'   data should be split by to reduce memory usage.
    #' @param forced_encoding The desired output encoding of the textual data.
    #'   This might be useful when the original encoding and the locale differ.
    #' @param verbose logical, whether or not to print messages. Defaults to FALSE.
    #' @return A new `LTDataset` object.
    initialize = function(textual_data = NULL,
                          textual_data_type = NULL,
                          external_resource = NULL,
                          attr_to_add = NULL,
                          match_by = NULL,
                          split_by = NULL,
                          verbose = TRUE,
                          forced_encoding = NULL){
      
      stopifnot(!is.null(textual_data),
                !is.null(textual_data_type),
                is.data.frame(external_resource),
                is.character(attr_to_add),
                is.character(match_by))
      
      self$textual_data <- textual_data
      self$textual_data_type <- textual_data_type
      self$split_by <- split_by
      self$forced_encoding <- forced_encoding

      # if the columns are the same, there might not be a name.
      if (is.null(names(match_by))) {
        names(match_by) <- match_by
      }
      
      self$match_by <- match_by
      
      # Checks which are independent from the corpus file format
      
      if (!is.null(names(attr_to_add))) {
        self$attribute_name_in_corpus <- names(attr_to_add)
        self$attribute_in_external_resource <- as.character(attr_to_add)
        names(self$attribute_in_external_resource) <- self$attribute_name_in_corpus  # naming is just to rename the columns accordingly.
      } else {
        # else it is the same
        self$attribute_name_in_corpus <- attr_to_add
        self$attribute_in_external_resource <- attr_to_add
        names(self$attribute_in_external_resource) <- attr_to_add # naming is just to rename the columns accordingly.
      }
      
      if (!is.null(self$textual_data) & self$textual_data_type == "cwb") {
        
        if (!is.character(self$textual_data)) {
          
          # if textual_data is no character, it might be a subcorpus, etc. Take
          # the corpus from slot.
          
          if ("corpus" %in% slotNames(self$textual_data)) {
            self$cpos_left = self$textual_data@cpos[1, 1] # take first row, first column from the cpos slot of the object
            self$cpos_right = self$textual_data@cpos[nrow(self$textual_data@cpos), 2] # take last row, second column from the cpos slot of the object
          }
          
        } else {
          # if it is a corpus, use entire range of cpos.
          self$cpos_left <- 0L
          self$cpos_right <- polmineR::size(self$textual_data) - 1
        }
        
        if (!is.null(self$textual_data) & self$attribute_name_in_corpus %in% polmineR::s_attributes(self$textual_data)) {
          
          stop("... attribute to add already exists.")

          # the new var already exists as an s-attribute. What to do? modify,
          # overwrite, stop? Modification would be best, keeping entries which aren't
          # "NA"?
          
        }
        
        if (any(!names(self$match_by) %in% polmineR::s_attributes(self$textual_data))) stop("... not all structural attributes in match_by are in corpus.")
        if (any(!as.character(self$match_by) %in% colnames(external_resource))) stop("... not all variables in match_by are in the external dataset.")
        
        if (!is.null(self$split_by)) {
          if (!self$split_by %in% polmineR::s_attributes(self$textual_data)) {
            stop(sprintf("... the variable defined in split_by (%s) is no structural attribute in %s",
                         self$split_by,
                         self$textual_data))
          }
        }
        
      } else if (is.character(self$textual_data) & self$textual_data_type == "xml") {
        # in this case, it is assumed that the input is a path to either a xml directory or an xml_file
        stop("xml not yet implemented.")
      } else if (is.character(self$textual_data) & self$textual_data_type == "quanteda") {
        stop("quanteda corpora not yet implemented.")
      } else {
        stop("Type not provided or not supported.")
      }
      
      if (!data.table::is.data.table(external_resource)) {
        external_resource <- data.table::as.data.table(external_resource)
        # otherwise these ".." assignments don't work.
      }
      
      cols_to_select <- c(self$match_by, self$attribute_in_external_resource)
      self$external_resource <- external_resource[, ..cols_to_select]
      
      # we set the names of the resource data.frame here. The setNames part
      # switches names and values of the previously assigned vector
      # match_by.
      
      data.table::setnames(self$external_resource, setNames(names(cols_to_select), cols_to_select))
      invisible(self)
    },
    
    #' @description print class \code{LTDataset}.
    print = function() {
      
      if (inherits(self$textual_data, "subcorpus")) {
        corpus_obj_name <- paste("Subcorpus of", self$textual_data@corpus)
      } else {
        
        corpus_obj_name <- switch(self$textual_data_type, 
                                  "cwb" = self$textual_data, 
                                  "xml" = "XML Files", 
                                  "quanteda" = "tba.")
      }
      
      cat("LTDataset Object: \n")
      cat("  Corpus: ", corpus_obj_name, "\n", sep = "")
      cat("  Attribute to be added:  ", self$attribute_name_in_corpus, "\n", sep = "")
      cat("  via:  ", paste(names(self$match_by), collapse = ", "), "\n", sep = "")
      invisible(self)
    }, 
    
    #' @description perform the actual merge of the data.
    #' @param na_value a \code{character vector} indicating which value
    #'   attributes should have that aren't merged.
    #' @references When formulating the data.table join functions in
    #' `join_textual_and_external_data()`, the following Stack Overflow links
    #' were useful:
    #' https://stackoverflow.com/questions/45043600/merging-all-column-by-reference-in-a-data-table
    #' and https://stackoverflow.com/questions/44433451/r-data-table-update-join
    join_textual_and_external_data = function(na_value = "NA") {
      
      if (self$textual_data_type == "cwb") {
        
        if (!is.null(self$split_by)) {
          
          # we create a data.table which already has all rows and columns and
          # update-join the real data later. This should be a bit faster and more
          # memory efficient.
          
          if (self$verbose) message("... creating empty cpos data.table.")
          self$text_dt <- data.table::data.table(cpos = self$cpos_left:self$cpos_right,
                                                 key = "cpos")
          
          self$text_dt[, eval(names(self$match_by)) := "NA"]
          self$text_dt[, eval(self$attribute_name_in_corpus) := na_value]
          
          # Note: This can be a big table.
          
          # split. Depends on whether this is a corpus object
          
          if (is.character(self$textual_data)) {
            split_text_object <- polmineR::corpus(self$textual_data) %>%
              polmineR::split(s_attribute = self$split_by)
          } else {
            # it might be a subcorpus, etc.
            split_text_object <- self$textual_data %>%
              polmineR::split(s_attribute = self$split_by)
            }
          
          if (self$verbose) message("... for each split, retrieve s-attributes for comparison.")
          
          garbage <- lapply(1:length(split_text_object), function(i_split) {
            
            if (self$verbose) message(sprintf("... processing split %s out of %s based on s-attribute %s.",
                                              i_split,
                                              length(split_text_object),
                                              self$split_by))
            
            if (self$verbose) message("...... extract s-attributes the data should be matched by.")
            
            current_split_cpos <- min(split_text_object[[i_split]]@cpos):max(split_text_object[[i_split]]@cpos)
            
            if (is.character(self$textual_data)) {
              corpus_name <- self$textual_data
            } else {
              corpus_name <- self$textual_data@corpus
            }

            s_attr_streams_for_split <- lapply(names(self$match_by), function(s_attr) {
              
              retval <- RcppCWB::cl_cpos2struc(corpus = corpus_name,
                                               s_attribute = s_attr,
                                               cpos = current_split_cpos) %>%
                RcppCWB::cl_struc2str(corpus = corpus_name,
                                      s_attribute = s_attr,
                                      struc = .)
              
              # sometimes if the corpus is latin1 and the locale is UTF-8
              if (!is.null(self$forced_encoding)) retval <- iconv(retval, polmineR::encoding(corpus_name), self$forced_encoding)
              return(retval)
            }
            )
            
            if (self$verbose) message("...... bind decoded s-attributes.")
            
            s_attr_streams_for_split_df <- do.call("cbind", s_attr_streams_for_split) %>%
              as.data.table() %>%
              data.table::setnames(names(self$match_by))
            
            rm(s_attr_streams_for_split)
            
            if (self$verbose) message("...... add cpos to split decoded stream.")
            
            # add cpos and split from above
            s_attr_streams_for_split_df[, cpos := current_split_cpos]
            
            if (self$verbose) message(sprintf('...... split based on s-attribute %s with column names "%s" finished.', self$split_by, paste(names(s_attr_streams_for_split_df), collapse = ", ")))
            
            # now merge. 
            # this should be done in the split because this join can be quite costly.
            
            if (self$verbose) message(sprintf('...... merge external attribute with decoded split stream.'))
            
            s_attr_streams_for_split_df[self$external_resource, on = names(self$match_by), 
                                        (self$attribute_name_in_corpus) := get(paste0("i.", self$attribute_name_in_corpus))]
            
            # Note: We renamed the columns in the external resource above which is why
            # we use the self$attribute_name_in_corpus name for both sides.
            
            if (self$verbose) message("...... bind split stream to the nearly empty data.table.")
            data.table::setkey(s_attr_streams_for_split_df, cpos)
            
            cols_to_add <- setdiff(names(s_attr_streams_for_split_df), "cpos")
            
            self$text_dt[s_attr_streams_for_split_df, (cols_to_add) := mget(paste0("i.", cols_to_add))]
            
            rm(s_attr_streams_for_split_df)
            return(NULL)
          }
          )
          
        } else {
          
          if (self$verbose) message("... decoding the entire object at once.")
          
          self$text_dt <- polmineR::decode(self$textual_data,
                                           s_attributes = names(self$match_by),
                                           p_attributes = character(),
                                           to = "data.table")
          
          # we remove struc which introduces a lot more regions than probably necessary.
          if ("struc" %in% colnames(self$text_dt)) {
            self$text_dt[, struc := NULL]
          }
          
          if (self$verbose) message(sprintf('...... merge external attribute with decoded token stream.'))
          
          self$text_dt[self$external_resource, on = names(self$match_by),
                       (self$attribute_name_in_corpus) := get(paste0("i.", self$attribute_name_in_corpus))]

        }
        
        # if CWB, then make region matrix
        self$external_attribute_to_region_matrix()
        
      } else {
        message("... not yet implemented.")
      }
      
      invisible(self)
    }, 
    
    #' @description transform the matched data to a matrix for encoding.
    external_attribute_to_region_matrix = function() {
      
      cpos_vec <- self$text_dt[["cpos"]]
      
      if (self$verbose) message("... preparing breaks for the encoding process.")
      
      # prepare breaks every time the speaker changes
      self$text_dt[, row_index := data.table::rleidv(self$text_dt, cols = setdiff(colnames(self$text_dt), c("cpos", self$attribute_name_in_corpus)))]

      breaks <- match(unique(self$text_dt$row_index), self$text_dt$row_index)
      cpos_breaks <- c(cpos_vec[breaks], cpos_vec[length(cpos_vec)])

      id_factor <- cut(
        x = cpos_vec,
        breaks = cpos_breaks,
        include.lowest = TRUE,
        right = FALSE
      )
      
      if (self$verbose) message("... preparing the region matrix for the encoding process.")
      
      id_cpos <- unname(split(x = cpos_vec, f = id_factor))
      self$region_matrix <- do.call(rbind, lapply(id_cpos, function(cpos) c(cpos[1L], cpos[length(cpos)])))
      
      self$text_dt[, cpos := NULL]
      self$text_dt <- unique(self$text_dt) # this might not be very memory effective
      self$values <- as.character(self$text_dt[[self$attribute_name_in_corpus]])
      
      # Last Sanity Checks

      # the last cpos of the region matrix must be equal to the size of
      # the corpus (-1 for 0-indexation).

      cpos_start <- self$region_matrix[1, 1]
      cpos_end <- self$region_matrix[nrow(self$region_matrix), 2]
      
      stopifnot((cpos_end - cpos_start) == (polmineR::size(self$textual_data) - 1))
      
      # there must be as many values as regions
      stopifnot(length(self$values) == nrow(self$region_matrix))
      invisible(self)
    }, 
    
    #' @description encodes the new attribute as a structural attribute (for CWB).
    #' @return A new `LTDataset` object.
    encode_new_s_attribute = function() {
      
      # if this is only a subcorpus, then we might want to add all cpos which
      # are not part of the subcorpus and add a value of "NA" to the missing
      # cpos.

      self$add_missing_regions()

      # replace all missing/NA values with literal "NA". Not done earlier
      # because in the shiny application user input is still possible.
      
      self$values[is.na(self$values)] <- "NA"

      corpus_name <- ifelse(is.character(self$textual_data), self$textual_data, self$textual_data@corpus)
      corpus_registry <- RcppCWB::corpus_registry_dir(corpus = corpus_name)

      corpus_encoding <- cwbtools::registry_file_parse(corpus = corpus_name,
                                                       registry = corpus_registry)[["properties"]][["charset"]]

      if (is.character(self$textual_data)) {
        data_dir_path <- RcppCWB::corpus_data_dir(corpus = self$textual_data,
                                                  registry = corpus_registry)
      } else {
        data_dir_path <- RcppCWB::corpus_data_dir(corpus = self$textual_data@corpus,
                                                  registry = corpus_registry)
      }

      if (self$verbose) message("... start encoding the s-attribute.")

      cwbtools::s_attribute_encode(
        values = self$values,
        data_dir = data_dir_path,
        s_attribute = tolower(self$attribute_name_in_corpus),
        corpus = corpus_name,
        region_matrix = self$region_matrix,
        method = self$encoding_method,
        registry_dir = corpus_registry,
        encoding = corpus_encoding,
        delete = TRUE,
        verbose = FALSE
      )
      
      if (self$verbose) message("... done encoding the s-attribute.")
      invisible(self)
    }, 
    
    #' @description returns a data.table of matched attributes. Can be used for
    #'   further manual checks.
    #' @param additional_attributes `a character vector` of additional
    #'   structural attributes which should be considered when evaluating the
    #'   results of the linkage.
    #' @param verbose \code{logical}
    #' @details `additional_attributes` might be useful when information of a
    #'   dataset is added which only covers a part of the corpus such as as
    #'   specific period of time or specific groups of speakers.
    create_attribute_region_datatable = function(verbose = FALSE, additional_attributes = NULL) {

      if (!is.null(additional_attributes)) {
        stopifnot(additional_attributes %in% polmineR::s_attributes(self$textual_data))
      }

      attributes_to_check <- c(names(self$match_by), additional_attributes)
      
      if (is.character(self$textual_data)) {
        corpus_name <- self$textual_data
        cwb_registry <- RcppCWB::corpus_registry_dir(self$textual_data)
      } else {
        corpus_name <- self$textual_data@corpus
        cwb_registry <- RcppCWB::corpus_registry_dir(self$textual_data@corpus)
      }

      attrs_by_region <- lapply(attributes_to_check, function(attr_to_check) {
        retval <- RcppCWB::cl_cpos2struc(corpus_name, s_attribute = attr_to_check, cpos = self$region_matrix[, 1], registry = cwb_registry) %>%
          RcppCWB::cl_struc2str(corpus = corpus_name, s_attribute = attr_to_check, struc = ., registry = cwb_registry)
        
        if (!is.null(self$forced_encoding)) retval <- iconv(retval, polmineR::encoding(corpus_name), self$forced_encoding)
        return(retval)
        
      }
      )
      
      self$attrs_by_region_dt <- data.table::as.data.table(do.call("cbind", attrs_by_region))
      rm(attrs_by_region)
      
      if (!is.null(additional_attributes)) {
        colnames(self$attrs_by_region_dt) <- c(names(self$match_by), additional_attributes)
      } else {
        colnames(self$attrs_by_region_dt) <- names(self$match_by)
      }

      self$attrs_by_region_dt[, (self$attribute_name_in_corpus) := self$values]
      invisible(self)
      
    }, 
    
    #' @description check the completeness of the merging operation and add
    #'   missing values interactively.
    #' @param check_for_groups a named `list`; in case not all elements are
    #'   expected to have been matched, select which elements should be checked.
    #' @param negate `logical`; whether the selection in `check_for_groups`
    #'   should be negated, i.e. if these elements should be filtered instead of
    #'   selected.
    #' @param modify `logical`; whether missing values should not only be
    #'   inspected but also modified interactively.
    #' @param match_fuzzily_by a `character vector`; if not NULL, a fuzzy
    #'   match will be performed on the column indicated.
    #' @param doc_dir a `character vector`; Indicating a directory in which
    #'   a text file is created which documents manual changes to the merge.
    #' @param ignore_case `logical` whether to match the case. This is a
    #'   argument of `stringdist_join` which is used in
    #'   `fuzzy_join_missing_values`
    #' @param dist_method A `character` The measurement of distance used by
    #'   `stringdist_join`. See `stringdist-metrics` in the `stringdist` package
    #'   which is used in `fuzzy_join_missing_values`.
    #' @param max_dist An `integer` value indicating the maximum distance
    #'   between the two input vectors. This is a argument of `stringdist_join`
    #'   which is used in `fuzzy_join_missing_values`.
    #' @param verbose `logical`; whether to print more comprehensive
    #'   messages.
    #' @references For fuzzy matching `fuzzyjoin` is a crucial dependency:
    #'
    #' David Robinson (2020). fuzzyjoin: Join Tables Together on
    #'   Inexact Matching. R package version 0.1.6.
    #'   https://CRAN.R-project.org/package=fuzzyjoin
    #' @references Using `stringdist_join` in combination with `fuzzy_join` in
    #'   `fuzzy_join_missing_values()` was inspired by the second answer here:
    #'   https://stackoverflow.com/questions/48008903/combined-fuzzy-and-exact-matching
    #' @references Using `formals` to reset the default values of
    #'   `stringdist_join` in `fuzzy_join_missing_values()` was inspired by the
    #'   second answer here:
    #'   https://stackoverflow.com/questions/27673415/store-function-arguments-inside-the-function-and-apply-them-for-future-use-in-r
    #' @references Large parts of `add_missing_attributes_via_shiny()` are taken
    #'   from the implementation of rhandsontable and shinyWidgets in: Blaette,
    #'   Andreas (2023). polmineR: Verbs and Nouns for Corpus Analysis. R
    #'   package version v0.8.8.
    check_and_add_missing_values = function(check_for_groups = NULL,
                                            negate = FALSE,
                                            modify = FALSE,
                                            match_fuzzily_by = NULL,
                                            doc_dir = NULL,
                                            ignore_case = TRUE,
                                            dist_method = "lv",
                                            max_dist = 4L,
                                            verbose = TRUE) {
      
      if (!is.null(check_for_groups)) {
        if (verbose) message("... subsetting by groups which should have been matched.")
        for (i in 1:length(check_for_groups)) {
          group_name <- names(check_for_groups)[[i]]
          if (isTRUE(negate)) {
            attrs_by_region_dt_min <- self$attrs_by_region_dt[!get(group_name) %in% check_for_groups[[i]], ]
          } else {
            attrs_by_region_dt_min <- self$attrs_by_region_dt[get(group_name) %in% check_for_groups[[i]], ]
          }
          attrs_by_region_dt_min <- unique(attrs_by_region_dt_min[is.na(get(self$attribute_name_in_corpus))])
        }
      } else {
        attrs_by_region_dt_min <- unique(self$attrs_by_region_dt[is.na(get(self$attribute_name_in_corpus))])
      }
      
      if (nrow(attrs_by_region_dt_min) > 0) {
        
        if (isTRUE(modify)) {
          
          add_manually <- menu(title = "After inspecting the results, do you want to add values for the missing attributes manually?\n\nThese additions are added to the value vector used for encoding.\n\nThese manual additions are documented in a log file.\n\nAlternatively, modify ID resource and redo.",
                               choices = c("Yes", "No"))
          
          if (!exists("add_manually") || add_manually == 2) {
            # do nothing
          } else {
            
            if (is.null(doc_dir)) stop("No existing directory provided.")
            
            if (!is.null(match_fuzzily_by)) {
              
              # if it is allowed to match fuzzily, the attribute stated in the
              # attribute is matched via fuzzy matching while all other
              # variables are used for literate matching.
              
              attrs_by_region_dt_min_mod <- self$fuzzy_join_missing_values(attrs_by_region_dt_min = attrs_by_region_dt_min,
                                                                           match_fuzzily_by = match_fuzzily_by,
                                                                           ignore_case = ignore_case,
                                                                           dist_method = dist_method,
                                                                           max_dist = max_dist)

              attrs_by_region_dt_min_mod <- self$add_missing_attributes_via_shiny(y = attrs_by_region_dt_min_mod,
                                                                                  doc_dir = doc_dir)
            } else {

              attrs_by_region_dt_min_mod <- self$add_missing_attributes_via_shiny(y = attrs_by_region_dt_min,
                                                                                  doc_dir = doc_dir)
            }

            # first check if any of the values should not be kept
            keep_not_idx <- which(attrs_by_region_dt_min_mod[["keep"]] == FALSE)
            if (length(keep_not_idx) > 0) attrs_by_region_dt_min_mod <- attrs_by_region_dt_min_mod[-keep_not_idx, ]
            
            # then we need to check if each speaker only occurs once with the
            # actual attribute columns.
            cols_to_keep_after_shiny <- c(names(self$match_by), self$attribute_name_in_corpus)
            
            if (nrow(unique(attrs_by_region_dt_min_mod[, ..cols_to_keep_after_shiny])) != nrow(attrs_by_region_dt_min)) {
              stop("... there seems to be a problem as there are more rows than unique attribute combinations, meaning that a single value has been added more than once.")
            } else {
              attrs_by_region_dt_min_mod <- attrs_by_region_dt_min_mod[, ..cols_to_keep_after_shiny]
            }
            
            attrs_by_region_dt_min_added <- attrs_by_region_dt_min_mod[!is.na(get(self$attribute_name_in_corpus)), ]
            self$missing_after_check <- attrs_by_region_dt_min_mod[is.na(get(self$attribute_name_in_corpus)), ]
            
            # indicate that this is added
            attrs_by_region_dt_min_added[, added := TRUE]
            
            # we have to change the values in self$values here
            join_cols <- setdiff(names(attrs_by_region_dt_min_mod), self$attribute_name_in_corpus)
            
            self$attrs_by_region_dt[attrs_by_region_dt_min_added, 
                                    c((self$attribute_name_in_corpus), "added") := .(get(paste0("i.", (self$attribute_name_in_corpus))), i.added),
                                    on = join_cols]
            
            # now which rows in attrs_by_region_dt are those added.
            rows_to_add_idx <- self$attrs_by_region_dt[added == TRUE, which = TRUE]
            
            if (length(rows_to_add_idx) > 0) {
              if (verbose) message("... adding manually identified attributes as values.")
              
              # final sanity checks.
              # (in case something was added twice nevertheless)
              if (nrow(self$attrs_by_region_dt) != length(self$values)) stop("... region table and values vector aren't the same length.")
              if (!all(is.na(self$values[rows_to_add_idx]))) stop("... region table contains non NA values which aren't expected.")
              
              self$values[rows_to_add_idx] <- self$attrs_by_region_dt[added == TRUE, get(self$attribute_name_in_corpus)]
            }
          }
        }
      }
      
      invisible(self)
    }, 
    
    #' @description Add missing values with shiny and rhandsontable. Called by
    #'   `check_and_add_missing_values()`.
    #' @param y a \code{data.table} containing incomplete rows after the merge.
    #' @param doc_dir a \code{character vector}; Indicating a directory in which
    #'   a text file is created which documents manual changes to the merge.
    add_missing_attributes_via_shiny = function(y, doc_dir) {
      
      # add keep column and set to TRUE per default
      y[, keep := TRUE]
      
      # the first attempts to make this work were done in the internal
      # and never really used WikiParliamentaryLookup package
      # (check_and_correct_incomplete_mps.R).

      # prepare shiny gadget
      .editing_gadget_ui <- function() {
        miniUI::miniPage(
          miniUI::gadgetTitleBar("Add missing attributes manually."), 
          miniUI::miniContentPanel(shiny::fillRow(rhandsontable::rHandsontableOutput("hot")))
        )
      }
      
      server <- function(input, output, session) {
        
        values <- shiny::reactiveValues()
        reactiveData <- shiny::reactive(y)
        
        .reset_values <- function(df){
          values[["hot"]] <- df
          y[, (self$attribute_name_in_corpus) := df[[self$attribute_name_in_corpus]]]
          y[, keep := df[["keep"]]]
        }
        
        output$hot <- rhandsontable::renderRHandsontable({
          data <- reactiveData()
          # Identical result with rhandsontable:::isErrorMessage(data), 
          # using the ::: can be avoided thereby
          if (inherits(data, "error_message")) return(NULL)
          df <- if (is.null(input$hot)) data else rhandsontable::hot_to_r(input$hot)
          .reset_values(df)
          rht <- rhandsontable::rhandsontable(df, allowedTags = "<font><div><u>",  height = 500)
          rht <- rhandsontable::hot_cols(rht, manualColumnResize = TRUE)
          rht <- rhandsontable::hot_table(rht, highlightCol = TRUE, highlightRow = TRUE, overflow = "hidden", stretchH = "all")
          rht <- rhandsontable::hot_col(rht, col = (1L:ncol(df))[which(!colnames(df) %in% c(self$attribute_name_in_corpus, "keep"))], readOnly = TRUE)
          rht
        })
        
        shiny::observeEvent(input$done, shiny::stopApp(returnValue = y))
      }
      
      # run shiny gadget
      y <- shiny::runGadget(.editing_gadget_ui(), server, viewer = shiny::paneViewer(minHeight = 550))
      
      # now extract the added attributes and document.
      document_log_file <- sprintf("%s/manual_addition_of_attribute_while_linking_%s.txt", 
                                   doc_dir, 
                                   Sys.Date())
      
      note_line <- sprintf('# Note: Attribute "%s" added manually to resource via linktools (version "%s") on %s', 
                           self$attribute_name_in_corpus, 
                           packageVersion("linktools"), 
                           Sys.Date())
      
      write.table(x = y[!is.na(get(self$attribute_name_in_corpus))], 
                  file = document_log_file, 
                  row.names = FALSE)
      
      # add note_line (as comment)
      write(c("", note_line), file = document_log_file, append = TRUE)
      
      return(y)
    }, 
    
    #' @description Add missing values using a fuzzy join. Called by
    #'   `check_and_add_missing_values()`.
    #' @param attrs_by_region_dt_min a `data.table` containing incomplete rows
    #'   after the merge.
    #' @param match_fuzzily_by a `character vector`; if not NULL, a fuzzy match
    #'   will be performed on the column indicated.
    #' @param ignore_case `logical` whether to match the case. This is a
    #'   argument of `stringdist_join`.
    #' @param dist_method A `character` The measurement of distance used by
    #'   `stringdist_join`. See `stringdist-metrics` in the `stringdist`
    #'   package.
    #' @param max_dist An `integer` value indicating the maximum distance
    #'   between the two input vectors. This is a argument of `stringdist_join`.
    fuzzy_join_missing_values = function(attrs_by_region_dt_min,
                                         match_fuzzily_by,
                                         ignore_case = TRUE,
                                         dist_method = "lv",
                                         max_dist = 4L) {
      
      message("... performing fuzzy matching.")
      
      # column names in external data are renamed in the class slot.
      fuzzy_var_name <- names(self$match_by)[match(match_fuzzily_by, names(self$match_by))]
      non_fuzzy_var_name <- names(self$match_by)[which(!names(self$match_by) %in% match_fuzzily_by)]
      
      by_vector <- c(fuzzy_var_name, non_fuzzy_var_name)
      target_var <- colnames(self$external_resource)[which(!colnames(self$external_resource) %in% by_vector)]
      
      # this might be not very robust.
      match_fun_list <- vector("list", length(c(fuzzy_var_name, non_fuzzy_var_name)))
      
      # replacing the default values of fuzzyjoin::stringdist_join
      stringdist_join_match_fun_for_list <- self$stringdist_join_match_fun

      formals(stringdist_join_match_fun_for_list)[["ignore_case"]] <- ignore_case
      formals(stringdist_join_match_fun_for_list)[["method"]] <- dist_method
      formals(stringdist_join_match_fun_for_list)[["max_dist"]] <- max_dist

      for (i in 1:length(fuzzy_var_name)) {
        match_fun_list[[i]] <- stringdist_join_match_fun_for_list
      }
      
      for (i in (length(fuzzy_var_name) + 1):length(c(fuzzy_var_name, non_fuzzy_var_name))) {
        match_fun_list[[i]] <- eval(`==`)
      }
      
      attrs_by_region_dt_min_joined <- fuzzyjoin::fuzzy_join(
        x = attrs_by_region_dt_min, 
        y = self$external_resource,
        by = by_vector,
        match_fun = match_fun_list,
        mode = "left"
      )
      
      # clean up. Remove all ".y" columns except for the fuzzily joined
      # attribute and all dist column.

      attrs_by_region_dt_min_joined <- data.table::as.data.table(attrs_by_region_dt_min_joined)

      columns_to_omit <- c(paste0(by_vector, ".y"), paste0(by_vector, ".distance_col"))
      
      fuzzy_var_name_joined <- paste0(fuzzy_var_name, ".y")
      columns_to_omit <- columns_to_omit[columns_to_omit != fuzzy_var_name_joined]
      attrs_by_region_dt_min_joined[, (columns_to_omit) := NULL]
      
      # then use the joined new attribute to add to the missing values
      attribute_name_in_corpus_in_x <- paste0(self$attribute_name_in_corpus, ".x")
      attribute_name_in_corpus_in_y <- paste0(self$attribute_name_in_corpus, ".y")
      
      # we remove the column with the old, missing values
      attrs_by_region_dt_min_joined[, (attribute_name_in_corpus_in_x) := NULL]

      # and rename it accordingly
      data.table::setnames(attrs_by_region_dt_min_joined,
                           old = attribute_name_in_corpus_in_y,
                           new = self$attribute_name_in_corpus)

      # and do this for all the other textual data columns (".x") as well.
      data.table::setnames(attrs_by_region_dt_min_joined, 
                           old = paste0(names(self$match_by), ".x"), 
                           new = names(self$match_by))

      # finally, we rename the fuzzily matched match variable to make
      # transparent

      data.table::setnames(attrs_by_region_dt_min_joined,
                           old = fuzzy_var_name_joined,
                           new = paste0(fuzzy_var_name, "_fuzzy_matched"))

      data.table::setcolorder(attrs_by_region_dt_min_joined, c(names(self$match_by),
                                                               paste0(fuzzy_var_name, "_fuzzy_matched"),
                                                               target_var))
      
      return(attrs_by_region_dt_min_joined)
      
    },

    #' @description The function used for those attributes which are matched
    #'   fuzzily. Called by `fuzzy_join_missing_values()`. This function is
    #'   identical to the "match_fun" of `stringdist_join()` of the `fuzzyjoin`
    #'   package with changed defaults.
    #' @details The defaults were changed. "lv" is now the default method,
    #'   max_dist is 4 and ignore_case is now TRUE.
    #' @param v1,v2 `character vectors` to be compared fuzzily.
    #' @param ignore_case `logical` whether to match the case. This is a
    #'   argument of `stringdist_join`.
    #' @param method A `character` The measurement of distance used by
    #'   `stringdist_join`. See `stringdist-metrics` in the `stringdist`
    #'   package.
    #' @param max_dist An `integer` value indicating the maximum distance
    #'   between the two input vectors. This is a argument of `stringdist_join`.
    stringdist_join_match_fun = function(v1,
                                         v2,
                                         max_dist = 4L,
                                         method = c("lv", "osa", "dl",
                                                    "hamming", "lcs", "qgram",
                                                    "cosine", "jaccard", "jw",
                                                    "soundex"),
                                         ignore_case = TRUE) {
      method <- match.arg(method)

      if (method == "soundex") {
        max_dist <- 0.5
      }

        if (ignore_case) {
          v1 <- stringr::str_to_lower(v1)
          v2 <- stringr::str_to_lower(v2)
        }
        if (method %in% c("osa", "lv", "dl")) {
          length_diff <- abs(stringr::str_length(v1) - stringr::str_length(v2))
          include <- length_diff <= max_dist
          dists <- rep(NA, length(v1))
          dists[include] <- stringdist::stringdist(v1[include],
                                                   v2[include],
                                                   method = method)
        }
        else {
          dists <- stringdist::stringdist(v1, v2, method = method)
        }
        ret <- tibble::tibble(include = (dists <= max_dist))
        ret[["distance_col"]] <- dists
        return(ret)
    },

    #' @description The function checks if the entire corpus is covered by the
    #'   region matrix. If not, the missing regions of the corpus are added to
    #'   the region matrix and the value vector. This is needed to encode the
    #'   new data.
    add_missing_regions = function() {

      if (self$verbose) message("... adding missing regions to region matrix and value vector.")

      if (isTRUE(is.character(self$textual_data))) {

        all_cpos_in_corpus <- 0L:(polmineR::size(self$textual_data) - 1)

      } else {

        # otherwise, its a subcorpus and the name of the corpus is taken from
        # the slot.

        all_cpos_in_corpus <- 0L:(polmineR::size(self$textual_data@corpus) - 1)
      }

      cpos_missing <- which(!all_cpos_in_corpus %in% self$cpos_left:self$cpos_right) - 1

      if (length(cpos_missing) > 0) {

        # see stack overflow for cumsum: https://stackoverflow.com/questions/24837401/find-consecutive-values-in-vector-in-r
        missing_consecutive <- split(cpos_missing, cumsum(c(1, diff(cpos_missing) != 1)))
        missing_matrix <- t(sapply(missing_consecutive, function(x) c(min(x), max(x))))
        rownames(missing_matrix) <- NULL

        # bind it at the end of the region_matrix
        self$region_matrix <- rbind(self$region_matrix, missing_matrix)

        # and add NA value to the values slot
        self$values <- c(self$values,
                         rep(x = NA, times = nrow(missing_matrix)))

        # reorder by first cpos per region
        order_vector <- order(self$region_matrix[, 1])
        self$region_matrix <- self$region_matrix[order_vector, ]
        self$values <- self$values[order_vector]

        # last sanity check here
        stopifnot(nrow(self$region_matrix) == length(self$values))
      }

      invisible(self)
    }
  )
)
