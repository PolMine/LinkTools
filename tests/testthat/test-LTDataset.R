test_that("all unique speaker attribute pairs from the text data are kept and no observations are dropped implicitly when enriching entire corpora", {
  
  withr::local_package("polmineR")
  use("polmineR")
  
  # all unique attribute combinations in GERMAPARLMINI
  
  s_attributes_gpmini <- s_attributes("GERMAPARLMINI", c("speaker", "party", "protocol_lp"))
  s_attribute_combinations <- s_attributes_gpmini[, c("value", "party", "protocol_lp")] |>
    unique() |>
    nrow()
  
  # all unique attribute combinations in the enriched data.frame
  
  LTD <- LTDataset$new(textual_data = "GERMAPARLMINI", 
                       textual_data_type = "cwb",
                       external_resource = btmp::btmp_de,
                       attr_to_add = c("id" = "wikidata_id"), 
                       match_by = c("speaker" = "full_name",
                                    "party" = "party_wikipedia",
                                    "protocol_lp" = "legislative_period"),
                       split_by = NULL,
                       verbose = TRUE, 
                       forced_encoding = "UTF-8")
  
  LTD$join_textual_and_external_data()
  LTD$create_attribute_region_datatable()
  n_attributes_by_region <- LTD$attrs_by_region_dt |>
    unique() |>
    nrow()
  
  RcppCWB::cqp_reset_registry()
  
  expect_equal(n_attributes_by_region, s_attribute_combinations)
})

test_that("all unique speaker attribute pairs from the text data are kept and no observations are dropped implicitly when enriching subcorpora", {
  
  withr::local_package("polmineR")
  use("polmineR")
  
  gpmini_day <- corpus("GERMAPARLMINI") |>
    polmineR::subset(date == "2009-11-12")
  
  s_attributes_gpmini <- s_attributes(gpmini_day, c("speaker", "party")) |>
    nrow()
  
  LTD <- LTDataset$new(textual_data = gpmini_day, 
                       textual_data_type = "cwb",
                       external_resource = btmp::btmp_de,
                       attr_to_add = c("id" = "wikidata_id"), 
                       match_by = c("speaker" = "full_name",
                                    "party" = "party_wikipedia"),
                       split_by = NULL,
                       verbose = TRUE, 
                       forced_encoding = "UTF-8")
  
  LTD$join_textual_and_external_data()
  LTD$create_attribute_region_datatable()
  
  n_attributes_by_region <- LTD$attrs_by_region_dt |>
    unique() |>
    nrow()
  
  RcppCWB::cqp_reset_registry()
  
  expect_equal(n_attributes_by_region, s_attributes_gpmini)
}
)


test_that("the output is identical regardless of the usage of split_by for corpora", {
  
  withr::local_package("polmineR")
  use("polmineR")
  
  LTD <- LTDataset$new(textual_data = "GERMAPARLMINI", 
                       textual_data_type = "cwb",
                       external_resource = btmp::btmp_de,
                       attr_to_add = c("id" = "wikidata_id"), 
                       match_by = c("speaker" = "full_name",
                                    "party" = "party_wikipedia",
                                    "protocol_lp" = "legislative_period"),
                       split_by = NULL,
                       verbose = TRUE, 
                       forced_encoding = "UTF-8")
  
  LTD$join_textual_and_external_data()
  
  LTD_split <- LTDataset$new(textual_data = "GERMAPARLMINI", 
                       textual_data_type = "cwb",
                       external_resource = btmp::btmp_de,
                       attr_to_add = c("id" = "wikidata_id"), 
                       match_by = c("speaker" = "full_name",
                                    "party" = "party_wikipedia",
                                    "protocol_lp" = "legislative_period"),
                       split_by = "date",
                       verbose = TRUE, 
                       forced_encoding = "UTF-8")
  
  LTD_split$join_textual_and_external_data()
  
  RcppCWB::cqp_reset_registry()
  
  expect_identical(LTD_split$text_dt, LTD$text_dt)
})


test_that("the output is identical regardless of the usage of split_by for subcorpora", {
  
  withr::local_package("polmineR")
  use("polmineR")
  
  gpmini_day <- corpus("GERMAPARLMINI") |>
    polmineR::subset(date == "2009-11-12")
  
  LTD <- LTDataset$new(textual_data = gpmini_day, 
                       textual_data_type = "cwb",
                       external_resource = btmp::btmp_de,
                       attr_to_add = c("id" = "wikidata_id"), 
                       match_by = c("speaker" = "full_name",
                                    "party" = "party_wikipedia"),
                       split_by = NULL,
                       verbose = TRUE, 
                       forced_encoding = "UTF-8")
  
  LTD$join_textual_and_external_data()
  
  LTD_split <- LTDataset$new(textual_data = gpmini_day, 
                             textual_data_type = "cwb",
                             external_resource = btmp::btmp_de,
                             attr_to_add = c("id" = "wikidata_id"), 
                             match_by = c("speaker" = "full_name",
                                          "party" = "party_wikipedia"),
                             split_by = "party",
                             verbose = TRUE, 
                             forced_encoding = "UTF-8")
  
  LTD_split$join_textual_and_external_data()
  
  RcppCWB::cqp_reset_registry()
  
  expect_identical(LTD_split$text_dt, LTD$text_dt)
})


test_that("the region matrix in the end only contains each cpos once and that the length of unique cpos is equal to the corpus size", {
  
  withr::local_package("polmineR")
  use("polmineR")
  
  gpmini_day <- corpus("GERMAPARLMINI") |>
    polmineR::subset(date == "2009-11-10")
  
  LTD <- LTDataset$new(textual_data = gpmini_day, 
                       textual_data_type = "cwb",
                       external_resource = btmp::btmp_de,
                       attr_to_add = c("id" = "wikidata_id"), 
                       match_by = c("speaker" = "full_name",
                                    "party" = "party_wikipedia"),
                       split_by = NULL,
                       verbose = TRUE, 
                       forced_encoding = "UTF-8")
  
  LTD$join_textual_and_external_data()
  LTD$create_attribute_region_datatable()
  LTD$add_missing_regions()
  
  cpos_vector <- apply(LTD$region_matrix, MARGIN = 1, function(row) c(row[[1]]:row[[2]])) |>
    unlist()
  
  RcppCWB::cqp_reset_registry()

  expect_equal(length(cpos_vector), length(unique(cpos_vector)))
  expect_equal(length(unique(cpos_vector)), size("GERMAPARLMINI"))
}
)


test_that("The new attribute is correctly encoded in the corpus", {
  
  withr::local_package("polmineR")
  use("polmineR")

  # copy corpus to temp dir, using the current default values of
  # cwbtools::corpus_copy explicitly.
  
  new_registry_dir <- fs::path(tempdir(), "cwb", "registry")
  new_data_dir <- fs::path(tempdir(), "cwb", "indexed_corpora", tolower("GERMAPARLMINI"))
  
  cwbtools::corpus_copy(corpus = "GERMAPARLMINI",
                        registry_dir = RcppCWB::corpus_registry_dir(corpus = "GERMAPARLMINI"),
                        data_dir =  RcppCWB::corpus_data_dir(corpus = "GERMAPARLMINI",
                                                             registry = RcppCWB::corpus_registry_dir(corpus = "GERMAPARLMINI")),
                        registry_dir_new = new_registry_dir,
                        data_dir_new = new_data_dir)
  
  cwbtools::corpus_rename(
    old = "GERMAPARLMINI",
    new = "GERMAPARLMINI_TMP",
    registry_dir = new_registry_dir,
    verbose = TRUE
  )

  old_corpus_registry <- Sys.getenv("CORPUS_REGISTRY")

  RcppCWB::cqp_initialize(registry = new_registry_dir)
  stopifnot("GERMAPARLMINI_TMP" %in% corpus()[["corpus"]])
  
  s_attributes_before <- s_attributes("GERMAPARLMINI_TMP")
  
  gpmini_day <- corpus("GERMAPARLMINI_TMP") |>
    polmineR::subset(date == "2009-11-10")
  
  LTD <- LTDataset$new(textual_data = gpmini_day, 
                       textual_data_type = "cwb",
                       external_resource = btmp::btmp_de,
                       attr_to_add = c("id" = "wikidata_id"), 
                       match_by = c("speaker" = "full_name",
                                    "party" = "party_wikipedia"),
                       split_by = NULL,
                       verbose = TRUE, 
                       forced_encoding = "UTF-8")
  
  LTD$join_textual_and_external_data()
  LTD$create_attribute_region_datatable()

  LTD$encode_new_s_attribute()
  
  RcppCWB::cqp_reset_registry()
  
  s_attributes_after <- s_attributes("GERMAPARLMINI_TMP")
  
  # expect that new s_attribute is there
  expect_true(setdiff(s_attributes_after, s_attributes_before) == "id")
  
  
  # except that this corresponds to the joins in the object
  speaker_id_combination_encoded <- s_attributes(gpmini_day, c("speaker", "id")) |>
    unique()

  data.table::setorder(speaker_id_combination_encoded, speaker)
  
  speaker_id_combination_ltd <- LTD$attrs_by_region_dt |>
    unique() |>
    data.table::setorder(speaker)
  
  speaker_id_combination_ltd <- speaker_id_combination_ltd[, c("speaker", "id")]
  speaker_id_combination_ltd[is.na(speaker_id_combination_ltd$id), "id"] <- "NA"
  
  speaker_id_data_table <- merge(speaker_id_combination_encoded, speaker_id_combination_ltd, by = "speaker")
  
  expect_equal(speaker_id_data_table$id.x, speaker_id_data_table$id.y)
  
  RcppCWB::cqp_reset_registry(registry = old_corpus_registry)
}
)

