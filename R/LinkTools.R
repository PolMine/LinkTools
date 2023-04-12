#' R-package 'LinkTools'
#' 
#' Tools for linking data sets via shared unique identifiers.
#' 
#' Four steps are integrated into this package:
#' 
#' a) the preparation of data sets which should be linked, i.e. the
#' transformation into a comparable format and the assignment of shared unique
#' identifiers,
#' 
#' b) the merge of data sets based on these identifiers,
#' 
#' c) the encoding or enrichment of the data with three output formats
#' (data.table, XML or CWB).
#' 
#' d) the package includes a wrapper for the Named Entity Linking of textual
#' data based on DBPedia Spotlight.
#' @keywords package
#' @docType package
#' @aliases LinkTools LinkTools-package
#' @name LinkTools-package
#' @rdname LinkTools-package
NULL

#' Stammdaten with WikiData-IDs
#' 
#' A minimized version of the Stammdaten of the German Bundestag of the 13th and
#' 14th legislative period with added WikiData IDs retrieved via the Wikidata Query 
#' Service and added party affiliations specific for the legislative period retrieved 
#' from Wikipedia. For preparation see bt_stammdaten.
#' @source https://www.bundestag.de/services/opendata (Creation Date 2021-11-04)
#' @source https://de.wikipedia.org/wiki/Liste_der_Mitglieder_des_Deutschen_Bundestages_(13._Wahlperiode) (Information Retrieved on 2021-11-23)
#' @source https://de.wikipedia.org/wiki/Liste_der_Mitglieder_des_Deutschen_Bundestages_(14._Wahlperiode) (Information Retrieved on 2021-11-23)
#' @docType data
#' @keywords datasets
"stammdaten_wikidatafied_2022_02_01_min"
