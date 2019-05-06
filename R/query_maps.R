query_map_string <- readr::read_csv(
  system.file('extdata',
              'query_map_string.csv',
              package = 'apihelpers'))
usethis::use_data(query_map_string, overwrite = TRUE)

#' query_map_string
"query_map_string"

query_map_bioware <- readr::read_csv(
  system.file('extdata',
              'query_map_bioware.csv',
              package = 'apihelpers'))
usethis::use_data(query_map_bioware, overwrite = TRUE)

#' query_map_bioware
"query_map_bioware"

query_map_biogrid <- readr::read_csv(
  system.file('extdata',
              'query_map_biogrid.csv',
              package = 'apihelpers'))
usethis::use_data(query_map_biogrid, overwrite = TRUE)

#' query_map_bioware
"query_map_biogrid"

