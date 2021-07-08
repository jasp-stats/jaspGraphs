# string utilities. This is mainly here to avoid a dependency on stringr

str_extract_all <- function(string, pattern) {
  regmatches(string, gregexpr(pattern, string, perl = TRUE))
}

str_remove_all <- function(string, toRemove) {
  gsub(toRemove, "", string, fixed = TRUE)
}

