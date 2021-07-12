# identical to stop(), but these strings should not be translated
stop2 <- function(..., call. = TRUE, domain = NA) {
  stop(..., call. = call., domain = domain)
}
