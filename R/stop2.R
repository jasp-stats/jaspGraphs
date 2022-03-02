# identical to stop(), but these strings should not be translated
stop2 <- function(..., call. = TRUE, domain = NA) {
  stop(..., call. = call., domain = domain)
}

# identical to warning(), but these strings should not be translated
warning2 <- function(..., , call. = TRUE, immediate. = FALSE, noBreaks. = FALSE, domain = NA) {
  warning(..., call. = call., immediate. = immediate., noBreaks. = noBreaks., domain = domain)
}
