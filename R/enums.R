# This should be safer than using random strings everywhere, but it is a bit uncommon for R

createEnum <- function(nameValuePairs) {

  if (!is.list(nameValuePairs))
    stop2("createEnum: nameValuePairs should be a list!")
  if (length(unique(names(nameValuePairs))) != length(nameValuePairs))
    stop2("createEnum: unique names and values have different length!")

  e <- list2env(nameValuePairs)
  class(e) <- c("Enum", class(e))
  e
}

`$.Enum` <- function(x, y) {
  out <- NextMethod(x, y)
  if (is.null(out)) stop2(sprintf("nonexisting enum type %s!", y))
  out
}

is.Enum <- function(x) inherits(x, "Enum")

switchEnum <- function(EXPR, ENUM, ..., warnIfNonExhaustive = TRUE, checkValidCases = TRUE) {

  if (!is.character(EXPR))
    stop2("EXPR should be character!")
  if (!is.Enum(ENUM))
    stop2("ENUM should an enum!")

  call <- match.call()
  nmsInCall <- names(call)[-(1:3)]

  if (warnIfNonExhaustive && all(nmsInCall != "") && !all(names(ENUM) %in% nmsInCall))
    warning("switchEnum: no default case nor an exhaustive set of cases!")

  if (checkValidCases && length(invalid <- setdiff(nmsInCall[nmsInCall != ""], names(ENUM))) > 0L)
    stop2("switchEnum: Invalid case(s) detected: ", paste(invalid, collapse = ", "), "\n  Valid cases are: ", paste(names(ENUM), collapse = ", "))

  # this is a bit inefficient...
  idx <- names(ENUM)[match(EXPR, unlist(as.list(ENUM)), nomatch = 0L)]
  if (length(idx) == 0)
    idx <-  3L + which(nmsInCall == "")[1L]

  return(eval(call[[idx]], envir = parent.frame(1L)))
}

# the data here should match 1 to 1 with QML!
BreaksType <- createEnum(list(Manual     = "manual",     Range     = "range",     Null   = "NULL"))
LimitsType <- createEnum(list(Manual     = "manual",     Data      = "data",      Breaks = "breaks"))
TitleType  <- createEnum(list(Expression = "expression", Character = "character", Null   = "NULL"))


ErrorType <- createEnum(list(Success = "success", ValidationError = "validationError", FatalError = "fatalError"))
