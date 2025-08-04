#' remove.na
#'
#' @description Removes all rows with NA values for an object (frame, list, or vector).
#'
#' @param object frame, list, or vector
#' @param column column with NA if object is a frame (default is NA)
#' @return object
#' @importFrom methods is
#' @export
#' @examples
#' remove.na(list(NA, 1, NA, 2, 3))
remove.na <- function(object, column = NA) {
  if (is(object, "data.frame")) {
    if (!is.na(column)) {
      if (length(which(is.na(object[, column]) == FALSE)) !=
          nrow(object)) {
        object <- object[-c(which(is.na(object[, column]) ==
                                    TRUE)), ]
      }
    }
    else {
      for (a in 1:length(ncol(object))) {
        if (length(which(is.na(object[, a]) == FALSE)) !=
            nrow(object)) {
          object <- object[-c(which(is.na(object[, a]) ==
                                      TRUE)), ]
        }
      }
    }
  }
  else if (is(object, "list") || is(object, "character") ||
           is(object, "integer")) {
    if (length(which(is.na(object) == FALSE)) != length(object)) {
      object <- object[-c(which(is.na(object) == TRUE))]
    }
  }
  # else if (is(object, "phyloseq")) {
  #   object.frame <- sample.to.data.frame(object)
  #   if (any(is.na(object.frame[, column]))) {
  #     non.na <- row.names(object.frame[c(which(is.na(object.frame[,
  #                                                                 column]) == FALSE)), ])
  #     object <- prune_samples(non.na, object)
  #   }
  # }
  return(object)
}

#' replace.na
#'
#' @description Replaces all NA in object with specified value
#'
#' @param object frame, list, or vector
#' @param value value to replace NA
#' @return object
#' @export
#' @examples
#' replace.na(list(NA, 1, NA, 2, 3), 0)
replace.na <- function(object, value) {
  object[is.na(object)] <- value
  return(object)
}

#' replace.nan
#'
#' @description Replaces all NaN in object with specified value
#'
#' @param object frame, list, or vector
#' @param value value to replace NaN
#' @return object
#' @export
#' @examples
#' replace.nan(c(NaN, 1, NaN, 2, 3), 0)
replace.nan <- function(object, value) {
  object[is.nan(object)] <- value
  return(object)
}

#' replace.specific
#'
#' @description Locates all instances of a value and replaces them with the provided value.
#'
#' @param vector vector
#' @param value existing value
#' @param replacement replacement value
#' @return vector
#' @export
#' @examples
#' replace.specific(c(1, 2, 3), 1, 3)
replace.specific <- function(vector, value, replacement) {
  vector[vector == value] <- replacement
  return(vector)
}

