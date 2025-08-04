#' Duplicated values
#'
#' @description Identifies all of the duplicated values in a vector.
#'
#' @param vector vector
#' @param non.duplicated boolean switch between duplicated and non duplicated values (default is FALSE)
#' @return vector
#' @export duplicated.values
#' @examples
#' frame <- data.frame("a"=rep(1, 9), "b"=c(rep(1, 3), rep(2, 3), rep(3, 3)), "c"=1:9)
#' duplicated.values(frame[,"b"])
duplicated.values <- function(vector, non.duplicated = FALSE) {
  duplicated <- vector[duplicated(vector)]
  if (non.duplicated == TRUE) {
    non.duplicated <- subset.object(vector, duplicated,
                                           remove = TRUE)
    return(non.duplicated)
  }
  else {
    return(duplicated)
  }
}

#' Unlist unname unique
#'
#' @description Unlists, unnames, and identifies the unique values in the provided object (list or vector).
#'
#' @param object list or vector
#' @return object
#' @export
#' @examples
#' frame <- data.frame("a"=rep(1, 9), "b"=c(rep(1, 3), rep(2, 3), rep(3, 3)), "c"=1:9)
#' uuln(frame[,"b"])
uuln <- function(object) {
  return(unique(unname(unlist(object))))
}

#' Unlist unname
#'
#' @description Unlists and unnames the provided object.
#'
#' @param object list or vector
#' @return object
#' @export
#' @examples
#' frame <- data.frame("a"=rep(1, 9), "b"=c(rep(1, 3), rep(2, 3), rep(3, 3)), "c"=1:9)
#' uln(frame[,"b"])
uln <- function(object) {
  return(unname(unlist(object)))
}

#' two.by.two.paired
#'
#' @description Compares two columns in a frame to generate a two by two table summarizing presence of a value (>0).
#'
#' @param frame frame
#' @param col.1 column of numeric values (0 or value > 0).
#' @param col.2 column of numeric values (0 or value > 0).
#' @return table
#' @export
#' @examples
#' frame <- data.frame("a"=c(3, 0.12, 0, 0, 0.001), "b"=c(0, 1, 0, 6, 9))
#' two.by.two.paired(frame, "a", "b")
two.by.two.paired <- function(frame, col.1, col.2) {
  col.1.row.1 <- 0
  col.1.row.2 <- 0
  col.2.row.1 <- 0
  col.2.row.2 <- 0
  for (a in 1:nrow(frame)) {
    if (frame[a, col.1] != 0 & frame[a, col.2] != 0) {
      col.1.row.1 <- col.1.row.1 + 1
    }
    else if (frame[a, col.1] != 0 & frame[a, col.2] == 0) {
      col.1.row.2 <- col.1.row.2 + 1
    }
    else if (frame[a, col.1] == 0 & frame[a, col.2] != 0) {
      col.2.row.1 <- col.2.row.1 + 1
    }
    else if (frame[a, col.1] == 0 & frame[a, col.2] == 0) {
      col.2.row.2 <- col.2.row.2 + 1
    }
  }
  two.by.two <- data.frame(col.1 = c(col.1.row.1, col.1.row.2),
                           col.2 = c(col.2.row.1, col.2.row.2))
  colnames(two.by.two) <- c(paste0(col.1, ".yes"), paste0(col.1, ".no"))
  rownames(two.by.two) <- c(paste0(col.2, ".yes"), paste0(col.2, ".no"))
  return(two.by.two)
}

#' subset.object
#'
#' @description Subsets or removes specified values provided object (frame, list, or vector).
#'
#' @param object frame, list, or vector
#' @param value value
#' @param col.name column with value
#' @param remove boolean switch between retaining only the specified value or remove the value from the object (default is FALSE)
#' @return object
#' @importFrom methods is
#' @export subset.object
#' @examples
#' frame <- data.frame("a"=rep(1, 9), "b"=c(rep(1, 3), rep(2, 3), rep(3, 3)), "c"=1:9)
#' subset.object(frame, 1, "b")
subset.object <- function(object, value, col.name = NULL, remove = FALSE) {
  if (is(object, "data.frame")) {
    object.og <- object
    if (is.null(col.name)) {
      if (remove == FALSE) {
        if (length(value) > 1) {
          for (a in 1:length(value)) {
            for (b in 1:ncol(object)) {
              if (value %in% object[, b]) {
                merge.object <- object.og[c(which(object.og[, b] == c(value[a]))), ]
              }
            }
            if (a == 1) {
              object <- merge.object
            }
            else {
              object <- rbind(object, merge.object)
            }
          }
        }
        else {
          for (a in 1:ncol(object)) {
            if (value %in% object[, a]) {
              object <- object[c(which(object[, a] == c(value))), ]
            }
          }
        }
      }
      else if (remove == TRUE) {
        for (a in 1:ncol(object)) {
          if (value %in% object[, a]) {
            object <- object[c(which(object[, a] != c(value))),
            ]
          }
        }
      }
    }
    else {
      if (remove == FALSE) {
        if (length(value) > 1) {
          for (a in 1:length(value)) {
            merge.object <- object.og[c(which(object.og[, col.name] == c(value[a]))), ]
            if (a == 1) {
              object <- merge.object
            }
            else {
              object <- rbind(object, merge.object)
            }
          }
        }
        else {
          object <- object[c(which(object[, col.name] == c(value))), ]
        }
      }
      else if (remove == TRUE) {
        if (length(value) > 1) {
          for (a in 1:length(value)) {
            object <- object[c(which(object[, col.name] != c(value[a]))), ]
          }
        }
        else {
          object <- object[c(which(object[, col.name] != c(value))), ]
        }
      }
    }
  }
  else if (is(object, "list") || is(object, "character")) {
    object.og <- object
    if (remove == FALSE) {
      if (length(value) > 1) {
        for (a in 1:length(value)) {
          merge.object <- object.og[c(which(object.og == c(value[a])))]
          if (a == 1) {
            object <- merge.object
          }
          else {
            object <- c(object, merge.object)
          }
        }
      }
      else {
        object <- object[c(which(object == c(value)))]
      }
    }
    else if (remove == TRUE) {
      if (length(value) > 1) {
        for (a in 1:length(value)) {
          object <- object[c(which(object != c(value[a])))]
        }
      }
      else {
        object <- object[c(which(object != c(value)))]
      }
    }
  }
  return(object)
}
