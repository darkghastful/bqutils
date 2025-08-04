#' count.decimals
#'
#' @description Counts the number of decimal places in provided number.
#'
#' @param number number
#' @return integer
#' @export
#' @examples
#' count.decimals(0.00987)
count.decimals <- function(number) {
  if (abs(number - round(number)) > .Machine$double.eps^0.5) {
    decimal <- nchar(strsplit(sub("0+$", "", as.character(number)),
                              ".", fixed = TRUE)[[1]][[2]])
    # if (place) {
    #     return(as.numeric(paste0("0.", rep("0", decimal -
    #         1), 1)))
    # }
    # else {
    return(decimal)
    # }
  }
  else {
    return(0)
  }
}

#' round.to
#'
#' @description Round the number to a the provided decimal place (0.001)
#'
#' @param number number
#' @param to decimal place for rounding (accepts 0.001 or alike)
#' @param top.bottom switch to round up or down (default is traditional rounding)
#' @return numeric
#' @export round.to
#' @examples
#' round.to(0.00987, 0.001, "bottom")
round.to <- function(number, to, top.bottom = NA) {
  if (is.na(top.bottom)) {
    return(round(number/to) * to)
  }
  else if (top.bottom == "top") {
    return(ceiling(number/to) * to)
  }
  else if (top.bottom == "bottom") {
    return(floor(number/to) * to)
  }
}

#' sigfill
#'
#' @description Rounds number to desired significance and formats as a p-value.
#'
#' @param numbers numbers
#' @param sigfigs number of significant figures
#' @param as.p boolean switch between p-value formating (default is FALSE)
#' @return strings
#' @export
#' @examples
#' sigfill(c(0.00123, 0.000987), 3, TRUE)
sigfill <- function(numbers, sigfigs = 2, as.p = FALSE) {
  numbers <- uln(numbers)
  out <- c()
  if (!as.p) {
    for (a in 1:length(numbers)) {
      if (numbers[a] == 1) {
        out[a] <- paste0("1.", paste0(rep(0, sigfigs),
                                      collapse = ""))
      }
      else if ((numbers[a] < as.numeric(paste0("0.", paste0(rep(0,
                                                               (sigfigs - 1)), collapse = ""), "1")))) {
        out[a] <- paste0("<0.", paste0(rep(0, (sigfigs -
                                                 1)), collapse = ""), "1")
      }
      else {
        sig.dec <- (which(strsplit(as.character(numbers[a]),
                                   "")[[1]] == ".") - 1) + sigfigs
        out[a] <- formatC(round(numbers[a], digits = sigfigs),
                          digits = sig.dec, format = "fg", flag = "#")
        out[a] <- strtrim(out[a], sig.dec + c(1, 2)[grepl("-",
                                                          out[a], fixed = TRUE) + 1])
      }
    }
  }
  else {
    for (a in 1:length(numbers)) {
      if (numbers[a] == 1) {
        out[a] <- paste0("p=1.", paste0(rep(0, sigfigs),
                                        collapse = ""))
      }
      else if ((numbers[a] < as.numeric(paste0("0.", paste0(rep(0,
                                                               (sigfigs - 1)), collapse = ""), "1")))) {
        out[a] <- paste0("p<0.", paste0(rep(0, (sigfigs -
                                                  1)), collapse = ""), "1")
      }
      else {
        sig.dec <- (which(strsplit(as.character(numbers[a]),
                                   "")[[1]] == ".") - 1) + sigfigs
        out[a] <- formatC(round(numbers[a], digits = sigfigs),
                          digits = sig.dec, format = "fg", flag = "#")
        out[a] <- paste0("p=", strtrim(out[a], sig.dec +
                                         c(1, 2)[grepl("-", out[a], fixed = TRUE) +
                                                   1]))
      }
    }
  }
  return(out)
}

