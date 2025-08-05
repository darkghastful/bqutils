#' Curve equation
#'
#' @param x.y either "x" or "y"
#' @param x1 x coord for first point
#' @param y1 y coord for first point
#' @param x2 x coord for second point
#' @param y2 y coord for second point
#' @param a a value of curve
#'
#' @return list(pos.function, neg.function)
#' @export
#'
#' @examples
#' curve.equation("y", 0, 0, 3, 3, 0.7)
curve.equation <- function(x.y, x1, y1, x2, y2, a) {
  b <- ((a*((x2^2)-(x1^2)))+y1-y2)/(x1-x2)
  c <- (y1)-(((x1)^2)*(a))-((x1)*(((a*((x2^2)-(x1^2)))+y1-y2)/(x1-x2)))

  if(x.y=="x"){
    equation <- paste0("function(x) {((", a, "*(x^2))+(x*(", b, "))+(", c, "))}")
    return(equation)
  }else if(x.y=="y"){
    pos.function <- paste0("function(y) {(-", b, "+sqrt((", b, "^2) + ((4*", a, ")*(-", c, "+y))))/(2*", a, ")}")
    neg.function <- paste0("function(y) {-(", b, "+ sqrt((", b, "^2) - (4*", a, "*", c, ") + (4*", a, "*y)))/(2*", a, ")}")
    return(list(pos.function, neg.function))
  }
}

#' Slope
#'
#' @param point.1 coords of first point c(x, y)
#' @param point.2 coords of second point c(x, y)
#'
#' @return slope
#' @export
#'
#' @examples
#' slope (c(0,3), c(9, 6))
slope <- function(point.1, point.2){
  return((point.1[2]-point.2[2])/(point.1[1]-point.2[1]))
}


#' Linear equation
#' @description Creates a linear equation with the given information.
#' Can provide x, y, and slope or y and equation
#' linear.equation(y, equation) out:list(x, y)
#' linear.equation(x, equation) out:
#' list(x, y)
#' linear.equation(x, y, slope) out:list(slope, b)
#'
#' @param x numeric
#' @param y numeric
#' @param slope numeric
#' @param equation named list("slope"=numeric, "b"=numeric)
#'
#' @return varies
#' @export
#'
#' @examples
#' linear.equation(3, 9, 4/5)
linear.equation <- function(x=NA, y=NA, slope=NA, equation=NA){
  if(all(is.na(x), !is.na(y), class(equation)=="list")){
    x <- ((y)-(equation$b))/(equation$slope)
    return(list(x, y))
  }else if(all(is.na(y), !is.na(x), class(equation)=="list")){
    y <- (x*equation$slope)+(equation$b)
    return(list(x, y))
  }else if(all(class(equation)=="logical", !is.na(x), !is.na(y), !is.na(slope))){
    b <- (y)-(x*slope)
    equation <- list(slope, b)
    names(equation) <- c("slope", "b")
    return(equation)
  }else{
    return(list(x, y))
  }
}


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

