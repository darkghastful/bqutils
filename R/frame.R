#' count.n
#'
#' @description Counts the number of unique elements in specified columns of a provided frame or the frame as a whole.
#'
#' @param frame frame
#' @param columns one or more columns (default is NA)
#' @return integer
#' @export
#' @examples
#' frame <- data.frame("a"=rep(1, 9), "b"=c(rep(1, 3), rep(2, 3), rep(3, 3)), "c"=1:9)
#' count.n(frame)
#' count.n(frame, c("a", "b"))
count.n <- function(frame, columns=NA) {
  if(all(!is.na(columns))){
    frame <- frame[, columns]
  }
  if (length(columns) > 1) {
    frame <- frame[!duplicated(frame), ]
    n <- nrow(frame)
  }
  else {
    n <- length(uuln(frame))
  }
  return(n)
}

#' locate.element
#'
#' @description Identifies location of specified element within a column of the frame.
#'
#' @param frame frame
#' @param value value to be identified
#' @param column column
#' @return vector
#' @export
#' @examples
#' frame <- data.frame("a"=rep(1, 9), "b"=c(rep(1, 3), rep(2, 3), rep(3, 3)), "c"=1:9)
#' locate.element(frame, 1, "b")
locate.element <- function(frame, value, column) {
  return(which(frame[, column] == value))
}

#' merge.by.overlap
#'
#' @description Merges two data frames by their overlapping columns.
#'
#' @param frame.list list of frames
#' @param all boolean switch determining inclusion of NA cells (default is FALSE)
#' @return dataframe
#' @export merge.by.overlap
#' @examples
#' frame <- data.frame("a"=rep(1, 9), "b"=c(rep(1, 3), rep(2, 3), rep(3, 3)), "c"=1:9)
#' frame_2 <- data.frame(frame, "d"=rowSums(frame))
#' merge.by.overlap(list(frame, frame_2))
merge.by.overlap <- function(frame.list, all = FALSE) {
  temp.cols <- colnames(frame.list[[1]])
  for(element in 2:length(frame.list)){
    temp.cols <- duplicated.values(c(temp.cols, colnames(frame.list[[element]])))
  }

  merge.frame <- frame.list[[1]]
  for(element in 2:length(frame.list)){
    merge.frame <- merge(merge.frame, frame.list[[element]], by = temp.cols, all = all)
  }

  return(merge.frame)
}

#' rename.column
#'
#' @description (relys on which.col.name) Renames specified column in frame.
#'
#' @param frame frame
#' @param colname.from existing column name
#' @param colname.to relacement column name
#' @return frame
#' @export
#' @examples
#' frame <- data.frame("a"=rep(1, 9), "b"=c(rep(1, 3), rep(2, 3), rep(3, 3)), "c"=1:9)
#' rename.column(frame, "c", "d")
rename.column <- function(frame, colname.from, colname.to) {
  colnames(frame)[which.col.name(frame, colname.from)] <- colname.to
  return(frame)
}

#' row.bind
#'
#' @description Binds multiple frames together by appending rows.
#'
#' @param frames list of data frames
#' @return frame
#' @export
#' @examples
#' frame <- data.frame("a"=rep(1, 9), "b"=c(rep(1, 3), rep(2, 3), rep(3, 3)), "c"=1:9)
#' row.bind(list(frame, frame))
row.bind <- function(frames) {
  merged.frames <- frames[[1]]
  for (a in 2:length(frames)) {
    merged.frames <- rbind(merged.frames, frames[[a]])
  }
  return(merged.frames)
}

#' row.names.to.column
#'
#' @description Preserves a frames row names as a column.
#'
#' @param frame data frame
#' @param numerical.rows boolean switch specifying if row names should be numeric (default is FALSE)
#' @return frame
#' @export row.names.to.column
#' @examples
#' frame <- data.frame("a"=rep(1, 9), "b"=c(rep(1, 3), rep(2, 3), rep(3, 3)), "c"=1:9)
#' rownames(frame) <- LETTERS[1:9]
#' row.names.to.column(frame)
row.names.to.column <- function(frame, numerical.rows = FALSE) {
  frame[, "row.name"] <- row.names(frame)
  if (numerical.rows) {
    row.names(frame) <- 1:nrow(frame)
  }
  return(frame)
}

#' which.col.name
#'
#' @description (replace with which col row name) Identifies location of specified columns in frame.
#'
#' @param dataframe data frame
#' @param col.name column
#' @return vector
#' @export
#' @examples
#' which.col.name(frame, c("c", "a", "b"))
which.col.name <- function(dataframe, col.name) {
  col.name.list <- c()
  for (a in 1:length(col.name)) {
    col.name.list[a] <- which(colnames(dataframe) == col.name[a])
  }
  return(col.name.list)
}

