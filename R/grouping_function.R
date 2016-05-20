#' @title Groups counts based on range.
#'
#' @description Created to count the number of people in a certain age range. Function tested
#' using a data frame with continuous age values in one column, and counts of
#' people in another column.
#'
#' @param x A dataframe
#' @param group_of The range to group together.
#' @param FUN A string of the function to be performed in the aggregate section.
#'   Default = "sum"
#' @param age_labels Logical. If true, age labels are attempted. Only works if
#'   1st age group starts at 0.
#' @return A dataset that has been groupped by \code{group_of} and has had
#'   \code{FUN} perfomed on the grouping.
#' @examples ### Sum the number of men in each user defined age group
#'  # create data
#'  x <- data.frame(age = c(1:6), males = c(0,1,0,2,3,5), females = c(4,6,2,1,0,1))
#'  group2(x, 3, "sum")
#'@export


group2 <- function(x, group_of, FUN = "sum", age_labels = F) {
  #browser()
  if ((nrow(x) / group_of)%%1 != 0) {
    stop("The number of age groups is incorrect for data set. The number of groups
         should be evenly divisible by the number of rows. i.e. there are 5
         groups you can have 20 rows, but not 21 etc.")
  } else {
    # Accepts inttiger age values and intiger age groups
    group_of <- group_of
    for (i in 1:nrow(x)){
      # Assign groups to each row
      x$group[i]  <- ceiling(i/group_of)
    }
    # Sum by group number
    x2 <- aggregate(. ~ group, x, FUN)

    if (age_labels == T) {
    # Assign labels to group
    x2$labels <- paste((x2$group * group_of)-group_of,
                       "-",
                       (x2$group * group_of) - 1)
    }
    return(x2)
  }
}
