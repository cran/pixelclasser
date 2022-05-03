#' Places a line on the rgb plot
#'
#' A wrapper function for \code{graphics::locator} that helps in creating rules.
#'
#' @param x_axis a character string indicating the colour variable that
#'   corresponds to the x axis, one of \code{"r"}, \code{"g"} or \code{"b"}.
#' @param y_axis a character string indicating the colour variable that
#'   corresponds to the y axis, one of \code{"r"}, \code{"g"} or \code{"b"}.
#' @param line_type a character string indicating that the line is vertical
#'   (\code{"v"}), horizontal (\code{"h"}) or free (\code{"f"}, the default).
#'
#' @return An object of class \code{"pixel_rule_points"} containing these
#'   elements:
#' \itemize{
#' \item \code{x_axis}: a character string containing the colour variable
#'   selected as \code{x} axis (one of "r, "g or "b").
#' \item \code{y_axis}: a character string containing the colour variable
#'   selected as \code{y} axis.
#' }
#' 
#' @details This function calls \code{graphics::locator} on a previously plotted
#'   rgb plane to select two points with the mouse. Then it plots the line
#'   joining them and returns an object of class \code{"pixel_rule_object"}.
#'   These objects are passed as parameters to \code{pixel_rule()} to create
#'   \code{"pixel_rule"} objects.
#'
#'   True horizontal and vertical lines are difficult to create by hand. In
#'   these cases, specifying \code{"vertical"} or \code{"horizontal"} (partial
#'   match allowed, i.e. \code{"h"}) will copy the appropriate coordinate value
#'   from the first point to the second to make them the same. Note that this is
#'   done after \code{locator()} returns, so the plot will show the line joining
#'   the original points, not the corrected ones. Use \code{plot_rule()} to see
#'   the corrected line.
#'
#' @seealso \code{\link[graphics]{locator}}, \code{\link{pixel_rule}},
#'   \code{\link{plot_rule}}, \code{\link{plot_rgb_plane}}
#' @examples
#' \dontrun{
#' plot_rgb_plane("r", "g")
#' line01 <- place_rule("r", "g")          # A "free" line
#' line02 <- place_rule("r", "g", "h")     # A horizontal line
#' }
#' 
#' @export

place_rule <- function(x_axis, y_axis, line_type = 'f'){

  # Parameter tests ------------------------------------------------------------
  if (missing(x_axis) | missing(y_axis)){
    stop('X or Y colour variable is missing', call. = FALSE)
  } else {
    if (!(x_axis %in% c("r", "g", "b"))){
      stop('X colour variable is "', x_axis, 
           '", but must be one of "r", "g" or "b".', call. = FALSE)
    }
    if (!(y_axis %in% c("r", "g", "b"))){
      stop('Y colour variable is "', y_axis, 
           '", but must be one of "r", "g" or "b".', call. = FALSE)
    }
    if (!(line_type %in% c("h", "v", "f"))){
      stop('Parameter "line_type" is "', line_type, 
           '", but must be one of "v", "h" or "f".', call. = FALSE)
    }
  }
  
  # Main code ------------------------------------------------------------------
  x <- 1
  y <- 2
  
  result <- vector(mode = "list", length = 4)
  names(result) <- c("x_axis", "y_axis", "first_point", "second_point")
  class(result) <- "pixel_rule_points"
  
  coordinates <- graphics::locator(n = 2, type = "l")
  result$first_point <- c(coordinates$x[1], coordinates$y[1])
  result$second_point <- c(coordinates$x[2], coordinates$y[2])
  names(result$first_point) <- c("x", "y")
  names(result$second_point) <- c("x", "y")
  result$x_axis <- x_axis
  result$y_axis <- y_axis
  
  if (line_type == "v"){
    result$second_point["x"] <- result$first_point["x"]
  }
  
  if (line_type == "h"){
    result$second_point["y"] <- result$first_point["y"]
  }
  
  return(result)
}

# is. --------------------------------------------------------------------------
#' @rdname place_rule
#' @param x the R object being tested
#' @export

is.rule_points<- function(x){
  return (inherits(x, "pixel_rule_points"))
}

# print ------------------------------------------------------------------------
#' @export

print.rule_points <- function(x,
                              digits = NULL,
                              quote = TRUE,
                              na.print = NULL,
                              print.gap = NULL,
                              right = FALSE,
                              max = NULL,
                              useSource = TRUE,
                              ...){
  
  cat('Colour variables: "', x$x_axis, '", "', x$y_axis, '"\n', sep = '')
  cat('First point: (', x$first_point['x'],', ', x$first_point['y'], ')\n',
      sep = '')
  cat('Second point: (', x$second_point['x'],', ', x$second_point['y'], ')\n',
      sep = '')
}
