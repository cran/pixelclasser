#' Creates a rule object
#'
#' Creates an object of class \code{pixel_rule} from a line in \code{rgb} space,
#' defined by the user, and a relational operator.
#'
#' @param rule_name a character string containing the name of the rule.
#' @param x_axis a character string selecting the colour variable used as x
#'   axis, one of \code{"r"}, \code{"g"} or \code{"b"}.
#' @param y_axis a character string selecting the colour variable used as y
#'   axis, one of \code{"r"}, \code{"g"} or \code{"b"}.
#' @param line_points either an object of  of class \code{"rule_points"} created
#'   with function \code{place_rule()}, or a list containing the coordinates of
#'   two points defining the line. The coordinates are two-element numeric
#'   vectors.
#' @param comp_op a character string containing one of the comparison operators
#'   \code{">", ">=", "<", "<="}.
#' @param \dots additional parameters to pass to the function.
#' 
#' @return An object of class \code{"pixel_rule"} containing these elements:
#'   \itemize{
#'   \item \code{rule_name}: a character string containing the rule name.
#'   \item \code{rule_text}: a character string containing the mathematical
#'   expression of the rule.
#'   \item \code{comp_op}: a character string containing the comparison operator
#'   used in the rule.
#'   \item \code{a}: a numerical vector containing the parameter \code{a}
#'   (slope) of the line.
#'   \item \code{c}: a numerical vector containing the parameter \code{c}
#'   (intercept) of the line.
#'   \item \code{x_axis}: a character string containing the colour variable
#'   selected as \code{x} axis (one of "r", "g" or "b").
#'   \item \code{y_axis}: a character string containing the colour variable
#'   selected as \code{y} axis.
#'   \item \code{first_point}: a numerical vector containing the coordinates of
#'   the first point used to estimate the line equation.
#'   \item \code{second_point}: a numerical vector containing the coordinates of
#'   the second point. }
#'
#' @details This function estimates the slope (\code{a}) and intercept
#'   (\code{c}) of the line \code{y = ax + c} from the coordinates of two points
#'   on the line. \code{x} and \code{y} are two colour variables selected by the
#'   user (\code{r}, \code{g}, or \code{b}). The line divides the plane in two
#'   subsets and the comparison operator selects the subset that contains the
#'   points (pixels) of interest.
#'
#'   When the line is defined by a list containing a couple of points, it is
#'   first converted into an object of class \code{"pixel_rule_points"} (see
#'   \code{\link{place_rule}}, and the examples below).
#'
#'   The lines are mathematical objects that extend without bound, i.e. all
#'   along the x axis. The pair of points do not set the line limits, they only
#'   allow the estimation of the line parameters. Therefore, they are not
#'   constrained to be inside the triangular area occupied by the pixels, and
#'   the points can be selected in the most convenient way, provided that the
#'   line divides correctly the categories. Convenience in this context means
#'   that the line should seem nice in the plot, if this matters.
#'
#'   Because the variables were transformed into proportions, the pixels in the
#'   plot are always inside the triangle defined by the points \code{(0, 0), (1,
#'   0), (0, 1)}. So, the sides of this triangle can be considered as implicit
#'   rules which do not need to be created explicitly. In this way, a single
#'   line creates two polygons by cutting the triangle in two. Usually, the
#'   implicit rules reduce the number of rules to create.
#'
#' @seealso \code{\link{pixel_subcategory}}, \code{\link{pixel_category}},
#'   \code{\link{plot_rule}}, \code{\link{plot_rgb_plane}}
#'   
#' @examples
#' # Creating the line by passing the coordinates of two points on the line:
#' rule01 <- pixel_rule("rule01", "g", "b",
#'                       list(c(0.35, 0.30), c(0.45, 0.10)),">")
#'
#' # A vertical line as a rule; note that the equation will be simplified
#' rule02 <- pixel_rule("rule02", "g", "b",
#'                       list(c(0.35, 0.30), c(0.35, 0.00)), ">")
#' \dontrun{
#' # Creating the rule by passing an object of type rule_point:
#' rule_points01 <- place_rule("g", "b")
#' rule03 <- pixel_rule("rule03", "g", "b", rule_points01,">")
#'
#' # Note that the creation of the intermediate object can be avoided:
#' rule04 <- pixel_rule("rule04", "g", "b", place_rule("g", "b"),">")
#' }
#' @export

pixel_rule <- function(rule_name, x_axis, y_axis, line_points, comp_op){

  x <- 1
  y <- 2
  
  # line_points type checking and optional rule_points creation
  if (!is.rule_points(line_points)){
    if (is.list(line_points)){
      if (length(line_points) != 2){
        stop('The list passed in "line_points" must contain the coordinates of',
          ' two points, but contains ', length(line_points), ' points.',
          call. = FALSE)
      }
      internal_rule_points <- list("x_axis" = x_axis,
                                   "y_axis" = y_axis, 
                                   "first_point" = line_points[[1]], 
                                   "second_point" = line_points[[2]])
      class(internal_rule_points) <- "rule_points"
    } else {
      stop('The object passed in "line_points" must be of class "list" or',
           ' "pixel_rule_points", but it is of class "',
           class(line_points), '".', call. = FALSE)
    }
  } else {
    internal_rule_points <- line_points
  }

  # Parameter checks -----------------------------------------------------------
  if (is.rule_points(line_points)){
    if (!(identical(x_axis, line_points$x_axis))){
      stop('The value in x_axis, "', x_axis, '", does not match the value in ',
           deparse(substitute(line_points)), ', "', line_points$x_axis, '".',
           call. = FALSE)
    }

    if (!(identical(y_axis, line_points$y_axis))){
      stop('The value in y_axis, "', y_axis, '", does not match the value in ',
           deparse(substitute(line_points)), ', "', line_points$y_axis, '".',
           call. = FALSE)
    }
  }

  if (!(x_axis %in% c('r', 'g', 'b'))){
    stop('The value in x_axis is "', x_axis, '" but must be one of "r", "g" ',
         'or "b".', call. = FALSE)
  }
  if (!(y_axis %in% c('r', 'g', 'b'))){
    stop('The value in y_axis is "', y_axis, '" but must be one of "r", "g" ',
         'or "b".', call. = FALSE)
  }

  if (!(comp_op %in% c(">", ">=", "<", "<="))){
    stop('The comparison operator is "', comp_op,
         '", but must be one of ">", ">=", "<" or "<=".', call. = FALSE)
  }
  
  if (identical(x_axis, y_axis)){
    stop('x_axis and y_axis must be different.')
  }
  if (identical(internal_rule_points$first_point,
                internal_rule_points$second_point)){
    stop('Start and end points are the same.')
  }

  # Line estimation ------------------------------------------------------------
  a <- (internal_rule_points$second_point[y] - 
          internal_rule_points$first_point[y]) /
       (internal_rule_points$second_point[x] - 
          internal_rule_points$first_point[x])

  if (is.infinite(a)){
    # A vertical line; formula: x_axis <comp_op> c
    c <- internal_rule_points$first_point[x]
    rule_text <- paste("image_prop[,, ", x_axis, "]",comp_op, c)
  } else{
    # Other lines: formula: y_axis <comp_op> a * x_axis + c
    c <- internal_rule_points$first_point[y] -
      (a * internal_rule_points$first_point[x])
    rule_text <- paste("image_prop[,, ", y_axis, "]", comp_op,
                       a, "*", "image_prop[,,", x_axis, "]", "+", c)
  }
  # Result construction --------------------------------------------------------
  result <- list("rule_name" = rule_name,
                 "rule_text" = rule_text,
                 "comp_op" = comp_op,
                 "a" = a,
                 "c" = c,
                 "x_axis" = x_axis,
                 "y_axis" = y_axis,
                 "first_point" = internal_rule_points$first_point,
                 "second_point" = internal_rule_points$second_point)
  class(result) <- c("pixel_rule")

  return(result)
}

# is. --------------------------------------------------------------------------
#' @rdname pixel_rule
#' @param x the R object being tested
#' @export

is.rule <- function(x){
  return (inherits(x, "pixel_rule"))
}

# print ------------------------------------------------------------------------
#' @export

print.pixel_rule <- function(x,
                             digits = NULL,
                             quote = TRUE,
                             na.print = NULL,
                             print.gap = NULL,
                             right = FALSE,
                             max = NULL,
                             useSource = TRUE,
                             ...){
  
  cat('Rule name: "', x$rule_name, '"\n', sep = '')
  if (x$c < 0){
    cat("Line equation: ", x$y_axis, ' ', x$comp_op, ' ', x$a, ' ', x$x_axis,
        ' - ', abs(x$c), '\n', sep = '')
  } else {
    cat("Line equation: ", x$y_axis, ' ', x$comp_op, ' ', x$a, ' ', x$x_axis,
        ' + ', x$c, '\n', sep = '')
  }
  cat("Point coordinates: (", x$first_point[1], ', ', x$first_point[2], '); (',
      x$second_point[1], ', ', x$second_point[2], ')', sep =  '')
}

# summary ----------------------------------------------------------------------
#' @rdname pixel_rule
#' @param object an object of class \code{"pixel_category"}.
#' @export

summary.pixel_rule <- function(object, ...){
  
  cat('Rule "', object$rule_name, '" is defined by this equation:\n', sep = '')
  if (object$c < 0){
    cat('    ', object$y_axis, ' ', object$comp_op, ' ', object$a, ' ', 
        object$x_axis, ' - ', abs(object$c), '\n', sep = '')
  } else {
    cat("    ", object$y_axis, ' ', object$comp_op, ' ', object$a, ' ', 
        object$x_axis, ' + ', object$c, '\n', sep = '')
  }
  if (object$a == "Inf"){
    cat('The slope value "Inf" denotes that this is a vertical line.\n')
  }
  if (object$a == 0){
    cat('The slope value "0" denotes that this is a horizontal line.\n')
  }
}