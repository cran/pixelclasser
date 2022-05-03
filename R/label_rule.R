#' Adds a label to the rule
#'
#' This function adds a label to a line representing a rule on the plots created
#' by \code{plot_rgb_plane()}.
#'
#' @param rule an object of class \code{"pixel_rule"}.
#' @param label a string to label the line. It is attached at the coordinates of
#'   the start (first point) of the line.
#' @param shift a numeric vector to set the displacement of the label from the
#'   start of the line. Expressed in graph units, defaults to \code{c(0, 0)}.
#' @param \dots additional graphical parameters passed to the underlying
#'   \code{graphics::text()} function.
#'
#' @return The function does not return any value.
#'
#' @details The function uses the information stored in \code{"pixel_rule"}
#'   objects to label the line at its start point. The \code{shift} values,
#'   expressed in plot coordinates, are added to the coordinates of that point
#'   to place the label elsewhere. Note that \dots can be used to pass
#'   additional parameters (\code{pos} and \code{adj}) to the underlying
#'   \code{graphics::text()} function, to further adjust the position of the
#'   label. Pass a character string understood by \code{grDevices::col2rgb()} to
#'   set the colour of the label (\code{col}).
#'
#' @seealso \code{\link{plot_rgb_plane}}, \code{\link{pixel_rule}},
#'   \code{\link[grDevices]{col2rgb}}, \code{\link[graphics]{text}}
#'
#' @examples
#' rule_01 <- pixel_rule("rule_01", "g", "b",
#'                        list(c(0.1, 0.8), c(0.40, 0.10)), "<")
#' plot_rgb_plane("g", "b")
#'
#' # The rule is represented as a green line
#' plot_rule(rule_01, col = "green")
#'
#' # And the label is added in three different positions by passing col and adj
#' # to the underlying function
#' label_rule(rule_01, label = expression('R'[1]*''), shift = c(0,0),
#'            col = 'black', adj = 1.5)
#' label_rule(rule_01, label = expression('R'[1]*''), shift = c(0.2, -0.4),
#'            col = 'blue', adj = 0)
#' label_rule(rule_01, label = expression('R'[1]*''), shift = c(0.3, -0.7),
#'            col = 'black', adj = -0.5)
#'
#' @export

label_rule <- function(rule, label = '', shift = c(0, 0), ...){

  if (!is.rule(rule)){
    stop('The object ', deparse(substitute(rule)), ' is of class ', class(rule),
         ' but it must be of class "pixel_rule".', call. = FALSE)
  }
  
  x_pos <- rule$first_point[1] + shift[1]
  y_pos <- rule$first_point[2] + shift[2]
  graphics::text(x_pos, y_pos, label, ... = ...)
}