#' Plot the pixels of a transformed image
#'
#' This function is a wrapper for function \code{graphics::points()} for
#' plotting the pixels of an object of class \code{"pixel_transformed_image"} on
#' an rgb plot.
#'
#' @param image_rgb an object of class \code{"pixel_transformed_image"} produced
#'   by \code{read_image()}.
#' @param x_axis a character string indicating the colour variable that
#'   corresponds to the x axis, one of \code{"r"}, \code{"g"} or \code{"b"}.
#' @param y_axis a character string indicating the colour variable that
#'   corresponds to the x axis.
#' @param \dots additional graphical parameters to be passed to
#'   \code{graphics::points()}, mainly to set the colour (\code{col}) of the
#'   points.
#'
#' @return The function does not return any value.
#'
#' @details It is advantageous to specify a colour such as \code{"#00000005"}
#'   which is black but almost transparent. In this way a kind of density plot
#'   is created because the clustering of points creates areas of darker colour.
#'   Note that a colour without specific transparency information defaults to an
#'   opaque colour, so \code{"#000000"} is the same as \code{"#000000ff"}. The
#'   colours can be specified in any form understandable by
#'   \code{grDevices::col2rgb}, but the hexadecimal string allows setting the
#'   colour transparency. Note also that the points are plotted using \code{pch
#'   = "."}, as any other symbol would clutter the graph.
#'
#'   Warning: plotting several million points in an R graph is a slow process.
#'   Be patient or reduce the size of the images as much as possible. A nice
#'   smartphone with a petapixel camera sensor is good for artistic purposes,
#'   but not always for efficient scientific work.
#'
#' @seealso \code{\link{plot_rgb_plane}}, \code{\link[grDevices]{col2rgb}}
#'
#' @examples
#'
#' # Plotting the pixels of the example image included in this package
#' ivy_oak_rgb <- read_image(system.file("extdata", "IvyOak400x300.JPG",
#'                                        package = "pixelclasser"))
#' plot_rgb_plane("g", "b")
#' plot_pixels(ivy_oak_rgb, "g", "b", col = "#00000005")
#'
#' @export

plot_pixels <- function(image_rgb, x_axis, y_axis, ...){

  if (is.transformed_image(image_rgb)){
    if (!(x_axis %in% c('r', 'g', 'b'))){
      stop('The x_axis is "', x_axis, '" but must be "r", "g" or "b"',
           call. = FALSE)
    }
    if (!(y_axis %in% c('r', 'g', 'b'))){
      stop('The y_axis is "', y_axis, '", but must be "r", "g" or "b"',
           call. = FALSE)
    }
    graphics::points(image_rgb[,, x_axis], image_rgb[,, y_axis],
                     pch = ".", ... =  ...)
  } else {
    stop('The image object, ', deparse(substitute(image_rgb)), 
         ', is of class "', class(image_rgb),
         '", but must be of class "transformed_image".', call. = FALSE)
  }
}
