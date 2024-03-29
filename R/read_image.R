#' Imports  and transforms a jpg or tiff file.
#'
#' Imports an image file (in JPEG or TIFF format) into an array, and converts
#' the original \code{R}, \code{G} and \code{B} values in the file into
#' proportions (\code{r}, \code{g} and \code{b} variables).
#'
#' @param file_name A character string containing the name of the image file.
#' @param \dots other parameters passed to the function.
#'
#' @return Returns an object of class  \code{"pixel_transformed_image"}, which
#'   is an array of dimensions \code{r x c x 3},  being \code{r} and \code{c}
#'   the number of rows and columns in the image. The last dimension corresponds
#'   to the original \code{R}, \code{G} and \code{B} variables (= bands) that
#'   define the colours of the pixels. The values in the array are the
#'   proportions of each colour (\code{r, g, b}), i.e. \code{r} = \code{R} /
#'   (\code{R + G + B}), and so on.
#'
#' @details This function calls the functions \code{jpeg::readJPEG()} or
#'   \code{tiff::readTIFF()} to import the image into an R array. Then it
#'   transforms the data into proportions
#'
#' @seealso For more information about jpeg and tiff file formats, see the help
#'   pages of \code{\link[jpeg]{readJPEG}} and
#'   \code{\link[tiff]{readTIFF}} functions.
#'
#' @examples
#'
#' # An example that loads the example file included in the package
#' ivy_oak_rgb <- read_image(system.file("extdata", "IvyOak400x300.JPG",
#'                                        package = "pixelclasser"))
#'
#' @export

read_image <- function(file_name){

  file_name_elements <- unlist(strsplit(file_name, "[.]"))
  if (file_name_elements[length(file_name_elements)] %in%
      c("jpg", "JPG", "jpeg", "JPEG")){
    result <- jpeg::readJPEG(file_name)
  } else {
    if (file_name_elements[length(file_name_elements)] %in%
        c("tif", "TIF", "tiff", "TIFF")){
      result <- tiff::readTIFF(file_name)
    } else {
      stop('The file type of ', file_name, ' is ',
      file_name_elements[length(file_name_elements)],
  ', but must be "jpg", "JPG", "jpeg", "JPEG", "tif", "TIF", "tiff" or "TIFF".')
    }
  }

  result <- transform_colours(result)

  return(result)
}

# is. --------------------------------------------------------------------------
#' @rdname read_image
#' @param x the R object being tested
#' @export

is.transformed_image <- function(x){
  return (inherits(x, "pixel_transformed_image"))
}

# summary ----------------------------------------------------------------------
#' @rdname read_image
#' @param object an object of class \code{"pixel_transformed_image"}.
#' @export

summary.pixel_transformed_image <- function(object, ...){
  
  cat('The image has ', dim(object)[1], ' rows and ', dim(object)[2],
      ' columns.\n', sep = '')
}