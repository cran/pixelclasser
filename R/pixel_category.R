#' Creates a category object
#'
#' Creates an object of class \code{"pixel_category"}, which contains a list of
#' objects of class \code{"pixel_subcategory"}.
#'
#' @param cat_name a character string containing the name of the category.
#' @param cat_colour a character string defining the colour to paint the pixels
#'   with when creating a classified picture.
#' 
#' @param \dots in \code{pixel_category()} a list of objects of class
#'   \code{"pixel_subcategory"} or \code{"pixel_rule"}; in \code{summary()},
#'   additional parameters (none needed by now).
#'
#' @return An object of class \code{"pixel_category"} which is a list with the
#'   following elements:
#'   \itemize{
#'   \item \code{name}: the character string with the name of the pixel
#'   category.
#'   \item \code{colour}: a character string describing the
#'   colour of the pixels of the category in the classified images.
#'   \item \code{subcats}: a list of \code{"pixel_subcategory"} objects.}
#'
#' @details The function receives a list of objects of class
#'   \code{"pixel_subcategory"} and creates an object of class
#'   \code{"pixel_category"} with them. However, subcategories are not always
#'   needed (see \code{\link{pixel_subcategory}}). In these cases
#'   \code{"pixel_rule"} objects can be passed to this function, which creates
#'   an internal subcategory object (named "S0") to contain them. See the
#'   examples below.
#'
#'   Note that it is an error to pass a mixture of \code{"pixel_rule"} and
#'   \code{"pixel_subcategory"} objects.
#'
#'   \code{colour} can be specified in any form understood by
#'   \code{grDevices::col2grb}.
#'
#' @seealso \code{\link{pixel_rule}}, \code{\link{pixel_subcategory}},
#'   \code{\link[grDevices]{col2rgb}}
#'
#' @examples
#' # This set of rules is not consistent, they are only useful as examples
#' rule01 <- pixel_rule("R01", "g", "b",
#'                       list(c(0.35, 0.30), c(0.45, 0.10)), ">=")
#' rule02 <- pixel_rule("R02", "g", "b",
#'                       list(c(0.35, 0.253), c(0.45, 0.253)), ">=")
#' rule03 <- pixel_rule("R03", "g", "b",
#'                       list(c(0.35, 0.29), c(0.49, 0.178)), ">=")
#' rule04 <- pixel_rule("R04", "g", "b",
#'                       list(c(0.35, 0.253), c(0.45, 0.253)), "<")
#'
#' subcat01 <- pixel_subcategory("Subcat01", rule01, rule02)
#' subcat02 <- pixel_subcategory("Subcat02", rule03, rule04)
#'
#' cat01 <- pixel_category("Cat01", "#ffae2a", subcat01, subcat02)
#'
#' # A single category defined by a set of rules, not subcategories
#' cat02 <- pixel_category("Cat02", "#00ae2a", rule01, rule02, rule03)
#'
#' @export

pixel_category <- function(cat_name, cat_colour, ...){

  object_list <- list(...)
  object_names <- vapply(match.call(expand.dots = FALSE)$..., deparse,
                         FUN.VALUE = 'vector')

  # Checking the categories of the objects -------------------------------------
  number_of_subcats <- 0
  number_of_rules <- 0

  for (i in seq_along(object_list)){
    if (is.rule(object_list[[i]]) || is.category(object_list[[i]])){
      number_of_rules <- number_of_rules + 1
    } else{
      if (is.subcategory(object_list[[i]])){
        number_of_subcats <- number_of_subcats + 1
      } else {
        stop('Object ', object_names[i], ' is of class ', 
             class(object_names[[i]]),
             'but must be of classes "pixel_rule" or "pixel_subcategory".',
             call. = FALSE)
      }
    }
  }
  if ((number_of_rules > 0) & (number_of_subcats > 0)){
    stop('The objects that define the category must be either rules or ',
         'subcategories, but they are a mixture of both.', call. = FALSE)
  }

  # Creating the object of class "pixel_category" ------------------------------
  if (number_of_rules > 0){
    # Creating a subcategory for the set of rules
    new_subcat <- pixel_subcategory("S0", ...=...)
    result <- c(list(name = cat_name,
                     colour = grDevices::col2rgb(cat_colour)/255,
                     subcats = vector(mode = "list", length = 1)))
    result$subcats[[1]] <- new_subcat
    names(result$subcats)[1] <- "S0"
  } else {
    result <- c(list(name=cat_name, colour=grDevices::col2rgb(cat_colour)/255))
    for (i in seq_along(object_list)){
      result$subcats[[i]] <- object_list[[i]]
    }
    names(result$subcats) <- object_names
  }
  class(result) <- "pixel_category"

  return(result)
}

# is.category ------------------------------------------------------------------
#' @rdname pixel_category
#' @param x the R object being tested
#' @export

is.category<- function(x){
  return (inherits(x, "pixel_category"))
}

# print ------------------------------------------------------------------------
#' @export

print.pixel_category <- function(x,
                                 digits = NULL,
                                 quote = TRUE,
                                 na.print = NULL,
                                 print.gap = NULL,
                                 right = FALSE,
                                 max = NULL,
                                 useSource = TRUE,
                                 ...){
  cat('\nCategory name: "', x$name, '"\n', sep = '')
  cat('Colour (as RGB [0:255] values): (',
      round(x$colour[1,1]*255), ', ', 
      round(x$colour[2,1]*255), ', ',
      round(x$colour[3,1]*255), ')\n',
      sep = '')
  cat("Subcategories:\n\n")
  for (i in seq_along(x$subcats)){
    print(x$subcats[i])
  }
}

# summary ----------------------------------------------------------------------
#' @rdname pixel_category
#' @param object an object of class \code{"pixel_category"}.
#' @export

summary.pixel_category <- function(object, ...){
  
  cat('Category "', object$name,
      '" contains the following subcategories and rules:\n', sep = '')
  for (i in seq_along(names(object$subcats))){
    cat('    Subcategory: "', names(object$subcats[i]),
        '" contains:\n', sep = '')
    for (j in seq_along(names(object$subcats[[i]]$rules))){
      cat('        "', names(object$subcats[[i]]$rules)[j], '"\n', sep = '')
    }
  }
}