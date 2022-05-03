#' Classifies the pixels of an image
#'
#' Classifies the pixels represented in an object of class
#' \code{"pixel_transformed_image"} using the rules contained in a list of
#' objects of class \code{"pixel_category"}.
#'
#' @param image_prop an object of class \code{"pixel_transformed_image"} created
#'   by function \code{read_image()}.
#' @param \dots in \code{classify_pixels()} a list of objects of class
#'   \code{"pixel_category"}; in \code{summary()}, additional parameters (none
#'   needed by now).
#' @param unclassed_colour a character string defining the colour of the
#'   unclassified pixels. Defaults to \code{"black"}.
#' @param verbose a logical value. When \code{TRUE} (default) the function
#'   prints some statistics about the classification.
#'
#' @return Returns an object of class \code{"pixel_classified_image"}, which is
#'   a list containing nested lists. Each first-level element corresponds to one
#'   of the pixel categories and its name is the category name. They contains
#'   the second-level list, which have the following elements:
#'   \itemize{
#'   \item \code{colour}: a matrix defining a colour to paint the pixels in the
#'   classified image. Inherited from the \code{"pixel_class"} object defining
#'   the
#'   class.
#'   \item \code{incid_mat}: a logical matrix where \code{TRUE} values
#'   indicate that the pixel belongs to this pixel category. }
#'
#' @details This function uses the rules contained in \code{"pixel_category"}
#'   objects to create a set of incidence matrices indicating whether a pixel
#'   belongs to a pixel category or not.
#'
#'   To create the incidence matrices for each category, a matrix for each rule
#'   is created and then combined with the matrices of the other using the
#'   \code{and} operator. An additional matrix identifies the pixels that do not
#'   belong to the defined categories, i.e. unclassed pixels. It is always
#'   present and named \code{unclassified} even in the case that all pixels were
#'   assigned to some class.
#'
#'   When a category is divided in a set of subcategories the pixels are
#'   classfied in subcategories as explained in the previous paragraph, and then
#'   the matrices of the subcategories are combined again, this time using the
#'   \code{or} operator. See \code{"pixel_subcategory"} for more details.
#'
#'   \code{unclassed_colour} can be specified in any form understood by
#'   \code{grDevices::col2grb}.
#'
#' @seealso \code{\link{pixel_category}}, \code{\link[grDevices]{col2rgb}}.
#'
#' @examples
#'
#' # These are the steps to classify an example image supplied in the package
#'
#' yellow <- "#ffcd0eff"
#' blue <- "#5536ffff"
#'
#' ivy_oak_rgb <- read_image(system.file("extdata", "IvyOak400x300.JPG",
#'                           package = "pixelclasser"))
#'
#' rule_01 <- pixel_rule("rule_01", "g", "b",
#'                        list(c(0.345, 1/3), c(0.40, 0.10)), comp_op = "<")
#' rule_02 <- pixel_rule("rule_02", "g", "b",
#'                        list(c(0.345, 1/3), c(0.40, 0.10)), comp_op = ">=")
#'
#' cat_dead_leaves <- pixel_category("dead_leaves", blue, rule_01)
#' cat_living_leaves <- pixel_category("living_leaves", yellow, rule_02)
#'
#' ivy_oak_classified <- classify_pixels(ivy_oak_rgb, cat_dead_leaves,
#'                         cat_living_leaves)
#'
#' @export

classify_pixels <- function(image_prop, ..., unclassed_colour = "black",
                            verbose = TRUE){

  # Function eval() below needs this definitions to work
  r <- 1
  g <- 2
  b <- 3

  # Checking the categories of the objects -------------------------------------
  cats_list <- list(...)
  cats_names <- vapply(match.call(expand.dots = FALSE)$...,
                       deparse, FUN.VALUE = 'vector')
  num_cats <- length(cats_list)

  for (i in seq_along(cats_list)){
    if (!is.category(cats_list[[i]])){
      stop('Object ', cats_names[i], ' is of class ', class(cats_list[[i]]), 
          ' but must be of class pixel_category.',call. = FALSE)
    }
  }

  result <- vector(mode = "list", length = num_cats + 2)
  cats_names <- c(cats_names, "unclassified", "summary")
  names(result) <- cats_names

  # Creating the incidence matrix corresponding to the unclassified pixels
  result[[num_cats + 1]] <- list("colour" =
                                 (grDevices::col2rgb(unclassed_colour) / 255),
                                 "incid_mat" = matrix(nrow = dim(image_prop)[1],
                                 ncol = dim(image_prop)[2], data = TRUE))

  # Auxiliary matrices for temporal storage
  cat_incid    <- matrix(data = FALSE, nrow = dim(image_prop)[1],
                         ncol = dim(image_prop)[2])
  subcat_incid <- matrix(data = TRUE, nrow = dim(image_prop)[1],
                         ncol = dim(image_prop)[2])

  for (category in 1:num_cats){
    for (subcateg in seq_along(cats_list[[category]]$subcats)){
      for (rule in seq_along(cats_list[[category]]$subcats[[subcateg]]$rules)){
        actual_rule <- cats_list[[category]]$subcats[[subcateg]]$rules[[rule]]
        subcat_incid <- subcat_incid & eval(parse(text = actual_rule$rule_text))
      }
      cat_incid <- cat_incid | subcat_incid
      subcat_incid[,] <- TRUE
    }

    result[[category]] <- list("colour" = cats_list[[category]]$colour,
                               "incid_mat" = cat_incid)

    # Setting classified pixels = FALSE in the "unclassified" incidence matrix
    result[[num_cats + 1]]$incid_mat <-
      result[[num_cats + 1]]$incid_mat & (!cat_incid)

    names(result)[category] <- cats_list[[category]]$name
    cat_incid[,] <- FALSE
  }

  # Checking for pixels counted twice or more
  classified_pixels <- 0
  message_list <- vector(mode = "list", length = (num_cats + 4))
  for (category in 1:(num_cats + 1)){
    classified_pixels  <- classified_pixels + sum(result[[category]]$incid_mat)
    message_list[[category]] <- paste('Pixels in category "',
                                      names(result)[category],
                                      '": ',
                                      sum(result[[category]]$incid_mat),
                                      '\n', sep = '')
    if (verbose){
      message(message_list[[category]])
    }
  }

  # Checks for errors in the rules
  number_pixels <- length(cat_incid)
  duplicate_pixels <- classified_pixels - number_pixels
  if (duplicate_pixels != 0){
    message_list[[num_cats + 2]] <-
      "There are pixels counted twice. Revise the rules."
    warning(message_list[[num_cats + 2]], call. = FALSE)
  } else {
    message_list[[num_cats + 2]] <- 
      "No pixels were counted twice. The rules seem correct."
  }
  message_list[[num_cats + 3]] <- paste('Duplicate pixels:',
                                        duplicate_pixels, '\n')
  message_list[[num_cats + 4]] <- paste('Total number of pixels:', 
                                        number_pixels, '\n')
  if (verbose){
    message(message_list[[num_cats + 3]])
    message(message_list[[num_cats + 4]])
  }

  # Adding the incidence matrix for unclassified pixels 
  result[[num_cats + 1]]$incid_mat <- !(result[[num_cats + 1]]$incid_mat)
  result$summary <- message_list
  class(result) <- "pixel_classified_image"
  return(result)
}

# is. --------------------------------------------------------------------------
#' @rdname classify_pixels
#' @param x the R object being tested.
#' @export

is.classified_image <- function(x){
  return (inherits(x, "pixel_classified_image"))
}

# print ------------------------------------------------------------------------
#' @export

print.classified_image <- function(x, digits = NULL, quote = TRUE,
                                   na.print = NULL, print.gap = NULL,
                                   right = FALSE, max = NULL, useSource = TRUE,
                                   ...){
  for (item in seq(1, length(x)- 1)){
    cat('\nCategory name: "', names(x)[item], '"\n')
    cat('Colour (as RGB [0:255] values): (',
        round(x[[item]]$colour[1,1]*255), ', ',
        round(x[[item]]$colour[2,1]*255), ', ',
        round(x[[item]]$colour[3,1]*255), ')\n',
        sep = '')
    cat(x$summary[[item]], '\n')
  }
  cat(x$summary[[length(x)]], '\n')
}

# summary ----------------------------------------------------------------------
#' @rdname classify_pixels
#' @param object an object of class \code{"pixel_classified_image"}.
#' @export

summary.pixel_classified_image <- function(object, ...){
  
  cat('Classification results:\n', sep = '')
  cat('    ', object$summary[[1]])
  cat('    ', object$summary[[2]])
  cat('    ', object$summary[[3]])
  cat('    ', object$summary[[4]])
  cat('    ', object$summary[[5]])
}