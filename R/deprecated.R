#' Deprecated functions
#'
#' @details These functions were constructors for pixelclasser objects, but now
#'   they are substituted by constructors with the same name as the class, as is
#'   customary in R.
#' @param cat_name a character string containing the name of the category.
#' @param cat_colour a character string defining the colour to paint the pixels
#'   with when creating a classified picture.
#' @param \dots a list of \code{pixel_subcat} objects, or \code{pixel_rule}
#'   objects in case that subcategories are not needed. A mixed list of
#'   \code{pixel_rule} and \code{pixel_subcat} objects is not allowed.
#' @param rule_name a character string containing the name of the rule.
#' @param x_axis a character string selecting the colour variable used as x
#'   axis, one of \code{"r"}, \code{"g"} or \code{"b"}.
#' @param y_axis a character string selecting the colour variable used as y
#'   axis, one of \code{"r"}, \code{"g"} or \code{"b"}.
#' @param rule_points either an object of  of class \code{"rule_points"} created
#'   with function \code{place_rule()}, or a list containing the coordinates of
#'   two points defining the line.
#' @param comp_op a character string containing one of the comparison operators
#'   \code{">", ">=", "<", "<="}.
#' @param subcat_name a character string containing the name of the subcategory.
#' @param \dots a list of objects of class \code{pixel_rule}.
#'
#' @seealso \code{\link{pixel_category}}, \code{\link{pixel_subcategory}} and
#'   \code{\link{pixel_rule}}.
#' @name deprecated
NULL
#> NULL

# define_cat -------------------------------------------------------------------
#' @rdname deprecated
#' @export

define_cat <- function(cat_name, cat_colour, ...){
  
  .Deprecated("pixel_category")
  
  object_list <- list(...)
  object_names <- vapply(match.call(expand.dots = FALSE)$...,
                         deparse,
                         FUN.VALUE = 'vector')
  
  # Checking the categories of the objects -------------------------------------
  number_of_subcats <- 0
  number_of_rules <- 0
  
  for (i in seq_along(object_list)){
    if (identical(class(object_list[[i]]), "pixel_rule")){
      number_of_rules <- number_of_rules + 1
    } else{
      if (identical(class(object_list[[i]]), "pixel_subcategory")){
        number_of_subcats <- number_of_subcats + 1
      } else {
        stop("Object ", object_names[i],
            " is neither of class pixel_rule nor of class pixel_subcategory.\n",
             call. = FALSE)
      }
    }
  }
  if ((number_of_rules > 0) & (number_of_subcats > 0)){
    stop("The objects must be rules or subcategories, not a mixture",
         .call = FALSE)
  }
  
  # Creating the object of class "pixel_category" ------------------------------
  if (number_of_rules > 0){
    # Creating a subcategory for the set of rules
    new_subcat <- define_subcat("S0", ...=...)
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

# define_subcat ----------------------------------------------------------------
#' @rdname deprecated
#' @export

define_subcat <- function(subcat_name,  ...){
  
  .Deprecated("pixel_subcategory")
  
  rules_list <- list(...)
  rules_names <- vapply(match.call(expand.dots = FALSE)$..., deparse,
                        FUN.VALUE = 'vector')
  
  for (i in seq_along( rules_list)){
    if (!identical(class( rules_list[[i]]), "pixel_rule")){
      stop("Object ", rules_names[i], " is not of class 'pixel_rule'.\n",
           "See function define_rule()", call. = FALSE)
    }
  }
  
  rule_names <- character(length = length(rules_list))
  result <- list("name" = subcat_name,
                 rules = vector(mode = "list", length = length( rules_list)))
  
  for (i in seq_along(rules_list)){
    result$rules[[i]] <-  rules_list[[i]]
    names(result$rules)[i] <- rules_list[[i]]$rule_name
  }
  class(result) <- "pixel_subcategory"
  
  return(result)
}

# define_rule ------------------------------------------------------------------
#' @rdname deprecated
#' @export

define_rule <- function(rule_name, x_axis, y_axis, rule_points, comp_op){
  
  .Deprecated("pixel_rule")
  
  x <- 1
  y <- 2
  
  if (identical(class(rule_points), "list")){
    if (length(rule_points) != 2){
      stop('rule_points must contain two points', call. = FALSE)
    }
    names(rule_points) <- c("first_point", "second_point")
    class(rule_points) <- "rule_points"
  } else {
    if (!identical(class(rule_points), "rule_points")){
      stop('rule points must contain a list or a rule_point object',
           call. = FALSE)
    }
    if (!(identical(x_axis, rule_points$x_axis) & 
          identical(y_axis, rule_points$y_axis))){
      stop('rule_points axis are not the same as x_axis and y_axis',
           .call = FALSE)
    }
  }
  
  # Parameter checks -----------------------------------------------------------
  
  if (!(x_axis %in% c('r', 'g', 'b'))){
    stop('The x_axis must be one of "r", "g" or "b"', call. = FALSE)
  }
  if (!(y_axis %in% c('r', 'g', 'b'))){
    stop('The y_axis must be one of "r", "g" or "b"', call. = FALSE)
  }
  if (!(comp_op %in% c(">", ">=", "<", "<="))){
    stop('The comparation operator must be one of ">", ">=", "<" or "<="',
         call. = FALSE)
  }
  if (identical(x_axis, y_axis)){
    stop('x_axis and y_axis must be different')
  }
  if (identical(rule_points$first_point, rule_points$second_point)){
    stop('Start and end points are the same')
  }
  
  # Line estimation ------------------------------------------------------------
  a <- (rule_points$second_point[y] - rule_points$first_point[y]) /
       (rule_points$second_point[x] - rule_points$first_point[x])
  
  if (is.infinite(a)){
    # A vertical line; formula: x_axis <comp_op> c
    c <- rule_points$first_point[x]
    rule_text <- paste("image_prop[,, ", x_axis, "]",comp_op, c)
  } else{
    # Other lines: formula: y_axis <comp_op> a * x_axis + c
    c <- rule_points$first_point[y] - (a * rule_points$first_point[x])
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
                  "first_point" = rule_points$first_point,
                 "second_point" = rule_points$second_point)
  class(result) <- c("pixel_rule")
  
  return(result)
}
