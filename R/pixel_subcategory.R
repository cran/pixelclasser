#' Creates a subcategory object
#'
#' Creates an object of class \code{pixel_subcategory} from a list of objects of
#' class \code{pixel_rule}.
#'
#' @param subcat_name a character string containing the name of the subcategory.
#' @param \dots in \code{pixel_subcategory()} a list of objects of class
#'   \code{"pixel_rule"}; in \code{summary()}, additional parameters (none
#'   needed by now).
#'
#' @return An object of class \code{"pixel_subcategory"}, which is a list with
#'   these elements:
#'   \itemize{
#'   \item \code{name} a character string containing the name of the
#'   subcategory.
#'   \item \code{rules_list} a list of  \code{pixel_rule} objects.
#'   }
#'   
#' @details When the shape of the cluster of pixels belonging to one category is
#'   not convex, the rules become inconsistent (the lines cross in awkward ways)
#'   and the classification produced is erroneous. To solve this problem, the
#'   complete set of rules is divided into several subsets (subcategories) that
#'   break the original non-convex shape into a set of convex polygons. Note
#'   that any polygon can be divided in a number of triangles, so this problem
#'   always has solution. However, in many cases (such as the one presented in
#'   the pixelclasser vignette) a complete triangulation is not needed.
#'   
#'   Internally, \code{classify_pixels()} classifies the points belonging to
#'   each subcategory and then joins the incidence matrices using the \code{or}
#'   operator, to create the matrix for the whole category.
#'
#' @seealso \code{\link{pixel_rule}}, \code{\link{pixel_category}}
#'
#' @examples
#' rule01 <- pixel_rule("R01", "g", "b",
#'                       list(c(0.35, 0.30), c(0.45, 0.10)), ">=")
#' rule02 <- pixel_rule("R02", "g", "b",
#'                       list(c(0.35, 0.253), c(0.45, 0.253)), ">=")
#'
#' subcat01 <- pixel_subcategory("Subcat_01", rule01, rule02)
#'
#' @export

pixel_subcategory <- function(subcat_name,  ...){

  rules_list <- list(...)
  rules_names <- vapply(match.call(expand.dots = FALSE)$...,
                        deparse,
                        FUN.VALUE = 'vector')

  for (i in seq_along(rules_list)){
    if (!is.rule(rules_list[[i]])){
      stop('Object "', rules_names[i], '" is of class "',
           class(rules_list[[i]]),
           '", but must be of class "pixel_rule".', call. = FALSE)
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

is.subcategory<- function(x){
  return (inherits(x, "pixel_subcategory"))
}

# is.category ------------------------------------------------------------------
#' @rdname pixel_subcategory
#' @param x the R object being tested
#' @export

is.subcategory<- function(x){
  return (inherits(x, "pixel_subcategory"))
}

# print ------------------------------------------------------------------------
#' @export

print.pixel_subcategory <- function(x,
                                    digits = NULL,
                                    quote = TRUE,
                                    na.print = NULL,
                                    print.gap = NULL,
                                    right = FALSE,
                                    max = NULL,
                                    useSource = TRUE,
                                    ...){
  
  cat('Subcategory name: "', x$name, '"\n')
  cat("Rules:\n")
  for (i in seq_along(x$rules)){
    print(x$rules[i])
  }
}

# summary ----------------------------------------------------------------------
#' @rdname pixel_subcategory
#' @param object an object of class \code{"pixel_subcategory"}.
#' @export

summary.pixel_subcategory <- function(object, ...){
  
  cat('Subcategory "', object$name, '" contains the following rules:\n',
      sep = '')
  for (i in seq_along(names(object$rules))){
    cat('    "', names(object$rules)[i], '"\n', sep = '')
  }
}