context('Testing function interfaces')
library(pixelclasser)

source(system.file("extdata", "TestObjects.R", package = "pixelclasser"))

# Function place_rule() --------------------------------------------------------

test_that("Parameters in place_rule() are verified", {
  
  expect_error(place_rule(), "X or Y colour variable is missing")
  expect_error(place_rule("g"), "X or Y colour variable is missing")
  expect_error(place_rule("r", "v"),
  'Y colour variable is "v", but must be one of "r", "g" or "b".')
  expect_error(place_rule("r", "g", "g"),
  'Parameter "line_type" is "g", but must be one of "v", "h" or "f".')
})

# Function read_image() --------------------------------------------------------

test_that("Parameters in read_image() are verified", {

  expect_error(read_image("ExampleImages.png"),
  'The file type of ExampleImages.png is png, but must be "jpg", "JPG", "jpeg", "JPEG", "tif", "TIF", "tiff" or "TIFF".')
})

test_that("Output of read_image() is correct", {

  load(system.file("extdata", "test_image_rgb.R", package = "pixelclasser"))
  new_test_image <- read_image(system.file("extdata", "TestImage.JPG",
                                           package = "pixelclasser"))

  expect_equal(class(test_image_rgb), "pixel_transformed_image")
  expect_equal(new_test_image, test_image_rgb)
})

# Function pixel_rule() -------------------------------------------------------

test_that("Parameters in pixel_rule() are verified", {

  expect_error(pixel_rule('R1', 'r', 'b', list(c(0.1, 0.1),c(0.1, 0.1),c(0.1, 0.1)), '>'),
"The list passed in \"line_points\" must contain the coordinates of two points, but contains 3 points.")
  
  expect_error(pixel_rule('R1', 'r', 'b', c(0.1, 0.1), '>'),
'The object passed in \"line_points\" must be of class \"list\" or \"pixel_rule_points\", but it is of class \"numeric\".')
  
  rp01 <- list("x_axis" = "r", "y_axis" = "b", list("first_point" = c(0.3, 0.3), 
               "second_point" = c(0.4, 0.4)))
  class(rp01) <- "pixel_rule_points"
  
  expect_error(pixel_rule('R1', 'g', 'b', rp01, '>'),
    'The value in x_axis, "g", does not match the value in rp01, "r".')
  
  expect_error(pixel_rule('R1', 'r', 'g', rp01, '>'),
    'The value in y_axis, "g", does not match the value in rp01, "b".')
  
  expect_error(pixel_rule('R1', 'p', 'b', list(c(0.1, 0.1), c(0.5, 0.5)), '>'),
      'The value in x_axis is "p" but must be one of "r", "g" or "b".')

  expect_error(pixel_rule('R1', 'r', 'p', list(c(0.1, 0.1), c(0.5, 0.5)), '>'),
      'The value in y_axis is "p" but must be one of "r", "g" or "b".')
  
  expect_error(pixel_rule('R1', 'r', 'g', list(c(0.1, 0.1), c(0.5, 0.5)), '$'),
   'The comparison operator is "$", but must be one of \">\", \">=\", \"<\" or \"<=\".',
   fixed = TRUE)

  expect_error(pixel_rule('R1', 'b', 'b', list(c(0.1, 0.1), c(0.5, 0.5)), '>'),
               'x_axis and y_axis must be different.')

  expect_error(pixel_rule('R1', 'r', 'g', list(c(0.5, 0.5), c(0.5, 0.5)), '>'),
               'Start and end points are the same.')

  
})

test_that("Output of pixel_rule() is correct", {
  
  new_rule01 <- pixel_rule('Rule01', 'r', 'g',
                            list(c(0.3, 0.0), c(0.3, 0.55)), comp_op = '>')
  new_rule03 <- pixel_rule('Rule03', 'r', 'g',
                            list(c(0.0, 0.3), c(0.55, 0.3)), comp_op = '>')
  new_rule05 <- pixel_rule('Rule05', 'r', 'g',
                            list(c(0.15, 0.00), c(0.55, 0.40)), comp_op = '>')

  expect_equal(class(new_rule01), "pixel_rule")
  expect_equal(new_rule01, rule01)
  expect_equal(new_rule03, rule03)
  expect_equal(new_rule05, rule05)
})

# Function pixel_subcategory() -----------------------------------------------------

test_that("Parameters in pixel_subcategory() are verified", {

  #load(system.file("extdata", "test_subcat.R", package = "pixelclasser"))

  expect_error(subcat01 <- pixel_subcategory('Subcat01', rule01, subcat01))
})

test_that("Output of pixel_subcategory() is correct", {

  #load(system.file("extdata", "test_subcat.R", package = "pixelclasser"))

  new_subcat01 <- pixel_subcategory('Subcat01', rule01, rule04)

  expect_equal(class(new_subcat01), "pixel_subcategory")
  expect_equal(new_subcat01, subcat01)
})

# Function pixel_category() ----------------------------------------------------

test_that("Parameters in pixel_category() are verified", {

  expect_error(new_cat_A <- pixel_category('Cat_A', 'red', rule05, subcat01),
  'The objects that define the category must be either rules or subcategories, but they are a mixture of both.')
  expect_error(new_cat_A <- pixel_category('Cat_A', 'red', 'A', 'B'))
})

test_that("Output of pixel_category() is correct", {

  new_cat_A <- pixel_category('Cat_A', 'red', rule05, rule04)
  expect_equal(class(new_cat_A), "pixel_category")
  expect_equal(new_cat_A, cat_A)
  
  new_cat_D <- pixel_category("Cat_D", "green", subcat01, subcat02)
  expect_equal(class(subcat01), "pixel_subcategory")
  expect_equal(class(subcat02), "pixel_subcategory")
  expect_equal(class(new_cat_D), "pixel_category")
  expect_equal(new_cat_D, cat_D)
})

# Function classify_pixels() ---------------------------------------------------

test_that("Parameters in classify_pixels() are verified", {

  expect_error(test_image_classif <- classify_pixels(test_image_rgb,
                                                     list('A', 'B'),
                                                     cat_B, cat_C))
})

test_that("Output of classify_pixels() is correct", {

  load(system.file("extdata", "test_image_rgb.R",    package = "pixelclasser"))
  load(system.file("extdata", "test_cat.R", package = "pixelclasser"))
  load(system.file("extdata", "test_image_classified.R", package = "pixelclasser"))

  new_test_image_classif <- classify_pixels(test_image_rgb, cat_A, cat_B, cat_C)

  expect_equal(new_test_image_classif, test_image_classified)
})

# Function plot_rule() ---------------------------------------------------------

test_that("Parameters in plot_rule() are verified", {

  #plot_rgb_plane('r', 'g')

  expect_error(plot_rule(rule = subcat01),
  'The object subcat01 is of class "pixel_subcategory" but must be of class "pixel_rule".')
})

# Function label_rule() --------------------------------------------------------

test_that("Parameters in label_rule() are verified", {
  # load(system.file("extdata", "test_subcat.R", package = "pixelclasser"))
  
  expect_error(label_rule(rule = subcat01))
})

# Function save_classif_image() ------------------------------------------------
test_that("Parameters in save_classif_image() are verified", {

  load(system.file("extdata", "test_image_classified.R",
                   package = "pixelclasser"))

  expect_error(save_classif_image(test_image_classif,
                               file = "../vignettes/test_image_classified.txt"))
})
