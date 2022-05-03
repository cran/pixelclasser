## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(pixelclasser)

## ----echo=FALSE, fig.cap = 'Figure 1. The example image and the test images.', fig.align='center', out.width = "50%"----
knitr::include_graphics('../inst/extdata/ExampleImages.png')

## -----------------------------------------------------------------------------
ivy_oak_rgb <- read_image(system.file("extdata", "IvyOak400x300.JPG", package = "pixelclasser"))
test_ivy_rgb <- read_image(system.file("extdata", "TestIvy.JPG", package = "pixelclasser"))
test_oak_rgb <- read_image(system.file("extdata", "TestOak.JPG", package = "pixelclasser"))
test_dead_rgb <- read_image(system.file("extdata", "TestDeadLeaves.JPG", package = "pixelclasser"))

## -----------------------------------------------------------------------------
transparent_black <- "#00000008"
brown <- "#c86432ff"
yellow <- "#ffcd0eff"
blue <- "#5536ffff"
green <- "#559800ff"

## ---- out.width = "50%", fig.cap = "Figure 2. The plane of variables r and b, with the pixels of the example image.", fig.align="center", out.width = "50%"----
plot_rgb_plane("r", "b", main = "Image: ivy and oak")
plot_pixels(ivy_oak_rgb, "r", "b", col = transparent_black)

## ---- fig.cap = "Figure 3. Another representation of the pixels of the example figure. The auxiliary lines were omitted and the colour variables changed.", fig.align="center", out.width = "50%"----
plot_rgb_plane("r", "g", plot_limits = FALSE, plot_guides = FALSE, plot_grid = FALSE)
plot_pixels(ivy_oak_rgb, "r", "g", col = transparent_black)

## ---- fig.align="center", out.width = "50%", fig.cap="Figure 4. The pixels of the dead (brown), ivy (blue) and oak (green) test images overlayed on the pixels of the example image (black). Only the area occupied by the pixels was represented."----
plot_rgb_plane("g", "b", xlim = c(0.2, 0.6), ylim = c(0.1, 0.33))
plot_pixels(ivy_oak_rgb, "g", "b", col = transparent_black)
plot_pixels(test_oak_rgb, "g", "b", col = green)
plot_pixels(test_ivy_rgb, "g", "b", col = blue)
plot_pixels(test_dead_rgb, "g", "b", col = brown)

## -----------------------------------------------------------------------------
rule_01 <- pixel_rule("rule_01", "g", "b", list(c(0.345, 1/3), c(0.40, 0.10)), "<")
rule_02 <- pixel_rule("rule_02", "g", "b", list(c(0.345, 1/3), c(0.40, 0.10)), ">=")

## ---- fig.align="center", out.width = "50%", fig.cap="Figure 5. Same as Fig. 4 but the line between dead and live leaves (defining `rule01` and `rule02`) was added."----
plot_rgb_plane("g", "b", xlim = c(0.2, 0.6), ylim = c(0.1, 0.33))
plot_pixels(ivy_oak_rgb, "g", "b", col = transparent_black)
plot_pixels(test_oak_rgb, "g", "b", col = green)
plot_pixels(test_ivy_rgb, "g", "b", col = blue)
plot_pixels(test_dead_rgb, "g", "b", col = brown)
plot_rule(rule_01, lty = 2, col = brown)

## -----------------------------------------------------------------------------
rule_03 <- pixel_rule("rule_03","g", "b", list(c(0.35, 0.30), c(0.565, 0.10)), "<")
rule_04 <- pixel_rule("rule_04","g", "b", list(c(0.35, 0.25), c(0.5, 0.25)), "<")

## ---- fig.align="center", out.width = "50%", fig.cap="Figure 6. The pixels of the oak test image and the lines that limit this category. Note that the line separatin the dead leaves is used again."----
plot_rgb_plane("g", "b", xlim = c(0.2, 0.6), ylim = c(0.1, 0.33), plot_limits = F, plot_guides = F)
plot_pixels(test_oak_rgb, "g", "b", col = green)
plot_rule(rule_01, lty = 2, col = green)
plot_rule(rule_03, lty = 2, col = green)
plot_rule(rule_04, lty = 2, col = green)

## ---- fig.align="center", out.width = "50%", fig.cap="Figure 7. The pixels of the ivy test image and the lines defining the rules to classify the oak pixels (not shown). The lines can be used to define the ivy category, but the overlay between the two classes prevents a complete separation between the two categories. Some of the pixels trespass into the oak area."----
plot_rgb_plane("g", "b", xlim = c(0.2,0.6), ylim=c(0.1,0.33), plot_limits = F, plot_guides = F)
plot_pixels(test_ivy_rgb, "g", "b", col = blue)

plot_rule(rule_02, lty = 1, col = green)
label_rule(rule_02, label = expression('L'[1]*' (R'[1]*',R'[2]*')'), shift = c(0.035, -0.004), col = green)

plot_rule(rule_03, lty = 1, col = green)
label_rule(rule_03, label = expression('L'[2]*' (R'[3]*',R'[5]*')'), shift = c(0.20, -0.15), col = green)

plot_rule(rule_04, lty = 1, col = green)
label_rule(rule_04, label = expression('L'[3]*' (R'[4]*',R'[6]*')'), shift = c(0.19, 0.0), col = green)

## -----------------------------------------------------------------------------
rule_05 <- pixel_rule("rule_05", "g", "b", list(c(0.35, 0.30), c(0.565, 0.16)), ">=")
rule_06 <- pixel_rule("rule_06", "g", "b", list(c(0.35, 0.25), c(0.5, 0.25)), ">=")

## -----------------------------------------------------------------------------
cat_dead_leaves <- pixel_category("dead_leaves", blue, rule_01)

## -----------------------------------------------------------------------------
cat_living_leaves <- pixel_category("living_leaves", yellow, rule_02)

## -----------------------------------------------------------------------------
cat_oak_leaves <- pixel_category("oak_leaves", green, rule_02, rule_03, rule_04)

## -----------------------------------------------------------------------------
subcat_ivy01 <- pixel_subcategory("ivy01", rule_02, rule_06)
subcat_ivy02 <- pixel_subcategory("ivy02", rule_04, rule_05)

cat_ivy_leaves <- pixel_category("ivy_leaves", yellow, subcat_ivy01, subcat_ivy02)

## -----------------------------------------------------------------------------
dead_live_classified <- classify_pixels(ivy_oak_rgb, cat_dead_leaves, cat_living_leaves)

## ---- eval=FALSE--------------------------------------------------------------
#  save_classif_image(dead_live_classified, "DeadLiveClassified.JPG", quality = 1)

## -----------------------------------------------------------------------------
ivy_oak_classified <- classify_pixels(ivy_oak_rgb, cat_dead_leaves, cat_ivy_leaves, cat_oak_leaves)

## -----------------------------------------------------------------------------
rule_05 <- pixel_rule("rule_05", "g", "b", list(c(0.35, 0.30), c(0.565, 0.10)), ">=")
subcat_ivy02 <- pixel_subcategory("ivy02", rule_04, rule_05)
cat_ivy_leaves <- pixel_category("ivy_leaves", yellow, subcat_ivy01, subcat_ivy02)
ivy_oak_classified <- classify_pixels(ivy_oak_rgb, cat_dead_leaves, cat_ivy_leaves, cat_oak_leaves)

## ---- eval = FALSE------------------------------------------------------------
#  save_classif_image(ivy_oak_classified, "IvyOakClassified.TIFF")

## ----echo=FALSE, fig.align='center', out.width= "150%", fig.cap="Figure 8. From left to right: the original example image, the image produced by the first classification (dead vs. living leaves) and the image produced by the second classification (dead/ivy/oak leaves)."----
knitr::include_graphics('../inst/extdata/ClassifResults.png')

## -----------------------------------------------------------------------------
test_dead <- classify_pixels(test_dead_rgb, cat_dead_leaves, cat_ivy_leaves, cat_oak_leaves)
test_ivy <- classify_pixels(test_ivy_rgb, cat_dead_leaves, cat_ivy_leaves, cat_oak_leaves)
test_oak <- classify_pixels(test_oak_rgb, cat_dead_leaves, cat_ivy_leaves, cat_oak_leaves)

