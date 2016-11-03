## trelliscopecore

This is an experimental R package that provides lower-level functions that generate a Trelliscope spec that can be read by the [TrelliscopeJS](https://github.com/hafen/trelliscopejs) viewer, as well as some high-level utility functions for plugging into popular visualization systems like ggplot2 with `facet_trelliscope()`.

### Install

```r
devtools::install_github("hafen/trelliscopecore")
```

### Usage with ggplot

Example:

```r
library(ggplot2)

p <- qplot(cty, hwy, data = mpg) +
  facet_trelliscope(
    ~ class + manufacturer,
    path = "_test",
    name = "city_vs_highway_mpg_gg",
    width = 800
  )
p
```

### Usage with dplyr (and rbokeh)

Example:

```r
library(trelliscopecore)
library(dplyr)
library(rbokeh)

p <- ggplot2::mpg %>%
  group_by(class, manufacturer) %>%
  summarise(
    mean_city_mpg = cog(mean(cty), desc = "Mean city mpg"),
    mean_hwy_mpg = cog(mean(hwy), desc = "Mean highway mpg"),
    most_common_drv = cog(tail(names(table(drv)), 1), desc = "Most common drive type"),
    panel = panel(
      figure(xlab = "City mpg", ylab = "Highway mpg") %>%
        ly_points(cty, hwy,
          hover = data_frame(model = paste(year, trans, model),
          cty = cty, hwy = hwy)) %>%
        y_range(c(9, 47)) %>%
        x_range(c(7, 37)))) %>%
  trelliscope(name = "city_vs_highway_mpg_bk", path = "_test",
    cond_cols = c("class", "manufacturer"))
p
```

### Low-level functions

The general steps of creating a display ready for TrelliscopeJS are the following:

1. Create a list of plot objects to be used as panels in the display and use `write_panels()` to write them to json files ready for the viewer to consume.  Panels can be Lattice, ggplot2, or htmlwidget objects.
2. Create a cognostics data frame where each row of the data frame points to one of the panels that were written in step 1.  The helper function `as_cognostics()` will ensure that the cognostics data frame is properly formed and that there is a key column that points to each panel, etc.
3. Call `write_display_obj()` which will write out the cognostics data frame in the proper format, extract cognostics distributions, etc., and store all necessary metadata about the display in a display object json file.
4. Call `prepare_display()` which calls the following functions (which can also be called independently)
  - `update_display_list()`: loops over all display object files and builds a master display list (must be called each time a new display has been added or an existing display has been updated)
  - `write_config()`: writes a basic app config file neccessary for the viewer to run
  - `copy_viewer_files()`: grabs the latest TrelliscopeJS dependencies from the web (only needs to be called each time a new viewer is available)

All of the "write" functions have an option `jsonp`, which if `TRUE` (default) will use jsonp instead of json so that the display can be viewed without a web server.

### Example

```r
library(dplyr)
library(rbokeh)
library(ggplot2)

# cognostics data frame
iris_cog_df <- iris %>%
  group_by(Species) %>%
  summarise(
    mean_sl = cog(mean(Sepal.Length), desc = "mean sepal length"),
    mean_sw = cog(mean(Sepal.Width), desc = "mean sepal length"),
    mean_pl = cog(mean(Petal.Length), desc = "mean sepal length"),
    mean_pw = cog(mean(Petal.Width), desc = "mean sepal length")
  )

iris_cog_df <- as_cognostics(iris_cog_df, cond_cols = "Species", key_col = "Species")

# list of panels
panels <- iris %>%
  split(iris$Species) %>%
  lapply(function(x) {
    figure(width = 500, height = 500) %>%
      ly_points(Sepal.Length, Sepal.Width, data = x)
  })

base_path <- "~/Desktop/test"

write_panels(panels, base_path = base_path, name = "iris")

write_display_obj(
  iris_cog_df,
  panel_example = panels[[1]],
  base_path = base_path,
  name = "iris"
)

prepare_display(base_path)

view_display(base_path)

## using ggplot2 (reuse cognostics)
##---------------------------------------------------------

panels_gg <- iris %>%
  split(iris$Species) %>%
  lapply(function(x) {
    qplot(Sepal.Length, Sepal.Width, data = x)
  })

base_path <- "~/Desktop/test"

write_panels(panels_gg, base_path = base_path, name = "iris2")

write_display_obj(
  iris_cog_df,
  panel_example = panels_gg[[1]],
  base_path = base_path,
  name = "iris2"
)

# still need to update the display list so both displays are registered
prepare_display(base_path, copy_viewer_files = FALSE)

view_display(base_path)
```
