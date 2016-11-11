\dontrun{
library(dplyr)
library(rbokeh)
library(ggplot2)

# dplyr + rbokeh
d <- mpg %>%
  group_by(manufacturer, class) %>%
  summarise(
    panel = panel(
      figure(xlab = "City mpg", ylab = "Highway mpg") %>%
        ly_points(cty, hwy)))

d %>% trelliscope(name = "city_vs_highway_mpg")

# if you want to use in RStudio Viewer or RMarkdown Notebook, use self_containedd
# (this will hopefully change, and you should avoid self_contained whenever possible)
d %>% trelliscope(name = "city_vs_highway_mpg", self_contained = TRUE)

# set default layout
d %>% trelliscope(name = "city_vs_highway_mpg", nrow = 2, ncol = 3)

# set the output path for where files will be stored
d %>% trelliscope(name = "city_vs_highway_mpg", path = "mydisplays")

# multiple displays can be added to the same path and all will be available in the viewer
d %>% trelliscope(name = "city_vs_highway_mpg2", path = "mydisplays")

# ordering the data frame will set default sort order of the display
d %>%
  arrange(-mean_city_mpg) %>%
  trelliscope(name = "city_vs_highway_mpg")

# dplyr + ggplot2
mpg %>%
  group_by(manufacturer, class) %>%
  summarise(
    panel = panel(
      qplot(cty, hwy) + xlab("cty") + ylab("hwy") +
        xlim(7, 37) + ylim(9, 47) + theme_bw())) %>%
  trelliscope(name = "dplyr_gg")

# computing additional cognostics
mpg %>%
  group_by(manufacturer, class) %>%
  summarise(
    mean_city_mpg = mean(cty),
    mean_hwy_mpg = mean(hwy),
    most_common_drv = tail(names(table(drv)), 1),
    panel = panel(
      figure(xlab = "City mpg", ylab = "Highway mpg",
        xlim = c(9, 47), ylim = c(7, 37)) %>%
        ly_points(cty, hwy,
          hover = data_frame(model = paste(year, model),
          cty = cty, hwy = hwy)))) %>%
  trelliscope(name = "city_vs_highway_mpg", nrow = 1, ncol = 2)

# computing additional cognostics explicitly using cog()
# so we can specify descriptions, etc.
mpg %>%
  group_by(manufacturer, class) %>%
  summarise(
    mean_city_mpg = cog(mean(cty), desc = "Mean city mpg"),
    mean_hwy_mpg = cog(mean(hwy), desc = "Mean highway mpg"),
    most_common_drv = cog(tail(names(table(drv)), 1), desc = "Most common drive type"),
    panel = panel(
      figure(xlab = "City mpg", ylab = "Highway mpg",
        xlim = c(9, 47), ylim = c(7, 37)) %>%
        ly_points(cty, hwy,
          hover = data_frame(model = paste(year, model),
          cty = cty, hwy = hwy)))) %>%
  trelliscope(name = "city_vs_highway_mpg", nrow = 1, ncol = 2)
}
