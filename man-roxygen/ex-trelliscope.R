\donttest{
library(dplyr)
library(tidyr)
library(purrr)
library(rbokeh)
library(ggplot2)

# tidyverse + rbokeh
d <- mpg %>%
  group_by(manufacturer, class) %>%
  nest() %>%
  mutate(
    mean_city_mpg = map_dbl(data, ~ mean(.$cty)),
    panel = map_plot(data, ~
      figure(., xlab = "City mpg", ylab = "Highway mpg") %>%
        ly_points(cty, hwy))
  )

d %>% trelliscope(name = "city_vs_highway_mpg")

# if you want to use in RStudio Viewer or RMarkdown Notebook, use self_containedd
# (this will hopefully change, and you should avoid self_contained whenever possible)
d %>% trelliscope(name = "city_vs_highway_mpg", self_contained = TRUE)

# set default layout
d %>% trelliscope(name = "city_vs_highway_mpg", nrow = 2, ncol = 3)

# set the output path for where files will be stored
my_displays <- tempfile()
d %>% trelliscope(name = "city_vs_highway_mpg", path = my_displays)

# multiple displays can be added to the same path and all will be available in the viewer
d %>% trelliscope(name = "city_vs_highway_mpg2", path = my_displays)

# ordering the data frame will set default sort order of the display
d %>%
  arrange(-mean_city_mpg) %>%
  trelliscope(name = "city_vs_highway_mpg")

# tidyverse + ggplot2
mpg %>%
  group_by(manufacturer, class) %>%
  nest() %>%
  mutate(
    panel = map_plot(data, ~
      qplot(cty, hwy, data = .) + xlab("cty") + ylab("hwy") +
        xlim(7, 37) + ylim(9, 47) + theme_bw())) %>%
  trelliscope(name = "tidy_gg")

# computing additional cognostics
mpg_cog <- mpg %>%
  group_by(manufacturer, class) %>%
  nest() %>%
  mutate(
    cogs = map_cog(data, ~ tibble(
      mean_city_mpg = mean(.$cty),
      mean_hwy_mpg = mean(.$hwy),
      most_common_drv = tail(names(table(.$drv)), 1)
    )),
    panel = map_plot(data, ~
      figure(., xlab = "City mpg", ylab = "Highway mpg",
        xlim = c(9, 47), ylim = c(7, 37)) %>%
        ly_points(cty, hwy,
          hover = list(year, model))
    )
  )

mpg_cog %>%
  trelliscope(name = "city_vs_highway_mpg", nrow = 1, ncol = 2)

# computing additional cognostics explicitly using cog()
# so we can specify descriptions, etc.
mpg_cog2 <- mpg %>%
  group_by(manufacturer, class) %>%
  nest() %>%
  mutate(
    cogs = map_cog(data, ~ tibble(
      mean_city_mpg = cog(mean(.$cty), desc = "Mean city mpg"),
      mean_hwy_mpg = cog(mean(.$hwy), desc = "Mean highway mpg"),
      most_common_drv = cog(tail(names(table(.$drv)), 1), desc = "Most common drive type")
    )),
    panel = map_plot(data, ~
      figure(., xlab = "City mpg", ylab = "Highway mpg",
        xlim = c(9, 47), ylim = c(7, 37)) %>%
        ly_points(cty, hwy,
          hover = list(year, model))
    )
  )

mpg_cog2 %>%
  trelliscope(name = "city_vs_highway_mpg", nrow = 1, ncol = 2)
}
