\donttest{
library(dplyr)
library(tidyr)
library(purrr)
library(plotly)
library(ggplot2)

# tidyverse + plotly
d <- mpg %>%
  nest(data = !one_of(c("manufacturer", "class"))) %>%
  mutate(
    mean_city_mpg = map_dbl(data, ~ mean(.$cty)),
    panel = map_plot(data, function(x) {
      plot_ly(data = x, x = ~cty, y = ~hwy,
        type = "scatter", mode = "markers")
    })
  )

d %>% trelliscope(name = "city_vs_highway_mpg")

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
  nest(data = !one_of(c("manufacturer", "class"))) %>%
  mutate(
    panel = map_plot(data, ~
      qplot(cty, hwy, data = .) + xlab("cty") + ylab("hwy") +
        xlim(7, 37) + ylim(9, 47) + theme_bw())) %>%
  trelliscope(name = "tidy_gg")

# computing additional cognostics
mpg_cog <- mpg %>%
  nest(data = !one_of(c("manufacturer", "class"))) %>%
  mutate(
    cogs = map_cog(data, ~ tibble(
      mean_city_mpg = mean(.$cty),
      mean_hwy_mpg = mean(.$hwy),
      most_common_drv = tail(names(table(.$drv)), 1)
    ))
  )

# computing additional cognostics explicitly using cog()
# so we can specify descriptions, etc.
mpg_cog2 <- mpg %>%
  nest(data = !one_of(c("manufacturer", "class"))) %>%
  mutate(
    cogs = map_cog(data, ~ tibble(
      mean_city_mpg = cog(mean(.$cty), desc = "Mean city mpg"),
      mean_hwy_mpg = cog(mean(.$hwy), desc = "Mean highway mpg"),
      most_common_drv = cog(tail(names(table(.$drv)), 1), desc = "Most common drive type")
    )),
    panel = map_plot(data, function(x) {
      plot_ly(data = x, x = ~cty, y = ~hwy,
        type = "scatter", mode = "markers") %>%
        layout(
          xaxis = list(range = c(9, 47)),
          yaxis = list(range = c(7, 37)))
    })
  )

mpg_cog2 %>%
  trelliscope(name = "city_vs_highway_mpg", nrow = 1, ncol = 2)
}
