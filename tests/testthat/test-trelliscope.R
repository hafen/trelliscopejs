context("trelliscope")

test_that("examples run without barfing", {

  library(dplyr)
  library(rbokeh)
  library(ggplot2)
  library(tidyr)

  # dplyr + rbokeh
  d <- mpg %>%
    group_by(manufacturer, class) %>%
    summarise(
      mean_city_mpg = mean(cty),
      panel = panel(
        figure(xlab = "City mpg", ylab = "Highway mpg") %>%
          ly_points(cty, hwy)))

  p <- d %>% trelliscope(name = "city_vs_highway_mpg", thumb = FALSE)
  print(p)

  # should give an error in the browser unless you launch with servr or something like it
  p <- d %>% trelliscope(name = "city_vs_highway_mpg", thumb = FALSE, jsonp = FALSE)
  print(p)
  # instead do something like:
  # servr::httd(attr(p, "trelliscope_pars")$www_dir)

  # if you want to use in RStudio Viewer or RMarkdown Notebook, use self_containedd
  # (this will hopefully change, and you should avoid self_contained whenever possible)
  p <- d %>% trelliscope(name = "city_vs_highway_mpg",
    self_contained = TRUE, thumb = FALSE)
  print(p)

  # set default layout
  d %>% trelliscope(name = "city_vs_highway_mpg", nrow = 2, ncol = 3, thumb = FALSE)

  tf <- tempfile("trelliscopetest")

  # set the output path for where files will be stored
  d %>% trelliscope(name = "city_vs_highway_mpg", path = tf, thumb = FALSE)

  # multiple displays can be added to the same path and all will be available in the viewer
  p <- d %>% trelliscope(name = "city_vs_highway_mpg2", path = tf, thumb = FALSE)
  print(p)

  # ordering the data frame will set default sort order of the display
  d %>%
    arrange(-mean_city_mpg) %>%
    trelliscope(name = "city_vs_highway_mpg", thumb = FALSE)

  # dplyr + ggplot2
  mpg %>%
    group_by(manufacturer, class) %>%
    summarise(
      panel = panel(
        qplot(cty, hwy) + xlab("cty") + ylab("hwy") +
          xlim(7, 37) + ylim(9, 47) + theme_bw())) %>%
    trelliscope(name = "dplyr_gg", thumb = FALSE)

  # computing cognostics explicitly using cog()
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
    trelliscope(name = "city_vs_highway_mpg", nrow = 1, ncol = 2, thumb = FALSE)

  p <- mpg %>%
    group_by(manufacturer, class) %>%
    nest() %>%
    mutate(panel = panels(data,
      ~ figure(xlab = "City mpg", ylab = "Highway mpg") %>%
          ly_points(cty, hwy, data = .x))) %>%
    trelliscope(name = "city_vs_highway_mpg", thumb = FALSE)
  print(p)

  p <- iris %>%
    group_by(Species) %>%
    summarise(
      wiki_link = cog_href(paste0("https://en.wikipedia.org/wiki/Iris_",
        tolower(Species))[1], default_label = TRUE,
        desc = "link to species on wikipedia"),
      panel = panel(figure(xlab = "Sepal Length", ylab = "Sepal Width") %>%
        ly_points(Sepal.Length, Sepal.Width))) %>%
    trelliscope(name = "iris_species", ncol = 3, thumb = FALSE)
  print(p)

  ## ggplot2
  ##---------------------------------------------------------

  p <- qplot(cty, hwy, data = mpg) +
    facet_trelliscope(~ class + manufacturer, self_contained = TRUE)
  print(p)

  # not required, but if you set labels, these will be added as
  # descriptions to the cognostics that are automatically computed
  mpg <- set_labels(mpg, mpg_labels)

  p <- qplot(cty, hwy, data = mpg) +
    xlim(7, 37) + ylim(9, 47) + theme_bw() +
    facet_trelliscope(~ manufacturer + class, nrow = 2, ncol = 4)
  print(p)

  p <- qplot(class, cty, data = mpg, geom = c("boxplot", "jitter")) +
    facet_trelliscope(~ class, ncol = 7, height = 800, width = 200,
      state = list(sort = list(sort_spec("cty_mean"))), path = "_test") +
    ylim(7, 37) + theme_bw()
  print(p)

  ## cogs
  ##---------------------------------------------------------

  p <- ggplot2::mpg %>%
    group_by(manufacturer, class) %>%
    nest() %>%
    mutate(
      additional_cogs = cogs(data,
        ~ data_frame(
          max_city_mpg = cog(max(.x$cty), desc = "Max city mpg"),
          min_city_mpg = cog(min(.x$cty), desc = "Min city mpg"))),
      panel = panels(data, ~ figure(xlab = "City mpg", ylab = "Highway mpg") %>%
        ly_points(cty, hwy, data = .x))) %>%
    trelliscope(name = "city_vs_highway_mpg", nrow = 1, ncol = 2, thumb = FALSE)
  print(p)
})
