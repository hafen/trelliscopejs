context("trelliscope")

test_that("examples run without barfing", {

  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(tidyr))
  suppressPackageStartupMessages(library(purrr))
  suppressPackageStartupMessages(library(plotly))
  suppressPackageStartupMessages(library(ggplot2))

  # tidyverse + plotly
  d <- mpg %>%
    nest(data = !one_of(c("manufacturer", "class"))) %>%
    mutate(
      mean_city_mpg = map_dbl(data, ~ mean(.$cty)),
      panel = map_plot(data, function(x) {
        plot_ly(data = x, x = ~cty, y = ~hwy,
          type = "scatter", mode = "markers")
      }
    ))

  p <- d %>% trelliscope(name = "city_vs_highway_mpg", thumb = FALSE)
  print(p)

  # set default layout
  p <- d %>% trelliscope(name = "city_vs_highway_mpg", nrow = 2, ncol = 3, thumb = FALSE)
  print(p)

  tf <- tempfile("trelliscopetest")

  # set the output path for where files will be stored
  p <- d %>% trelliscope(name = "city_vs_highway_mpg", path = tf, thumb = FALSE)
  print(p)

  # multiple displays can be added to the same path and all will be available in the viewer
  p <- d %>% trelliscope(name = "city_vs_highway_mpg2", path = tf, thumb = FALSE)
  print(p)

  # ordering the data frame will set default sort order of the display
  p <- d %>%
    arrange(-mean_city_mpg) %>%
    trelliscope(name = "city_vs_highway_mpg", thumb = FALSE)
  print(p)

  # tidyverse + ggplot2
  p <- mpg %>%
    nest(data = !one_of(c("manufacturer", "class"))) %>%
    mutate(
      panel = map_plot(data, ~
        qplot(cty, hwy, data = .) + xlab("cty") + ylab("hwy") +
          xlim(7, 37) + ylim(9, 47) + theme_bw())) %>%
    trelliscope(name = "tidy_gg", thumb = FALSE)
  print(p)

  # computing additional cognostics
  mpg_cog <- mpg %>%
    nest(data = !one_of(c("manufacturer", "class"))) %>%
    mutate(
      cogs = map_cog(data, ~ tibble(
        mean_city_mpg = mean(.$cty),
        mean_hwy_mpg = mean(.$hwy),
        most_common_drv = tail(names(table(.$drv)), 1)
      )),
      panel = map_plot(data, function(x) {
        plot_ly(data = x, x = ~cty, y = ~hwy,
          type = "scatter", mode = "markers")
      })
    )

  p <- mpg_cog %>%
    trelliscope(name = "city_vs_highway_mpg", nrow = 1, ncol = 2, thumb = FALSE)
  print(p)

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
          type = "scatter", mode = "markers")
      })
    )

  p <- mpg_cog2 %>%
    trelliscope(name = "city_vs_highway_mpg", nrow = 1, ncol = 2, thumb = FALSE)
  print(p)

  p <- ggplot2::mpg %>%
    group_by(manufacturer, class) %>%
    summarise(
      wiki_link = cog_href(paste0("https://en.wikipedia.org/wiki/",
        manufacturer)[1], default_label = TRUE,
        desc = "link to species on wikipedia"),
      panel = panel(
        plot_ly(x = cty, y = hwy,
          type = "scatter", mode = "markers")
      )
    ) %>%
    trelliscope("mpg")
  print(p)

  ## ggplot2
  ##---------------------------------------------------------

  # library(ggplot2)
  # qplot(cty, hwy, data = mpg) +
  #   facet_trelliscope(~ class, auto_cog = FALSE)

  # test using date variables
  x <- factor(LETTERS[1:4])
  names(x) <- letters[1:4]
  data_year_date <- data.frame(
    year = structure(seq(1950, 1989, 1), class = "Date"),
    var = rep(x, 10), y = seq(1, 200, 5))
  ggplot(data_year_date, aes(year, y)) + geom_point() +
    facet_trelliscope(~ var)

  # update variable with cog()
  mpg2 <- mpg
  mpg2$class2 <- as.integer(factor(mpg2$class))
  mpg2$class2 <- cog(mpg2$class2, desc = "custom cognostic label test")

  p <- qplot(cty, hwy, data = mpg2) +
    facet_trelliscope(~ class)
  print(p)

  # not required, but if you set labels, these will be added as
  # descriptions to the cognostics that are automatically computed
  mpg <- set_labels(mpg, mpg_labels)

  p <- qplot(cty, hwy, data = mpg) +
    theme_bw() +
    facet_trelliscope(~ class, nrow = 2, ncol = 4)
  print(p)

  p <- qplot(cty, hwy, data = mpg) +
    theme_bw() +
    facet_trelliscope(~ class + manufacturer, nrow = 2, ncol = 4)
  print(p)

  if (utils::packageVersion("ggplot2") > "2.2.1") {
    p <- qplot(cty, hwy, data = mpg) +
      theme_bw() +
      facet_trelliscope(vars(class), nrow = 2, ncol = 4)
    print(p)

    p <- qplot(cty, hwy, data = mpg) +
      theme_bw() +
      facet_trelliscope(vars(class, manufacturer), nrow = 2, ncol = 4)
    print(p)
  }

  p <- qplot(class, cty, data = mpg, geom = c("boxplot", "jitter"),
    na.rm = TRUE) +
    facet_trelliscope(~ class + manufacturer, ncol = 7,
      height = 800, width = 200,
      state = list(sort = list(sort_spec("cty_mean"))),
      scales = c("free", "same")) +
    theme_bw()
  print(p)

  p <- qplot(class, cty, data = mpg, geom = c("boxplot", "jitter"),
    na.rm = TRUE) +
    facet_trelliscope(~ class, ncol = 7, height = 800, width = 200,
      state = list(sort = list(sort_spec("cty_mean"))),
      scales = c("free", "same"), as_plotly = TRUE) +
    theme_bw()
  print(p)

  ## cogs
  ##---------------------------------------------------------

  p <- ggplot2::mpg %>%
    nest(data = !one_of(c("manufacturer", "class"))) %>%
    mutate(
      additional_cogs = map_cog(data, function(x) {
        tibble(
          max_city_mpg = cog(max(x$cty), desc = "Max city mpg"),
          min_city_mpg = cog(min(x$cty), desc = "Min city mpg"))
      }),
      panel = map_plot(data, function(x) {
        plot_ly(data = x, x = ~cty, y = ~hwy,
          type = "scatter", mode = "markers")
      })) %>%
    trelliscope(name = "city_vs_highway_mpg", nrow = 1, ncol = 2, thumb = FALSE)
  print(p)

  ## other tidyverse functions

  p <- iris %>%
    nest(data = -Species) %>%
    mutate(
      mod = map(data, ~ lm(Sepal.Length ~ Sepal.Width, data = .x)),
      cogs = map2_cog(data, mod, function(data, mod) {
        tibble(max_sl = max(data$Sepal.Length), slope = coef(mod)[2])
      }),
      panel = map2_plot(data, mod, function(data, mod) {
        plot_ly(data = data, x = ~Sepal.Width, y = ~Sepal.Length,
          type = "scatter", mode = "markers", name = "data") %>%
          add_trace(data = data, x = ~Sepal.Width, y = ~predict(mod),
            mode = "lines", name = "lm")
      })) %>%
    trelliscope(name = "iris", thumb = FALSE)
  print(p)

  p <- iris %>%
    nest(data = -Species) %>%
    mutate(
      mod = map(data, ~ lm(Sepal.Length ~ Sepal.Width, data = .x)),
      cogs = pmap_cog(list(data = data), function(data) {
        tibble(max_sl = max(data$Sepal.Length))
      }),
      panel = pmap_plot(list(data = data, mod = mod), function(data, mod) {
        plot_ly(data = data, x = ~Sepal.Width, y = ~Sepal.Length,
          type = "scatter", mode = "markers", name = "data") %>%
          add_trace(data = data, x = ~Sepal.Width, y = ~predict(mod),
            mode = "lines", name = "lm")
      })) %>%
    trelliscope(name = "iris", thumb = FALSE)
  print(p)

  iris_sample <- iris %>% mutate(sample = sample(Species, 150))
  expect_error({
    p <- qplot(Sepal.Width, Sepal.Length, data = iris_sample) +
      facet_trelliscope(~ Species, "iris_sample", auto_cog = TRUE)
    print(p)
  })
  expect_error({
    p <- qplot(Sepal.Width, Sepal.Length, data = iris_sample) +
      facet_trelliscope(~ Species, 1, "iris_sample", auto_cog = TRUE)
    print(p)
  })
  expect_error({
    p <- qplot(Sepal.Width, Sepal.Length, data = iris_sample) +
      facet_trelliscope(~ Species, 1:10, name = "iris_sample", auto_cog = TRUE)
    print(p)
  })
  expect_error({
    p <- qplot(Sepal.Width, Sepal.Length, data = iris_sample) +
      facet_trelliscope(~ Species, 2.3, name = "iris_sample", auto_cog = TRUE)
    print(p)
  })
  expect_error({
    p <- qplot(Sepal.Width, Sepal.Length, data = iris_sample) +
      facet_trelliscope(~ Species, 0, name = "iris_sample", auto_cog = TRUE)
    print(p)
  })
  p <- qplot(Sepal.Width, Sepal.Length, data = iris_sample) +
    facet_trelliscope(~ Species, name = "iris_sample", auto_cog = TRUE)
  print(p)

})
