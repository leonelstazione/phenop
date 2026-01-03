
fungi_insect_data <- local({
  set.seed(123)
  n <- 600
  data.frame(
    fungus_strain = rep(paste0("H", 1:20), each = n/20),
    insect_population = rep(paste0("I", 1:10), times = n/10),
    temperature = rep(seq(15, 35, length.out = 10), times = n/10),
    humidity = rep(c(30, 60, 90), each = n/3),
    fungus_growth = rnorm(n, 10 + rep(1:20, each = n/20)/2, 2),
    virulence = plogis(rnorm(n, rep(c(-1, 0, 1), each = n/3) +
                               rep(seq(-0.5, 0.5, length.out = 20), each = n/20), 0.3)),
    insect_size = rnorm(n, 15 + rep(1:10, times = n/10)/3, 2),
    resistance = plogis(rnorm(n, rep(c(0.5, -0.5, 0), each = n/3) +
                                rep(seq(-0.3, 0.3, length.out = 10), times = n/10), 0.2))
  )
})

usethis::use_data(fungi_insect_data, overwrite = TRUE)
