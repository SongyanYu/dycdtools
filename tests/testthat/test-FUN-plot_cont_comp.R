
test_that('plot_cont_comp accept right format of date',{

  obs <- data.frame(Date = c(rep('2020-01-01', 6), rep('2020-01-02', 6)),
                    Depth = rep(0:5, 2),
                    TEMP = rep(29:24,2))

  sim <- matrix(c(28,28,28,27,25,24,12,13,14,15,16,17),
                nrow = 6,
                ncol = 2)

  expect_error(is.object(plot_cont_comp(sim = sim,
                                       obs = obs,
                                       sim.start = '01-01-2020',
                                       sim.end = '2020-01-01',
                                       plot.start = '2020-01-01',
                                       plot.end = '2020-01-01',
                                       legend.title = 'degrees',
                                       min.depth = 0,
                                       max.depth = 5,
                                       by.value = 1,
                                       nlevels = 20)))

})
