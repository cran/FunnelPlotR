test_that("`test the classes return stuff", {
  dt <-
    data.frame(
      num = c(100, 150, 180, 80, 120, 225)
      , denom = c(108, 112, 165, 95, 100, 220)
      , group = factor(c("a", "b", "c", "d", "e", "f"))
    )

  a <-
    funnel_plot(
      dt
      , num
      , denom
      , group
    )

  expect_s3_class(a, "funnelplot")
  expect_equal(round(phi.funnelplot(a), 6), 2.948033)
  expect_type(a, "list")
  expect_true(is_ggplot(a[[1]]))
  expect_s3_class(a[[2]], "data.frame")
  expect_s3_class(a[[3]], "data.frame")
  expect_length(a[[3]]$group, 6)
  expect_length(a[[3]], 22)


})
