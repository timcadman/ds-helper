library(mockery)

test_that("Test if vars exists", {
  vars <- c("height", "weight")
  df <- data.frame(c("weight", "height"))
  dsColnames <- mock(ds.colnames = data.frame(c("weight", "height")))
  conns <- mock(name = "validate")
  with_mock(
    "dsBaseClient::ds.colnames" = dsColnames,
    result <- dh.doVarsExist(conns, df, vars, cohorts)
  )
  expect_equal(result, NULL)
})
