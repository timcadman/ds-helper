library(mockery)

test_that("Test if vars exists", {
  vars <- c("height", "weight")
  opalDf <- data.frame(c("weight", "height"))
  dsColnames <- mock(ds.colnames = data.frame(c("weight", "height")) )
  with_mock(
    "dsBaseClient::ds.colnames" = dsColnames,
    result <- cs.doVarsExist(opalDf, vars)    
  )
  expect_equal(result, NULL)
})