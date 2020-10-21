library(mockery)

test_that("Test if vars exists", {
  vars <- c("weight", "height")
  df <- data.frame(c("weight", "height"))
  conns <- data.frame(validate = "OpalDriver")
  with_mock(
    "dsBaseClient::ds.colnames" = mock(data.frame(c("weight", "height"))),
    {
      result <- dh.doVarsExist(conns, df, vars)
      expect_equal(result, NULL)
    }
  )
})
