library(mockery)

test_that("Test if vars exists", {
  vars <- c("weight", "height")
  df <- data.frame(c("weight", "height"))
  conns <- data.frame(validate = "OpalDriver")
  with_mock("DSI::datashield.aggregate" = mock(data.frame(c("weight", "height"))), {
    result <- dh.doVarsExist(df, vars, conns)
    expect_equal(result, NULL)
  })
})
