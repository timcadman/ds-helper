

webmockr::enable()
webmockr::httr_mock()

# Run cleanup after all tests
withr::defer(webmockr::stub_registry_clear(), testthat::teardown_env())

# Set up global test values
#example: assign("armadillo_url", "https://test.nl/", envir = .pkgglobalenv)