webmockr::enable()
webmockr::httr_mock()

# Run cleanup after all tests
withr::defer(stub_registry_clear(), teardown_env())

# Set up global test values
# example: assign("armadillo_url", "https://test.nl/", envir = .pkgglobalenv)
