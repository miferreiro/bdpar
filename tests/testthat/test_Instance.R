testthat::context("Instance")

test_that("initialize",{

  path <- "example.tsms"

  instance <- Instance$new(path)
  testthat::expect_equal(instance$.__enclos_env__$private$path,
                        path)
  testthat::expect_equal(instance$.__enclos_env__$private$properties,
                        list(Initial_path = path))
})

testthat::test_that("initialize path type error",{

  path <- NULL

  testthat::expect_error(Instance$new(path),
                         "[Instance][initialize][Error] Checking the type of the 'path' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("obtainDate",{

  path <- "example.tsms"

  testthat::expect_error(Instance$new(path)$obtainDate(),
                         "[Instance][obtainDate][Error] I am an abstract interface method",
                         fixed = TRUE)
})

testthat::test_that("obtainSource",{

  path <- "example.tsms"

  testthat::expect_error(Instance$new(path)$obtainSource(),
                         "[Instance][obtainSource][Error] I am an abstract interface method",
                         fixed = TRUE)
})

testthat::test_that("getDate",{

  path <- "example.tsms"

  instance <- Instance$new(path)
  testthat::expect_equal(instance$getDate(),
                         "")
})

testthat::test_that("getSource",{

  path <- "example.tsms"

  instance <- Instance$new(path)
  testthat::expect_equal(instance$getSource(),
                         "")
})

testthat::test_that("getPath",{

  path <- "example.tsms"

  instance <- Instance$new(path)
  testthat::expect_equal(instance$getPath(),
                         path)
})

testthat::test_that("getData",{

  path <- "example.tsms"

  instance <- Instance$new(path)
  testthat::expect_equal(instance$getData(),
                         "")
})

testthat::test_that("getProperties",{

  path <- "example.tsms"

  instance <- Instance$new(path)
  testthat::expect_equal(instance$getProperties(),
                        list(Initial_path = path))
})

testthat::test_that("setSource",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  sourceExpected <- "exampleExpected"

  instance$setSource(sourceExpected)
  testthat::expect_equal(instance$getSource(),
                        sourceExpected)
})

testthat::test_that("setSource source type error",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  sourceExpected <- NULL

  testthat::expect_error(instance$setSource(sourceExpected),
                         "[Instance][setSource][Error] Checking the type of the 'source' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("setDate",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  dateExpected <- "exampleExpected"

  instance$setDate(dateExpected)
  testthat::expect_equal(instance$getDate(),
                         dateExpected)
})

testthat::test_that("setDate source type error",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  dateExpected <- NULL

  testthat::expect_error(instance$setDate(dateExpected),
                         "[Instance][setDate][Error] Checking the type of the 'date' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("setProperties",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  propertiesExpected <- list(example = "example")

  instance$setProperties(propertiesExpected)
  testthat::expect_equal(instance$getProperties(),
                         propertiesExpected)
})

testthat::test_that("setProperties source type error",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  propertiesExpected <- NULL

  testthat::expect_error(instance$setProperties(propertiesExpected),
                         "[Instance][setProperties][Error] Checking the type of the 'properties' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("addProperties",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  valueExpected <- "valueExpected"
  nameExpected <- "nameExpected"

  instance$addProperties(valueExpected, nameExpected)
  testthat::expect_equal(instance$getProperties(),
                         list(Initial_path = path, nameExpected = valueExpected))
})

testthat::test_that("addProperties propertyName type error",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  valueExpected <- "valueExpected"
  nameExpected <- NULL

  testthat::expect_error(instance$addProperties(valueExpected, nameExpected),
                         "[Instance][addProperties][Error] Checking the type of the 'propertyName' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("getSpecificProperty",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  nameExpected <- "Initial_path"

  testthat::expect_equal(instance$getSpecificProperty(nameExpected),
                         path)
})

testthat::test_that("getSpecificProperty propertyName type error",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  nameExpected <- NULL

  testthat::expect_error(instance$getSpecificProperty(nameExpected),
                         "[Instance][getSpecificProperty][Error] Checking the type of the 'propertyName' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("isSpecificProperty TRUE",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  nameExpected <- "Initial_path"

  testthat::expect_equal(instance$isSpecificProperty(nameExpected),
                         TRUE)
})

testthat::test_that("isSpecificProperty FALSE",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  nameExpected <- "example"

  testthat::expect_equal(instance$isSpecificProperty(nameExpected),
                         FALSE)
})

testthat::test_that("setSpecificProperty",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  nameExpected <- "nameExpected"
  valueExpected <- "valueExpected"

  instance$setSpecificProperty(nameExpected, valueExpected)
  testthat::expect_equal(instance$getSpecificProperty(nameExpected),
                         valueExpected)
})

testthat::test_that("setSpecificProperty propertyName type error",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  nameExpected <- NULL
  valueExpected <- "valueExpected"

  testthat::expect_error(instance$setSpecificProperty(nameExpected, valueExpected),
                         "[Instance][setSpecificProperty][Error] Checking the type of the 'propertyName' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("getNamesOfProperties",{

  path <- "example.tsms"

  instance <- Instance$new(path)
  testthat::expect_equal(instance$getNamesOfProperties(),
                         c("Initial_path"))
})


testthat::test_that("setData",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  dataExpected <- "exampleExpected"

  instance$setData(dataExpected)
  testthat::expect_equal(instance$getData(),
                         dataExpected)
})

testthat::test_that("setData source type error",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  dataExpected <- NULL

  testthat::expect_error(instance$setData(dataExpected),
                         "[Instance][setData][Error] Checking the type of the 'data' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("isInstanceValid",{

  path <- "example.tsms"

  instance <- Instance$new(path)
  testthat::expect_equal(instance$isInstanceValid(),
                         TRUE)
})

testthat::test_that("invalidate",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  instance$invalidate()

  testthat::expect_equal(instance$isInstanceValid(),
                         FALSE)
})

testthat::test_that("getFlowPipes",{

  path <- "example.tsms"

  instance <- Instance$new(path)
  testthat::expect_equal(instance$getFlowPipes(),
                         list())
})

testthat::test_that("addFlowPipes",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  nameExpected <- "pipe"

  instance$addFlowPipes(nameExpected)
  testthat::expect_equal(instance$getFlowPipes(),
                         list(nameExpected))
})

testthat::test_that("addFlowPipes namePipe type error",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  nameExpected <- NULL

  testthat::expect_error(instance$addFlowPipes(nameExpected),
                         "[Instance][addFlowPipes][Error] Checking the type of the 'namePipe' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("getBanPipes",{

  path <- "example.tsms"

  instance <- Instance$new(path)
  testthat::expect_equal(instance$getBanPipes(),
                         c())
})


testthat::test_that("addBanPipes",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  nameExpected <- "pipe"

  instance$addBanPipes(nameExpected)
  testthat::expect_equal(instance$getBanPipes(),
                         c(nameExpected))
})

testthat::test_that("addBanPipes namePipe type error",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  nameExpected <- 1

  testthat::expect_error(instance$addBanPipes(nameExpected),
                         "[Instance][addBanPipes][Error] Checking the type of the 'namePipe' variable: numeric",
                         fixed = TRUE)
})

testthat::test_that("checkCompatibility TRUE",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  nameExpected <- "pipeExample"
  alwaysBefore <- list()

  testthat::expect_equal(instance$checkCompatibility(nameExpected, alwaysBefore),
                         TRUE)
})

testthat::test_that("checkCompatibility FALSE alwaysBefore",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  nameExpected <- "pipeExample"
  alwaysBefore <- list("pipeExample")

  testthat::expect_equal(instance$checkCompatibility(nameExpected, alwaysBefore),
                         FALSE)
})

testthat::test_that("checkCompatibility FALSE banAfter",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  nameExpected <- "pipeExample"
  alwaysBefore <- list()
  instance$addBanPipes("pipeExample")
  testthat::expect_equal(instance$checkCompatibility(nameExpected, alwaysBefore),
                         FALSE)
})

testthat::test_that("checkCompatibility namePipe type error",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  nameExpected <- NULL
  alwaysBefore <- list()

  testthat::expect_error(instance$checkCompatibility(nameExpected, alwaysBefore),
                         "[Instance][checkCompatibility][Error] Checking the type of the 'namePipe' variable: NULL",
                         fixed = TRUE)
})

testthat::test_that("checkCompatibility namePipe type error",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  nameExpected <- "pipeExample"
  alwaysBefore <- NULL

  testthat::expect_error(instance$checkCompatibility(nameExpected, alwaysBefore),
                         "[Instance][checkCompatibility][Error] Checking the type of the 'alwaysBefore' variable: NULL",
                         fixed = TRUE)
})
