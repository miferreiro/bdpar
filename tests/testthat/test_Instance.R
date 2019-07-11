context("Instance")

test_that("initialize",{

  path <- "example.tsms"

  instance <- Instance$new(path)
  expect_equal(instance$.__enclos_env__$private$path, path)
  expect_equal(instance$.__enclos_env__$private$properties, list(Initial_path = path))
})

test_that("initialize path type error",{

  path <- NULL

  expect_error(Instance$new(path),"\\[Instance\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: path NULL")

})

test_that("obtainDate",{

  path <- "example.tsms"

  expect_error(Instance$new(path)$obtainDate(),"\\[Instance\\]\\[obtainDate\\]\\[Error\\]
              I'm an abstract interface method")

})

test_that("obtainSource",{

  path <- "example.tsms"

  expect_error(Instance$new(path)$obtainSource(),"\\[Instance\\]\\[obtainSource\\]\\[Error\\]
              I'm an abstract interface method")

})

test_that("getDate",{

  path <- "example.tsms"

  instance <- Instance$new(path)
  expect_equal(instance$getDate(), "")

})

test_that("getSource",{

  path <- "example.tsms"

  instance <- Instance$new(path)
  expect_equal(instance$getSource(), "")

})

test_that("getPath",{

  path <- "example.tsms"

  instance <- Instance$new(path)
  expect_equal(instance$getPath(), path)

})

test_that("getData",{

  path <- "example.tsms"

  instance <- Instance$new(path)
  expect_equal(instance$getData(), "")

})

test_that("getProperties",{

  path <- "example.tsms"

  instance <- Instance$new(path)
  expect_equal(instance$getProperties(), list(Initial_path = path))

})

test_that("setSource",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  sourceExpected <- "exampleExpected"

  instance$setSource(sourceExpected)
  expect_equal(instance$getSource(), sourceExpected)

})

test_that("setSource source type error",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  sourceExpected <- NULL

  expect_error(instance$setSource(sourceExpected),"\\[Instance\\]\\[setSource\\]\\[Error\\]
                Checking the type of the variable: source NULL")
})

test_that("setDate",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  dateExpected <- "exampleExpected"

  instance$setDate(dateExpected)
  expect_equal(instance$getDate(), dateExpected)

})

test_that("setDate source type error",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  dateExpected <- NULL

  expect_error(instance$setDate(dateExpected),"\\[Instance\\]\\[setDate\\]\\[Error\\]
                Checking the type of the variable: date NULL")
})

test_that("setProperties",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  propertiesExpected <- list(example = "example")

  instance$setProperties(propertiesExpected)
  expect_equal(instance$getProperties(), propertiesExpected)

})

test_that("setProperties source type error",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  propertiesExpected <- NULL

  expect_error(instance$setProperties(propertiesExpected),"\\[Instance\\]\\[setProperties\\]\\[Error\\]
                Checking the type of the variable: properties NULL")
})

test_that("addProperties",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  valueExpected <- "valueExpected"
  nameExpected <- "nameExpected"

  instance$addProperties(valueExpected, nameExpected)
  expect_equal(instance$getProperties(), list(Initial_path = path, nameExpected = valueExpected))

})

test_that("addProperties propertyName type error",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  valueExpected <- "valueExpected"
  nameExpected <- NULL

  expect_error(instance$addProperties(valueExpected, nameExpected),"\\[Instance\\]\\[addProperties\\]\\[Error\\]
                Checking the type of the variable: propertyName NULL")

})

test_that("getSpecificProperty",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  nameExpected <- "Initial_path"

  expect_equal(instance$getSpecificProperty(nameExpected), path)

})

test_that("getSpecificProperty propertyName type error",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  nameExpected <- NULL

  expect_error(instance$getSpecificProperty(nameExpected),"\\[Instance\\]\\[getSpecificProperty\\]\\[Error\\]
                Checking the type of the variable: propertyName NULL")

})

test_that("isSpecificProperty TRUE",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  nameExpected <- "Initial_path"

  expect_equal(instance$isSpecificProperty(nameExpected), TRUE)

})

test_that("isSpecificProperty FALSE",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  nameExpected <- "example"

  expect_equal(instance$isSpecificProperty(nameExpected), FALSE)

})

test_that("setSpecificProperty",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  nameExpected <- "nameExpected"
  valueExpected <- "valueExpected"

  instance$setSpecificProperty(nameExpected, valueExpected)
  expect_equal(instance$getSpecificProperty(nameExpected), valueExpected)

})

test_that("setSpecificProperty propertyName type error",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  nameExpected <- NULL
  valueExpected <- "valueExpected"

  expect_error(instance$setSpecificProperty(nameExpected, valueExpected),"\\[Instance\\]\\[setSpecificProperty\\]\\[Error\\]
                Checking the type of the variable: propertyName NULL")
})

test_that("getNamesOfProperties",{

  path <- "example.tsms"

  instance <- Instance$new(path)
  expect_equal(instance$getNamesOfProperties(), c("Initial_path"))

})


test_that("setData",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  dataExpected <- "exampleExpected"

  instance$setData(dataExpected)
  expect_equal(instance$getData(), dataExpected)

})

test_that("setData source type error",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  dataExpected <- NULL

  expect_error(instance$setData(dataExpected),"\\[Instance\\]\\[setData\\]\\[Error\\]
                Checking the type of the variable: data NULL")
})

test_that("isInstanceValid",{

  path <- "example.tsms"

  instance <- Instance$new(path)
  expect_equal(instance$isInstanceValid(), TRUE)

})

test_that("invalidate",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  instance$invalidate()

  expect_equal(instance$isInstanceValid(), FALSE)

})

test_that("getFlowPipes",{

  path <- "example.tsms"

  instance <- Instance$new(path)
  expect_equal(instance$getFlowPipes(), list())

})

test_that("addFlowPipes",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  nameExpected <- "pipe"

  instance$addFlowPipes(nameExpected)
  expect_equal(instance$getFlowPipes(), list(nameExpected))

})

test_that("addFlowPipes namePipe type error",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  nameExpected <- NULL

  expect_error(instance$addFlowPipes(nameExpected),"\\[Instance\\]\\[addFlowPipes\\]\\[Error\\]
                Checking the type of the variable: namePipe NULL")

})

test_that("getBanPipes",{

  path <- "example.tsms"

  instance <- Instance$new(path)
  expect_equal(instance$getBanPipes(), c())

})


test_that("addBanPipes",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  nameExpected <- "pipe"

  instance$addBanPipes(nameExpected)
  expect_equal(instance$getBanPipes(), c(nameExpected))

})

test_that("addBanPipes namePipe type error",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  nameExpected <- 1

  expect_error(instance$addBanPipes(nameExpected),"\\[Instance\\]\\[addBanPipes\\]\\[Error\\]
                Checking the type of the variable: namePipe numeric")

})

test_that("checkCompatibility TRUE",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  nameExpected <- "pipeExample"
  alwaysBefore <- list()

  expect_equal(instance$checkCompatibility(nameExpected, alwaysBefore),TRUE)

})

test_that("checkCompatibility FALSE alwaysBefore",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  nameExpected <- "pipeExample"
  alwaysBefore <- list("pipeExample")

  expect_equal(instance$checkCompatibility(nameExpected, alwaysBefore), FALSE)

})

test_that("checkCompatibility FALSE banAfter",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  nameExpected <- "pipeExample"
  alwaysBefore <- list()
  instance$addBanPipes("pipeExample")
  expect_equal(instance$checkCompatibility(nameExpected, alwaysBefore), FALSE)

})

test_that("checkCompatibility namePipe type error",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  nameExpected <- NULL
  alwaysBefore <- list()

  expect_error(instance$checkCompatibility(nameExpected, alwaysBefore),"\\[Instance\\]\\[checkCompatibility\\]\\[Error\\]
                Checking the type of the variable: namePipe NULL")
})

test_that("checkCompatibility namePipe type error",{

  path <- "example.tsms"

  instance <- Instance$new(path)

  nameExpected <- "pipeExample"
  alwaysBefore <- NULL

  expect_error(instance$checkCompatibility(nameExpected, alwaysBefore),"\\[Instance\\]\\[checkCompatibility\\]\\[Error\\]
                Checking the type of the variable: alwaysBefore NULL")
})
