context("TargetAssigningPipe")

test_that("initialize",{
  skip_if_not_installed("stringi")
  targets <- list("ham","spam")
  targetsName <- list("_ham_","_spam_")
  propertyName <- "target"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_silent(TargetAssigningPipe$new(targets, targetsName, propertyName, alwaysBeforeDeps, notAfterDeps))
})

test_that("initialize targets type error",{
  skip_if_not_installed("stringi")
  targets <- NULL
  targetsName <- list("_ham_","_spam_")
  propertyName <- "target"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_error(TargetAssigningPipe$new(targets, targetsName, propertyName, alwaysBeforeDeps, notAfterDeps),"\\[TargetAssigningPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: targets NULL")
})

test_that("initialize targetsName type error",{
  skip_if_not_installed("stringi")
  targets <- list("ham","spam")
  targetsName <- NULL
  propertyName <- "target"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_error(TargetAssigningPipe$new(targets, targetsName, propertyName, alwaysBeforeDeps, notAfterDeps),"\\[TargetAssigningPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: targetsName NULL")
})

test_that("initialize propertyName type error",{
  skip_if_not_installed("stringi")
  targets <- list("ham","spam")
  targetsName <- list("_ham_","_spam_")
  propertyName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  expect_error(TargetAssigningPipe$new(targets, targetsName, propertyName, alwaysBeforeDeps, notAfterDeps),"\\[TargetAssigningPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: propertyName NULL")

})

test_that("initialize alwaysBeforeDeps type error",{
  skip_if_not_installed("stringi")
  targets <- list("ham","spam")
  targetsName <- list("_ham_","_spam_")
  propertyName <- "target"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()

  expect_error(TargetAssigningPipe$new(targets, targetsName, propertyName, alwaysBeforeDeps, notAfterDeps),"\\[TargetAssigningPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: alwaysBeforeDeps NULL")

})

test_that("initialize notAfterDeps type error",{
  skip_if_not_installed("stringi")
  targets <- list("ham","spam")
  targetsName <- list("_ham_","_spam_")
  propertyName <- "target"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL

  expect_error(TargetAssigningPipe$new(targets, targetsName, propertyName, alwaysBeforeDeps, notAfterDeps),"\\[TargetAssigningPipe\\]\\[initialize\\]\\[Error\\]
                Checking the type of the variable: notAfterDeps NULL")

})

test_that("pipe",{
  skip_if_not_installed("stringi")
  skip_if_not_installed("readr")
  targets <- list("ham","spam")
  targetsName <- list("_ham_","_spam_")
  propertyName <- "target"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- TargetAssigningPipe$new(targets, targetsName, propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testTargetAssigningPipe",
                    "files",
                    "_ham_",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)

  expect_equal(pipe$pipe(instance)$getSpecificProperty("target"),"ham")
})

test_that("pipe Bad compatibility between Pipes.",{
  skip_if_not_installed("stringi")
  skip_if_not_installed("readr")
  targets <- list("ham","spam")
  targetsName <- list("_ham_","_spam_")
  propertyName <- "target"
  alwaysBeforeDeps <- list("pipeExample")
  notAfterDeps <- list()
  pipe <- TargetAssigningPipe$new(targets, targetsName, propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- file.path("testFiles",
                    "testTargetAssigningPipe",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)
  instance$addBanPipes("pipeExample")
  expect_error(pipe$pipe(instance),"\\[TargetAssigningPipe\\]\\[pipe\\]\\[Error\\] Bad compatibility between Pipes.")

})

test_that("pipe unrecognizable target",{
  skip_if_not_installed("stringi")
  skip_if_not_installed("readr")
  targets <- list("ham","spam")
  targetsName <- list("_ham_","_spam_")
  propertyName <- "target"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- TargetAssigningPipe$new(targets, targetsName, propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- "testFiles/_pan_/30.tsms"

  instance <- ExtractorSms$new(path)

  expect_warning(pipe$pipe(instance),"\\[TargetAssigningPipe\\]\\[pipe\\]\\[Warning\\] The file: testFiles/_pan_/30.tsms has a target unrecognizable ")
})


test_that("pipe instance type error",{
  skip_if_not_installed("stringi")
  targets <- list("ham","spam")
  targetsName <- list("_ham_","_spam_")
  propertyName <- "target"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- TargetAssigningPipe$new(targets, targetsName, propertyName, alwaysBeforeDeps, notAfterDeps)

  instance <- NULL
  expect_error(pipe$pipe(instance),"\\[TargetAssigningPipe\\]\\[pipe\\]\\[Error\\]
                 Checking the type of the variable: instance NULL")

})

test_that("getTarget",{
  skip_if_not_installed("stringi")
  targets <- list("ham", "spam")
  targetsName <- list("_ham_", "_spam_")
  propertyName <- "target"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- TargetAssigningPipe$new(targets, targetsName, propertyName, alwaysBeforeDeps, notAfterDeps)


  path <- "testFiles/_ham_/30.tsms"
  expect_equal(pipe$getTarget(path), "ham")

})

test_that("getTarget unrecognizable",{
  skip_if_not_installed("stringi")
  targets <- list("ham", "spam")
  targetsName <- list("_ham_", "_spam_")
  propertyName <- "target"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- TargetAssigningPipe$new(targets, targetsName, propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- "testFiles/_pan_/30.tsms"
  expect_equal(pipe$getTarget(path), "unrecognizable")

})

test_that("getTarget path type error",{
  skip_if_not_installed("stringi")
  targets <- list("ham", "spam")
  targetsName <- list("_ham_", "_spam_")
  propertyName <- "target"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- TargetAssigningPipe$new(targets, targetsName, propertyName, alwaysBeforeDeps, notAfterDeps)

  path <- NULL
  expect_error(pipe$getTarget(path),"\\[TargetAssigningPipe\\]\\[getTarget\\]\\[Error\\]
                Checking the type of the variable: path NULL")

})

test_that("checkTarget",{
  skip_if_not_installed("stringi")
  targets <- list("ham", "spam")
  targetsName <- list("_ham_", "_spam_")
  propertyName <- "target"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- TargetAssigningPipe$new(targets, targetsName, propertyName, alwaysBeforeDeps, notAfterDeps)

  target <- "_ham_"
  path <- "testFiles/_ham_/30.tsms"
  expect_equal(pipe$checkTarget(target, path), list("_ham_" = "ham"))

})

test_that("checkTarget target type error",{
  skip_if_not_installed("stringi")
  targets <- list("ham", "spam")
  targetsName <- list("_ham_", "_spam_")
  propertyName <- "target"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- TargetAssigningPipe$new(targets, targetsName, propertyName, alwaysBeforeDeps, notAfterDeps)

  target <- NULL
  path <- "testFiles/_ham_/30.tsms"

  expect_error(pipe$checkTarget(target, path),"\\[TargetAssigningPipe\\]\\[checkTarget\\]\\[Error\\]
                Checking the type of the variable: target NULL")

})

test_that("checkTarget path type error",{
  skip_if_not_installed("stringi")
  targets <- list("ham","spam")
  targetsName <- list("_ham_","_spam_")
  propertyName <- "target"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- TargetAssigningPipe$new(targets, targetsName, propertyName, alwaysBeforeDeps, notAfterDeps)

  target <- "_ham_"
  path <- NULL

  expect_error(pipe$checkTarget(target, path),"\\[TargetAssigningPipe\\]\\[checkTarget\\]\\[Error\\]
                Checking the type of the variable: path NULL")

})

test_that("getTargets",{
  skip_if_not_installed("stringi")
  targets <- list("ham", "spam")
  targetsName <- list("_ham_", "_spam_")
  propertyName <- "target"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- TargetAssigningPipe$new(targets, targetsName, propertyName, alwaysBeforeDeps, notAfterDeps)

  expect_equal(pipe$getTargets(), list("_ham_" = "ham", "_spam_" = "spam"))

})
