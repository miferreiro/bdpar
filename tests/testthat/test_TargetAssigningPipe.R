testthat::context("TargetAssigningPipe")

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("initialize",{
  testthat::skip_if_not_installed("stringi")
  targets <- list("ham","spam")
  targetsName <- list("_ham_","_spam_")
  propertyName <- "target"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  testthat::expect_silent(TargetAssigningPipe$new(targets,
                                                  targetsName,
                                                  propertyName,
                                                  alwaysBeforeDeps,
                                                  notAfterDeps))
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("initialize targets type error",{
  testthat::skip_if_not_installed("stringi")
  targets <- NULL
  targetsName <- list("_ham_","_spam_")
  propertyName <- "target"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  testthat::expect_error(TargetAssigningPipe$new(targets,
                                                 targetsName,
                                                 propertyName,
                                                 alwaysBeforeDeps,
                                                 notAfterDeps),
                         "[TargetAssigningPipe][initialize][FATAL] Checking the type of the 'targets' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("initialize targetsName type error",{
  testthat::skip_if_not_installed("stringi")
  targets <- list("ham","spam")
  targetsName <- NULL
  propertyName <- "target"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  testthat::expect_error(TargetAssigningPipe$new(targets,
                                                 targetsName,
                                                 propertyName,
                                                 alwaysBeforeDeps,
                                                 notAfterDeps),
                         "[TargetAssigningPipe][initialize][FATAL] Checking the type of the 'targetsName' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("initialize propertyName type error",{
  testthat::skip_if_not_installed("stringi")
  targets <- list("ham","spam")
  targetsName <- list("_ham_","_spam_")
  propertyName <- NULL
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  testthat::expect_error(TargetAssigningPipe$new(targets,
                                                 targetsName,
                                                 propertyName,
                                                 alwaysBeforeDeps,
                                                 notAfterDeps),
                         "[TargetAssigningPipe][initialize][FATAL] Checking the type of the 'propertyName' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("initialize alwaysBeforeDeps type error",{
  testthat::skip_if_not_installed("stringi")
  targets <- list("ham","spam")
  targetsName <- list("_ham_","_spam_")
  propertyName <- "target"
  alwaysBeforeDeps <- NULL
  notAfterDeps <- list()

  testthat::expect_error(TargetAssigningPipe$new(targets,
                                                 targetsName,
                                                 propertyName,
                                                 alwaysBeforeDeps,
                                                 notAfterDeps),
                         "[TargetAssigningPipe][initialize][FATAL] Checking the type of the 'alwaysBeforeDeps' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("initialize notAfterDeps type error",{
  testthat::skip_if_not_installed("stringi")
  targets <- list("ham","spam")
  targetsName <- list("_ham_","_spam_")
  propertyName <- "target"
  alwaysBeforeDeps <- list()
  notAfterDeps <- NULL

  testthat::expect_error(TargetAssigningPipe$new(targets,
                                                 targetsName,
                                                 propertyName,
                                                 alwaysBeforeDeps,
                                                 notAfterDeps),
                         "[TargetAssigningPipe][initialize][FATAL] Checking the type of the 'notAfterDeps' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("pipe",{
  testthat::skip_if_not_installed("stringi")
  testthat::skip_if_not_installed("readr")
  targets <- list("ham","spam")
  targetsName <- list("_ham_","_spam_")
  propertyName <- "target"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- TargetAssigningPipe$new(targets,
                                  targetsName,
                                  propertyName,
                                  alwaysBeforeDeps,
                                  notAfterDeps)

  path <- file.path("testFiles",
                    "testTargetAssigningPipe",
                    "files",
                    "_ham_",
                    "testFile.tsms")

  instance <- ExtractorSms$new(path)

  testthat::expect_equal(pipe$pipe(instance)$getSpecificProperty("target"),
                         "ham")
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("pipe unrecognizable target",{
  testthat::skip_if_not_installed("stringi")
  testthat::skip_if_not_installed("readr")
  targets <- list("ham","spam")
  targetsName <- list("_ham_","_spam_")
  propertyName <- "target"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()

  pipe <- TargetAssigningPipe$new(targets,
                                  targetsName,
                                  propertyName,
                                  alwaysBeforeDeps,
                                  notAfterDeps)

  path <- "testFiles/_pan_/30.tsms"

  instance <- ExtractorSms$new(path)

  testthat::expect_warning(pipe$pipe(instance),
                           "\\[TargetAssigningPipe\\]\\[pipe\\]\\[WARN\\] The file: testFiles/_pan_/30.tsms has a target unrecognizable")
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("pipe instance type error",{
  testthat::skip_if_not_installed("stringi")
  targets <- list("ham","spam")
  targetsName <- list("_ham_","_spam_")
  propertyName <- "target"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- TargetAssigningPipe$new(targets,
                                  targetsName,
                                  propertyName,
                                  alwaysBeforeDeps,
                                  notAfterDeps)

  instance <- NULL
  testthat::expect_error(pipe$pipe(instance),
                         "[TargetAssigningPipe][pipe][FATAL] Checking the type of the 'instance' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("getTarget",{
  testthat::skip_if_not_installed("stringi")
  targets <- list("ham", "spam")
  targetsName <- list("_ham_", "_spam_")
  propertyName <- "target"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- TargetAssigningPipe$new(targets,
                                  targetsName,
                                  propertyName,
                                  alwaysBeforeDeps,
                                  notAfterDeps)


  path <- "testFiles/_ham_/30.tsms"
  testthat::expect_equal(pipe$getTarget(path),
                         "ham")
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("getTarget unrecognizable",{
  testthat::skip_if_not_installed("stringi")
  targets <- list("ham", "spam")
  targetsName <- list("_ham_", "_spam_")
  propertyName <- "target"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- TargetAssigningPipe$new(targets,
                                  targetsName,
                                  propertyName,
                                  alwaysBeforeDeps,
                                  notAfterDeps)

  path <- "testFiles/_pan_/30.tsms"
  testthat::expect_equal(pipe$getTarget(path),
                         "unrecognizable")
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("getTarget path type error",{
  testthat::skip_if_not_installed("stringi")
  targets <- list("ham", "spam")
  targetsName <- list("_ham_", "_spam_")
  propertyName <- "target"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- TargetAssigningPipe$new(targets,
                                  targetsName,
                                  propertyName,
                                  alwaysBeforeDeps,
                                  notAfterDeps)

  path <- NULL
  testthat::expect_error(pipe$getTarget(path),
                         "[TargetAssigningPipe][getTarget][FATAL] Checking the type of the 'path' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("checkTarget",{
  testthat::skip_if_not_installed("stringi")
  targets <- list("ham", "spam")
  targetsName <- list("_ham_", "_spam_")
  propertyName <- "target"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- TargetAssigningPipe$new(targets,
                                  targetsName,
                                  propertyName,
                                  alwaysBeforeDeps,
                                  notAfterDeps)

  target <- "_ham_"
  path <- "testFiles/_ham_/30.tsms"
  testthat::expect_equal(pipe$checkTarget(target,
                                          path),
                         list("_ham_" = "ham"))
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("checkTarget target type error",{
  testthat::skip_if_not_installed("stringi")
  targets <- list("ham", "spam")
  targetsName <- list("_ham_", "_spam_")
  propertyName <- "target"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- TargetAssigningPipe$new(targets,
                                  targetsName,
                                  propertyName,
                                  alwaysBeforeDeps,
                                  notAfterDeps)

  target <- NULL
  path <- "testFiles/_ham_/30.tsms"

  testthat::expect_error(pipe$checkTarget(target,
                                          path),
                         "[TargetAssigningPipe][checkTarget][FATAL] Checking the type of the 'target' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("checkTarget path type error",{
  testthat::skip_if_not_installed("stringi")
  targets <- list("ham","spam")
  targetsName <- list("_ham_","_spam_")
  propertyName <- "target"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- TargetAssigningPipe$new(targets,
                                  targetsName,
                                  propertyName,
                                  alwaysBeforeDeps,
                                  notAfterDeps)

  target <- "_ham_"
  path <- NULL

  testthat::expect_error(pipe$checkTarget(target,
                                          path),
                         "[TargetAssigningPipe][checkTarget][FATAL] Checking the type of the 'path' variable: NULL",
                         fixed = TRUE)
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::setup({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})

testthat::test_that("getTargets",{
  testthat::skip_if_not_installed("stringi")
  targets <- list("ham", "spam")
  targetsName <- list("_ham_", "_spam_")
  propertyName <- "target"
  alwaysBeforeDeps <- list()
  notAfterDeps <- list()
  pipe <- TargetAssigningPipe$new(targets,
                                  targetsName,
                                  propertyName,
                                  alwaysBeforeDeps,
                                  notAfterDeps)

  testthat::expect_equal(pipe$getTargets(),
                         list("_ham_" = "ham", "_spam_" = "spam"))
})

testthat::teardown({
  bdpar.Options$reset()
  bdpar.Options$configureLog()
})
