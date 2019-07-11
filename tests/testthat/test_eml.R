context("eml")

# test_that("read_emails",{
#
#   devtools::load_all()
#
#   email_file <- system.file("testFiles_pipeline_execute_eml",
#                             package = "bdpar")
#
#   PartSelectedOnMPAlternative <- "text/plain"
#
#   expect_equal(read_emails(email_file, PartSelectedOnMPAlternative),NULL)
#
# })

test_that("read_emails email_file type error",{

  devtools::load_all()
  email_file <- NULL

  PartSelectedOnMPAlternative <- "text/plain"

  expect_error(read_emails(email_file, PartSelectedOnMPAlternative),"email_files must be a character vector containing file paths to email txt files...")

})
