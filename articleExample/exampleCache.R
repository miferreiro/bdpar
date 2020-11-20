library(bdpar)

bdpar.Options$configureLog(console = FALSE, threshold = "INFO", file = NULL)
path <- system.file(file.path("example"),
                    package = "bdpar")

bdpar.Options$set("cache", TRUE)
system.time(runPipeline(path = path))

system.time(runPipeline(path = path))
