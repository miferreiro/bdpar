library(bdpar)

bdpar.Options$configureLog(console = FALSE, threshold = "INFO", file = NULL)
path <- system.file(file.path("example"),
                    package = "bdpar")

bdpar.Options$set("numCores", 1)
system.time(runPipeline(path = path))

bdpar.Options$set("numCores", 2)
system.time(runPipeline(path = path))

bdpar.Options$set("numCores", 6)
system.time(runPipeline(path = path))
