library(bdpar)

path <- system.file(file.path("example"),
                    package = "bdpar")

bdpar.Options$set("numCores", 1)
system.time(runPipeline(path = path,
                        cache = FALSE,
                        verbose = FALSE,
                        summary = FALSE))

bdpar.Options$set("numCores", 2)
system.time(runPipeline(path = path,
                        cache = FALSE,
                        verbose = FALSE,
                        summary = FALSE))

bdpar.Options$set("numCores", 6)
system.time(runPipeline(path = path,
                        cache = FALSE,
                        verbose = FALSE,
                        summary = FALSE))
