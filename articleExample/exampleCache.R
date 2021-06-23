library(bdpar)

path <- system.file("example",
                    package = "bdpar")

system.time(runPipeline(path = path))

system.time(runPipeline(path = path))
