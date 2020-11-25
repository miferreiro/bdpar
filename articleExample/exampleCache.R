library(bdpar)

path <- system.file(file.path("example"),
                    package = "bdpar")

system.time(runPipeline(path = path))

system.time(runPipeline(path = path))
