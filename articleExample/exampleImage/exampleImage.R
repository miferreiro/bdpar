library(bdpar)
library(imager)
source("ExtractorImage.R")

extractors <- ExtractorFactory$new()
extractors$registerExtractor(extension = "png", extractor = ExtractorImage)

source("Image2Pipe.R")
source("ImageCroppingPipe.R")
source("ImageResizePipe.R")

pipeline <- DynamicPipeline$new(pipeline = list(Image2Pipe$new(),
                                                ImageCroppingPipe$new(),
                                                ImageResizePipe$new()))

output <- runPipeline(path = "parrots.png",
                      extractors = extractors,
                      pipeline = pipeline,
                      cache = FALSE,
                      verbose = TRUE,
                      summary = FALSE)

layout(c(1,2))

plot(imager::load.image("parrots.png"))
plot(output[[1]]$getData())
