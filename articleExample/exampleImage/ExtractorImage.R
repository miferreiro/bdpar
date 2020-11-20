ExtractorImage <- R6::R6Class(

  classname = "ExtractorImage",

  inherit = Instance,

  public = list(
    initialize = function(path) {
      super$initialize(path)
    },
    obtainSource = function() {

      source <- imager::load.image(super$getPath())

      super$setSource(source)
      super$setData(source)
    }
  )
)
