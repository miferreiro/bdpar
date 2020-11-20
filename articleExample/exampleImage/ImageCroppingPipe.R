ImageCroppingPipe <- R6::R6Class(
  "ImageCroppingPipe",
  inherit = GenericPipe,
  public = list(
    initialize = function(propertyName = "",
                          alwaysBeforeDeps = list(),
                          notAfterDeps = list()) {
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    },
    pipe = function(instance) {
      data <- instance$getData()
      data <- imager::imsub(data, x > height/2)
      instance$setData(data)
      instance
    }
  )
)
