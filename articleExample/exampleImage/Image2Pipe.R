Image2Pipe <- R6::R6Class(
  "Image2Pipe",
  inherit = GenericPipe,
  public = list(
    initialize = function(propertyName = "",
                          alwaysBeforeDeps = list(),
                          notAfterDeps = list()) {
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    },
    pipe = function(instance) {

      instance$obtainSource()

      instance
    }
  )
)
