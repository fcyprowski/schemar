#' @title Enable pipe validations
#'
#' @param fun function created from isThisDFOk or other
#'
#' @return function that takes df as it's argument and checks it's validity from
#' function you've provided
#' @export makeItPassIf
#'
#' @examples
#' thisIsIris = isThisDFOk(schema_iris)
#' df = makeItPassIf(thisIsIris)(iris)
#' head(df)
#' 
#' makeItPassIf(thisIsIris)(mtcars)
makeItPassIf = function(fun) {
  function(df) {
    x = fun(df)
    if (!is.logical(x)) {
      stop("Function you've provided doesn't return logical value.")
    }
    return(df)
  }
}