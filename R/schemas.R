# basic functions ---------------------------------------------------------

schema_example = list(
  "day" = "Date",
  "dim_group" = "character",
  "dim_name" = "character",
  "dim_value" = "numeric",
  "campaign_id" = "character"
)
schema_iris = list(
  "Sepal.Length" =  "numeric",
  "Sepal.Width" =  "numeric",
  "Petal.Length" =  "numeric",
  "Petal.Width" =  "numeric",
  "Species" =  "factor")
#' @title Does the DF has a good schema?
#'
#' @param df data.frame you want to check
#' @param schema list with names and r data types. Type: schema_example
#' in the console to see example schema
#'
#' @return TRUE/FALSE (is it ok or is not)
#' @export correspondsToSchema
#'
#' @examples
#' 
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @importFrom purrr map
correspondsToSchema = function(df, schema) {
  df_schema = map(df, ~class(.)) %>% 
    as.data.frame() %>%
    select(sort(names(.)))
  target_schema = schema %>% 
    as.data.frame() %>%
    select(sort(names(.)))
  comparisons = df_schema == target_schema
  result = all(comparisons)
  if (!result) {
    print(comparisons)
  }
  return(result)
}
#' @title Create check function
#' @description this function create checker functions (it's mostly for readability)
#'
#' @param df data.frame you want to check
#' @param schema schema you want ot compare to
#'
#' @return function
#' @export isThisDFOk
#'
#' @examples
#' df = data.frame(foo = "x", bar = "y")
#' isFooBar = isThisDFOk(schema_example)
#' isFooBar(df)
isThisDFOk = function(schema) {
  assertthat::assert_that(
    is.list(schema)  # TODO: validation for the schema object (only atomic vectors)
  )
  function(df) {
    assertthat::assert_that(
      is.data.frame(df),
      length(df) == length(schema),
      assertthat::has_name(
        df,
        names(schema)
      ),
      correspondsToSchema(df, schema)
    )
  }
}
#' @title get schema from data.frame
#' @description it's only for help purpose. You still need to convert the result
#' to "list" function call and you would probably do it by hand :(
#' In some cases you can use that as a snapshot and don't write it down... but
#' it wouldn't be a good practice (you should always know the schema beforehand)
#'
#' @param df data.frame
#'
#' @return list of colnames and their correspondent types
#' @export getSchema
#'
#' @examples
#' getSchema(iris)
#' 
#' @importFrom purrr map
getSchema = function(df) {
  map(df, ~class(.))  %>% 
    jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE) %>% 
    gsub("\\{", "(", .) %>% 
    gsub("\\}", "\\)", .) %>% 
    gsub(":", " =", .)
}

# schemas -----------------------------------------------------------------

schema_budget = list(
  day = "Date",
  dim_group = "character",
  dim_name = "character",
  dim_value = "numeric",
  campaign_id = "character"
)
isBudget = isThisDFOk(schema_budget)
