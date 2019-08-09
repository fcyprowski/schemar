context("schemas")

test_that("does correspondsToSchema gives TRUE when the schema corresponds to df", {
  expect_true(correspondsToSchema(iris, schema_iris))
})
test_that("isThisDFOk creates a function that works",{
  thisIsIris = isThisDFOk(schema_iris)
  expect_true(thisIsIris(iris))
  expect_error(thisIsIris(mtcars), "not equal to length")
})