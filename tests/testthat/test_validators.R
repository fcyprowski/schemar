context("validators")

thisIsIris = isThisDFOk(schema_iris)
test_that("makeItPassIf returns df if used correctly", {
  df = makeItPassIf(thisIsIris)(iris)
  expect_is(df, "data.frame")
  expect_true(all(df == iris))
})