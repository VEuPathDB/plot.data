context('bin')

test_that("binProportion() returns consistent results", {
  dt <- binProportion(data.xy,'y')
  expect_equal_to_reference(dt,"proportion.rds")
  dt <- binProportion(data.xy, 'y','group')
  expect_equal_to_reference(dt,"proportion.group.rds")
  dt <- binProportion(data.xy, 'y', NULL, 'panel')
  expect_equal_to_reference(dt,"proportion.panel.rds")
  dt <- binProportion(data.xy, 'y', 'group', 'panel')
  expect_equal_to_reference(dt,"proportion.group.panel.rds")  
})
