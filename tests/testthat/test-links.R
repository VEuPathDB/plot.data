test_that("Links work", {
  # Make a link
  nodeA <- Node(
    id = 'A'
  )
  nodeB <- Node(
    id = 'B'
  )
  link <- Link(source = nodeA, target = nodeB)
  expect_equal(class(link)[1], 'Link')
})

test_that("Link methods work", {

  nodeA <- Node(
    id = 'A'
  )
  nodeB <- Node(
    id = 'B'
  )

  link <- Link(source = nodeA, target = nodeB)
  expect_equal(source(link), nodeA)
  expect_equal(target(link), nodeB)
  expect_equal(weight(link), 1)
  expect_true(is.null(color(link)))

  link <- Link(source = nodeA, target = nodeB, color = 'red', weight = 10)
  expect_equal(source(link), nodeA)
  expect_equal(target(link), nodeB)
  expect_equal(color(link), 'red')
  expect_equal(weight(link), 10)

})
