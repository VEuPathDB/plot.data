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

test_that("LinkList methods work", {

  # Create some nodes
  nodeA <- Node(
    id = 'A'
  )
  nodeB <- Node(
    id = 'B'
  )
  nodeC <- Node(
    id = 'C'
  )

  # Create some links
  link1 <- Link(source = nodeA, target = nodeB)
  link2 <- Link(source = nodeB, target = nodeC)
  link3 <- Link(source = nodeC, target = nodeA)

  linkList <- LinkList(S4Vectors::SimpleList(c(link1, link2, link3)))
  expect_equal(length(linkList), 3)
  expect_equal(getSourceNodes(linkList), list(nodeA, nodeB, nodeC))
  expect_equal(getTargetNodes(linkList), list(nodeB, nodeC, nodeA))
  expect_equal(getWeights(linkList), c(1, 1, 1))
  expect_equal(getColors(linkList), c(NULL, NULL, NULL))


  # Create some more links
  link1 <- Link(source = nodeA, target = nodeB, weight = 2, color = 'red')
  link2 <- Link(source = nodeB, target = nodeC, weight = 0.1, color = 'blue')
  link3 <- Link(source = nodeC, target = nodeA, weight = 3, color = 'green')

  linkList <- LinkList(S4Vectors::SimpleList(c(link1, link2, link3)))
  expect_equal(length(linkList), 3)
  expect_equal(getSourceNodes(linkList), list(nodeA, nodeB, nodeC))
  expect_equal(getTargetNodes(linkList), list(nodeB, nodeC, nodeA))
  expect_equal(getWeights(linkList), c(2, 0.1, 3))
  expect_equal(getColors(linkList), c('red', 'blue', 'green'))
})

test_that("Links cannot be created from nonsensical inputs", {

  # Create nodes
  nodeA <- Node(
    id = 'A'
  )
  nodeB <- Node(
    id = 'B'
  )

  expect_error(Link(source = nodeA, target = nodeB, color = false, weight = 10))
})

test_that("LinkLists cannot be created from nonsensical inputs", {

  # Create nodes
  nodeA <- Node(
    id = 'A'
  )
  nodeB <- Node(
    id = 'B'
  )
  nodeC <- Node(
    id = 'C'
  )
  
  # Create links
  link1 <- Link(source = nodeA, target = nodeB)
  link2 <- Link(source = nodeB, target = nodeC)
  link3 <- Link(source = nodeC, target = nodeA, color='red')

  # If one link has a color, all must have colors
  expect_error(LinkList(S4Vectors::SimpleList(c(link1, link2, link3))))

  # Link colors must be of the same class
  color(link2) <- 2
  expect_error(LinkList(S4Vectors::SimpleList(c(link1, link2, link3))))

  # If one link has a weight, all must have weights
  weight(link3) <- 100
  expect_error(LinkList(S4Vectors::SimpleList(c(link1, link2, link3))))
})
