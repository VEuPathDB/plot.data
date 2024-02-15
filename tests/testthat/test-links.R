test_that("Links work", {
  # Make a link
  nodeA <- Node(
    id = NodeId('A')
  )
  nodeB <- Node(
    id = NodeId('B')
  )

  link <- Link(source = nodeA, target = nodeB)
  expect_equal(class(link)[1], 'Link')
  expect_equal(source(link), nodeA)
  expect_equal(target(link), nodeB)
  expect_equal(weight(link), 1)
  expect_true(is.null(color(link)))

  # Make another link w colors and weights
  link <- Link(source = nodeA, target = nodeB, color = 'red', weight = 10)
  expect_equal(class(link)[1], 'Link')
  expect_equal(source(link), nodeA)
  expect_equal(target(link), nodeB)
  expect_equal(color(link), 'red')
  expect_equal(weight(link), 10)

  # pass characters for node ids
  link <- Link(source = 'A', target = 'B')
  expect_equal(class(link)[1], 'Link')
  expect_equal(source(link), nodeA)
  expect_equal(target(link), nodeB)
  expect_equal(weight(link), 1)
  expect_true(is.null(color(link)))

  # pass numbers for node ids
  link <- Link(source = 1, target = 2)
  expect_equal(class(link)[1], 'Link')
  expect_equal(source(link), node(1))
  expect_equal(target(link), node(2))
  expect_equal(weight(link), 1)
  expect_true(is.null(color(link)))

  # pass NodeId objects for node ids
  link <- Link(source = NodeId('A'), target = NodeId('B'))
  expect_equal(class(link)[1], 'Link')
  expect_equal(source(link), nodeA)
  expect_equal(target(link), nodeB)
  expect_equal(weight(link), 1)
  expect_true(is.null(color(link)))

  # an empty one
  link <- Link()
  expect_equal(class(link)[1], 'Link')
  expect_equal(source(link), NULL)
  expect_equal(target(link), NULL)
  expect_equal(weight(link), 1)
  expect_true(is.null(color(link)))
})

test_that("Link methods work", {

  nodeA <- Node(
    id = NodeId('A')
  )
  nodeB <- Node(
    id = NodeId('B')
  )

  link <- Link(source = nodeA, target = nodeB)
  expect_equal(source(link), nodeA)
  expect_equal(target(link), nodeB)
  expect_equal(weight(link), 1)
  expect_true(is.null(color(link)))
  expect_equal(isDirected(link), FALSE)

  link <- Link(source = nodeA, target = nodeB, color = 'red', weight = 10)
  expect_equal(source(link), nodeA)
  expect_equal(target(link), nodeB)
  expect_equal(color(link), 'red')
  expect_equal(weight(link), 10)
  expect_equal(isDirected(link), FALSE)

})

test_that("LinkList methods work", {

  # Create some nodes
  nodeA <- Node(
    id = NodeId('A')
  )
  nodeB <- Node(
    id = NodeId('B')
  )
  nodeC <- Node(
    id = NodeId('C')
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


  # Create some more links with colors and weights
  link1 <- Link(source = nodeA, target = nodeB, weight = 2, color = 'red')
  link2 <- Link(source = nodeB, target = nodeC, weight = 0.1, color = 'blue')
  link3 <- Link(source = nodeC, target = nodeA, weight = 3, color = 'green')

  linkList <- LinkList(S4Vectors::SimpleList(c(link1, link2, link3)))
  expect_equal(length(linkList), 3)
  expect_equal(getSourceNodes(linkList), list(nodeA, nodeB, nodeC))
  expect_equal(getTargetNodes(linkList), list(nodeB, nodeC, nodeA))
  expect_equal(getWeights(linkList), c(2, 0.1, 3))
  expect_equal(getColors(linkList), c('red', 'blue', 'green'))

  # use a list to make LinkList
  linkList <- LinkList(list(link1, link2, link3))
  expect_equal(length(linkList), 3)
  expect_equal(getSourceNodes(linkList), list(nodeA, nodeB, nodeC))
  expect_equal(getTargetNodes(linkList), list(nodeB, nodeC, nodeA))
  expect_equal(getWeights(linkList), c(2, 0.1, 3))
  expect_equal(getColors(linkList), c('red', 'blue', 'green'))

  # use an edgeList to make LinkList
  edgeList <- data.frame(source = 'A', target = 'B')
  expect_equal(class(LinkList(edgeList))[1], 'LinkList')
  expect_equal(length(LinkList(edgeList)), 1)
})

test_that("Links cannot be created from nonsensical inputs", {

  # Create nodes
  nodeA <- Node(
    id = NodeId('A')
  )
  nodeB <- Node(
    id = NodeId('B')
  )

  # self links should fail
  expect_error(Link(source = nodeA, target = nodeA))
  # color should be a string or number or NULL
  expect_error(Link(source = nodeA, target = nodeB, color = false, weight = 10))
})

test_that("LinkLists cannot be created from nonsensical inputs", {

  # Create nodes
  nodeA <- Node(
    id = NodeId('A')
  )
  nodeB <- Node(
    id = NodeId('B')
  )
  nodeC <- Node(
    id = NodeId('C')
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

test_that("toJSON methods for links work", {
  nodeA <- Node('A')
  nodeB <- Node('B')
  link <- Link(source = nodeA, target = nodeB)
  expect_equal(toJSON(link), '{"source":"A","target":"B"}')
  expect_equal(toJSON(link, named = TRUE), '{"link":{"source":"A","target":"B"}}')

  # w colors and weights
  link <- Link(source = nodeA, target = nodeB, color = 'red', weight = 10)
  expect_equal(toJSON(link), '{"source":"A","target":"B","color":"red","weight":10}')
  expect_equal(toJSON(link, named = TRUE), '{"link":{"source":"A","target":"B","color":"red","weight":10}}')

  # LinkList
  linkList <- LinkList(list(link))
  expect_equal(toJSON(linkList), '{"links":[{"source":"A","target":"B","color":"red","weight":10}]}')
  expect_equal(toJSON(linkList, named = FALSE), '[{"source":"A","target":"B","color":"red","weight":10}]')
})