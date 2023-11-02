test_that("Node methods work", {

  # Create a node
  nodeA <- Node(
    id = 'A'
  )
  expect_equal(id(nodeA), 'A')
  expect_equal(color(nodeA), character())
  expect_equal(weight(nodeA), numeric())

  nodeB <- Node(
    id = 'B',
    color = 'red',
    weight = 10
  )

  expect_equal(id(nodeB), 'B')
  expect_equal(color(nodeB), 'red')
  expect_equal(weight(nodeB), 10)
})

test_that("NodeList methods work", {

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

  nodeList <- NodeList(S4Vectors::SimpleList(c(nodeA, nodeB, nodeC)))
  expect_equal(length(nodeList), 3)
  expect_equal(getNodeIds(nodeList), c('A', 'B', 'C'))
  expect_equal(getWeights(nodeList), c(numeric(), numeric(), numeric()))
  expect_equal(getColors(nodeList), c(character(), character(), character()))


  # Create more interesting nodes
  nodeA <- Node(
    id = 'A',
    color = 'red',
    weight = 10
  )
  nodeB <- Node(
    id = 'B',
    color = 'blue',
    weight = 20
  )
  nodeC <- Node(
    id = 'C',
    color = 'green',
    weight = 30
  )

  nodeList <- NodeList(S4Vectors::SimpleList(c(nodeA, nodeB, nodeC)))
  expect_equal(length(nodeList), 3)
  expect_equal(getNodeIds(nodeList), c('A', 'B', 'C'))
  expect_equal(getWeights(nodeList), c(10, 20, 30))
  expect_equal(getColors(nodeList), c('red', 'blue', 'green'))



})

test_that("We cannot make nonsensical nodes", {

  expect_error(Node(id = FALSE))
  expect_error(Node(id = 10))
  expect_error(Node(id = 'A', color = FALSE))
  expect_error(Node(id = 'A', weight = '10'))
})


test_that("We cannot make nonsensical NodeLists", {

 # Create some nodes
  nodeA <- Node(
    id = 'A'
  ) 
  nodeB <- Node(
    id = 'B',
    color = 'red'
  )

  # If one node has a color, all must have colors
  expect_error(NodeList(S4Vectors::SimpleList(c(nodeA, nodeB))))

  # Nodes must have the same class of colors
  color(nodeA) <- 1
  expect_error(NodeList(S4Vectors::SimpleList(c(nodeA, nodeB))))

  # If one node has a weight, all much have weights
  weight(nodeA) <- 100
  expect_error(NodeList(S4Vectors::SimpleList(c(nodeA, nodeB))))
})
