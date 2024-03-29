test_that("NodeId works", {
  expect_equal(class(NodeId('A'))[1], 'NodeId')
})

test_that("NodeIdList works", {
  expect_equal(class(NodeIdList(list(NodeId('A'), NodeId('B'))))[1], 'NodeIdList')
  expect_equal(class(NodeIdList(list('A', 'B')))[1], 'NodeIdList')
  expect_equal(class(NodeIdList(list(Node(id=NodeId('A')))))[1], 'NodeIdList')
  expect_equal(class(NodeIdList(Node(id=NodeId('A'))))[1], 'NodeIdList')
  expect_equal(class(NodeIdList(list(Node(id=NodeId('A')), Node(id=NodeId('B')))))[1], 'NodeIdList')
  expect_equal(class(NodeIdList(c('A', 'B')))[1], 'NodeIdList')
  expect_equal(class(NodeIdList())[1], 'NodeIdList')

  edgeList <- data.frame(source = 'A', target = 'B')
  expect_equal(class(NodeIdList(edgeList))[1], 'NodeIdList')

  expect_error(NodeIdList(S4Vectors::SimpleList(c(NodeId('A'), 'B'))))
  expect_error(NodeIdList(S4Vectors::SimpleList(c('A', 'B'))))
})

test_that("Node methods work", {

  # Create a node
  nodeA <- Node(
    id = NodeId('A')
  )

  expect_equal(class(nodeA)[1], 'Node')
  expect_equal(id(nodeA), 'A')
  expect_equal(color(nodeA), NULL)
  expect_equal(weight(nodeA), NULL)

  # use a different constructor method
  nodeB <- Node('B', color = 'red', weight = 10)

  expect_equal(class(nodeB)[1], 'Node')
  expect_equal(id(nodeB), 'B')
  expect_equal(color(nodeB), 'red')
  expect_equal(weight(nodeB), 10)

  # numeric id
  nodeC <- Node(1, color = 'red', weight = 10)

  expect_equal(class(nodeC)[1], 'Node')
  expect_equal(id(nodeC), "1")
  expect_equal(color(nodeC), 'red')
  expect_equal(weight(nodeC), 10)
})

test_that("NodeList methods work", {

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

  nodeList <- NodeList(S4Vectors::SimpleList(c(nodeA, nodeB, nodeC)))
  expect_equal(length(nodeList), 3)
  expect_equal(getNodeIds(nodeList), c('A', 'B', 'C'))
  expect_equal(getWeights(nodeList), c(NULL, NULL, NULL))
  expect_equal(getColors(nodeList), c(NULL, NULL, NULL))


  # Create more interesting nodes
  nodeA <- Node(
    id = NodeId('A'),
    color = 'red',
    weight = 10
  )
  nodeB <- Node(
    id = NodeId('B'),
    color = 'blue',
    weight = 20
  )
  nodeC <- Node(
    id = NodeId('C'),
    color = 'green',
    weight = 30
  )

  nodeList <- NodeList(S4Vectors::SimpleList(c(nodeA, nodeB, nodeC)))
  expect_equal(length(nodeList), 3)
  expect_equal(getNodeIds(nodeList), c('A', 'B', 'C'))
  expect_equal(getWeights(nodeList), c(10, 20, 30))
  expect_equal(getColors(nodeList), c('red', 'blue', 'green'))

  # use a different constructor method
  nodeList <- NodeList(c(nodeA, nodeB, nodeC))
  expect_equal(length(nodeList), 3)
  expect_equal(getNodeIds(nodeList), c('A', 'B', 'C'))
  expect_equal(getWeights(nodeList), c(10, 20, 30))
  expect_equal(getColors(nodeList), c('red', 'blue', 'green'))

  # use a different constructor method
  nodeList <- NodeList(list(nodeA, nodeB, nodeC))
  expect_equal(length(nodeList), 3)
  expect_equal(getNodeIds(nodeList), c('A', 'B', 'C'))
  expect_equal(getWeights(nodeList), c(10, 20, 30))
  expect_equal(getColors(nodeList), c('red', 'blue', 'green'))

  edgeList <- data.frame(source = 'A', target = 'B')
  expect_equal(class(NodeList(edgeList))[1], 'NodeList')

  # multiple edges in edgeList, test degrees
  edgeList <- data.frame(source = c('A', 'B'), target = c('B', 'C'))
  expect_equal(class(NodeList(edgeList))[1], 'NodeList')
  expect_equal(length(NodeList(edgeList)), 3)
  expect_equal(getNodeIds(NodeList(edgeList)), c('A', 'B', 'C'))
  expect_equal(getWeights(NodeList(edgeList)), c(NULL, NULL, NULL))
  expect_equal(getColors(NodeList(edgeList)), c(NULL, NULL, NULL))
  expect_equal(getDegrees(NodeList(edgeList)), c(1, 2, 1))

})

test_that("We cannot make nonsensical nodes", {

  expect_error(Node(id = FALSE))
  expect_error(Node(id = 'A', color = FALSE))
  expect_error(Node(id = 'A', weight = '10'))
})


test_that("We cannot make nonsensical NodeLists", {

  # Create some nodes
  nodeA <- Node(
    id = NodeId('A')
  ) 
  nodeB <- Node(
    id = NodeId('B'),
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

test_that("toJSON methods for nodes work", {
  nodeA <- Node('A')
  expect_equal(veupathUtils::toJSON(nodeA), '{"id":"A"}')
  expect_equal(veupathUtils::toJSON(nodeA, named = TRUE), '{"node":{"id":"A"}}')

  # w weights and colors
  nodeB <- Node('B', color = 'red', weight = 10)
  expect_equal(veupathUtils::toJSON(nodeB), '{"id":"B","color":"red","weight":10}')
  expect_equal(veupathUtils::toJSON(nodeB, named = TRUE), '{"node":{"id":"B","color":"red","weight":10}}')

  #NodeList
  nodeA <- Node('A', color='blue', weight=5)
  nodeList <- NodeList(list(nodeA, nodeB))
  expect_equal(veupathUtils::toJSON(nodeList), '{"nodes":[{"id":"A","color":"blue","weight":5},{"id":"B","color":"red","weight":10}]}')
  expect_equal(veupathUtils::toJSON(nodeList, named = FALSE), '[{"id":"A","color":"blue","weight":5},{"id":"B","color":"red","weight":10}]')

  #NodeIdList
  nodeIdList <- NodeIdList(list(NodeId('A'), NodeId('B')))
  expect_equal(veupathUtils::toJSON(nodeIdList), '{"nodeIds":["A","B"]}')
  expect_equal(veupathUtils::toJSON(nodeIdList, named = FALSE), '["A","B"]')
})
