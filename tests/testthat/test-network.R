test_that("Networks can be created and their properties accessed", {

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

  # Create some edges
  link1 <- Link(source = nodeA, target = nodeB, color = 1, weight = 10)
  link2 <- Link(source = nodeB, target = nodeC, color = 2, weight = 20)
  link3 <- Link(source = nodeC, target = nodeA, color = 3, weight = 30)

  # Create a network
  net <- Network(links = LinkList(c(link1, link2, link3)), nodes = NodeList(c(nodeA, nodeB, nodeC)))

  expect_equal(getNodes(net), NodeList(c(nodeA, nodeB, nodeC)))
  expect_equal(getLinks(net), LinkList(c(link1, link2, link3)))
  expect_equal(getLinkColorScheme(net), 'none')


})

test_that("We cannot make inappropriate networks", {

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

  # Create links
  link1 <- Link(source = nodeA, target = nodeB, color = 1, weight = 10)
  link2 <- Link(source = nodeB, target = nodeC, color = 2, weight = 20)

  # Create a network with a node in links that isn't in nodes
  expect_error(Network(links = LinkList(c(link1, link2)), nodes = NodeList(c(nodeB, nodeC))))

  # Create a network with an invalid linkColorScheme
  expect_error(Network(links = LinkList(c(link1, link2)), nodes = NodeList(c(nodeA, nodeB)), linkColorScheme = 'nope'))

  # Create a network with duplicate nodes
  expect_error(Network(links = LinkList(c(link1, link2)), nodes = NodeList(c(nodeA, nodeB, nodeB))))
  
})

test_that("We can remove isolated nodes", {
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
  nodeD <- Node(
    id = NodeId('D')
  )

  # Create some links
  link1 <- Link(source = nodeA, target = nodeB, weight = 10)
  link2 <- Link(source = nodeB, target = nodeC, weight = 20)
  link3 <- Link(source = nodeC, target = nodeA, weight = 30)

  # Create the network. nodeD has no links
  net <- Network(links = LinkList(c(link1, link2, link3)), nodes = NodeList(c(nodeA, nodeB, nodeC, nodeD)))
  lonelyNodes <- getIsolatedNodes(net)
  expect_equal(lonelyNodes, NodeList(c(nodeD)))

  netNoIsolatedNodes <- pruneIsolatedNodes(net, verbose = T)
  expect_equal(getNodes(netNoIsolatedNodes), NodeList(c(nodeA, nodeB, nodeC)))
  expect_equal(getLinks(netNoIsolatedNodes), LinkList(c(link1, link2, link3)))
  expect_equal(getLinkColorScheme(netNoIsolatedNodes), 'none')

})

test_that("we can remove duplicate links", {

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
  link1 <- Link(source = nodeA, target = nodeB, weight = 10)
  link2 <- Link(source = nodeB, target = nodeC, weight = 20)
  link3 <- Link(source = nodeC, target = nodeA, weight = 30)

  # Create the network
  net <- Network(links = LinkList(c(link1, link2, link3, link3)), nodes = NodeList(c(nodeA, nodeB, nodeC)))
  netNoDups <- pruneDuplicateLinks(net)
  expect_equal(getNodes(netNoDups), NodeList(c(nodeA, nodeB, nodeC)))
  expect_equal(getLinks(netNoDups), LinkList(c(link1, link2, link3)))
  expect_equal(getLinkColorScheme(netNoDups), 'none')
})

test_that("We can remove links by weight", {
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
  link1 <- Link(source = nodeA, target = nodeB, weight = 10)
  link2 <- Link(source = nodeB, target = nodeC, weight = 20)
  link3 <- Link(source = nodeC, target = nodeA, weight = 30)

  # Create the network
  net <- Network(links = LinkList(c(link1, link2, link3)), nodes = NodeList(c(nodeA, nodeB, nodeC)))

  netNoSmallLinks <- pruneLinksBelowWeight(net, threshold = 20)
  expect_equal(getNodes(netNoSmallLinks), NodeList(c(nodeA, nodeB, nodeC)))
  expect_equal(getLinks(netNoSmallLinks), LinkList(c(link2, link3)))
  expect_equal(getLinkColorScheme(netNoSmallLinks), 'none')

  netNoLargeLinks <- pruneLinksAboveWeight(net, threshold = 10)
  expect_equal(getNodes(netNoLargeLinks), NodeList(c(nodeA, nodeB, nodeC)))
  expect_equal(getLinks(netNoLargeLinks), LinkList(c(link1)))
  expect_equal(getLinkColorScheme(netNoLargeLinks), 'none')

})

test_that("toJSON works for networks", {
  # Create some nodes
  nodeA <- Node(
    id = NodeId('A'),
    degree = 2
  )
  nodeB <- Node(
    id = NodeId('B'),
    degree = 2
  )
  nodeC <- Node(
    id = NodeId('C'),
    degree = 2
  )
  
  # Create some links
  link1 <- Link(source = nodeA, target = nodeB, weight = 10)
  link2 <- Link(source = nodeB, target = nodeC, weight = 20)
  link3 <- Link(source = nodeC, target = nodeA, weight = 30)
  
  # Create the network
  net <- Network(links = LinkList(c(link1, link2, link3)), nodes = NodeList(c(nodeA, nodeB, nodeC)))
  json <- veupathUtils::toJSON(net)
  jsonList <- jsonlite::fromJSON(json)
  expect_equal(jsonList$network$data$links$source, c('A','B','C'))
  expect_equal(jsonList$network$data$links$target, c('B','C','A'))
  expect_equal(jsonList$network$data$links$weight, c(10,20,30))
  expect_equal(jsonList$network$data$nodes$id, c('A','B','C'))
  expect_equal(jsonList$network$data$nodes$degree, c(2,2,2))
  expect_equal(length(jsonList$network$config$variables), 0)

})

test_that("we can build a Network from an edgeList data.frame", {
  edgeList <- data.frame(
    source = c('a', 'b', 'c'),
    target = c('b', 'c', 'a')
  )
  net <- Network(object = edgeList)
  expect_equal(getNodeIds(net), c('a', 'b', 'c'))
  expect_equal(getDegrees(net), c(2, 2, 2))
  expect_equal(!is.null(getCoords(net)), TRUE)
  expect_equal(getLinks(net)[[1]]@source, NodeId('a'))
  expect_equal(getLinks(net)[[1]]@target, NodeId('b'))
  expect_equal(getLinks(net)[[2]]@source, NodeId('b'))
  expect_equal(getLinks(net)[[2]]@target, NodeId('c'))
  expect_equal(getLinks(net)[[3]]@source, NodeId('c'))
  expect_equal(getLinks(net)[[3]]@target, NodeId('a'))
  expect_equal(getLinkColorScheme(net), 'none')
  expect_equal(getDegrees(net), c(2, 2, 2))

  #w a weight column
  edgeList <- data.frame(
    source = c('a', 'b', 'c'),
    target = c('b', 'c', 'a'),
    weight = c(1,2,3)
  )
  net <- Network(object = edgeList)
  expect_equal(getNodeIds(net), c('a', 'b', 'c'))
  expect_equal(getDegrees(net), c(2, 2, 2))
  expect_equal(!is.null(getCoords(net)), TRUE)
  expect_equal(getLinks(net)[[2]]@weight, 2)
  expect_equal(getLinks(net)[[3]]@weight, 3)
  expect_equal(getLinkColorScheme(net), 'none')
  expect_equal(getDegrees(net), c(2, 2, 2))

  #w a color scheme
  edgeList <- data.frame(
    source = c('a', 'b', 'c'),
    target = c('b', 'c', 'a'),
    weight = c(-10,0,10)
  )
  net <- Network(object = edgeList, linkColorScheme = 'posneg')
  expect_equal(getNodeIds(net), c('a', 'b', 'c'))
  expect_equal(getDegrees(net), c(2, 2, 2))
  expect_equal(!is.null(getCoords(net)), TRUE)
  expect_equal(getLinks(net)[[1]]@weight, -10)
  expect_equal(getLinks(net)[[2]]@weight, 0)
  expect_equal(getLinks(net)[[3]]@weight, 10)
  expect_equal(getLinks(net)[[1]]@color, -1)
  expect_equal(getLinks(net)[[2]]@color, 0)
  expect_equal(getLinks(net)[[3]]@color, 1)
  expect_equal(getLinkColorScheme(net), 'posneg')
  expect_equal(getDegrees(net), c(2, 2, 2))
})