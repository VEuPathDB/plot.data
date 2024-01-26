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
  
})
