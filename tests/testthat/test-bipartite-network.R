test_that("Bipartite networks can be created", {
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
  link1 <- Link(source = nodeA, target = nodeB, color = 1, weight = 10)
  link2 <- Link(source = nodeB, target = nodeC, color = 2, weight = 20)
  link3 <- Link(source = nodeC, target = nodeA, color = 3, weight = 30)

  # Create columns
  col1IDs <- c('A', 'B')
  col2IDs <- c('C')

  # Create bipartite network
  bpnet <- BipartiteNetwork(
    links = LinkList(c(link1, link2, link3)),
    nodes = NodeList(c(nodeA, nodeB, nodeC)),
    column1NodeIDs = col1IDs,
    column2NodeIDs = col2IDs
  )

  expect_equal(getNodes(bpnet), NodeList(c(nodeA, nodeB, nodeC)))
  expect_equal(getLinks(bpnet), LinkList(c(link1, link2, link3)))
  expect_equal(getLinkColorScheme(bpnet), 'none')

})

test_that("Bipartite networks cannot be created from nonsensical inputs", {

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
  link1 <- Link(source = nodeA, target = nodeB, color = 1, weight = 10)
  link2 <- Link(source = nodeB, target = nodeC, color = 2, weight = 20)
  link3 <- Link(source = nodeC, target = nodeA, color = 3, weight = 30) 

  # Create columns
  col1IDs <- c('A', 'B', 'C')
  col2IDs <- c('C') 

  # Nodes can't be in both columns
  expect_error(BipartiteNetwork(
    links = LinkList(c(link1, link2, link3)),
    nodes = NodeList(c(nodeA, nodeB, nodeC)),
    column1NodeIDs = col1IDs,
    column2NodeIDs = col2IDs
  ))

  # All nodes must be in one of the columns
  col1IDs <- c('A')
  col2IDs <- c('C')
  expect_error(BipartiteNetwork(
    links = LinkList(c(link1, link2, link3)),
    nodes = NodeList(c(nodeA, nodeB, nodeC)),
    column1NodeIDs = col1IDs,
    column2NodeIDs = col2IDs
  ))

})
