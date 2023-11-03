test_that("k-partite networks can be created", {
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

  # Create partitions
  partition1IDs <- c('A', 'B')
  partition2IDs <- c('C')

  # Create k-partite network
  bpnet <- KPartiteNetwork(
    links = LinkList(c(link1, link2, link3)),
    nodes = NodeList(c(nodeA, nodeB, nodeC)),
    partitions = list(partition1IDs, partition2IDs)
  )

  expect_equal(getNodes(bpnet), NodeList(c(nodeA, nodeB, nodeC)))
  expect_equal(getLinks(bpnet), LinkList(c(link1, link2, link3)))
  expect_equal(getLinkColorScheme(bpnet), 'none')

})

test_that("k-partite networks cannot be created from nonsensical inputs", {

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
  partition1IDs <- c('A', 'B', 'C')
  partition2IDs <- c('C') 

  # Nodes can't be in both columns
  expect_error(KPartiteNetwork(
    links = LinkList(c(link1, link2, link3)),
    nodes = NodeList(c(nodeA, nodeB, nodeC)),
    partitons = list(partition1IDs, partition2IDs)
  ))

  # All nodes must be in one of the columns
  partition1IDs <- c('A')
  partition2IDs <- c('C')
  expect_error(KPartiteNetwork(
    links = LinkList(c(link1, link2, link3)),
    nodes = NodeList(c(nodeA, nodeB, nodeC)),
    partitons = list(partition1IDs, partition2IDs)
  ))

})
