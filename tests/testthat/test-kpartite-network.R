test_that("k-partite networks can be created", {
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
  link1 <- Link(source = nodeA, target = nodeB, color = 1, weight = 10)
  link2 <- Link(source = nodeB, target = nodeC, color = 2, weight = 20)
  link3 <- Link(source = nodeC, target = nodeA, color = 3, weight = 30)

  # Create partitions
  partition1 <- Partition(list(nodeA, nodeB))
  partition2 <- Partition(nodeC)

  # Create k-partite network
  bpnet <- KPartiteNetwork(
    links = LinkList(list(link1, link2, link3)),
    nodes = NodeList(list(nodeA, nodeB, nodeC)),
    partitions = Partitions(list(partition1, partition2))
  )

  expect_equal(getNodes(bpnet), NodeList(c(nodeA, nodeB, nodeC)))
  expect_equal(getLinks(bpnet), LinkList(c(link1, link2, link3)))
  expect_equal(getLinkColorScheme(bpnet), 'none')

})

test_that("k-partite networks cannot be created from nonsensical inputs", {

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
  link1 <- Link(source = nodeA, target = nodeB, color = 1, weight = 10)
  link2 <- Link(source = nodeB, target = nodeC, color = 2, weight = 20)
  link3 <- Link(source = nodeC, target = nodeA, color = 3, weight = 30) 

  # Create columns
  partition1 <- Partition(list(nodeA, nodeB, nodeC))
  partition2 <- Partition(nodeC)


  # Nodes can't be in both columns
  expect_error(KPartiteNetwork(
    links = LinkList(list(link1, link2, link3)),
    nodes = NodeList(list(nodeA, nodeB, nodeC)),
    partitions = list(partition1, partition2)
  ))

  # All nodes must be in one of the columns
  partition1 <- Partition(nodeA)
  partition2 <- Partition(nodeC)
  expect_error(KPartiteNetwork(
    links = LinkList(c(link1, link2, link3)),
    nodes = NodeList(c(nodeA, nodeB, nodeC)),
    partitions = list(partition1IDs, partition2IDs)
  ))

})
