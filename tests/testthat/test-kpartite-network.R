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
  nodeD <- Node(
    id = NodeId('D')
  )

  # Create links
  link1 <- Link(source = nodeA, target = nodeB, color = 1, weight = 10)
  link2 <- Link(source = nodeB, target = nodeA, color = 2, weight = 20)
  link3 <- Link(source = nodeC, target = nodeD, color = 3, weight = 30)

  # Create partitions
  partition1 <- Partition(list(nodeA, nodeC))
  partition2 <- Partition(list(nodeB, nodeD))

  # Create k-partite network
  bpnet <- KPartiteNetwork(
    links = LinkList(list(link1, link2, link3)),
    nodes = NodeList(list(nodeA, nodeB, nodeC, nodeD)),
    partitions = Partitions(list(partition1, partition2))
  )

  expect_equal(getNodes(bpnet), NodeList(c(nodeA, nodeB, nodeC, nodeD)))
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
  nodeD <- Node(
    id = NodeId('D')
  )

  # Create links
  link1 <- Link(source = nodeA, target = nodeB, color = 1, weight = 10)
  link2 <- Link(source = nodeB, target = nodeA, color = 2, weight = 20)
  link3 <- Link(source = nodeC, target = nodeD, color = 3, weight = 30) 

  # Create columns
  partition1 <- Partition(list(nodeA, nodeB, nodeC))
  partition2 <- Partition(list(nodeB, nodeD))


  # Nodes can't be in both columns
  expect_error(KPartiteNetwork(
    links = LinkList(list(link1, link2, link3)),
    nodes = NodeList(list(nodeA, nodeB, nodeC, nodeD)),
    partitions = list(partition1, partition2)
  ))

  # All nodes must be in one of the columns
  partition1 <- Partition(nodeA)
  partition2 <- Partition(nodeC)
  expect_error(KPartiteNetwork(
    links = LinkList(c(link1, link2, link3)),
    nodes = NodeList(c(nodeA, nodeB, nodeC)),
    partitions = list(partition1, partition2)
  ))

  # No links within a partition
  partition1 <- Partition(list(nodeA, nodeB))
  partition2 <- Partition(list(nodeC, nodeD))
  link4 <- Link(source = nodeA, target = nodeC, color = 1, weight = 10)
  expect_error(KPartiteNetwork(
    links = LinkList(c(link1, link2, link3, link4)),
    nodes = NodeList(c(nodeA, nodeB, nodeC)),
    partitions = list(partition1, partition2)
  ))

  # no partitions specified
  expect_error(KPartiteNetwork(
    links = LinkList(c(link1, link2, link3)),
    nodes = NodeList(c(nodeA, nodeB, nodeC))
  ))
})

test_that("k-partite methods work", {
  edgeList <- data.frame(
    source = c('a', 'b', 'c'),
    target = c('b', 'a', 'd')
  )
  net <- KPartiteNetwork(
    object = edgeList, 
    partitions = Partitions(list(
      Partition(list(Node('a'), Node('c'))), 
      Partition(list(Node('b'), Node('d')))
    ))
  )

  # test getAllNodeIds
  expect_equal(getAllNodeIds(net), NodeList(c(Node('a'), Node('b'), Node('c'), Node('d'))))

  # test partitions
  expect_equal(partitions(net), Partitions(list(Partition(list(Node('a'), Node('c'))), Partition(list(Node('b'), Node('d'))))))
})

test_that("toJSON works for k-partite networks", {
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
  link2 <- Link(source = nodeB, target = nodeA, weight = 20)
  link3 <- Link(source = nodeC, target = nodeD, weight = 30)
  
  # Create the network
  net <- KPartiteNetwork(
    links = LinkList(c(link1, link2, link3)), 
    nodes = NodeList(c(nodeA, nodeB, nodeC, nodeD)),
    partitions = Partitions(list(Partition(list(nodeA, nodeC)), Partition(list(nodeB, nodeD))))
  )
  json <- veupathUtils::toJSON(net)
  jsonList <- jsonlite::fromJSON(json)
  expect_equal(jsonList$bipartitenetwork$data$links$source, c('A','B','C'))
  expect_equal(jsonList$bipartitenetwork$data$links$target, c('B','A','D'))
  expect_equal(jsonList$bipartitenetwork$data$links$weight, c(10,20,30))
  expect_equal(jsonList$bipartitenetwork$data$nodes$id, c('A','B','C','D'))
  expect_equal(jsonList$bipartitenetwork$data$partitions$nodeIds, list(c('A','C'), c('B','D')))
  expect_equal(length(jsonList$bipartitenetwork$config$variables), 0)
})

test_that("we can build a KPartiteNetwork from an edgeList data.frame", {
  edgeList <- data.frame(
    source = c('a', 'b', 'c'),
    target = c('b', 'a', 'd')
  )
  net <- KPartiteNetwork(
    object = edgeList, 
    partitions = Partitions(list(
      Partition(list(Node('a'), Node('c'))), 
      Partition(list(Node('b'), Node('d')))
    ))
  )

  expect_equal(getNodes(net), NodeList(c(Node('a'), Node('b'), Node('c'), Node('d'))))
  expect_equal(getLinks(net), LinkList(c(Link(source = Node('a'), target = Node('b')), Link(source = Node('b'), target = Node('a')), Link(source = Node('c'), target = Node('d')))))
  expect_equal(partitions(net), Partitions(list(Partition(list(Node('a'), Node('c'))), Partition(list(Node('b'), Node('d'))))))
  expect_equal(getLinkColorScheme(net), 'none')
})