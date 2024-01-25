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
  
  # Create some links
  link1 <- Link(source = nodeA, target = nodeB, weight = 10)
  link2 <- Link(source = nodeB, target = nodeC, weight = 20)
  link3 <- Link(source = nodeC, target = nodeA, weight = 30)
  
  # Create the network w a single default partition
  net <- KPartiteNetwork(links = LinkList(c(link1, link2, link3)), nodes = NodeList(c(nodeA, nodeB, nodeC)))
  json <- veupathUtils::toJSON(net)
  jsonList <- jsonlite::fromJSON(json)
  expect_equal(jsonList$network$data$links$source, c('A','B','C'))
  expect_equal(jsonList$network$data$links$target, c('B','C','A'))
  expect_equal(jsonList$network$data$links$weight, c(10,20,30))
  expect_equal(jsonList$network$data$nodes$id, c('A','B','C'))
  expect_equal(as.list(jsonList$network$data$partitions), list('A','B','C'))
  expect_equal(length(jsonList$network$config$variables), 0)

  # Create partitions
  partition1 <- Partition(list(nodeA, nodeB))
  partition2 <- Partition(nodeC)

  # Create k-partite network
  bpnet <- KPartiteNetwork(
    links = LinkList(list(link1, link2, link3)),
    nodes = NodeList(list(nodeA, nodeB, nodeC)),
    partitions = Partitions(list(partition1, partition2))
  )
  json <- veupathUtils::toJSON(bpnet)
  jsonList <- jsonlite::fromJSON(json)
  expect_equal(jsonList$network$data$links$source, c('A','B','C'))
  expect_equal(jsonList$network$data$links$target, c('B','C','A'))
  expect_equal(jsonList$network$data$links$weight, c(10,20,30))
  expect_equal(jsonList$network$data$nodes$id, c('A','B','C'))
  expect_equal(jsonList$network$data$partitions, list(c('A','B'), c('C')))
  expect_equal(length(jsonList$network$config$variables), 0)
})

test_that("we can build a KPartiteNetwork from an edgeList data.frame", {
  edgeList <- data.frame(
    source = c('a', 'b', 'c'),
    target = c('b', 'c', 'a')
  )
  net <- KPartiteNetwork(object = edgeList)
  expect_equal(getNodes(net), NodeList(c(Node('a'), Node('b'), Node('c'))))
  expect_equal(getLinks(net), LinkList(c(Link(source = Node('a'), target = Node('b')), Link(source = Node('b'), target = Node('c')), Link(source = Node('c'), target = Node('a')))))
  expect_equal(partitions(net), Partitions(list(Partition(list(Node('a'), Node('b'), Node('c'))))))
  expect_equal(getLinkColorScheme(net), 'none')
})