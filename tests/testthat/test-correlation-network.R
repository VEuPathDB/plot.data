test_that("correlation networks can be created", {
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
    link1 <- CorrelationLink(source = nodeA, target = nodeB, correlationCoef = .8, pValue = .01)
    link2 <- CorrelationLink(source = nodeB, target = nodeC, correlationCoef = .3, pValue = .001)
    link3 <- CorrelationLink(source = nodeC, target = nodeA, correlationCoef = -.8, pValue = .1)

    # Create a network
    net <- CorrelationNetwork(
        links = CorrelationLinkList(c(link1, link2, link3)), 
        nodes = NodeList(c(nodeA, nodeB, nodeC)),
        pValueThreshold = .05
    )

    expect_equal(getNodes(net), NodeList(c(nodeA, nodeB, nodeC)))
    expect_equal(getLinks(net), CorrelationLinkList(c(link1, link2))) ## link 3 is pruned for high pValue
    expect_equal(getLinkColorScheme(net), 'posneg')
    
    # Create a network
    net <- CorrelationNetwork(
        links = CorrelationLinkList(c(link1, link2, link3)), 
        nodes = NodeList(c(nodeA, nodeB, nodeC))
    )

    expect_equal(getNodes(net), NodeList(c(nodeA, nodeB, nodeC)))
    expect_equal(getLinks(net), CorrelationLinkList(c(link1, link2, link3))) ## link 3 should be back
    expect_equal(getLinkColorScheme(net), 'posneg')
})

test_that("we cannot make inappropriate correlation networks", {
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
    link1 <- CorrelationLink(source = nodeA, target = nodeB, correlationCoef = .8, pValue = .01)
    link2 <- CorrelationLink(source = nodeB, target = nodeC, correlationCoef = .3, pValue = .001)

    # Create a link w non-sensical pValue and correlationCoef
    expect_error(CorrelationLink(source = nodeC, target = nodeA, correlationCoef = -.8, pValue = 1.1))
    expect_error(CorrelationLink(source = nodeC, target = nodeA, correlationCoef = 1.1, pValue = .1))

    # Create a network with a node in links that isn't in nodes
    expect_error(CorrelationNetwork(links = CorrelationLinkList(c(link1, link2)), nodes = NodeList(c(nodeB, nodeC))))

    # Create a network with an invalid linkColorScheme
    expect_error(CorrelationNetwork(links = CorrelationLinkList(c(link1, link2)), nodes = NodeList(c(nodeA, nodeB)), linkColorScheme = 'nope'))

    # Create a network with duplicate nodes
    expect_error(CorrelationNetwork(links = CorrelationLinkList(c(link1, link2)), nodes = NodeList(c(nodeA, nodeB, nodeB))))

    # Create a network where links dont meet threshold
    net <- CorrelationNetwork(links = CorrelationLinkList(c(link1, link2)), nodes = NodeList(c(nodeA, nodeB, nodeC)))
    net@pValueThreshold <- .001
    expect_error(validObject(net))

})

test_that("correlation networks can be pruned by threshold", {
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
    link1 <- CorrelationLink(source = nodeA, target = nodeB, correlationCoef = .8, pValue = .01)
    link2 <- CorrelationLink(source = nodeB, target = nodeC, correlationCoef = .3, pValue = .001)
    link3 <- CorrelationLink(source = nodeC, target = nodeA, correlationCoef = -.8, pValue = .1)

    # Create a network
    net <- CorrelationNetwork(
        links = CorrelationLinkList(c(link1, link2, link3)), 
        nodes = NodeList(c(nodeA, nodeB, nodeC))
    )

    net <- pruneCorrelationLinks(net, pValueThreshold = .05)

    # links should be modified and nothing else
    expect_equal(getNodes(net), NodeList(c(nodeA, nodeB, nodeC)))
    expect_equal(getLinks(net), CorrelationLinkList(c(link1, link2))) ## link 3 is pruned for high pValue
    expect_equal(getLinkColorScheme(net), 'posneg')
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

    # Create some edges
    link1 <- CorrelationLink(source = nodeA, target = nodeB, correlationCoef = .8, pValue = .01)
    link2 <- CorrelationLink(source = nodeB, target = nodeC, correlationCoef = .3, pValue = .001)
    link3 <- CorrelationLink(source = nodeC, target = nodeA, correlationCoef = -.8, pValue = .1)

    # Create a network
    net <- CorrelationNetwork(
        links = CorrelationLinkList(c(link1, link2, link3)), 
        nodes = NodeList(c(nodeA, nodeB, nodeC))
    )

    # Convert to JSON
    json <- veupathUtils::toJSON(net)
    jsonList <- jsonlite::fromJSON(json)
    expect_equal(jsonList$network$data$links$source, c('A','B','C'))
    expect_equal(jsonList$network$data$links$target, c('B','C','A'))
    expect_equal(jsonList$network$data$links$weight, c(.8,.3,.8))
    expect_equal(jsonList$network$data$nodes$id, c('A','B','C'))
    expect_equal(jsonList$network$data$nodes$degree, c(2,2,2))
    expect_equal(length(jsonList$network$config$variables), 0)
    expect_equal(jsonList$network$config$correlationCoefThreshold, "NA")
    expect_equal(jsonList$network$config$pValueThreshold, "NA")

    net <- CorrelationNetwork(
        links = CorrelationLinkList(c(link1, link2, link3)), 
        nodes = NodeList(c(nodeA, nodeB, nodeC)),
        pValueThreshold = .05
    )

    json <- veupathUtils::toJSON(net)
    jsonList <- jsonlite::fromJSON(json)
    expect_equal(jsonList$network$data$links$source, c('A','B'))
    expect_equal(jsonList$network$data$links$target, c('B','C'))
    expect_equal(jsonList$network$data$links$weight, c(.8,.3))
    expect_equal(jsonList$network$data$nodes$id, c('A','B','C'))
    expect_equal(jsonList$network$data$nodes$degree, c(2,2,2))
    expect_equal(length(jsonList$network$config$variables), 0)
    expect_equal(jsonList$network$config$correlationCoefThreshold, "NA")
    expect_equal(jsonList$network$config$pValueThreshold, 0.05)
})

test_that("we can build a Network from an edgeList data.frame", {
    #w a weight column
    edgeList <- data.frame(
        source = c('a', 'b', 'c'),
        target = c('b', 'c', 'a'),
        correlationCoef = c(.8,.3,-.8),
        pValue = c(.01,.001,.1)
    )
    net <- CorrelationNetwork(object = edgeList, linkColorScheme = 'none')
    expect_equal(getNodeIds(net), c('a', 'b', 'c'))
    expect_equal(getDegrees(net), c(2, 2, 2))
    expect_equal(!is.null(getCoords(net)), TRUE)
    expect_equal(getLinks(net)[[2]]@weight, .3)
    expect_equal(getLinks(net)[[3]]@weight, .8)
    expect_equal(getLinkColorScheme(net), 'none')
    expect_equal(getDegrees(net), c(2, 2, 2))

    #w a color scheme
    edgeList <- data.frame(
        source = c('a', 'b', 'c'),
        target = c('b', 'c', 'a'),
        correlationCoef = c(.8,.3,-.8),
        pValue = c(.01,.001,.1)
    )
    net <- CorrelationNetwork(object = edgeList, linkColorScheme = 'posneg')
    expect_equal(getNodeIds(net), c('a', 'b', 'c'))
    expect_equal(getDegrees(net), c(2, 2, 2))
    expect_equal(!is.null(getCoords(net)), TRUE)
    expect_equal(getLinks(net)[[1]]@weight, .8)
    expect_equal(getLinks(net)[[2]]@weight, .3)
    expect_equal(getLinks(net)[[3]]@weight, .8)
    expect_equal(getLinks(net)[[1]]@color, 1)
    expect_equal(getLinks(net)[[2]]@color, 1)
    expect_equal(getLinks(net)[[3]]@color, -1)
    expect_equal(getLinkColorScheme(net), 'posneg')
    expect_equal(getDegrees(net), c(2, 2, 2))

    #w a pValueThreshold
    edgeList <- data.frame(
        source = c('a', 'b', 'c'),
        target = c('b', 'c', 'a'),
        correlationCoef = c(.8,.3,-.8),
        pValue = c(.01,.001,.1)
    )
    net <- CorrelationNetwork(object = edgeList, pValueThreshold = .05)
    expect_equal(getNodeIds(net), c('a', 'b', 'c'))
    expect_equal(getDegrees(net), c(2, 2, 2))
    expect_equal(!is.null(getCoords(net)), TRUE)
    expect_equal(getLinkColorScheme(net), 'posneg')
    expect_equal(length(getLinks(net)), 2)
    expect_equal(getLinks(net)[[1]]@weight, .8)
    expect_equal(getLinks(net)[[2]]@weight, .3)

    #w a correlationCoefThreshold
    edgeList <- data.frame(
        source = c('a', 'b', 'c'),
        target = c('b', 'c', 'a'),
        correlationCoef = c(.8,.3,-.8),
        pValue = c(.01,.001,.1)
    )
    net <- CorrelationNetwork(object = edgeList, correlationCoefThreshold = .5)
    expect_equal(getNodeIds(net), c('a', 'b', 'c'))
    expect_equal(getDegrees(net), c(2, 2, 2))
    expect_equal(!is.null(getCoords(net)), TRUE)
    expect_equal(getLinkColorScheme(net), 'posneg')
    expect_equal(length(getLinks(net)), 2)
    expect_equal(getLinks(net)[[1]]@weight, .8)
    expect_equal(getLinks(net)[[2]]@weight, .8) #second link is actually third link bc of correlationCoefThreshold!!
})

test_that("empty edgeLists produce empty CorrelationNetworks rather than err", {
  edgeList <- data.table::data.table(source='a',target='b',correlationCoef=NA,pValue=NA)
  net <- CorrelationNetwork(object = edgeList)
  expect_s4_class(net, "CorrelationNetwork")
  expect_equal(length(net@links),0)
  expect_equal(length(net@nodes),0)

  edgeList <- edgeList[complete.cases(edgeList),]
  net <- CorrelationNetwork(object = edgeList)
  expect_s4_class(net, "CorrelationNetwork")
  expect_equal(length(net@links),0)
  expect_equal(length(net@nodes),0)

  edgeList <- data.table::data.table(source='a',target='b',correlationCoef=.1,pValue=.008)
  net <- CorrelationNetwork(object = edgeList, correlationCoefThreshold = .5)
  expect_s4_class(net, "CorrelationNetwork")
  expect_equal(length(net@links),0)
  expect_equal(length(net@nodes),0)

  edgeList <- data.table::data.table(source='a',target='b',correlationCoef=.9,pValue=.8)
  net <- CorrelationNetwork(object = edgeList, pValueThreshold = .05)
  expect_s4_class(net, "CorrelationNetwork")
  expect_equal(length(net@links),0)
  expect_equal(length(net@nodes),0)
})
