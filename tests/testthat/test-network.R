test_that("Network objects have the correct attributes", {

  nNodes <- 50
  nLinks <- 500

  nodeIDs <- unique(stringi::stri_rand_strings(nNodes, 5, '[A-Z]'))
  nNodes <- length(nodeIDs)

  networkData <- data.frame(
    source1 = sample(nodeIDs, nLinks, replace=T),
    target1 = sample(nodeIDs, nLinks, replace=T)
  )

  ## The simplest case - a binary network with no colors or isolated nodes
  network <- newNetwork(df = networkData, sourceNodeColumn = 'source1', targetNodeColumn = 'target1', verbose = 'TRUE')
  attributes <- attributes(network)
  expect_equal(attributes$class, c('network', 'data.table', 'data.frame'))
  expect_equal(attributes$nodes, sort(unique(nodeIDs)))
  expect_equal(attributes$linkColorScheme, 'none')

  ## Network with edge weights and colors
  networkData$edgeData <- rnorm(nLinks)
  network <- newNetwork(df = networkData, sourceNodeColumn = 'source1', targetNodeColumn = 'target1', linkWeightColumn = 'edgeData', linkColorScheme = 'posneg', verbose = 'TRUE')
  attributes <- attributes(network)
  expect_equal(attributes$class, c('network', 'data.table', 'data.frame'))
  expect_equal(attributes$nodes, sort(unique(nodeIDs)))
  expect_equal(attributes$linkColorScheme, 'posneg')

})

test_that("Networks objects contain the correct link data", {

  nNodes <- 50
  nLinks <- 500

  nodeIDs <- stringi::stri_rand_strings(nNodes, 5, '[A-Z]')

  networkData <- data.frame(
    source1 = sample(nodeIDs, nLinks, replace=T),
    target1 = sample(nodeIDs, nLinks, replace=T)
  )

  ## The simplest case - a binary network with no colors or isolated nodes
  network <- newNetwork(df = networkData, sourceNodeColumn = 'source1', targetNodeColumn = 'target1', verbose = 'TRUE')
  expect_equal(names(network), c('source', 'target'))
  expect_equal(nrow(network), nLinks)
  expect_equal(unname(unlist(lapply(network, class))), c('character', 'character'))

  ## Network with weighted, colored links
  networkData$edgeData <- rnorm(nLinks)
  network <- newNetwork(df = networkData, sourceNodeColumn = 'source1', targetNodeColumn = 'target1', linkWeightColumn = 'edgeData', linkColorScheme = 'posneg', verbose = 'TRUE')
  expect_equal(names(network), c('source', 'target', 'weight', 'color'))
  expect_equal(nrow(network), nLinks)
  expect_equal(unname(unlist(lapply(network, class))), c('character', 'character', 'numeric', 'numeric'))
  expect_true(all(unique(network$color) %in% c(-1, 0, 1)))


})
