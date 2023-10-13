test_that("Bipartite network objects have the correct attributes", {

  nNodesColumn1 <- 30
  nNodesColumn2 <- 50
  nLinks <- 500

  column1NodeIDs <- stringi::stri_rand_strings(nNodesColumn1, 5, '[A-Z]')
  column2NodeIDs <- stringi::stri_rand_strings(nNodesColumn2, 5, '[A-Z]')

  networkData <- data.frame(list(
    source1 = sample(column1NodeIDs, nLinks, replace=T),
    target1 = sample(column2NodeIDs, nLinks, replace=T)
  ))

  ## The simplest case - a binary network with no colors or isolated nodes
  network <- bipartiteNetwork(df = networkData, sourceNodeColumn = 'source1', targetNodeColumn = 'target1', verbose = 'TRUE')
  attributes <- attributes(network)
  expect_equal(attributes$class, c('bipartite', 'network', 'data.table', 'data.frame'))
  expect_equal(attributes$nodes, sort(unique(c(column1NodeIDs, column2NodeIDs))))
  expect_equal(attributes$linkColorScheme, 'none')
  expect_equal(attributes$column1NodeIDs, sort(unique(column1NodeIDs)))
  expect_equal(attributes$column2NodeIDs, sort(unique(column2NodeIDs)))

  ## Network with edge weights and colors
  networkData$edgeData <- rnorm(nLinks)
  network <- bipartiteNetwork(df = networkData, sourceNodeColumn = 'source1', targetNodeColumn = 'target1', linkWeightColumn = 'edgeData', linkColorScheme = 'posneg', verbose = 'TRUE')
  attributes <- attributes(network)
  expect_equal(attributes$class, c('bipartite', 'network', 'data.table', 'data.frame'))
  expect_equal(attributes$nodes, sort(unique(c(column1NodeIDs, column2NodeIDs))))
  expect_equal(attributes$linkColorScheme, 'posneg')
  expect_equal(attributes$column1NodeIDs, sort(unique(column1NodeIDs)))
  expect_equal(attributes$column2NodeIDs, sort(unique(column2NodeIDs)))

})

test_that("Bipartite network objects contain the correct link data", {

  nNodesColumn1 <- 30
  nNodesColumn2 <- 50
  nLinks <- 500

  column1NodeIDs <- stringi::stri_rand_strings(nNodesColumn1, 5, '[A-Z]')
  column2NodeIDs <- stringi::stri_rand_strings(nNodesColumn2, 5, '[A-Z]')

  networkData <- data.frame(list(
    source1 = sample(column1NodeIDs, nLinks, replace=T),
    target1 = sample(column2NodeIDs, nLinks, replace=T)
  ))

  ## The simplest case - a binary network with no colors or isolated nodes
  network <- bipartiteNetwork(df = networkData, sourceNodeColumn = 'source1', targetNodeColumn = 'target1', verbose = 'TRUE')
  expect_equal(names(network), c('source', 'target'))
  expect_equal(nrow(network), nLinks)
  expect_equal(unname(unlist(lapply(network, class))), c('character', 'character'))

  ## Network with weighted, colored links
  networkData$edgeData <- rnorm(nLinks)
  network <- bipartiteNetwork(df = networkData, sourceNodeColumn = 'source1', targetNodeColumn = 'target1', linkWeightColumn = 'edgeData', linkColorScheme = 'posneg', verbose = 'TRUE')
  expect_equal(names(network), c('source', 'target', 'linkWeight', 'linkColor'))
  expect_equal(nrow(network), nLinks)
  expect_equal(unname(unlist(lapply(network, class))), c('character', 'character', 'numeric', 'numeric'))
  expect_true(all(unique(network$linkColor) %in% c(-1, 0, 1)))


})
