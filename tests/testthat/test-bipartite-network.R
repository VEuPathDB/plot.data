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
  bpnet <- bipartiteNetwork(df = networkData, sourceNodeColumn = 'source1', targetNodeColumn = 'target1', verbose = 'TRUE')
  attributes <- attributes(bpnet)
  expect_equal(attributes$class, c('bipartite', 'network', 'data.table', 'data.frame'))
  expect_equal(attributes$nodes, sort(unique(c(column1NodeIDs, column2NodeIDs))))
  expect_equal(attributes$linkColorScheme, 'none')
  expect_equal(attributes$column1NodeIDs, sort(unique(column1NodeIDs)))
  expect_equal(attributes$column2NodeIDs, sort(unique(column2NodeIDs)))

  ## Network with edge weights and colors
  networkData$edgeData <- rnorm(nLinks)
  bpnet <- bipartiteNetwork(df = networkData, sourceNodeColumn = 'source1', targetNodeColumn = 'target1', linkWeightColumn = 'edgeData', linkColorScheme = 'posneg', verbose = 'TRUE')
  attributes <- attributes(bpnet)
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
  bpnet <- bipartiteNetwork(df = networkData, sourceNodeColumn = 'source1', targetNodeColumn = 'target1', verbose = 'TRUE')
  expect_equal(names(bpnet), c('source', 'target'))
  expect_equal(nrow(bpnet), nLinks)
  expect_equal(unname(unlist(lapply(bpnet, class))), c('character', 'character'))

  ## Network with weighted, colored links
  networkData$edgeData <- rnorm(nLinks)
  bpnet <- bipartiteNetwork(df = networkData, sourceNodeColumn = 'source1', targetNodeColumn = 'target1', linkWeightColumn = 'edgeData', linkColorScheme = 'posneg', verbose = 'TRUE')
  expect_equal(names(bpnet), c('source', 'target', 'weight', 'color'))
  expect_equal(nrow(bpnet), nLinks)
  expect_equal(unname(unlist(lapply(bpnet, class))), c('character', 'character', 'numeric', 'numeric'))
  expect_true(all(unique(bpnet$color) %in% c(-1, 0, 1)))

})

test_that("Writing a bipartite network to json works as expected", {

  nNodesColumn1 <- length(letters)
  nNodesColumn2 <- length(LETTERS)

  column1NodeIDs <- letters
  column2NodeIDs <- LETTERS

  # Make terribly boring network
  networkData <- data.frame(list(
    source1 = column1NodeIDs,
    target1 = column2NodeIDs
  ))
  nLinks <- nrow(networkData)
  
  ## The simple case with no edge weights
  bpnet <- bipartiteNetwork(df = networkData, sourceNodeColumn = 'source1', targetNodeColumn = 'target1', verbose = 'TRUE')

  outJSON <- getNetworkJSON(bpnet, verbose=FALSE)
  jsonList <- jsonlite::fromJSON(outJSON)
  expect_equal(names(jsonList), c('nodes', 'links', 'column1NodeIDs', 'column2NodeIDs'))
  expect_equal(jsonList$nodes$id, sort(unique(c(column1NodeIDs, column2NodeIDs))))
  expect_equal(names(jsonList$links), c('source', 'target'))
  expect_equal(names(jsonList$links$source), c('id'))
  expect_equal(names(jsonList$links$target), c('id'))
  expect_equal(nrow(jsonList$links), nLinks)
  expect_equal(unname(unlist(lapply(jsonList$links, class))), c('data.frame', 'data.frame'))
  expect_equal(jsonList$column1NodeIDs, sort(column1NodeIDs))
  expect_equal(jsonList$column2NodeIDs, sort(column2NodeIDs))
  

  ## With link weights and colors
  networkData$edgeData <- rnorm(nLinks)
  bpnet <- bipartiteNetwork(df = networkData, sourceNodeColumn = 'source1', targetNodeColumn = 'target1', linkWeightColumn = 'edgeData', linkColorScheme = 'posneg', verbose = 'TRUE')

  outJSON <- getNetworkJSON(bpnet, verbose=FALSE)
  jsonList <- jsonlite::fromJSON(outJSON)
  expect_equal(names(jsonList), c('nodes', 'links', 'column1NodeIDs', 'column2NodeIDs'))
  expect_equal(jsonList$nodes$id, sort(unique(c(column1NodeIDs, column2NodeIDs))))
  expect_equal(names(jsonList$links), c('source', 'target', 'weight', 'color'))
  expect_equal(names(jsonList$links$source), c('id'))
  expect_equal(names(jsonList$links$target), c('id'))
  expect_equal(nrow(jsonList$links), nLinks)
  expect_equal(unname(unlist(lapply(jsonList$links, class))), c('data.frame', 'data.frame', 'character', 'character'))
  expect_equal(jsonList$column1NodeIDs, sort(column1NodeIDs))
  expect_equal(jsonList$column2NodeIDs, sort(column2NodeIDs))
})
