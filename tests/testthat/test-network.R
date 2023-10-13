test_that("network class is created okay", {

  nNodes <- 50
  nLinks <- 500

  nodeIDs <- stringi::stri_rand_strings(nNodes, 5, '[A-Z]')

  networkData <- data.table::as.data.table(list(
    source1 = sample(nodeIDs, nLinks, replace=T),
    target1 = sample(nodeIDs, nLinks, replace=T)
  ))

  network <- newNetwork(dt = networkData, sourceNodeColumn = 'source1', targetNodeColumn = 'target1', verbose = 'TRUE')

})