context('upset')

test_that("upset.dt() returns an appropriately sized data.table", {
  nPoints <- 10000
  df <- data.frame('x' = runif(nPoints), 'y' = runif(nPoints), 'z' = runif(nPoints),
                   'group' = sample(c('g1','g2','g3','g4'), nPoints, replace=T),
                   'panel' = sample(c('p1','p2','p3'), nPoints, replace=T))
  df <- as.data.table(df)
  
  # Add 200 missing values to each column
  df$x[sample(1:500, 200, replace=F)] <- NA
  df$y[sample(1:500, 200, replace=F)] <- NA
  df$group[sample(1:500, 200, replace=F)] <- NA
  df$panel[sample(1:500, 200, replace=F)] <- NA
  
  map <- data.frame('id' = c('group', 'x', 'panel'), 'plotRef' = c('aVariable','aVariable','aVariable'), stringsAsFactors=FALSE)
  # Note -- currently not doing anything with map
  
  dt <- setSets.dt(df, map, relation = 'intersection', queries = 'missingness')
  # expect_is(dt, 'plot.data')
  # expect_is(dt, 'barplot')
  expect_equal(nrow(dt), 31)
  expect_equal(colnames(dt), c('sets','cardinality'))
  
  
  dt <- setSets.dt(df, map, relation = 'distinctIntersection', queries = 'missingness')
  expect_equal(nrow(dt), 31)
  expect_equal(colnames(dt), c('sets','cardinality'))
  
  
  outJson <- jsonlite::toJSON(dt)
  write(outJson, 'setsets.json')

})


# Consider moving to utils?
# test_that("upset.dt() returns correct information about missing data", {
#   df <- data.xy
#   na_locs <- sample(1:500, 100)
#   df[na_locs, c('group','x')] <- NA
#   df[na_locs[1:50], c('panel')] <- NA
#   pSet <- rje::powerSet(colnames(df))
#   # Remove empty set
#   pSet <- pSet[lapply(pSet, length) > 0]
#   
#   # Compute intersections and check outputs
#   pSet_intersection <- lapply(pSet, getUpsetIntersections, df=df, type='intersection')
#   print(pSet_intersection[[9]]$cardinality == 100) # group n x
#   print(pSet_intersection[[2]]$cardinality == 50) # panel
#   print(pSet_intersection[[11]]$cardinality == 50) # group n panel n x
#   
#   pSet_distinctIntersection <- lapply(pSet, getUpsetIntersections, df=df, type='distinctIntersection')
#   print(pSet_distinctIntersection[[9]]$cardinality == 50) # group n x
#   print(pSet_distinctIntersection[[2]]$cardinality == 0) # panel
#   print(pSet_distinctIntersection[[11]]$cardinality == 50) # group n panel n x
#   
# })