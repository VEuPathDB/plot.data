context('upset')

test_that("upset.dt() returns a valid object", {
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
  
  map <- data.frame('id' = c('group', 'x', 'panel'), stringsAsFactors=FALSE)
  dt <- upset.dt(df, map, relation = 'intersection')

})