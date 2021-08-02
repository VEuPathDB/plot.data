## Upset plot calculations dev

# install.packages('rje')
library(rje)
load_all()

df <- data.xy

# Add 200 missing values to each column
df$x[sample(1:500, 200, replace=F)] <- NA
df$y[sample(1:500, 200, replace=F)] <- NA
df$group[sample(1:500, 200, replace=F)] <- NA
df$panel[sample(1:500, 200, replace=F)] <- NA


# Get combinations in the upset format
# Dumb implementation - worth improving later for speed
# Also worth validating against upsetR

## Assume we implement missingness intersections, not completeness
getUpsetIntersections <- function(set, df, type) {
  
  if (type == 'intersection') {
    
    subsetDf <- df[, ..set]
    
  } else if (type=='distinctIntersection') {
    
    # Remove all rows which are missing data in any column not in the set
    notset <- setdiff(colnames(df), set)
    if (length(notset) > 0){
      # subsetDf <- df[rowSums(!is.na(df[, ..notset])) == length(notset), ..set]
      subsetDf <- df[complete.cases(df[, ..notset]), ..set]
    } else {
      subsetDf <- df
    }
    
  }
  
  # Now want number of all missing
  nMissing <- nrow(subsetDf[rowSums(is.na(subsetDf[, ..set])) == length(set), ])
  
  return (list("name" = paste(set,collapse=" n ")
               , "cardinality" = nMissing
               , "degree" = length(set)
               , "elems" = c()
               , "sets" = 'Set'
               , "type" = type))
} # End function


# Calculate power set of vars
pSet <- rje::powerSet(colnames(df))

# Remove empty set
pSet <- pSet[lapply(pSet, length) > 0]

# Find intersections
pSet_intersection <- lapply(pSet, getUpsetIntersections, df=df, type='intersection')
pSet_distinctIntersection <- lapply(pSet, getUpsetIntersections, df=df, type='distinctIntersection')


## Testing
df <- data.xy
na_locs <- sample(1:500, 100)
df[na_locs, c('group','x')] <- NA
df[na_locs[1:50], c('panel')] <- NA
pSet <- rje::powerSet(colnames(df))
# Remove empty set
pSet <- pSet[lapply(pSet, length) > 0]

# Compute intersections and check outputs
pSet_intersection <- lapply(pSet, getUpsetIntersections, df=df, type='intersection')
print(pSet_intersection[[9]]$cardinality == 100) # group n x
print(pSet_intersection[[2]]$cardinality == 50) # panel
print(pSet_intersection[[11]]$cardinality == 50) # group n panel n x

pSet_distinctIntersection <- lapply(pSet, getUpsetIntersections, df=df, type='distinctIntersection')
print(pSet_distinctIntersection[[9]]$cardinality == 50) # group n x
print(pSet_distinctIntersection[[2]]$cardinality == 0) # panel
print(pSet_distinctIntersection[[11]]$cardinality == 50) # group n panel n x
