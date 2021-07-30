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
# Dumb implementation - worth improving later
# Also worth validating against upsetR
getUpsetIntersections <- function(set, df, type) {
  
  if (type == 'intersection') {
    
    subsetDf <- df[, ..set]
    
  } else if (type=='distinctIntersection') {
    
    # Remove all rows which have values in columns not in the set
    notset <- setdiff(colnames(df), set)
    if (length(notset) > 0){
      subsetDf <- df[rowSums(is.na(df[, ..notset])) == length(notset), ..set]
    } else {
      subsetDf <- df
    }
    
  }
  
  # Now want complete cases of df
  ncomplete <- sum(complete.cases(subsetDf))
  return (list("name" = paste(set,collapse=" n ")
               , "cardinality" = ncomplete
               , "degree" = length(set)
               , "elems" = c()
               , "sets" = 'Set'
               , "type" = type))
} # End function



pSet <- rje::powerSet(colnames(df))

# Remove empty set
pSet <- pSet[lapply(pSet, length) > 0]

pSet_intersection <- lapply(pSet, getUpsetIntersections, df=df, type='intersection')
pSet_distinctIntersection <- lapply(pSet, getUpsetIntersections, df=df, type='distinctIntersection')

