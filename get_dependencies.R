# Set up app environment
packs <- c(
  'doParallel',
  'forecast',
  'MASS',
  'plotly',
  'pscl',
  'randomForest',
  'RSocrata',
  'tidyverse'
)

for(i in packs){
  if(length(grep(i, (.packages(all.available=T))))==0) install.packages(i, dependencies =TRUE)
  
  cat(i, rep("\n", 4))
}


