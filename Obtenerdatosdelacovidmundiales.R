# install.packages("openblender")

#Load OpenBlender package
library(openblender)


action <- "API_getObservationsFromDataset"

# ANCHOR: 'COVID-19 combined data'


parameters <- list( 
  token="603ae1cb951629350ce37471sYw2Ex4LzDMm0ZfrHBbi18HE4zuwUG",
  id_user="603ae1cb951629350ce37471",
  id_dataset="5e7a0d5d9516296cb86c6263" 
)


df <- openblender::call(action, parameters)$sample
head(df)