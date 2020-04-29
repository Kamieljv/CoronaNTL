# Project: Corona NTL
# Version: 29-04-2020
# Author: Kamiel Verhelst

# Function to plot some pixel time series in a single plot
# * df: data frame with pixel time series as columns

plotPixelSeries <- function (df) {
  
  p<-ggplot(data=df,aes(df[,1]))
  
  for(i in names(df)[-1]){
    p<-p+geom_line(aes_string(y=i))
  }
  
  return(p)
}