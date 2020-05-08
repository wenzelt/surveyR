# load R data objects globally
load.Rdata <- function( filename, objname )
{
  d1 <- load( filename )
  eval( parse( text=paste( objname,  "<<- ", d1  ) ) )
}

batch_shapiro <- function(df){
  dplyr::select_if(df, is.numeric)
  df <- try(select(df, -c(R233_01,R233_02,R233_03, S101_13)))
  df <- try(do.call(rbind, lapply(df, function(x) shapiro.test(x)[c("statistic", "p.value")])))
  return(df)
}
cleanSD <- function(column){
  upper = mean(column) + 2*sd(column)
  lower = mean(column) - 2*sd(column)
  filtered = subset(column, column < upper & column >lower)
  return(filtered)
}

