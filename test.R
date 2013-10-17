dimension <- 8
time_point <- 0:100

source('linear_ode_generation.R')

res <- linear_ode_generation ( dimension , time_point )

linODE <- function ( time , state , pars )
{
  res <- pars[[1]] %*% state
  if ( ! is.null ( pars[[2]] ) )
    res <- res + pars[[2]]
  return ( list ( res ) )
}

ode_res <-
  ode (
    res$observation[,1] , time_point , linODE ,
    list ( res$coefficient , NULL )
  )

rss <- norm ( t(ode_res[,-1]) - res$observation , 'F' )
