dimension <- 8
time_point <- 0:100
orthogonal_transformation <- c(1,5)
intercept <- c(1,0)

source('linear_ode_generation.R')

linear_ode <-
  linear_ode_generation (
    dimension
    , time_point
    , orthogonal_transformation = orthogonal_transformation
    , intercept = intercept
  )

linODE <- function ( time , state , pars )
{
  res <- pars[[1]] %*% state
  if ( ! is.null ( pars[[2]] ) )
    res <- res + pars[[2]]
  return ( list ( res ) )
}

ode_res <-
  ode (
    linear_ode$observation[,1] , time_point , linODE ,
    list ( linear_ode$coefficient , linear_ode$intercept )
  )

rss <- norm ( t(ode_res[,-1]) - linear_ode$observation , 'F' )

source('check.linear_ode.R')
ode_property <- check.linear_ode ( linear_ode )
