set.seed(0)
dimension <- 9
time_point <- 0:100
orthogonal_transformation <- list(c(2,3),c(4,5),c(6,9))
intercept <- list(0,1e-2)

source('linear_ode_generation.R')

linear_ode <-
  linear_ode_generation (
    dimension
    , time_point
    , orthogonal_transformation = orthogonal_transformation
    , row_column_permutation = FALSE
    , intercept = intercept
  )

linODE <- function ( time , state , pars )
{
  res <- pars[[1]] %*% state
  if ( ! is.null ( pars[[2]] ) )
    res <- res + pars[[2]]
  return ( list(res) )
}

require('deSolve')
ode_res <- ode (
  linear_ode$observation[1,-1]
  , linear_ode$observation[,1]
  , linODE
  , list ( linear_ode$coefficient , linear_ode$intercept )
)

rss <- norm ( ode_res[,-1] - linear_ode$observation[,-1] , 'F' )

source('check.linear_ode.R')
ode_property <- check.linear_ode(linear_ode)

source('data.linear_ode.R')
data.linear_ode (linear_ode)

source('plot.linear_ode.R')
plot.linear_ode(linear_ode)
