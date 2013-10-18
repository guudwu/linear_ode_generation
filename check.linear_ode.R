check.linear_ode <- function
(
  linear_ode
  , correlation = TRUE
  , vif = TRUE
)

{

# Correlation

if ( correlation )
{
  correlation <- cor ( t(linear_ode$observation) )
  correlation <- max ( abs ( correlation - diag(dimension) ) )
}

# Variation Inflation Factor

if ( vif )
{
  require('HH')
  observation_list <-
    lapply ( 1 : nrow(linear_ode$observation) ,
      function(ind)
      {
        return ( linear_ode$observation[ind,] )
      }
    )
  vif <- vif ( data.frame ( observation_list ) )
  vif <- as.vector(vif)
}

# Return

return (
  list (
    correlation = correlation
    , vif = vif
  )
)

}
