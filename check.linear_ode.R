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
  correlation <- cor ( linear_ode$observation[,-1] )
  correlation <- correlation - diag(ncol(linear_ode$observation)-1)
  correlation <- max(abs(correlation))
}

# Variation Inflation Factor

if ( vif )
{
  require('HH')
  observation_list <- lapply ( 1 : (ncol(linear_ode$observation)-1) ,
    function(index)
    {
      return ( linear_ode$observation[,index+1] )
    }
  )
  vif <- HH::vif ( data.frame ( observation_list ) )
  vif <- as.vector(vif)
}

# Return

return ( list (
  correlation = correlation
  , vif = vif
) )

}
