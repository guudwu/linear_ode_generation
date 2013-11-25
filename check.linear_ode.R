# Check property of generated ODE system.

# Currently only two properties are checked:
# Pair-wise correlation of curves.
# Variance Inflation Factor.

check.linear_ode <- function
(
  linear_ode
  , correlation = TRUE
  , vif = TRUE
)

# INPUT:
# linear_ode: Return value of "linear_ode_generation.R".
# correlation: Whether to check pair-wise correlation.
# vif: Whether to check Variance Inflation Factor.

# OUTPUT:
# correlation: Pair-wise correlation matrix.
# max_correlation: Maximal correlation, other than diagonal elements.
# vif: Variance Inflation Factor.

{

# Sanity check

correlation = as.logical(correlation)
if ( length(correlation)!=1 )
{
  stop('"correlation" must be a logical scalar.')
}

vif = as.logical(correlation)
if ( length(vif)!=1 )
{
  stop('"vif" must be a logical scalar.')
}

# Correlation

if ( correlation )
{
  correlation <- cor ( linear_ode$observation[,-1] )
  max_correlation <- correlation - diag(ncol(linear_ode$observation)-1)
  max_correlation <- max(abs(max_correlation))
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
  , max_correlation = max_correlation
  , vif = vif
) )

}
