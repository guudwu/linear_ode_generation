# Generate a linear ODE system
#
# Last modified on 2013-10-17 by GuuD WU.

linear_ode_generation <- function
(
  dimension
  , time_point
  , real_min = -0.7 / ( tail(time_point,1) - time_point[1] )
  , real_max = ifelse ( real_min<0 , real_min/2 , 2*real_min )
  , block_permute = TRUE
  , orthogonal_transformation = NULL
  , row_column_permutation = TRUE
)

{

# Sanity Check

if ( dimension <= 0 )
{
  stop('Argument "dimension" must be positive.')
}
if ( dimension%%2 != 0 )
{
  stop('Argument "dimension" must be even.')
}

time_point <- unique ( sort ( time_point ) )
if ( length(time_point) < 2 )
{
  stop('Length of argument "time_point" must be at least 2.')
}

if ( real_min > real_max )
{
  warning('Argument "real_min" larger than "real_max".  Automatically exchanged.')
  temp <- real_max
  real_max <- real_min
  real_min <- temp
}

if ( ! is.null(orthogonal_transformation) )
{
  orthogonal_transformation <-
    unique ( sort ( orthogonal_transformation ) )
  if ( min(orthogonal_transformation) < 1
    | max(orthogonal_transformation) > dimension )
  {
    stop('Out-of-bound index in "orthogonal_transformation".')
  }
  if ( ! all ( orthogonal_transformation%%2 == 1 ) )
  {
    stop('Indices in "orthogonal_transformation" must be odd.')
  }
}

# Eigenvalues

# Generate a 2x2 block-diagonal matrix.
#   Each block is of form:
#     a b
#    -b a
#   which yields an analytic solution:
#     exp(at)sin(bt)
#     exp(at)cos(bt)
#
# "a"s are stored in "eigen_real",
#   and "b"s in "eigen_imaginary".
#
# Generally "a" should be negative to make system stable.
# Each "a" is generated uniform-randomly in range:
#   [real_min,real_max].
# Default lower-bound makes the magnitude of each curve
#   diminishes a half,
#   and the upper-bound is half of the lower-bound.
#
# Each "b" should be bounded away from each other
#   to decrease correlation of the system.
# Currently they are chosen as an arithmetic sequence,
#   plus a small random noise.
# The smallest "b" is chosen to contain one period in the time span.

eigen_real <- runif ( dimension/2 , real_min , real_max )

time_diff <- tail(time_point,1) - time_point[1]
eigen_imaginary <-
  (
    seq ( dimension/2 )
    + rnorm ( (dimension/2) , 0 , 0.1 )
  ) * 2 * pi / time_diff

# Block Permutation

# Permute "b" of blocks to break the ascending order.

if ( block_permute == TRUE )
{
  require('permute')
  permute_index <- shuffle(dimension/2)
  eigen_imaginary <- eigen_imaginary[permute_index]
}

# Coefficient Matrix and Observation

ind <- seq.int ( 1 , dimension-1 , 2 )

coefficient <- matrix ( 0 , dimension , dimension )

coefficient [ cbind(ind,ind) ] <- eigen_real
coefficient [ cbind(ind+1,ind+1) ] <- eigen_real
coefficient [ cbind(ind,ind+1) ] <- eigen_imaginary
coefficient [ cbind(ind+1,ind) ] <- -eigen_imaginary

observation <- matrix ( 0 , dimension , length(time_point) )

lapply ( 1:(dimension/2) ,
  function(ind)
  {
    observation [ (2*ind-1):(2*ind) , ] <<-
      rbind (
        exp ( eigen_real[ind] * time_point ) *
          sin ( eigen_imaginary[ind] * time_point ) ,
        exp ( eigen_real[ind] * time_point ) *
          cos ( eigen_imaginary[ind] * time_point )
      )
  }
)

# Orthogonal Transformation

# Mix adjacent blocks
#   by left multiplying a block diagonal orthogonal matrix
#   and right multiplying its transpose to the coefficient matrix.
# "orthogonal_transformation" indicates the starting indices of
#   blocks in the orthogonal transformation matrix.

if ( ! is.null(orthogonal_transformation) )
{
  orthogonal_transformation <-
    c ( orthogonal_transformation , dimension+1 )
  ortho_tran <- matrix ( 0 , dimension , dimension )
  lapply ( 1 : (length(orthogonal_transformation)-1) ,
    function(ind)
    {
      ind1 <- orthogonal_transformation[ind]
      ind2 <- orthogonal_transformation[ind+1] - 1
      require('pracma')
      ortho_tran [ ind1:ind2 , ind1:ind2 ] <<-
        rortho ( ind2-ind1+1 )
    }
  )
  observation <- ortho_tran %*% observation
  coefficient <- ortho_tran %*% coefficient %*% t(ortho_tran)
}

# Row-Column Permutation

# Permute rows and columns
#   by left multiplying a permutation matrix
#   and right multiplying its transpose
#   to the coefficient matrix.
# This will make the sparsity structure less obvious,
#   however it does not change the property,
#   which means the system is still unconnected.

if ( row_column_permutation == TRUE )
{
  require('permute')
  permute_index <- shuffle ( dimension )
  coefficient <- coefficient [ permute_index , permute_index ]
  observation <- observation [ permute_index , ]
}

# Return

return (
  list (
    coefficient = coefficient
    , observation = observation
  )
)

}
