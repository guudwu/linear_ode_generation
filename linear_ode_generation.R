# Generate a linear ODE system

linear_ode_generation <- function
(
  dimension
  , time_point
  , orthogonal_transformation = list()
  , row_column_permutation = TRUE
  , intercept = list(0,0)
)

# INPUT:
# dimension: Dimension of the system.
#   Currently the system can contian complex eigen-values,
#   which comes in pairs,
#   and at most one real eigen-value,
#   since currently we have not found a good way to control
#   the correlation between curves by real eigen-values.
#   Hence if "dimension" is even, all eigen-values are complex,
#   otherwise there exists one real eigen-value in the spectrum.
# time_point: Time points for observations.
# orthogonal_transformation: A list which applies transformation
#   to coefficient matrix to adjust the sparsity and structure.
#   Each component of the list is a 2-tuple (M,N)
#   A random orthogonal matrix of dimension (N-M+1) is left multipled
#   to Mth-Nth row of the coefficient matrix,
#   and its transpose right multipled to Mth-Nth column of the
#   coefficient matrix.
# row_column_permutation: Make the sparsity structure less obvious
#   by permuting rows and columns of the coefficient matrix.
# intercept: A list of two vectors of length "dimension"
#   indicating lower and upper bounds of intercept term.
#   Each component can also be a scalar,
#   which will be automatically expanded to a vector.
#   The default value disables intercept term.

# OUTPUT:
# coefficient: Coefficient matrix of the system.
# intercept: Intercept term of the system.
# observation: Observation matrix.
#   Each row stands for a time point.
#   Each column is a curve.
#   The first column is time points.
# eigen_imaginary: Imaginary parts of complex eigen-values.
# eigen_real: Real parts of eigen-values.
#   If there exists a real eigen-value, it is the last.

{

# Sanity Check#{{{

dimension <- as.integer(dimension)
if ( length(dimension)!=1 || dimension<=0 )
{
  stop('Argument "dimension" must be a positive integer.')
}

time_point <- as.numeric(unique(sort(time_point)))
if ( length(time_point) < 2 )
{
  stop('"time_point" must be a vector ' ,
    'longer than 1 with no identical elements.')
}

row_column_permutation <- as.logical(row_column_permutation)

intercept <- as.list(intercept)
if ( length(intercept)!=2 )
{
  stop('"intercept" must be a list of two vectors or scalars.')
}
intercept[[1]] <- as.numeric(intercept[[1]])
if ( length(intercept[[1]])==1 )
{
  intercept[[1]] <- rep ( intercept[[1]] , dimension )
}
if ( length(intercept[[1]])!=dimension )
{
  stop('Each item of "intercept" must be a scalar or ' ,
    'a vector of length "dimension".')
}
intercept[[2]] <- as.numeric(intercept[[2]])
if ( length(intercept[[2]])==1 )
{
  intercept[[2]] <- rep ( intercept[[2]] , dimension )
}
if ( length(intercept[[2]])!=dimension )
{
  stop('Each item of "intercept" must be a scalar or ' ,
    'a vector of length "dimension".')
}
#}}}

# Generate eigenvalues#{{{

# Generate a block-diagonal matrix.
# Each block is a scalar "a",
# indicating a real eigen-value,
# or of form:
#     a b
#    -b a
# which indicates a pair of complex eigen-values: a+-bi
# The analytic solution is:
#     exp(at)
# for real eigen-value, and
#     exp(at)sin(bt)
#     exp(at)cos(bt)
# for a pair of complex eigen-values.

# "a"s are stored in "eigen_real",
# and "b"s in "eigen_imaginary".

# If there exists one real eigen-value,
# it is the last in "a".

# Generally "a" should be negative, or at least not too positive,
# to make the system stable.
# Each "a" is generated independently and uniform-randomly in range:
#     [real_min,real_max].
# The "real_min" value satisfies that the magnitude at the last
# time point is approximately half of that at the first.
# "real_max" is half of the lower-bound.

# Each "b" should be bounded away from each other
# to decrease correlation of the system.
# Currently they are chosen as an arithmetic sequence,
# plus a small random white noise.
# The smallest "b" is chosen to contain approximately
# one period in the time span when no real eigen-value exists,
# and double that value when there exists one real eigen-value.

num_real_eigen <- dimension%%2
num_complex_eigen <- floor(dimension/2)

time_span <- tail(time_point,1) - time_point[1]

real_min = -0.7/time_span
real_max = real_min/2

eigen_real <- runif (
  num_complex_eigen + num_real_eigen
  , real_min
  , real_max
)

eigen_imaginary_random_level <- 0.1
eigen_imaginary <- 1:num_complex_eigen
if ( num_real_eigen==1 )
{
  eigen_imaginary <- eigen_imaginary + 1
}
eigen_imaginary <- eigen_imaginary +
  rnorm ( num_complex_eigen , 0 , eigen_imaginary_random_level )
eigen_imaginary <- eigen_imaginary * 2 * pi / time_span

require('permute')
permute_index <- permute::shuffle(dimension/2)
eigen_imaginary <- eigen_imaginary[permute_index]
#}}}

# Coefficient Matrix and Observation#{{{

temp <- (1:num_complex_eigen) * 2

coefficient <- matrix ( 0 , dimension , dimension )

coefficient [ cbind(temp,temp) ] <- eigen_real[1:num_complex_eigen]
coefficient [ cbind(temp-1,temp-1) ] <- eigen_real[1:num_complex_eigen]
coefficient [ cbind(temp-1,temp) ] <- eigen_imaginary
coefficient [ cbind(temp,temp-1) ] <- -eigen_imaginary

observation <- matrix ( 0 , length(time_point) , dimension )

lapply ( 1 : num_complex_eigen ,
  function(index)
  {
    temp <- exp ( eigen_real[index] * time_point )
    observation[,2*index-1] <<-
      temp * sin ( eigen_imaginary[index] * time_point )
    observation[,2*index] <<-
      temp * cos ( eigen_imaginary[index] * time_point )
  }
)

if ( num_real_eigen == 1 )
{
  coefficient[dimension,dimension] <- tail(eigen_real,1)
  observation[,dimension] <- exp ( tail(eigen_real,1) * time_point )
}
#}}}

# Orthogonal Transformation#{{{

orthogonal_transformation <- as.list(orthogonal_transformation)
for ( item in orthogonal_transformation )
{
  item <- as.integer(item)
  if ( length(item)!=2 )
  {
    stop('Each item of "orthogonal_transformation" must be length 2.')
  }
  if ( item[1]>=item[2] )
  {
    stop('In each item of "orthogonal_transformation", ' ,
      'The second component must be larger than first.'
    )
  }
  if ( item[1]<1 || item[2]>dimension )
  {
    stop('Out-of-bound index in items of "orthogonal_transformation".')
  }

  require('pracma')
  qq <- diag(dimension)
  qq[item[1]:item[2],item[1]:item[2]] <-
    pracma::rortho(item[2]-item[1]+1)
  observation <- observation %*% t(qq)
  coefficient <- qq %*% coefficient %*% t(qq)
}
#}}}

# Row-Column Permutation#{{{

# Permute rows and columns
# by left multiplying a permutation matrix
# and right multiplying its transpose
# to the coefficient matrix.
# This will make the sparsity structure less obvious,
# however it does not change the property,
# which means the system is still unconnected,
# if it is unconnected before this permutation.

if ( row_column_permutation == TRUE )
{
  require('permute')
  permute_index <- permute::shuffle ( dimension )
  coefficient <- coefficient [ permute_index , permute_index ]
  observation <- observation [ , permute_index ]
}
#}}}

# Intercept#{{{

ret <- list (
  coefficient = coefficient
  , eigen_imaginary = eigen_imaginary
  , eigen_real = eigen_real
)

if ( !all(intercept[[1]]==0) || !all(intercept[[2]]==0) )
{
  intercept <- runif ( dimension , intercept[[1]] , intercept[[2]] )
  temp <- solve ( coefficient , intercept )
  lapply ( 1 : length(time_point) , function(index)
  {
    observation[index,] <<- observation[index,] - temp
    return()
  } )
  ret$intercept <- intercept
}

ret$observation = cbind ( time_point , observation )
#}}}

# Return#{{{

return(ret)

#}}}

}
