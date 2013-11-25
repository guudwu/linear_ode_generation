# Output data of generated ODE system as ".data" files.

data.linear_ode <- function (
  linear_ode
  , sparse = FALSE
)

# INPUT:
# linear_ode: Return value of "linear_ode_generation.R".
# sparse: Whether to output coefficient matrix in sparse format.

{

sink('config.data')

cat (
  ncol(linear_ode$observation) - 1
  , nrow(linear_ode$observation)
  , sep = ' '
)
cat('\n')

cat (
  linear_ode$observation[,1]
  , sep = ' '
)
cat ('\n')

cat ( sum(linear_ode$coefficient!=0) )

sink(NULL)

write.table (
  as.numeric(linear_ode$coefficient)
  , file='coefficient.data'
  , row.names=FALSE , col.names=FALSE ,
  , sep=' ' , eol=' '
  , quote=FALSE
)

write.table (
  as.numeric(linear_ode$observation[,-1])
  , file='observation.data'
  , row.names=FALSE , col.names=FALSE ,
  , sep=' ' , eol=' '
  , quote=FALSE
)

write.table (
  as.numeric(linear_ode$intercept)
  , file='intercept.data'
  , row.names=FALSE , col.names=FALSE ,
  , sep=' ' , eol=' '
  , quote=FALSE
)

if ( sparse )
{
  position <- which ( linear_ode$coefficient!=0 )
  write.table (
    as.integer(position)
    , file='p_coefficient.data'
    , row.names=FALSE , col.names=FALSE ,
    , sep=' ' , eol=' '
    , quote=FALSE
  )

  write.table (
    as.numeric(linear_ode$coefficient[position])
    , file='s_coefficient.data'
    , row.names=FALSE , col.names=FALSE ,
    , sep=' ' , eol=' '
    , quote=FALSE
  )
}

return()

}
