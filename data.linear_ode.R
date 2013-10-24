data.linear_ode <- function
(
  linear_ode
  , sparse = TRUE
)

{

sink('config.data')
cat ( nrow(linear_ode$observation)
  , ncol(linear_ode$observation)
  , sep = ' '
)
cat ( '\n' )
cat (
  linear_ode$time_point
  , sep = ' '
)
sink(NULL)

write.table ( as.numeric ( linear_ode$coefficient )
  , file='coefficient.data'
  , row.names=FALSE , col.names=FALSE ,
  , sep=' ' , eol=' '
  , quote=FALSE
)

write.table ( as.numeric ( linear_ode$observation )
  , file='observation.data'
  , row.names=FALSE , col.names=FALSE ,
  , sep=' ' , eol=' '
  , quote=FALSE
)

write.table ( as.numeric ( linear_ode$intercept )
  , file='intercept.data'
  , row.names=FALSE , col.names=FALSE ,
  , sep=' ' , eol=' '
  , quote=FALSE
)

if ( sparse )
{
  write.table ( as.integer ( which(linear_ode$coefficient!=0) )
    , file='p_coefficient.data'
    , row.names=FALSE , col.names=FALSE ,
    , sep=' ' , eol=' '
    , quote=FALSE
  )

  write.table (
    as.numeric (
      linear_ode$coefficient [ which(linear_ode$coefficient!=0) ]
    )
    , file='s_coefficient.data'
    , row.names=FALSE , col.names=FALSE ,
    , sep=' ' , eol=' '
    , quote=FALSE
  )
}

sink(NULL)

}
