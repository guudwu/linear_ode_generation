# Plot curves of the generated ODE system

plot.linear_ode <- function (
  linear_ode
  , num_col = 4
)

# INPUT:
# linear_ode: Return value of "linear_ode_generation.R".
# num_col: Number of plots in each row.

{

observation <- linear_ode$observation[,-1]
time_point <- linear_ode$observation[,1]

upper <- max(observation)
lower <- min(observation)

par (
  mfrow = c ( ceiling(ncol(observation)/num_col) , num_col )
)

lapply ( 1 : ncol(observation) , function(index)
{
  plot (
    time_point
    , observation[,index]
    , ylim = c(lower,upper)
  )
} )

return()

}
