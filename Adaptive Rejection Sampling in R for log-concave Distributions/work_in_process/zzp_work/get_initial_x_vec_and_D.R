# This function gets the initial starting value of:
# the x vector
#     x = c(x1, x2, x3)
# the appropriate bound:
#     D = c(D_min, D_max)
get_initial_x_vec_and_D = function(target_density, x_domain, ...){
  
  # optimization starting point
  paramemter_init = 0
  multiples = 2
  if (x_domain[1] == -Inf && x_domain[2] == Inf){
    paramemter_init = 0
  }
  else if (x_domain[1] == -Inf){
    paramemter_init = x_domain[2] - multiples*abs(x_domain[2])
  }
  else if (x_domain[2] == Inf){
    paramemter_init = x_domain[1] + multiples*abs(x_domain[1])
  }
  else {
    paramemter_init = (x_domain[1] + x_domain[2]) / 2
  }
  
  # h(x) and -h(x)
  h_of_x = function(x){ log(target_density(x, ...)) }
  negative_h_of_x = function(x){ -h_of_x(x) }
  x_max_optim = optim(paramemter_init, negative_h_of_x, method='BFGS',
                      lower = -Inf, upper = Inf)
  
  # get the x where log(g(x)) is maximized
  x_mid = x_max_optim$par
  lower_bound = x_mid
  upper_bound = x_mid
  
  # get the lower bound
  pdf_at_x_mid = target_density(x_mid, ...)
  threshold_density = 1e-3 * pdf_at_x_mid
  if (x_domain[1] == -Inf){
    exponent = 0
    while (target_density(lower_bound, ...) > threshold_density){
      print('gg')
      exponent = exponent + 1
      lower_bound = x_mid - 10^exponent
    }
  } else{
    lower_bound = x_domain[1]
  }
  # get the upper bound
  if (x_domain[2] == Inf){
    exponent = 0
    while (target_density(upper_bound, ...) > threshold_density){
      exponent = exponent + 1
      upper_bound = x_mid + 10^exponent
    }
  } else{
    upper_bound = x_domain[2]
  }
  
  # return the x vector
  x1 = (lower_bound + x_mid) / 2
  x2 = x_mid
  x3 = (upper_bound + x_mid) / 2
  x_vec = c(x1, x2, x3)
  D_vec = c(lower_bound, upper_bound)
  return_list = list(x_vec, D_vec)
  return(return_list)
}