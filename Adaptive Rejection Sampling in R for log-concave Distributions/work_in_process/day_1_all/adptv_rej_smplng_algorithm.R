# install.packages('pracma')
# install.packages('bazar')
library(pracma)
library(bazar)

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
  threshold_density = 1e-2 * pdf_at_x_mid
  if (x_domain[1] == -Inf){
    exponent = 0
    while (target_density(lower_bound, ...) > threshold_density){
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
  nnn = 5
  x1_to_x_mid = seq(from=lower_bound, to=x_mid, length.out=nnn)
  x_mid_to_x_2n = seq(from=x_mid, to=upper_bound, length.out=nnn)
  x_vec = c(x1_to_x_mid[1:nnn-1], x_mid+1e-3, x_mid_to_x_2n[2:nnn])
  D_vec = c(lower_bound, upper_bound)
  return_list = list(x_vec, D_vec, x_mid)
  
  # this is a list of:
  # return_list[[1]] = x_vec <-- this is of length 2001
  # return_list[[2]] = c(lower_bound, upper_bound)
  return(return_list)
}


function_2_name <- function(xk, d, target_density, ...) {
  
  # h function h(x)=log(g(x))
  h = function(x, ...){ log(target_density(x, ...)) }
  # derivative of h
  h_prime_of = function(x_vec, ...){ derivative = numdiff(h, x_vec, ...) }
  
  l = length(xk)
  z = numeric(l+1)
  z[1] = d[1]
  z[l+1] = d[2]
  
  
  # for-loop function for z
  #for (i in 1:(l-1)) {
  #  z[i+1]=  (h(xk[i+1])- h(xk[i])- xk[i+1]*numderiv(h,xk[i+1])+xk[i]*numderiv(h,xk[i]))/(numderiv(xk[i])-numderiv(xk[i+1]))
  #}
  
  # vectorized function for z
  z[2:l] = ( h(xk[2:l], ...) - h(xk[1:(l-1)] , ...) - xk[2:l] * 
               h_prime_of( xk[2:l] , ...) + xk[1:(l-1)] * h_prime_of( xk[1:(l-1)] , ...)) /
    (h_prime_of( xk[1:(l-1)], ...) - h_prime_of( xk[2:l], ...))
  
  s = 0
  I = numeric(l)
  
  for (j in 1:l) {
    integ = exp(h(xk[j], ...))/h_prime_of(xk[j], ...)*(exp((z[j+1]-xk[j])*h_prime_of(xk[j], ...))-exp((z[j]-xk[j])*h_prime_of(xk[j], ...)))
    s = s+integ
    I[j] = s
  }
  
  I = I/I[l]
  return(list(z,I,s))
}



function_3_name= function(con, I) {
  
  check = 1
  search = 1
  if (con < I(search)) {
    check = 0
  }
  
  while(check>0) {
    search = search+1
    check = (con-I(search))*(con-I(search-1)) 
    
    if (check==0) {
      search=search-1
    }
  }
  return(search)
}


get_samples_from_density = function(target_density, x_domain, n, max_iter=10*n, ...){
  
  # get h(x) = log(g(x)), and, h_prime(x)
  h_of = function(x, ...){ log(target_density(x, ...)) }
  h_prime_of = function(x_vec, ...){ derivative = numdiff(h_of, x_vec, ...) }
  
  # get initial x vector and modified appropriate domain
  x_vec_and_D_and_x_mid =  get_initial_x_vec_and_D(target_density, x_domain, ...)
  x_vec = x_vec_and_D_and_x_mid[[1]]
  D = x_vec_and_D_and_x_mid[[2]]
  x_mid = x_vec_and_D_and_x_mid[[3]]
  
  # loop over to get n samples
  samples = rep(NULL, n)
  curr_num = 0
  num_iter = 0
  while (curr_num < n && num_iter < max_iter){
    num_iter = num_iter + 1
    
    # print(paste('maximum iteration is: ',max_iter))
    # print(paste('current iteration is: ',num_iter))
    
    # get z-vector and I-vector
    z_vec_and_I_vec = function_2_name(x_vec, D, target_density, ...)
    
    z_vec = z_vec_and_I_vec[[1]]
    
    I_vec = z_vec_and_I_vec[[2]]
    
    I_sum = z_vec_and_I_vec[[3]]
    
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    # return(list(x_vec, z_vec, I_vec, I_sum))
    
    # print('      z_vec      ')
    # print(z_vec)
    # print('      I_vec      ')
    # print(I_vec)
    # print('      I_sum      ')
    # print(I_sum)
    
    to_be_converted = runif(1,0,1)
    w = runif(1,0,1)
    
    ######################################################
    j = function_3_name(to_be_converted, I_vec)
    
    # print(j)
    
    #TODO THIS j IS NOT CORRECT 
    #TODO counter factual test value  for x_star
    #TODO NOTE: 
    #TODO this j = 2 works indicating 
    #TODO j = function_3_name(to_be_converted, I_vec)
    #TODO might not be correct
    # j = 1 
    ######################################################
    
    # define functions l_k_of_x and u_k_of_x
    x_j = x_vec[j]
    x_j_plus_one = x_vec[j+1]
    
    ######################################################
    s1 = I_sum*(to_be_converted-I_vec[j])*h_prime_of(x_j, ...)/exp(h_of(x_j, ...)) + exp(h_prime_of(x_j, ...)*(z_vec[j]-x_vec[j]))
    x_star = log(s1)/h_prime_of(x_j, ...) + x_vec[j]
    
    # print('      s1      ')
    # print(s1)
    # print('      x_star      ')
    # print(x_star)
    
    #TODO THIS s1 IS NOT CORRECT 
    #TODO thus the generated x_star value is not correct
    #TODO counter factual test value for x_star 
    #TODO NOTE: 
    #TODO this x_star = sample(c(0.25, 0.5, 1, 2), size=1, replace = TRUE) 
    #TODO works indicating previous x_star or s1 function have bugs
    # x_star = sample(c(0.25, 0.5, 1, 2), size=1, replace = TRUE)
    # x_star = -3
    ######################################################
    
    l_k_of = function(x, x_j, x_j_plus_one, ...){
      numerator = (x_j_plus_one-x)*h_of(x_j, ...) + (x-x_j)*h_of(x_j_plus_one, ...)
      denominator = x_j_plus_one - x_j
      return(numerator/denominator)
    }
    u_k_of = function(x, x_j, ...){ h_of(x_j, ...) + (x-x_j)*h_prime_of(x_j, ...) }
    
    # print('w = ')
    # print(w)
    # print('exp(l_k_of(x_star) - u_k_of(x_star)) = ')
    # print(exp(l_k_of(x_star, x_j, x_j_plus_one, ...) - u_k_of(x_star, x_j, ...)))
    
    # acceptation criterion
    if (w <= exp(l_k_of(x_star, x_j, x_j_plus_one, ...) - u_k_of(x_star, x_j, ...))){
      curr_num = curr_num + 1
      samples[curr_num] = x_star
    } else if (w <= exp(h_of(x_star, ...) - u_k_of(x_star, x_j, ...))){
      curr_num = curr_num + 1
      samples[curr_num] = x_star
      if ((x_star - x_mid) > 1e-7){
        x_vec = sort(almost.unique(c(x_vec, x_star), tolerance=1e-7))
      }
    } else {
      if ((x_star - x_mid) > 1e-7){
        x_vec = sort(almost.unique(c(x_vec, x_star), tolerance=1e-7))
      }
    }
  }
  return(samples)
}
