get_samples_from_density(target_density, x_domain, n, ...){
  
  # get h(x) = log(g(x))
  h_of = function(x){ log(target_density(x, ...)) }
  
  # get initial x vector and modified approperiate domain
  x_vec_and_D =  get_initial_x_vec_and_D(target_density, x_domain, ...)
  x_vec = x_vec_and_D[[1]]
  D = x_vec_and_D[[2]]
  
  # loop over to get n samples
  samples = rep(NULL, n)
  curr_num = 0
  while (curr_num < n){
    z_vec_and_I_vec = function_2_name(target_density, x_domain, x_vec, ...)
    z_vec = z_vec_and_I_vec[[1]]
    I_vec = z_vec_and_I_vec[[2]]
    
    to_be_converted = runif(1,0,1)
    w = runif(1,0,1)
    
    j = function_3_name(to_be_converted, I_vec)
    
    ######################################################
    x_star = NULL
    ######################################################
    
    # define functions l_k_of_x and u_k_of_x
    x_j = x_vec[j]
    x_j_plus_one = x_vec[j+1]
    l_k_of = function(x){
      numerator = (x_j_plus_one-x)*h_of(x_j) + (x-x_j)*h_of(x_j_plus_one)
      denominator = x_j_plus_one - x_j
      return(numerator/denominator)
    }
    u_k_of = function(x){ h_of(x_j) + (x-x_j)*numderiv(h_of,x_j) }
    
    # acceptation criterion
    if (w <= exp(l_k_of(x_star) - u_k_of(x_star))){
      curr_num = curr_num + 1
      samples[curr_num] = x_star
    } else if (w <= exp(h_of(x_star) - u_k_of(x_star))){
      curr_num = curr_num + 1
      samples[curr_num] = x_star
    } else {
      x_vec = append(x_vec, x_star)
      x_vec = sort(x_vec)
    }
  }
  return(samples)
}