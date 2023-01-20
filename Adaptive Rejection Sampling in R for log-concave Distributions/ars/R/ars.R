#' Adaptive Rejection Sampling
#'
#' \code{ars} returns a sample of length n from the density function g
#'
#' Adaptive Rejection Sampling algorithm for sampling from any univariate log-concave density function g(x).
#' This algorithm is particularly useful in situations where computing g(x) may be difficult, where n
#' is the total number of samples.  Here, g(x) need not be normalized.
#'
#' @author Zhengpu Zhao, William Hymer, Dheeraj Prasanth
#' @usage ars(target_density, n, x_domain= c(-Inf,Inf), num_iter_allowed=10*n, ...)
#'
#' @param target_density density function from which to sample (must be log-concave)
#' @param x_domain (Optional) domain of the density function from which to sample, default is the whole real line
#' @param n number of samples
#' @param num_iter_allowed (Optional) maximum number of iterations for the algorithm, default is 10*n
#' @param ... additional arguments to pass to g such as mean and standard deviation
#'
#' @return returns (length n) vector of samples from the density function g(x)
#'
#' @references Section 2.2 of Gilks et al. (1992)
#'
#' @examples
#'
#' 'Standard normal distribution'
#' samps = ars(dnorm, 10000, mean=0, sd=1)
#'
#' 'Truncated normal distribution'
#' samps = ars(dnorm, 10000, c(-5, 5), mean=-3, sd=3)
#'
#' 'Beta distribution'
#' samps = ars(dbeta, n=3000, x_domain=c(0, 1), shape1=6, shape2=19)
#'
#' 'Logistic distribution'
#' samps = ars(dlogis, n=3000, x_domain=c(-Inf, Inf), location = 5, scale =2)
#'
#' # Can also use hist to visualize distribution, e.g.
#' # hist(samps, breaks = 100, freq=FALSE)
#'
#' @export

ars = function(target_density, n, x_domain=c(-Inf,Inf), num_iter_allowed=10*n, ...){

  ##################################################
  #           Adaptive Rejection Sampling
  # This Function will briefly check the correctness
  # of the input such as the log-concavity of the
  # input density. If input is valid, samples will
  # be generated.
  #
  # parameters:
  # {target_density}
  # log-concave function from which to sample.
  # {n}
  # number of samples.
  # {x_domain}
  # domain of the function from which to sample.
  # num_iter_allowed:
  # maximum number of iterations for the algorithm.
  # {...}
  # extra arguments for the density function.
  #
  # output:
  # a vector of n samples from the density function.
  ##################################################

  # input check
  if (!is.function(target_density)) { stop('Target Density is Not Function Object.') }
  if (!is.numeric(x_domain)) { stop('Domain Input is Not Numeric.') }
  if (!is.numeric(n)) { stop('n (number of sample) is Not Numeric.') }
  if (!is.numeric(num_iter_allowed)) {
    stop('maximum number of sampling iteration allowed is Not Numeric.') }
  # domain input wrong
  if (length(x_domain) != 2) { stop('Domain Input should have 2 Arguments.') }
  # domain too tiny
  if (abs(x_domain[1] - x_domain[2]) < 1e-8) { stop('Domain Error, Specified Domain too Small.') }
  # correct domain input if it's in reversed order
  if (x_domain[1] > x_domain[2]) { x_domain = c(x_domain[2],x_domain[1]) }

  # get h_of(x) = log(g(x)), and, h_prime(x)
  h_of = function(x_vec){ log(target_density(x_vec, ...)) }
  h_prime_of = function(x_vec){
    dx = 1e-8
    derivative = (h_of(x_vec+dx) - h_of(x_vec)) / dx
  }
  samps = get_samples_from_density(target_density, h_of, h_prime_of,
                                   x_domain, n, num_iter_allowed=num_iter_allowed, ...)
  if ( length(samps) < n ){
    warning('Sampling Inefficientcy Encountered.
            To Get Input Sized Samples, Please Increase { num_iter_allowed }.')
  }
  return(samps)
}

if_all_unique_append = function(x_vec, x){

  ##################################################
  #     append number to vector if that number is
  # at least 1e-8 from any other numbers in the
  # vector; else nothing happens.
  ##################################################

  EPSILON = 1e-8
  distance_vec = abs(x_vec - x)
  if (all(distance_vec >= EPSILON)){
    x_vec = c(x_vec, x)
    x_vec = sort(x_vec)
    return(x_vec)
  } else {
    return(x_vec)
  }
}


get_initial_x_vec_and_D = function(target_density, h_of, h_prime_of,
                                   x_domain, ...){

  ##################################################
  #     Initial x vector and finite bounds
  # Takes in the density function with (potentially)
  # infinite domain, log of and derivative of log of
  # the density function.
  #
  # parameters:
  # {target_density}
  # log-concave function from which to sample.
  # {h_of}
  # log of the target density function.
  # {h_prime_of}
  # derivative of the h_of function.
  #
  # output:
  # a list with two components
  #   1. initial x vector
  #   2. vector of finite lower & upper bounds
  ##################################################

  # optimization starting point
  parameter_init = 0
  multiples = 2
  if (x_domain[1] == -Inf && x_domain[2] == Inf){
    parameter_init = 0
  }
  else if (x_domain[1] == -Inf){
    parameter_init = x_domain[2] - multiples*abs(x_domain[2])
  }
  else if (x_domain[2] == Inf){
    parameter_init = x_domain[1] + multiples*abs(x_domain[1])
  }
  else {
    parameter_init = (x_domain[1] + x_domain[2]) / 2
    if (target_density(parameter_init, ...) < 1e-8){
      parameter_init = 0
    }
  }

  # -g(x) to be optimized to get where h'(x) == 0
  negative_g_of_x = function(x){ -target_density(x, ...) }
  x_max_optim = stats::optim(parameter_init, negative_g_of_x, method='BFGS')

  # get the x where log(g(x)) is maximized
  x_mid = x_max_optim$par
  x_mid = min(x_mid, x_domain[2])
  x_mid = max(x_mid, x_domain[1])
  lower_bound = x_mid
  upper_bound = x_mid

  # get the lower bound
  pdf_at_x_mid = target_density(x_mid, ...)
  threshold_density = 0.0001 * pdf_at_x_mid
  exponent = 0
  while (target_density(lower_bound, ...) > threshold_density){
    exponent = exponent + 1
    lower_bound = x_mid - 2^exponent
  }
  exponent = 0
  while (target_density(upper_bound, ...) > threshold_density){
    exponent = exponent + 1
    upper_bound = x_mid + 2^exponent
  }

  # check with user input
  lower_bound = max(x_domain[1], lower_bound)
  upper_bound = min(x_domain[2], upper_bound)

  # return the x vector
  x_vec_part_1 = seq(lower_bound, x_mid, length.out = 4)
  x_vec_part_2 = seq(x_mid, upper_bound, length.out = 4)
  x_vec_prop = unique(c(x_vec_part_1[1:3] , x_vec_part_2[2:4]))
  x_vec = c()

  # form initial x_vec with the x with non-zero h'(x)
  for (x in x_vec_prop){
    abs_h_prm_x = abs(h_prime_of(x))
    if (abs(x - x_mid) <= 1e-12){
      next;
    } else if (is.na(abs_h_prm_x)){
      next;
    } else if (abs_h_prm_x == Inf){
      next;
    } else if (abs_h_prm_x == -Inf){
      next;
    } else if (abs_h_prm_x > 1e-8){
      x_vec = append(x_vec, x)
    }
  }

  # bound vector and return
  D_vec = c(lower_bound, upper_bound)
  return_list = list(x_vec, D_vec, x_mid)

  # check for log-concavity of function
  EPS = 1e-8
  l = length(x_vec)
  if (l < 2) {
    stop('Please Respecify Bounds and Target Density:
    Given Bound too Flat to form the Initial X Vector,
    Numerically Violated Log Concavity.')
  }
  h_prime_jumps = h_prime_of(x_vec[2:l]) - h_prime_of(x_vec[1:(l-1)])
  if(!all(h_prime_jumps <= EPS)) {
    stop('Please Respecify Bounds and Target Density
         Input Target Density is not Log-Concave Within the Domain.')
  }

  return(return_list)
}

get_z_vec_and_I_vec = function(target_density, h_of, h_prime_of,
                               xk, d, ...) {

  ##################################################
  #         Create z-vector and I-vector
  # Takes in the h(x), h`(x), x and d from the main function
  # and computes the z- vector. Using the z- vector, the
  # integral (exp(u)) is computed for each interval of the
  # z-vector. This is stored in the I-vector after normalizing
  # with the sum of all integrals (over the full domain).
  #
  # parameters:
  # {h_of}
  # log of the target density function.
  # {h_prime_of}
  # derivative of the h_of function.
  # {xk)
  # x-vector
  # d
  # Upper and lower bounds of the domain
  # output:
  # a list with three components
  #   1. z-vector
  #   2. I-vector
  #   3. The sum of all integrals over the domain
  ##################################################

  l = length(xk)
  z = numeric(l+1)
  z[1] = d[1]
  z[l+1] = d[2]

  z[2:l] = ( h_of(xk[2:l])
             - h_of(xk[1:(l-1)])
             - xk[2:l] * h_prime_of( xk[2:l])
             + xk[1:(l-1)] * h_prime_of( xk[1:(l-1)])) /
    (h_prime_of( xk[1:(l-1)]) - h_prime_of( xk[2:l]))

  integ_first_part = exp(h_of(xk)) / h_prime_of(xk)
  integ_scnd_part = exp((z[2:(l+1)]-xk)*h_prime_of(xk))  -  exp((z[1:l]-xk)*h_prime_of(xk))
  integ = integ_first_part * integ_scnd_part

  integ_cum_sum = cumsum(integ)
  s = integ_cum_sum[l]
  I = integ_cum_sum/s

  return(list(z,I,s))
}

get_samples_from_density = function(target_density, h_of, h_prime_of,
                                    x_domain, n, num_iter_allowed, ...){

  ##################################################
  #           ARS Main Loop Function
  # This Function will run a loop where samples will
  # be generated. The loop will stop when n samples
  # are acquired or num_iter_allowed reached. The n
  # samples will be returned.
  #
  # parameters:
  #
  # {target_density}
  # log-concave function from which to sample.
  # {h_of}
  # log of the target density function.
  # {h_prime_of}
  # derivative of the h_of function.
  # {x_domain}
  # domain of the function from which to sample.
  # {n}
  # number of samples.
  # {num_iter_allowed}
  # maximum number of iterations for the algorithm.
  # {...}
  # extra arguments for the density function.
  #
  # output:
  # vector of n samples.
  ##################################################

  # get initial x vector and modified appropriate domain
  x_vec_and_D_and_x_mid =  get_initial_x_vec_and_D(target_density, h_of, h_prime_of,
                                                   x_domain, ...)
  x_vec = x_vec_and_D_and_x_mid[[1]]
  D = x_vec_and_D_and_x_mid[[2]]
  x_mid = x_vec_and_D_and_x_mid[[3]]

  # loop over to get n samples
  samples = rep(NULL, n)
  curr_num = 0
  num_iter = 0
  while (curr_num < n && num_iter < num_iter_allowed){
    # record the number of generations
    num_iter = num_iter + 1
    if (num_iter == num_iter_allowed) {
      warning('Preset Maximum Allowed Number of Sampling Iterations Reached.')
    }

    # get z-vector and I-vector
    z_vec_and_I_vec = get_z_vec_and_I_vec(target_density, h_of, h_prime_of,
                                          x_vec, D, ...)
    z_vec = z_vec_and_I_vec[[1]]
    I_vec = z_vec_and_I_vec[[2]]
    I_sum = z_vec_and_I_vec[[3]]

    c = stats::runif(1,0,1)
    w = stats::runif(1,0,1)

    # get index of where x_star would fall into
    j = sum(c > I_vec) + 1

    # define functions l_k_of_x and u_k_of_x
    x_j = x_vec[j]
    x_j_plus_one = x_vec[j+1]

    if (j == length(x_vec)){
      x_j_plus_one = D[2]
    }

    I_c = 0
    if(j != 1) {
      I_c = I_vec[j-1]
    }

    s1_first_part = I_sum*(c-I_c)*h_prime_of(x_j)/exp(h_of(x_j))
    s1_scnd_part = exp(h_prime_of(x_j)*(z_vec[j]-x_vec[j]))
    s1 = s1_first_part + s1_scnd_part

    x_star = log(s1)/h_prime_of(x_j) + x_vec[j]

    l_k_of = function(x, x_j, x_j_plus_one){
      numerator = (x_j_plus_one-x)*h_of(x_j) + (x-x_j)*h_of(x_j_plus_one)
      denominator = max(x_j_plus_one - x_j , 1e-8 )
      return(numerator/denominator)
    }
    u_k_of = function(x, x_j){ h_of(x_j) + (x-x_j)*h_prime_of(x_j) }

    # accept criterion
    first_threshold = exp(l_k_of(x_star, x_j, x_j_plus_one) - u_k_of(x_star, x_j))
    if (w <= first_threshold){
      curr_num = curr_num + 1
      samples[curr_num] = x_star
    }
    else
    {
      scnd_threshold = exp(h_of(x_star) - u_k_of(x_star, x_j))
      if (w <= scnd_threshold){
        curr_num = curr_num + 1
        samples[curr_num] = x_star
        if (abs(x_star - x_mid) > 1e-8){
          x_vec = if_all_unique_append(x_vec, x_star)
        }
      } else {
        if (abs(x_star - x_mid) > 1e-8){
          x_vec = if_all_unique_append(x_vec, x_star)
        }
      }
    }
  }
  return(samples)
}
