

test_that('sampling gives the right number of samples', {
  # checking for length of outputs
  expect_equal(length(ars(dnorm, 100)), 100)
  expect_equal(length(ars(dnorm, x_domain=c(0, Inf), n=1000)), 1000)
  expect_equal(length(ars(dnorm, n=10, mean=100, sd=10)), 10)
  expect_equal(length(ars(dexp, 1000)), 1000)
  expect_equal(length(ars(dchisq, 1000, df=10)), 1000)
  expect_equal(length(ars(dweibull, 2000, x_domain=c(0, 100), shape=5)), 2000)
  expect_equal(length(ars(dgamma, 1000, x_domain=c(0, 1), shape=69, rate=169)), 1000)
})


test_that('domain related numerical failure cases', {
  # domain too flat and density concentrate on one point
  expect_error(ars(dgamma, 1000, x_domain=c(0, Inf), shape=69, rate=169))
  # domain too flat at the tail of the normal distribution
  expect_error(ars(dnorm, 1000, x_domain=c(1000, Inf)))
  # the domain is simply wrong
  expect_error(ars(dchisq, 1000, x_domain=c(-Inf, -1000), df=10))
  # the domain is simply wrong
  expect_error(ars(dexp, 1000, x_domain=c(-Inf, -1000)))
  # domain too flat at the tail of the weibull distribution
  expect_error(ars(dweibull, 2000, x_domain=c(10000, Inf), shape=5))
})


test_that('target function being non-log-concave', {
  # some arbitrary non-log-concave densities
  non_log_concave_density = function (x) { return (exp(2 * x^2)) }
  expect_error(ars(non_log_concave_density, 1000, bounds = c(-5, 5)))
  non_log_concave_density = function (x) { return (x^2) }
  expect_error(ars(non_log_concave_density, 1000, bounds = c(-50, -10)))
  non_log_concave_density = function (x) { return (69-69*x) }
  expect_error(ars(non_log_concave_density, 1000, bounds = c(0, 1)))
  # some standard non-log-concave densities
  expect_error(ars(dweibull, 2000, x_domain=c(0, 100), shape=0.5))
  expect_error(ars(dbeta, 1000, x_domain=c(0, 1), shape1=0.5, shape2=2))
  expect_error(ars(dbeta, 1000, x_domain=c(0, 1), shape1=5, shape2=0.42))
  expect_error(ars(dbeta, 1000, x_domain=c(0, 1), shape1=0.05, shape2=0.42))
  expect_error(ars(dunif, 1000))
  expect_error(ars(dt, 1000))
  expect_error(ars(df, 1000))
  expect_error(ars(dcauchy, 1000))
  expect_error(ars(dpareto, 1000))
  expect_error(ars(dlnorm, 1000))
})


test_that('check if samples belong to the sampling distribution', {
  # going do the wikipedia page of log-concave distributions
  expect_equal( (ks.test(ars(dnorm, 1000), rnorm(1000))$p.value > 0.05) , TRUE)
  expect_equal(ks.test(ars(dexp, 1000, x_domain = c(0, Inf)), rexp(1000))$p.value > 0.05, TRUE)
  expect_equal(ks.test(ars(dgamma, 1000, x_domain = c(0, Inf), shape = 3, scale = 2), 
                       rgamma(1000, shape = 3, scale = 2))$p.value > 0.05, TRUE)
  expect_equal(ks.test(ars(dlogis, 1000), rlogis(1000))$p.value > 0.05, T)
  expect_equal(ks.test(ars(dbeta, 1000, x_domain = c(0, 1), shape1 = 3, shape2 = 2),
                       rbeta(1000, shape1 = 3, shape2 = 2))$p.value > 0.05, T)
  expect_equal(ks.test(ars(dweibull, 1000, x_domain = c(0, Inf), shape = 2), 
                       rweibull(1000, shape = 2))$p.value > 0.05, T)
  expect_equal(ks.test(ars(dchisq, 1000, x_domain = c(0, 100), df = 2),
                       rchisq(1000, df = 2))$p.value > 0.05, T)
})


test_that('some test cases for auxiliary functions', {
  # setup for the test cases
  target_density = dnorm
  h_of = function(x_vec){ log(target_density(x_vec)) }
  h_prime_of = function(x_vec){ dx = 1e-8
  derivative = (h_of(x_vec+dx) - h_of(x_vec)) / dx }
  x_vec_and_D = get_initial_x_vec_and_D(target_density, h_of, h_prime_of, x_domain=c(-Inf, Inf))
  x_vec = x_vec_and_D[[1]]
  D = x_vec_and_D[[2]]
  samps = get_samples_from_density(target_density, h_of, h_prime_of, 
                                   x_domain=c(-Inf, Inf), n=10, num_iter_allowed=1000)
  
  # test cases
  expect(length(x_vec) > 1, failure_message='x vector wrong')
  expect_equal(length(D),2)
  expect_equal(length(samps),10)
  expect_equal(if_all_unique_append(c(1,2,3,4,5),5.0000000001) , c(1,2,3,4,5))
  expect_equal(if_all_unique_append(c(1,2,3,4,5),6) , c(1,2,3,4,5,6))
})



