
function f2(xk, d, h) {
  
  l= length(xk)
  z= numeric(l+1)
  z[1]= d[1]
  z[l+1]= d[2]
  
  for (i in 1:(l-1)) {
    
    z[i+1]=  (h(xk[i+1])- h(xk[i])- xk[i+1]*numderiv(h,xk[i+1])+xk[i]*numderiv(h,xk[i]))/(numderiv(xk[i])-numderiv(xk[i+1])
                                                                                          
  }
  
  s=0
  I=numeric(l)
  
  for (j in 1:l) {
    
    integ= exp(h(xk[j]))/numderiv(xk[j])*(exp((z[j]-xk[j])*numderiv(xk[j]))-exp((z[j-1]-xk[j])*numderiv(xk[j])))
    
    s=s+integ
    
    I[j]=s
    
  }
  
  I=I/I[l]
  
}



