#'simulation d´une discret k fois
#'@export
#'@param x numeric vector representing the value of the random variable
#'@param p numeric vector representing probabilities
#'@param k numeric representing the number of repetition
rdistk=function (x, p, k)
{
  v = numeric(k)
  n = length(p)
  for (j in 1:k) {
    r = runif(1)
    b = p[1]
    if ((r >= 0) & (r <= b)) {
      v[j] = x[1]
    }
    else {
      a = p[1]
      b = b + p[2]
      for (i in 2:(n - 1)) {
        if ((r >= a) & (r <= b)) {
          v[j] = x[i]

        }
        else {
          a = b
          b = b + p[i + 1]
          i = i + 1 ;v[j]= x[n]} }




    }
  }

  return(v)
}

