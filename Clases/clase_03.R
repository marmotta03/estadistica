set.seed(42)

estimador1 <-function(v){
  mom1 <- 1/mean(v)
  return(mom1)
}

estimador2 <- function(v){
  s <- sum(v^2)
  n <- length(v)
  mom2 <- (-1 + sqrt(1 + (8/n*s)))/(2/n*s)
  return(mom2)
}

vector <- c(5,4,6,5,5,7,3,5,5,5,5,5,5,5)

estimador1(vector)
estimador2(vector)

vector_geometrico <- rgeom(200,0.2) + 1

estimador1(vector_geometrico)
estimador2(vector_geometrico)


# 5
vector_2 <- c(-1,-1,-1,-1,-1,-1,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

estimador_generalizado_q <- function(v){
  n <- length(v)
  indicadora <- 0
  for (i in 1:n){
    if(v[i] == 1){
      indicadora <- indicadora + 1
    }
  }
  mom_gen <- 1/n * indicadora
  return(mom_gen)
}

estimador_generalizado_q(vector_2)

estimador_primer_mom <- function(v){
  mom1 <- (3 - mean(v))/2
  return(mom1)
}

# q = (2-tita)^2

tita_est <- estimador_primer_mom(vector_2)
(2-tita_est)^2
