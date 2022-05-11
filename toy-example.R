#Libraries-----
library(lattice)
library(gridExtra)
library(gRain)
library(Rgraphviz)
library(bnlearn)


#Functions-----
rank_order_centroid <- function(n){
  weights <- numeric(n)
  for(i in 1:n){
    weights[i] <- (1/n) * (1/i)
  }
  
  weights
}
#Network-----
#Nodes
dag <- empty.graph(nodes = LETTERS[1:4])

#Arcs
dag <- set.arc(dag, from = "A", to = "C")
dag <- set.arc(dag, from = "A", to = "D")
dag <- set.arc(dag, from = "C", to = "B")
dag <- set.arc(dag, from = "D", to = "B")


#Set Levels
A.lv <- B.lv <- C.lv <- D.lv <- c("SI", "MI", "LI", "NI")

#Set Probabilities
#b_weights <- rank_order_centroid(4)
sample_probs <- c(0.3, 0.4, 0.2, 0.1)
A.prob <- array(sample_probs, dim = 4, dimnames = list(A = A.lv))
C.prob <- array(c(0.9, 0.067, 0.0165, 0.0165,
                  0.0425, 0.9, 0.0425, 0.015,
                  0.015, 0.0425, 0.9, 0.0425,
                  0.0165, 0.0165, 0.067, 0.9), dim = c(4, 4),
                dimnames = list(C = C.lv, A = A.lv))
D.prob <- array(c(0.9, 0.067, 0.0165, 0.0165,
                  0.0425, 0.9, 0.0425, 0.015,
                  0.015, 0.0425, 0.9, 0.0425,
                  0.0165, 0.0165, 0.067, 0.9), dim = c(4, 4),
                dimnames = list(D = C.lv, A = A.lv))
B.prob <- array(c(0.9, 0.067, 0.0165, 0.0165,
                  0.067, 0.9, 0.0165, 0.0165,
                  0.022, 0.8, 0.134, 0.044,
                  0.05, 0.7, 0.201, 0.049,
                  0.067, 0.9, 0.0165, 0.0165,
                  0.067, 0.9, 0.0165, 0.0165,
                  0.022, 0.134, 0.8, 0.044,
                  0.049, 0.05, 0.7, 0.201,
                  0.022, 0.8, 0.134, 0.044,
                  0.022, 0.134, 0.8, 0.044,
                  0.0165, 0.067, 0.9, 0.0165,
                  0.022, 0.044, 0.8, 0.134,
                  0.05, 0.7, 0.201, 0.049,
                  0.049, 0.05, 0.7, 0.201,
                  0.022, 0.044, 0.8, 0.134,
                  0.0165, 0.0165, 0.067, 0.9),
                dim = c(4, 4, 4),
                dimnames = list(B = B.lv, C = C.lv, D = D.lv))




#Fit Model
toy_model_cpt <- list(B = B.prob, A = A.prob, C = C.prob, D = D.prob)
toy_model_bn <- custom.fit(dag, toy_model_cpt)

#Graph Model
graphviz.chart(toy_model_bn, type = "barprob", grid = TRUE, bar.col = "green",
               strip.bg = "lightyellow", layout = "neato")

#Analyze Evidence
#'What is the probability distribution of Objective B given that Objective A is rates as "LI"?
extended_junction <- compile(as.grain(toy_model_bn))
obj_b_dist <- setEvidence(extended_junction, nodes = "A", states = "LI")
querygrain(obj_b_dist, nodes = "B")$B

#'What is the probability distribution of Objective B given that Objective A is rates as "LI",
#'and Objective C is rated as "MI"?
extended_junction <- compile(as.grain(toy_model_bn))
obj_b_dist <- setEvidence(extended_junction, nodes = c("A", "C", "D"), states = c("MI", "SI", "MI"))
querygrain(obj_b_dist, nodes = "B")$B
graphviz.chart(as.bn.fit(obj_b_dist, including.evidence = TRUE), type = "barprob", grid = TRUE, bar.col = "green",
               strip.bg = "lightyellow", layout = "neato")
