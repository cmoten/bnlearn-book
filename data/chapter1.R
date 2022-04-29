## ----chapter1-init-stanza, include = FALSE, cache = FALSE, echo = FALSE-------
library(lattice)
library(gridExtra)
library(gRain)
library(Rgraphviz)
library(bnlearn)
library(R.utils)

## ----for-starters-load-bnlearn------------------------------------------------
#library(bnlearn)


## ----create-an-empty-graph----------------------------------------------------
dag <- empty.graph(nodes = c("A", "S", "E", "O", "R", "T"))


## ----print-the-empty-graph----------------------------------------------------
dag


## ----set-first-arc------------------------------------------------------------
dag <- set.arc(dag, from = "A", to = "E")


## ----set-second-arc-----------------------------------------------------------
dag <- set.arc(dag, from = "S", to = "E")


## ----set-3rd-4th-arcs---------------------------------------------------------
dag <- set.arc(dag, from = "E", to = "O")
dag <- set.arc(dag, from = "E", to = "R")


## ----set-5th-6th-arcs---------------------------------------------------------
dag <- set.arc(dag, from = "O", to = "T")
dag <- set.arc(dag, from = "R", to = "T")


## ----print-again--------------------------------------------------------------
dag


## ----show-the-model-formula---------------------------------------------------
modelstring(dag)


## ----show-nodes---------------------------------------------------------------
nodes(dag)


## ----show-arcs----------------------------------------------------------------
arcs(dag)


## ----recreate-with-arc-set----------------------------------------------------
dag2 <- empty.graph(nodes = c("A", "S", "E", "O", "R", "T"))
arc.set <- matrix(c("A", "E",
                    "S", "E",
                    "E", "O",
                    "E", "R",
                    "O", "T",
                    "R", "T"),
             byrow = TRUE, ncol = 2,
             dimnames = list(NULL, c("from", "to")))
arcs(dag2) <- arc.set


## ----compare-to-previous-graph------------------------------------------------
all.equal(dag, dag2)


## ----showcase-error-handling--------------------------------------------------
set.arc(dag, from = "T", to = "E")


## ----create-levels-sets-------------------------------------------------------
A.lv <- c("young", "adult", "old")
S.lv <- c("M", "F")
E.lv <- c("high", "uni")
O.lv <- c("emp", "self")
R.lv <- c("small", "big")
T.lv <- c("car", "train", "other")


## ----cpts-A-S-----------------------------------------------------------------
A.prob <- array(c(0.30, 0.50, 0.20), dim = 3, dimnames = list(A = A.lv))
A.prob
S.prob <- array(c(0.60, 0.40), dim = 2, dimnames = list(S = S.lv))
S.prob


## ----cpts-O-R-----------------------------------------------------------------
O.prob <- array(c(0.96, 0.04, 0.92, 0.08), dim = c(2, 2),
            dimnames = list(O = O.lv, E = E.lv))
O.prob
R.prob <- array(c(0.25, 0.75, 0.20, 0.80), dim = c(2, 2),
            dimnames = list(R = R.lv, E = E.lv))
R.prob


## ----cpts-R-------------------------------------------------------------------
R.prob <- matrix(c(0.25, 0.75, 0.20, 0.80), ncol = 2,
            dimnames = list(R = R.lv, E = E.lv))


## ----cpts-E-T-----------------------------------------------------------------
E.prob <- array(c(0.75, 0.25, 0.72, 0.28, 0.88, 0.12, 0.64, 0.36, 0.70,
                  0.30, 0.90, 0.10), dim = c(2, 3, 2),
            dimnames = list(E = E.lv, A = A.lv, S = S.lv))

T.prob <- array(c(0.48, 0.42, 0.10, 0.56, 0.36, 0.08, 0.58, 0.24, 0.18,
                  0.70, 0.21, 0.09), dim = c(3, 2, 2),
            dimnames = list(T = T.lv, O = O.lv, R = R.lv))


## ----create-network-one-more-time---------------------------------------------
dag3 <- model2network("[A][S][E|A:S][O|E][R|E][T|O:R]")


## ----compare-one-more-time----------------------------------------------------
all.equal(dag, dag3)


## ----assembled-fitted-object--------------------------------------------------
cpt <- list(A = A.prob, S = S.prob, E = E.prob, O = O.prob, R = R.prob,
         T = T.prob)
bn <- custom.fit(dag, cpt)


## ----print-number-of-parameters-----------------------------------------------
nparams(bn)


## ----print-arcs-fitted--------------------------------------------------------
arcs(bn)


## ----print-fitted-node-R------------------------------------------------------
bn$R


## ----extract-probabilities-node-R---------------------------------------------
R.cpt <- coef(bn$R)


## ----print-whole-fitted-bn, eval = FALSE--------------------------------------
## bn


## ----read-survey-data---------------------------------------------------------
survey <- read.table("data/survey.txt", header = TRUE, colClasses = "factor")


## ----head-survey-data---------------------------------------------------------
head(survey)


## ----maximum-likelihood-fit---------------------------------------------------
bn.mle <- bn.fit(dag, data = survey, method = "mle")


## ----create-cpt-manually-O-E--------------------------------------------------
prop.table(table(survey[, c("O", "E")]), margin = 2)


## ----print-O-from-fitted------------------------------------------------------
bn.mle$O


## ----posterior-fit------------------------------------------------------------
bn.bayes <- bn.fit(dag, data = survey, method = "bayes", iss = 10)


## ----print-O-again------------------------------------------------------------
bn.bayes$O


## ----posterior-fit-larger-iss-------------------------------------------------
bn.bayes <- bn.fit(dag, data = survey, method = "bayes", iss = 20)
bn.bayes$O


## ----evolution-of-iss, echo = FALSE, results = "hide"-------------------------
local({
lattice.options(default.theme = canonical.theme(color = FALSE))
trace <- lapply(seq(from = 10, to = 10000, by = 10),
           function(iss) {
             c(iss, as.vector(coef(bn.fit(dag, survey, method = "bayes", iss = iss)$O)))
           })
trace <- as.data.frame(do.call("rbind", trace))
colnames(trace) <- c("ISS", "P1", "P2", "P3", "P4")

pdf(file = "figures/iss.pdf", width = 8, height = 5)
p1 <- xyplot(P1 + P2 ~ ISS, data = trace, type = "l",
  xlab = "imaginary sample size", ylab = "conditional probabilities",
  scales = list(y = list(at = c(0.1, 0.3, 0.5, 0.7, 0.9))),
  main = expression(Pr({O} * " | " * {E == high})),
  auto.key = list(lines = TRUE, points = FALSE, type = "l", corner = c(0.98, 0.93),
               text = c(expression(Pr({O == self} * " | " * {E == high})),
                        expression(Pr({O == emp} * " | " * {E == high})))),
  panel = function(...) {

    panel.xyplot(...)
    panel.abline(h = 0.5, col = "grey")

  }
)
p2 <- xyplot(P3 + P4 ~ ISS, data = trace, type = "l",
  xlab = "imaginary sample size", ylab = "conditional probabilities",
  scales = list(y = list(at = c(0.1, 0.3, 0.5, 0.7, 0.9))),
  main = expression(Pr({O} * " | " * {E == uni})),
  auto.key = list(lines = TRUE, points = FALSE, type = "l", corner = c(0.98, 0.93),
               text = c(expression(Pr({O == self} * " | " * {E == uni})),
                        expression(Pr({O == emp} * " | " * {E == uni})))),
  panel = function(...) {

    panel.xyplot(...)
    panel.abline(h = 0.5, col = "grey")

  }
)
grid.arrange(p1, p2, nrow = 1)
dev.off()
})


## ----tally-parameters---------------------------------------------------------

(nlevels(survey[, "T"]) - 1) * (nlevels(survey[, "E"]) - 1) *
  (nlevels(survey[, "O"]) * nlevels(survey[, "R"]))


## ----mutual-information-test--------------------------------------------------
ci.test("T", "E", c("O", "R"), test = "mi", data = survey)


## ----pearson-x2-test----------------------------------------------------------
ci.test("T", "E", c("O", "R"), test = "x2", data = survey)


## ----conditional-pearson-x2-test----------------------------------------------
ci.test("T", "O", "R", test = "x2", data = survey)


## ----print-arc-strength-by-pvalue---------------------------------------------
arc.strength(dag, data = survey, criterion = "x2")


## ----hidden-seed-, echo = FALSE----------------------------------------------
set.seed(456)

## ----scores-comparison--------------------------------------------------------
score(dag, data = survey, type = "bic")
score(dag, data = survey, type = "bde", iss = 10)


## ----different-iss-comparison-------------------------------------------------
score(dag, data = survey, type = "bde", iss = 1)


## ----change-the-model-and-score-again-----------------------------------------
dag4 <- set.arc(dag, from = "E", to = "T")
nparams(dag4, survey)
score(dag4, data = survey, type = "bic")


## ----score-a-random-graph-----------------------------------------------------
rnd <- random.graph(nodes = c("A", "S", "E", "O", "R", "T"))
modelstring(rnd)
score(rnd, data = survey, type = "bic")


## ----score-a-learned-graph----------------------------------------------------
learned <- hc(survey)
modelstring(learned)
score(learned, data = survey, type = "bic")


## ----score-a-graph-learned-with-a-different-score-----------------------------
learned2 <- hc(survey, score = "bde")


## ----arc-strength-by-score----------------------------------------------------
arc.strength(learned, data = survey, criterion = "bic")


## ----arc-strength-by-score-v2-------------------------------------------------
arc.strength(dag, data = survey, criterion = "bic")


## ----dseparation-checks-------------------------------------------------------
dsep(dag, x = "S", y = "R")
dsep(dag, x = "O", y = "R")


## ----path-checks--------------------------------------------------------------
path.exists(dag, from = "S", to = "R")


## ----compare-dsep-and-path----------------------------------------------------
dsep(dag, x = "S", y = "R", z = "E")


## ----one-more-dseparation-----------------------------------------------------
dsep(dag, x = "O", y = "R", z = "E")


## ----conditioning-introduces-dependencies-------------------------------------
dsep(dag, x = "A", y = "S")
dsep(dag, x = "A", y = "S", z = "E")


## ----different-fundamental-connections, echo = FALSE, results = "hide"--------
local({
pdf(file = "figures/dseparation.pdf", width = 15, height = 6)
par(mfrow = c(1, 3))
highlight <- list(nodes = nodes(dag), arcs = arcs(dag), col = "grey",
                  textCol = "grey")
p <- graphviz.plot(dag, highlight = highlight, render = FALSE)
p1 <- p
edgeRenderInfo(p1) <-
  list(col = c("S~E" = "black", "E~R" = "black"),
       lwd = c("S~E" = 3, "E~R" = 3))
nodeRenderInfo(p1) <-
  list(col = c("S" = "black", "E" = "black", "R" = "black"),
       textCol = c("S" = "black", "E" = "black", "R" = "black"),
       fill = c("E" = "grey"), fontsize = 12)
renderGraph(p1)
p2 <- p
edgeRenderInfo(p2) <-
  list(col = c("E~R" = "black", "E~O" = "black"),
       lwd = c("E~R" = 3, "E~O" = 3))
nodeRenderInfo(p2) <-
  list(col = c("O" = "black", "E" = "black", "R" = "black"),
       textCol = c("O" = "black", "E" = "black", "R" = "black"),
       fill = c("E" = "grey"), fontsize = 12)
renderGraph(p2)
p3 <- p
edgeRenderInfo(p3) <-
  list(col = c("A~E" = "black", "S~E" = "black"),
       lwd = c("A~E" = 3, "S~E" = 3))
nodeRenderInfo(p3) <-
  list(col = c("A" = "black", "E" = "black", "S" = "black"),
       textCol = c("A" = "black", "E" = "black", "S" = "black"),
       fill = c("E" = "grey"), fontsize = 12)
renderGraph(p3)
dev.off()
})


## ----load-gRain---------------------------------------------------------------
library(gRain)


## ----import-and-compile-------------------------------------------------------
junction <- compile(as.grain(bn))


## ----first-exact-query--------------------------------------------------------
querygrain(junction, nodes = "T")$T
jsex <- setEvidence(junction, nodes = "S", states = "F")
querygrain(jsex, nodes = "T")$T


## ----travel-barcharts-panels, echo = FALSE, results = "hide"------------------
local({
Evidence = factor(c(rep("Unconditional",3), rep("Female", 3), rep("Small City",3)),
             levels = c("Unconditional", "Female", "Small City"))
Travel = factor(rep(c("car", "train", "other"), 3),
           levels = c("other", "train", "car"))
d <- data.frame(Evidence = Evidence, Travel = Travel,
      Prob = c(0.5618, 0.2808, 0.15730, 0.5620, 0.2806, 0.1573, 0.4838, 0.4170, 0.0990))

lattice.options(default.theme = canonical.theme(color = FALSE))
pdf(file = "figures/evidence.pdf", width = 7, height = 3.5)
print(barchart(Travel ~ Prob | Evidence, data = d, horizontal = TRUE,
   scales = list(alternating = 1, tck = c(1, 0)),
   xlab = "probability", layout = c(3, 1),
   strip = strip.custom(factor.levels =
     c(expression(Pr(T)),
       expression(Pr({T} * " | " * {S == F})),
       expression(Pr({T} * " | " * {R == small})))),
   par.strip.text=list(lines=1.5), box.ratio = 1,
   panel = function(...) {

     panel.grid(h = 0, v = -1)
     panel.barchart(...)

   })
)
dev.off()
})


## ----second-exact-query-------------------------------------------------------
jres <- setEvidence(junction, nodes = "R", states = "small")
querygrain(jres, nodes = "T")$T


## ----third-exact-query--------------------------------------------------------
jedu <- setEvidence(junction, nodes = "E", states = "high")
SxT.cpt <- querygrain(jedu, nodes = c("S", "T"), type = "joint")
SxT.cpt


## ----marginal-from-query------------------------------------------------------
querygrain(jedu, nodes = c("S", "T"), type = "marginal")


## ----conditional-from-query---------------------------------------------------
querygrain(jedu, nodes = c("S", "T"), type = "conditional")


## ----save-exact-query-value, echo = FALSE-------------------------------------
exact.value <- SxT.cpt["M", "car"]


## ----exact-queries-and-dseparation--------------------------------------------
dsep(bn, x = "S", y = "T", z = "E")


## ----probabilities-and-counts-------------------------------------------------
SxT.ct = SxT.cpt * nrow(survey)


## ----counts-and-pearson-x2----------------------------------------------------
chisq.test(SxT.ct)


## ----reset-seed, echo = FALSE-------------------------------------------------
set.seed(123)

## ----first-approximate-query--------------------------------------------------
cpquery(bn, event = (S == "M") & (T == "car"), evidence = (E == "high"))


## ----first-query-more-particles-----------------------------------------------
cpquery(bn, event = (S == "M") & (T == "car"),
  evidence = (E == "high"), n = 10^6)


## ----reset-seed-again, echo = FALSE-------------------------------------------
set.seed(567)
approx.value <- cpquery(bn, event = (S == "M") & (T == "car"),
                  evidence = list(E = "high"), method = "lw")
set.seed(567)

## ----first-query-likelihood-weighting-----------------------------------------
cpquery(bn, event = (S == "M") & (T == "car"),
  evidence = list(E = "high"), method = "lw")


## ----reset-seed-once-more, echo = FALSE---------------------------------------
set.seed(123)

## ----second-approximate-query-------------------------------------------------
cpquery(bn, event = (S == "M") & (T == "car"),
  evidence = ((A == "young") & (E == "uni")) | (A == "adult"))


## ----cpquery-vs-cpdist--------------------------------------------------------
SxT <- cpdist(bn, nodes = c("S", "T"), evidence = (E == "high"))
head(SxT)


## ----counts-to-probabilities--------------------------------------------------
prop.table(table(SxT))


## ----trying-graphviz-layout, echo = FALSE, results = "hide"-------------------
local({
pdf(file = "figures/layout.pdf", width = 14, height = 6)
par(mfrow = c(1, 3), cex = 2)
graph.par(list(nodes = list(fontsize = 10)))
graphviz.plot(dag, layout = "dot", main = "layout = \"dot\"")
graphviz.plot(dag, layout = "fdp", main = "layout = \"fdp\"")
graphviz.plot(dag, layout = "circo", main = "layout = \"circo\"")
dev.off()
})


## ----mention-graphviz, eval = FALSE-------------------------------------------
## graphviz.plot(dag)


## ----specify-highlighting-----------------------------------------------------
hlight <- list(nodes = nodes(dag), arcs = arcs(dag), col = "grey",
               textCol = "grey")


## ----graphviz-with-highlighting-----------------------------------------------
pp <- graphviz.plot(dag, highlight = hlight, render = FALSE)


## ----call-renderinfo-arcs-----------------------------------------------------
library(Rgraphviz)
edgeRenderInfo(pp) <- list(col = c("S~E" = "black", "E~R" = "black"),
                           lwd = c("S~E" = 3, "E~R" = 3))


## ----call-renderingfo-nodes---------------------------------------------------
nodeRenderInfo(pp) <-
  list(col = c("S" = "black", "E" = "black", "R" = "black"),
    textCol = c("S" = "black", "E" = "black", "R" = "black"),
    fill = c("E" = "grey"))


## ----fake-graphviz-plotting, fig.keep = "none"--------------------------------
renderGraph(pp)


## ----create-real-barchart, echo = FALSE, results = "hide"---------------------
local({
lattice.options(default.theme = canonical.theme(color = FALSE))
pdf(file = "figures/barchart.pdf", width = 6, height = 4)
bn.fit.barchart(bn.mle$T, main = "Travel", xlab = "Pr(T | R, O)", ylab = "")
dev.off()
})


## ----fake-barchart-plot, eval = FALSE-----------------------------------------
## bn.fit.barchart(bn.mle$T, main = "Travel",
##   xlab = "Pr(T | R, O)", ylab = "")


## ----reformat-data-for-lattice------------------------------------------------
Evidence <- factor(c(rep("Unconditional",3), rep("Female", 3),
                     rep("Small City",3)),
              levels = c("Unconditional", "Female", "Small City"))
Travel <- factor(rep(c("car", "train", "other"), 3),
            levels = c("other", "train", "car"))
distr <- data.frame(Evidence = Evidence, Travel = Travel,
           Prob = c(0.5618, 0.2808, 0.15730, 0.5620, 0.2806,
                    0.1573, 0.4838, 0.4170, 0.0990))


## ----show-reshaped-data-------------------------------------------------------
head(distr)


## ----reproducing-the-barchart-in-lattice, fig.keep = "none"-------------------
library(lattice)
barchart(Travel ~ Prob | Evidence, data = distr,
   layout = c(3, 1), xlab = "probability",
   scales = list(alternating = 1, tck = c(1, 0)),
   strip = strip.custom(factor.levels =
     c(expression(Pr(T)),
       expression(Pr({T} * " | " * {S == F})),
       expression(Pr({T} * " | " * {R == small})))),
   panel = function(...) {
     panel.barchart(...)
     panel.grid(h = 0, v = -1)
   })


## ----graphviz-chart, echo = FALSE, results = "hide", warning = FALSE----------
local({
pdf(file = "figures/marginals.pdf", width = 6, height = 4)
par(mfrow = c(1, 2))
graphviz.chart(bn, grid = TRUE, main = "Original BN")
graphviz.chart(as.bn.fit(jedu, including.evidence = TRUE), grid = TRUE,
  bar.col = c(A = "black", S = "black", E = "grey", O = "black",
              R = "black", T = "black"),
  strip.bg = c(A = "transparent", S = "transparent", E = "grey",
               O = "transparent", R = "transparent", T = "transparent"),
  main = "BN with Evidence")
dev.off()
})


## ----fake-combined-plot, eval = FALSE-----------------------------------------
## graphviz.chart(bn, grid = TRUE, main = "Original BN")
## graphviz.chart(as.bn.fit(jedu, including.evidence = TRUE), grid = TRUE,
##   bar.col = c(A = "black", S = "black", E = "grey", O = "black",
##               R = "black", T = "black"),
##   strip.bg = c(A = "transparent", S = "transparent", E = "grey",
##               O = "transparent", R = "transparent", T = "transparent"),
##   main = "BN with Evidence")

