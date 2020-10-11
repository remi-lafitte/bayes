library(here)
d<-read.table(here::here("data-butter.txt"), header = T, sep = ",")
# import des données
str(d)
summary(d)
# 1. La tartine n'ayant que deux faces, le résultat s'apparente à un tirage sur une loi binomiale de paramètre θ inconnu. Quelle
# est la distribution postérieure du paramètre θ au vue de ces données, sachant que l'analyste n'avait aucun a priori (vous
# pouvez également utiliser vos propres connaissances a priori).

p_grid <- seq( from = 0, to = 1, length.out = 100)
# discrétise espace des probabilités entre 0 et 1 avec 100 palliers
prior <- rep(1, 100)
# uniform prior
sum(d$value)
# 57 tartines tombees du cote du beurre !
likelihood <- dbinom(57, size = 100, prob = p_grid)
hist(likelihood)
posterior <- (likelihood * prior) / sum(likelihood * prior)
hist(posterior)
# Échantillonner la distribution postérieure :
samp<-sample(p_grid, size = 1000, prob = posterior, replace = TRUE)
hist(samp)

# 2. Calculer le HDI à 95% de la distribution postérieure et en donner une représentation
# graphique (indice : utilisez le package BEST).
# 3. Peut-on rejeter l'hypothèse nulle selon laquelle θ
# = 0.5 ? Répondez à cette question en utilisant la procédure HDI+ROPE.

library(ggplot2)
library("BEST")
plotPost(
  samp, cex = 2, cex.axis = 1.5, cex.lab = 2,
  col = "red",
  xlab = expression(theta),
  ROPE = c(0.49, 0.51), compVal = 0.5
)
# marge d'erreur de +/1% pour ROPE ?
# 8 % in ROPE ?

# 4. Importer les observations du fichier experiment_TP2_2.csv. Mettre à jour le modèle en utilisant le mode de la
# distribution postérieure calculée précédemment.

d2<-read.table(here::here("data-butter2.txt"), header = T, sep = ",")
summary(d2)
p_grid2 <- seq( from = 0, to = 1, length.out = 500)
# discrétise espace des probabilités entre 0 et 1 avec 500 palliers
prior2<-sample(samp, size = 500, prob =samp, replace = TRUE)
hist(prior2)
# gaussian prior
sum(d2$value)
# 57 tartines tombees du cote du beurre !
likelihood2 <- dbinom(270, size = 500, prob = p_grid2)
hist(likelihood2)
# distribition très serrée !
posterior2 <- (likelihood2 * prior2) / sum(likelihood2 * prior2)
hist(posterior2)
# Échantillonner la distribution postérieure :
samp2<-sample(p_grid2, size = 1000, prob = posterior2, replace = TRUE)
hist(samp2)

plotPost(
  samp2, cex = 2, cex.axis = 1.5, cex.lab = 2,
  col = "red",
  xlab = expression(theta),
  ROPE = c(0.49, 0.51), compVal = 0.5
)
devtools::install_github("stan-dev/cmdstanr")
devtools::install_github("rmcelreath/rethinking")
# RcppEigen, V8, shape, rstan
