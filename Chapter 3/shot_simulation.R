require(ggplot2)
require(dplyr)
require(R2jags)
require(MCMCpack)

theta <- seq(from = 0, to = 2*pi, by = 1e-2)

concent <- data.frame(
    x = as.vector(
        sapply(
            1:10, 
            function(k) k * cos(theta)
        )
    ),
    y = as.vector(
        sapply(
            1:10,
            function(k) k * sin(theta)
        )
    ),
    id = rep(
        1:10,
        each = length(theta)
    )
)

p <- ggplot() +
    geom_path(data = concent, 
              aes(
                  x = x, 
                  y = y,
                  group = as.factor(id)
              ),
              linetype = 2)

set.seed(123)

radio <- 10 * sqrt(runif(n = 100))
angle <- 2 * pi * runif(n = 100)

disparos <- data.frame(
    r = radio,
    a = angle,
    x = radio * cos(angle),
    y = radio * sin(angle)
)

disparos <- disparos %>%
    mutate(ring = ceiling(r)) %>%
    mutate(r.norm = r/10) %>%
    mutate(a.norm = a / (2*pi))

###############################################################################
# Gráficas

# Tiro al blanco

p + geom_point(
    data = disparos, 
    aes(
        x = x, 
        y = y),
    shape = 4,
    stroke = 1
) +
    theme_minimal() +
    xlab("") +
    ylab("")

# Histograma anillo

ggplot(data = disparos, aes(x = ring)) + 
    geom_histogram(
        breaks = seq(
            from = 0.75,
            to = 11.25,
            by = 0.5
        )) +
    xlab("Región") +
    ylab("Frecuencia")

# Modelos anillos

# Inicial 

param1.ini <- rep(1, 10)
param1.fin <- param1.ini + sapply(1:10, function(k) sum(disparos$ring == k))

param2.ini <- rep(0,10)
param2.fin <- sapply(1:10, function(k) sum(disparos$ring == k))

param3.ini <- 2*(1:10)-1
param3.fin <- param3.ini + sapply(1:10, function(k) sum(disparos$ring == k))

probas.anillo.ini <- data.frame(
    p = c(
        param1.ini / sum(param1.ini),
        param2.ini / sum(param2.ini),
        param3.ini / sum(param3.ini)
    ),
    x = rep(1:10, 3),
    id = rep(1:3, each = 10)
)

probas.anillo <- data.frame(
    p = c(
        param1.fin / sum(param1.fin),
        param2.fin / sum(param2.fin),
        param3.fin / sum(param3.fin)
    ),
    x = rep(1:10, 3),
    id = rep(1:3, each = 10)
)

ggplot(
    data = probas.anillo.ini, 
    aes(x = x, y = p, fill = as.factor(id))
) +
    geom_col(position = "dodge") +
    xlab("Anillo") +
    ylab("Probabilidad") +
    labs(fill = "Distribución inicial")

ggplot(
    data = probas.anillo, 
    aes(x = x, y = p, fill = as.factor(id))
) +
    geom_col(position = "dodge") +
    xlab("Anillo") +
    ylab("Probabilidad") +
    labs(fill = "Distribución inicial")

# Modelos radio y ángulo

modelo1 <- function(){
    for(i in 1:N){
        s[i] ~ dbeta(bp[1], bp[2])
        phi[i] ~ dbeta(bp[3], bp[4])
    }
    for(i in 1:p){
        bp[i] ~ dgamma(hpa[i,1], hpa[i,2])
    }
    s.preds ~ dbeta(bp[1], bp[2])
    phi.preds ~ dbeta(bp[3], bp[4])
    d.lev <- dbeta(s.preds, bp[1], bp[2]) * dbeta(phi.preds, bp[3], bp[4])
}


jags.params <- c("s.preds", "phi.preds", "d.lev")

# Modelo 1

hiper1 <- matrix(rep(1,8), nrow = 4, byrow = T)

jags.modelo1 <- jags(model.file = modelo1,
                     parameters.to.save = jags.params,
                     data = list('s' = disparos$r.norm,
                                 'phi' = disparos$a.norm,
                                 'N' = 100,
                                 'p' = 4,
                                 'hpa' = hiper1),
                     n.chains = 1,
                     DIC = FALSE,
                     n.burnin = 1e3,
                     n.iter = 3e3,
                     n.thin = 1)

estimated1 <- jags.modelo1$BUGSoutput$sims.matrix
do.cont <- data.frame(
    r = 10 * estimated1[,3],
    phi = 2 * pi * estimated1[,2],
    lev = estimated1[,1]
)

do.cont <- do.cont %>%
    mutate(x = r * cos(phi),
           y = r * sin(phi))

ggplot() +
    stat_density2d(data = do.cont, aes(x = x, y = y, fill = ..level..),
                   geom = "polygon") +
    geom_path(data = concent, 
              aes(
                  x = x, 
                  y = y,
                  group = as.factor(id)
              ),
              linetype = 2) +
    xlab("") +
    ylab("") +
    labs(fill = "Densidad") +
    theme_minimal()

# Modelo 2

hiper2 <- matrix(
    c(
        5,1,0.1,1,10,1,8,1
    ), 
    nrow = 4, 
    byrow = T
)

jags.modelo2 <- jags(model.file = modelo1,
                     parameters.to.save = jags.params,
                     data = list('s' = disparos$r.norm,
                                 'phi' = disparos$a.norm,
                                 'N' = 100,
                                 'p' = 4,
                                 'hpa' = hiper2),
                     n.chains = 1,
                     DIC = FALSE,
                     n.burnin = 1e3,
                     n.iter = 3e3,
                     n.thin = 1)

estimated2 <- jags.modelo2$BUGSoutput$sims.matrix
do.cont <- data.frame(
    r = 10 * estimated2[,3],
    phi = 2 * pi * estimated2[,2],
    lev = estimated2[,1]
)

do.cont <- do.cont %>%
    mutate(x = r * cos(phi),
           y = r * sin(phi))

ggplot() +
    stat_density2d(data = do.cont, aes(x = x, y = y, fill = ..level..),
                   geom = "polygon") +
    geom_path(data = concent, 
              aes(
                  x = x, 
                  y = y,
                  group = as.factor(id)
              ),
              linetype = 2) +
    xlab("") +
    ylab("") +
    labs(fill = "Densidad") +
    theme_minimal()

# Modelo 3

hiper3 <- matrix(
    c(
        3,1,90,1,0.04,1,0.0009,1
    ), 
    nrow = 4, 
    byrow = T
)

jags.modelo3 <- jags(model.file = modelo1,
                     parameters.to.save = jags.params,
                     data = list('s' = disparos$r.norm,
                                 'phi' = disparos$a.norm,
                                 'N' = 100,
                                 'p' = 4,
                                 'hpa' = hiper3),
                     n.chains = 1,
                     DIC = FALSE,
                     n.burnin = 1e3,
                     n.iter = 3e3,
                     n.thin = 1)

estimated3 <- jags.modelo3$BUGSoutput$sims.matrix
do.cont <- data.frame(
    r = 10 * estimated3[,3],
    phi = 2 * pi * estimated3[,2],
    lev = estimated3[,1]
)

do.cont <- do.cont %>%
    mutate(x = r * cos(phi),
           y = r * sin(phi))

ggplot() +
    stat_density2d(data = do.cont, aes(x = x, y = y, fill = ..level..),
                   geom = "polygon") +
    geom_path(data = concent, 
              aes(
                  x = x, 
                  y = y,
                  group = as.factor(id)
              ),
              linetype = 2) +
    xlab("") +
    ylab("") +
    labs(fill = "Densidad") +
    theme_minimal()
