#!/usr/bin/env Rscript
# Where are the graphs to be saved?

args <- commandArgs(trailingOnly = TRUE)

# Test if there is at least one argument: if not, return an error

if (length(args) <= 1) {
  stop("At least one argument must be supplied (input file).\n", call.=FALSE)
}

#####
# Programming a non-parametric method

require(mvtnorm)
require(MCMCpack)
require(ggplot2)
require(reshape2)
require(wesanderson)

################################################################################
# Generating weigths via the Sethuraman representation of the Dirichlet Process
################################################################################
weigth_gen <- function(n, M, d = rep(0, n)){
    v <- numeric(n)
    for(i in 1:n){
        v[i] <- rbeta(n = 1, shape1 = 1 + sum(d == i), shape2 = M + sum(d > i))
    }
    return(v * cumprod(c(1, 1 - v[-n])))
}



################################################################################
# Samples from the Inverse Wishart-Gaussian distribution
################################################################################

iwisgaus_gen <- function(n, mu_0, psi, tau, nu, y = NULL, d = NULL){
    theta <- list(mu = list(), Sigma = list())
    for(i in 1:n){
        if( is.null(y) || ifelse(is.null(d), T, sum(d == i) == 0) ){
            theta$Sigma[[i]] <- MCMCpack::riwish(v = nu, S = psi)
            theta$mu[[i]] <- mvtnorm::rmvnorm(n = 1,
                                              mean = mu_0,
                                              sigma = tau * theta$Sigma[[i]])
        } else {
            ni <- sum(d == i)
            if(dim(y)[2] > 1){
                yi_m <- colMeans(y[d == i,])
            } else {
                yi_m <- mean(y[d == i,])
            }
            Si <- (ni / (1 + tau * ni)) * outer(yi_m - mu_0, yi_m - mu_0)
            for(j in which(d == i)){
                Si <- Si + outer(as.numeric(y[j,] - yi_m),
                                 as.numeric(y[j,] - yi_m))
            }
            theta$Sigma[[i]] <- MCMCpack::riwish(v = nu + ni,
                                                 S = psi + Si)
            theta$mu[[i]] <- mvtnorm::rmvnorm(n = 1,
                                              mean = (mu_0 + tau * ni * yi_m) / (1 + tau * ni),
                                              sigma = (tau/(1 + tau * ni)) * theta$Sigma[[i]])
        }
    }
    return(theta)
}


################################################################################
# Gaussian mixture with slice sampling
################################################################################

slice_sampler_norm <- function(y,
                               mu_0 = rep(0, dim(y)[2]),
                               psi = diag(rep(1, dim(y)[2])),
                               tau = 10,
                               nu = dim(y)[2],
                               M = 1,
                               burnin = 3e3,
                               N_mcmc = 1e3,
                               thinning = 10,
                               kappa = M / (1 + M)){
    n <- dim(y)[1]
    u <- runif(n = n)
    xi <- (kappa)^(0:9999)
    d <- numeric(n)
    l <- numeric(n)
    for(i in 1:n){
        l[i] <- which.min(xi > u[i]) - 1
        d[i] <- sample(x = 1:l[i], size = 1)
    }
    K <- max(l)
    w <- weigth_gen(n = K, M = M)
    theta <- iwisgaus_gen(n = K, mu_0 = mu_0, psi = psi, tau = tau, nu = nu)
    for(j in 1:burnin){
        u <- runif(n = n, min = 0, max = xi[d])
        K <- max(sapply(1:n, function(l) which.min(xi > u[l]) - 1))
        theta <- iwisgaus_gen(n = K, mu_0, psi = psi, tau = tau, nu = nu,
                                  y = y, d = d)
        w <- weigth_gen(n = K, M = M, d = d)
        for(i in 1:n){
            l <- which.min(xi > u[i]) - 1
            d[i] <- sample(x = 1:l, size = 1,
                           prob = sapply(1:l,
                                         function(k) w[k] *
                                             mvtnorm::dmvnorm(x = y[i,],
                                                              mean = theta$mu[[k]],
                                                              sigma = theta$Sigma[[k]]) /
                                             xi[k]))
        }
        if(j %% (burnin / 10) == 0){
            cat(sprintf("%4d burn-in iterations.\n", j))
        }
    }
    cat(sprintf("Burn-in is over.\n"))
    D <- list()
    Theta <- list()
    components <- numeric()
    h <- 0
    for(j in 1:N_mcmc){
        u <- runif(n = n, min = 0, max = xi[d])
        K <- max(sapply(1:n, function(l) which.min(xi > u[l]) - 1))
        theta <- iwisgaus_gen(n = K, mu_0, psi = psi, tau = tau, nu = nu,
                                  y = y, d = d)
        w <- weigth_gen(n = K, M = M, d = d)
        for(i in 1:n){
            l <- which.min(xi > u[i]) - 1
            d[i] <- sample(x = 1:l, size = 1,
                           prob = sapply(1:l,
                                         function(k) w[k] *
                                             mvtnorm::dmvnorm(x = y[i,],
                                                              mean = theta$mu[[k]],
                                                              sigma = theta$Sigma[[k]]) /
                                             xi[k]))
        }
        if(j %% thinning == 0){
            h <- h + 1
            D[[h]] <- d
            Theta[[h]] <- theta
            components <- c(components, K)
        }
        if(j %% (N_mcmc / 10) == 0){
            cat(sprintf("%4d MCMC iterations.\n", j))
            cat(sprintf("%3d iterations have been saved.\n", h))
        }
    }
    cat(sprintf("The MCMC algorithm is over.\n"))
    return(list(n = components,d = D, theta = Theta))
}

################################################################################
# Approximate a density with gaussian mixtures
################################################################################

aprox_norm <- function(x, theta){
    N <- length(theta$n)
    S <- 0
    for(i in 1:N){
        S_int <- 0
        for(j in 1:length(theta$d[[i]])){
            S_int <- S_int + mvtnorm::dmvnorm(x = x,
                                              mean = theta$theta[[i]]$mu[[theta$d[[i]][j]]],
                                              sigma = theta$theta[[i]]$Sigma[[theta$d[[i]][j]]])
        }
        S <- S + S_int / length(theta$d[[i]])
    }
    S <- S / N
    return(S)
}

################################################################################
# Classify given an MCMC chain derived from slice_sampler_norm
################################################################################

classify_dirichlet <- function(D){
    n <- length(D[[1]])
    m <- length(D)
    M <- matrix(data = 0, nrow = n, ncol = n)
    for(i in 1:n){
        for(j in 1:n){
            for(k in 1:m){
                M[i, j] <- M[i, j] + (D[[k]][i] == D[[k]][j])
            }
        }
    }
    return(M / m)
}

################################################################################
# Galaxies data set testing
################################################################################

Y <- data.frame(Velocity = MASS::galaxies / 1000)

theta1 <- slice_sampler_norm(y = Y, tau = 100, N_mcmc = 3e2, M = 1, kappa = 0.5,
                             burnin = 5e2)
theta2 <- slice_sampler_norm(y = Y, tau = 100, N_mcmc = 3e2, M = 1, kappa = 0.3,
                             burnin = 5e2)
theta3 <- slice_sampler_norm(y = Y, tau = 100, N_mcmc = 3e2, M = 1, kappa = 0.7,
                             burnin = 5e2)
theta4 <- slice_sampler_norm(y = Y, tau = 100, N_mcmc = 3e3, M = 10, kappa = 0.5,
                             burnin = 5e2)
theta5 <- slice_sampler_norm(y = Y, tau = 100, N_mcmc = 3e2, M = 10, kappa = 0.3,
                             burnin = 5e2)
theta6 <- slice_sampler_norm(y = Y, tau = 100, N_mcmc = 3e2, M = 10, kappa = 0.7,
                             burnin = 5e2)
theta7 <- slice_sampler_norm(y = Y, tau = 100, N_mcmc = 3e2, M = 0.1, kappa = 0.5,
                             burnin = 5e2)
theta8 <- slice_sampler_norm(y = Y, tau = 100, N_mcmc = 3e2, M = 0.1, kappa = 0.3,
                             burnin = 5e2)
theta9 <- slice_sampler_norm(y = Y, tau = 100, N_mcmc = 3e2, M = 0.1, kappa = 0.7,
                             burnin = 5e2)

x <- seq(from = 5, to = 40, by = 1e-1)
y_aprox1 <- numeric(length(x))
y_aprox2 <- numeric(length(x))
y_aprox3 <- numeric(length(x))
y_aprox4 <- numeric(length(x))
y_aprox5 <- numeric(length(x))
y_aprox6 <- numeric(length(x))
y_aprox7 <- numeric(length(x))
y_aprox8 <- numeric(length(x))
y_aprox9 <- numeric(length(x))
for(i in 1:length(x)){
    y_aprox1[i] <- aprox_norm(x = x[i], theta = theta1)
    y_aprox2[i] <- aprox_norm(x = x[i], theta = theta2)
    y_aprox3[i] <- aprox_norm(x = x[i], theta = theta3)
    y_aprox4[i] <- aprox_norm(x = x[i], theta = theta4)
    y_aprox5[i] <- aprox_norm(x = x[i], theta = theta5)
    y_aprox6[i] <- aprox_norm(x = x[i], theta = theta6)
    y_aprox7[i] <- aprox_norm(x = x[i], theta = theta7)
    y_aprox8[i] <- aprox_norm(x = x[i], theta = theta8)
    y_aprox9[i] <- aprox_norm(x = x[i], theta = theta9)
    if(i %% floor(length(x) / 100) == 0){
        cat(sprintf("%2d completed.\n", floor(i * 100 / length(x))))
    }
}

ppp <- hist(Y$Velocity, freq = F, ylim = c(0, max(c(y_aprox1,
                                             y_aprox2,
                                             y_aprox3,
                                             y_aprox4,
                                             y_aprox5,
                                             y_aprox6,
                                             y_aprox7,
                                             y_aprox8,
                                             y_aprox9)) + 0.05),
     breaks = 30,
     xlim = c(5, 40))
lines(x, y_aprox1, lwd = 3)
lines(x, y_aprox2, lwd = 3, col = "blue")
lines(x, y_aprox3, lwd = 3, col = "red")
lines(x, y_aprox4, lwd = 3)
lines(x, y_aprox5, lwd = 3, col = "blue")
lines(x, y_aprox6, lwd = 3, col = "red")
lines(x, y_aprox7, lwd = 3)
lines(x, y_aprox8, lwd = 3, col = "blue")
lines(x, y_aprox9, lwd = 3, col = "hot pink")

YY <- data.frame(x = rep(x, 9),
                 y  = c(y_aprox1, y_aprox2, y_aprox3, y_aprox4, y_aprox5,
                        y_aprox6, y_aprox7, y_aprox8, y_aprox9),
                 M = as.factor(rep(c(1, 10, 0.1), each = 3 * length(x))),
                 kappa = as.factor(rep(rep(c(0.5, 0.7, 0.3), each = length(x)), 3)))

p1 <- ggplot() +
    geom_histogram(data = Y, aes(x = Velocity, y = ..density..),
                   binwidth = 1.5, alpha = 0.1, fill = "pink") +
    geom_line(data = YY, aes(x = x, y = y, colour = interaction(M, kappa, sep = ",")),
              size = 1) +
    xlab("Galaxy velocity") +
    ylab("Density") +
    guides(colour = guide_legend(title = "Parameters of Dirichlet Process\n and slice sampler (M, kappa)"))

################################################################################
# Iris data set
################################################################################

data(iris)
Y <- iris[, -c(1,2,5)]

irischain <- slice_sampler_norm(y = Y, tau = 1e5, N_mcmc = 3e2,
                             M = 10, kappa = 0.5, burnin = 5e2)

################################################################################
# Matrices de clasificación
################################################################################

Mi <- classify_dirichlet(irischain$d)

colors <- wes_palette(n = 5, name = "Cavalcanti1", type = "continuous")

p2 <- ggplot(data = melt(Mi), aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = colors[4]) +
    labs(title = "Data agrupation", x = "Index", y = "Index",
         fill = "Association\nlevel")


real_species <- matrix(data = c(rep(c(rep(1, 50), rep(0,50), rep(0,50)), 50),
                          rep(c(rep(0, 50), rep(1,50), rep(0,50)), 50),
                          rep(c(rep(0, 50), rep(0,50), rep(1,50)), 50)),
                 ncol = 150)

p3 <- ggplot(data = melt(real_species), aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = colors[4]) +
    labs(title = "Data agrupation", x = "Index", y = "Index",
         fill = "Association\nlevel")

png(args[1], width = 750, height = 520)
plot(p1)
dev.off()

png(args[2], width = 700, height = 500)
plot(p2)
dev.off()

png(args[3], width = 700, height = 500)
plot(p3)
dev.off()
