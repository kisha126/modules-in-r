box::use(cor_hm = ./Modules/statistics/cor_npar)

set.seed(123)
xx <- rnorm(10000); yy <- rnorm(10000); zz <- rnorm(10000)

cor_hm$cor(xx, yy, zz)
cor_hm$cor(speed, dist, .data = cars)

# Pipe is provided

cars |> cor_hm$cor_pipe()
cars |> cor_hm$cor_pipe(speed, dist)

box::reload(cor_hm)
box::unload(cor_hm)