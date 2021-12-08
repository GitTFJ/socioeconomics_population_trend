source("Documents/ModellingTrends/Bayesian/Background/plot_dist.R")


#normal with uniform hyperprior
plot_dist(dists$normal, labels = c(mean = expression(mu[0]), right_sd = expression(sigma[U("1e-10", "100")])))

#Species intercept
plot_dist(dists$normal, labels = c(mean = expression(mu[genus]), right_sd = expression(sigma[U("1e-10", "100")])))

#Country intercept
plot_dist(dists$normal, labels = c(mean = expression(mu[region]), right_sd = expression(sigma[U("1e-10", "100")])))

#Model error
plot_dist(dists$normal, labels = c(mean = expression(mu[prediction]), right_sd = expression(sigma[weighted-error])))

#half t
plot_dist(dists$half_t, labels = c(scale = expression(sigma[0.0001]), df = expression(nu[1])))

#Mod1 
plot_dist(dists$t, labels = c(mean = expression(mu[pred1]), right_df = expression(df[gamma(0.1, 0.001)]), right_scale = expression(sigma[gamma(0.001, 0.001)])))

#Mod2 
plot_dist(dists$t, labels = c(mean = expression(mu[pred2]), right_df = expression(df[gamma(0.1, 0.001)]), right_scale = expression(sigma[U("1e-10", 1)])))

#Mod1 
plot_dist(dists$beta, labels = expression(B["2,8"]))


          