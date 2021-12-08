#Make a simulation to assess error
comb_df = NULL
for(run in 1:10){
  for(a in seq(0,1,0.2)){
    for(b in seq(0.02,0.22,0.04)){
      for(c in seq(2,20,2)){
        for(d in c(-1,1)){
          base_df = ((1:20*a) + rnorm(20,1,5))^2+runif(1,20,150)
          base_df_plus = ifelse(any(base_df <=0), abs(min(base_df)) + 1, 0)
          base_df = base_df + base_df_plus
          index = 1:20
          trend = coef(lm(log(base_df)~index))[2]

          #plot(index, base_df)
          expand_df = NULL
          for(e in 1:20){
            tmp_df = rnorm(50, base_df[e], b*base_df[e])
            expand_df = cbind(expand_df,tmp_df)
          }
          
          model_df = data.frame(
            year = 1:20,
            abundance_tr = base_df,
            abundance_est = apply(expand_df, 2, function(x) sample(x,1)))
          samp = sample(1:20,c)
          time_dif = max(samp) - min(samp)
          model_df$abundance = ifelse(model_df$abundance_est <0, 0, model_df$abundance_est)
          trend = coef(lm(log(abundance_tr) ~ year, data = model_df[c(min(samp):max(samp)),]))[2]
          trend = trend*d
          estimate = coef(lm(log(abundance_est) ~ year, data = model_df[samp,]))[2]
          estimate = estimate*d
          tmp_df = data.frame(
            run = run,
            sd = b,
            nobs = c,
            time_dif = time_dif,
            trend = trend,
            estimate = estimate,
            mn_ab = mean(model_df$abundance_tr[samp])
          )
          comb_df = rbind(comb_df, tmp_df)
        }
      }
    }
  }
}


ggplot() +
  geom_point(data = model_df, aes(x = year,
                 y = abundance_tr), size = 3.5, alpha = 0.6) +
  geom_smooth(data = model_df, aes(x = year,
                 y = abundance_tr), method = "lm", se = F, colour = "dark grey") +
  geom_point(data = model_df[samp,], aes(x = year,
                 y = abundance_est), colour = "red", shape = 4, size = 3.5) +
  geom_smooth(data = model_df[samp,], aes(x = year,
                 y = abundance_est), method = "lm", se = F, colour = "red") +
  theme_classic() +
  labs(x = "Year", y = "Abundance")
ggsave("example_simultating_weights.png", width = 5, height = 5)



comb_df$trend = (exp(comb_df$trend)-1)*100
comb_df$estimate = (exp(comb_df$estimate)-1)*100


a = ggplot() +
  geom_density(data = comb_df, aes(x = trend), fill = "pink", alpha = 0.4) +
  geom_density(data = comb_df, aes(x = estimate), fill = "blue", alpha = 0.2) +
  theme_classic() +
  scale_x_continuous(limits = c(-50,50)) +
  labs(x = "Annual rate of change (%)", y = "Density")
a
b = ggplot() +
  geom_density(data = Trends[[3]], aes(x = (exp(PopulationTrend)-1)*100), fill = "yellow", alpha = 0.2) +
  geom_density(data = comb_df, aes(x = estimate), fill = "blue", alpha = 0.2) +
  theme_classic() +
  scale_x_continuous(limits = c(-50,50)) +
  labs(x = "Annual rate of change (%)", y = " ")
b
comb_df$error = abs(comb_df$trend - comb_df$estimate)


ggarrange(a,b, ncol = 2, labels = c("a","b"), align = "h")
ggsave("simulated_trend_dist.png", width = 8, height = 3)

comb_df$time_dif = comb_df$time_dif+1
comb_df$samp_int = comb_df$nobs/comb_df$time_dif
m1 = lm(log(error) ~ sd + time_dif + samp_int, data = comb_df)
summary(m1)
vif(m1)
plot(m1)
hist(residuals(m1))
saveRDS(m1, "ErrorModel.rds")

pred = ggpredict(m1, terms = c("samp_int [0.1:1, by = 0.05]"))
a = ggplot(pred) +
  geom_line(aes(x = x*100, y = predicted)) +
  geom_ribbon(aes(x = x*100, ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  scale_x_continuous(expand = c(0,0), limits = c(0,105)) +
  theme_classic() +
  labs(x = "Sampling intensity (%)\n", y = "Error in annual\n rate of change (%)") 
a

pred = ggpredict(m1, terms = c("sd [0:0.2, by = 0.01]"))
b = ggplot(pred) +
  geom_line(aes(x = x, y = predicted)) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  scale_x_continuous(expand = c(0,0), limits = c(0,0.21)) +
  theme_classic() +
  labs(x = "Coefficient of variation of \nabundance estimates", y = "") 
b

pred = ggpredict(m1, terms = c("time_dif [2:20, by = 1]"))
c = ggplot(pred) +
  geom_line(aes(x = x, y = predicted)) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  scale_x_continuous(expand = c(0,0), limits = c(0,21)) +
  theme_classic() +
  labs(x = "Duration of population\nmonitoring period (years)", y = "") 
c

ggarrange(a,b,c, ncol = 3, labels = c("a","b","c"), align = "h")
ggsave("simulated_effects.png", width = 8, height = 3)
n