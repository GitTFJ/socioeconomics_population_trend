global = "
model {
for (i in 1:(stab_min - 1)) {
pt.pred[i] ~ dnorm(mod[i], tau * wg_a[i])
pt[i] ~ dnorm(mod[i], tau * wg_a[i])
mod[i] <- alpha + 
  mu.reg[reg[i]] + obs.site[cou[i]] + 
  mu.gen[gen[i]] + obs.spec[spec[i]] + 

  #Essential
  beta[1] * ar[i] + 
  beta[2] * exh[i] + 
  beta[3] * pdc[i] +
  beta[4] * prc[i] +
  beta[5] * bm[i] +
  beta[6] * gov_e[i] +
  beta[7] * pr[i] +
  
  #Optional
  beta[8] * drc[i] +
  beta[9] * ntc[i] +
  beta[10] * hdi_e[i] +
  beta[11] * hdc[i] +
  beta[12] * conf[i] +
  beta[13] * re_e[i] +
  beta[14] * ml[i] +
  beta[15] * gn_e[i] +
  beta[16] * gc[i] +
  
  #Optional interactions
  beta[17] * exh[i] * drc[i] +
  beta[18] * exh[i] * gc[i] +
  beta[19] * exh[i] * bm[i] +
  beta[20] * exh[i] * pr[i] +
  beta[21] * prc[i] * ntc[i] +
  beta[22] * prc[i] * pdc[i] +
  beta[23] * prc[i] * gn_e[i]

hdi_e[i] ~ dnorm(hdi[i], hdi_v[i]) 
gov_e[i] ~ dnorm(gov[i], gov_v[i]) 
re_e[i] ~ dnorm(re[i], re_v[i]) 
gn_e[i] ~ dnorm(gn[i], gn_v[i]) 
}

for (i in stab_min:inc_max) {
cens.pred[i] ~ dinterval(pt.pred[i], lim[i,])
cens[i] ~ dinterval(pt[i], lim[i,])
pt.pred[i] ~ dnorm(mod[i], tau * wg_a[i])
pt[i] ~ dnorm(mod[i], tau * wg_a[i])
mod[i] <- alpha + 
  mu.reg[reg[i]] + obs.site[cou[i]] + 
  mu.gen[gen[i]] + obs.spec[spec[i]] + 

  #Essential
  beta[1] * ar[i] + 
  beta[2] * exh[i] + 
  beta[3] * pdc[i] +
  beta[4] * prc[i] +
  beta[5] * bm[i] +
  beta[6] * gov_e[i] +
  beta[7] * pr[i] +
  
  #Optional
  beta[8] * drc[i] +
  beta[9] * ntc[i] +
  beta[10] * hdi_e[i] +
  beta[11] * hdc[i] +
  beta[12] * conf[i] +
  beta[13] * re_e[i] +
  beta[14] * ml[i] +
  beta[15] * gn_e[i] +
  beta[16] * gc[i] +
  
  #Optional interactions
  beta[17] * exh[i] * drc[i] +
  beta[18] * exh[i] * gc[i] +
  beta[19] * exh[i] * bm[i] +
  beta[20] * exh[i] * pr[i] +
  beta[21] * prc[i] * ntc[i] +
  beta[22] * prc[i] * pdc[i] +
  beta[23] * prc[i] * gn_e[i]

hdi_e[i] ~ dnorm(hdi[i], hdi_v[i]) 
gov_e[i] ~ dnorm(gov[i], gov_v[i]) 
re_e[i] ~ dnorm(re[i], re_v[i]) 
gn_e[i] ~ dnorm(gn[i], gn_v[i])
}

for(j in 1:reg_n){
mu.reg[j] ~ dnorm(0, tau.reg)
}
sd.reg ~ dunif(0.000000000001,100)
tau.reg = 1/pow(sd.reg, 2)

for(j in 1:cou_n){
obs.site[j] ~ dnorm(mu.reg[reg[j]], tau.cou)
}
sd.cou ~ dunif(0.000000000001,100)
tau.cou = 1/pow(sd.cou, 2)

#Nested species
for(k in 1:gen_n){
mu.gen[k] ~ dnorm(0, tau.gen)
}
sd.gen ~ dunif(0.000000000001,100)
tau.gen = 1/pow(sd.gen, 2)

for(k in 1:spec_n){
obs.spec[k] ~ dnorm(mu.gen[gen[k]], tau.spec)
}
sd.spec ~ dunif(0.000000000001,100)
tau.spec = 1/pow(sd.spec, 2)

#Intercept
alpha ~ dnorm(0, tau.alpha)
sd.alpha ~ dunif(0.000000000001,100)
tau.alpha = 1/(sd.alpha*sd.alpha)

#Slope
sd.gamma ~ dunif(0.000000000001,100)
tau.gamma = 1/(sd.gamma*sd.gamma)


for(l in 1:7){
  beta[l] ~ dnorm(0,tau.gamma)
}

for(l in 8:16){
  gamma[l] ~ dnorm(0,tau.gamma)
  delta[l] ~ dbern(p.select)
  beta[l]  <- gamma[l]*delta[l]
}
p.select ~ dbeta(2,8)

for(l in 17:23){
  gamma[l] ~ dnorm(0,tau.gamma)
  beta[l]  <- gamma[l]*delta[l]
}

int[1] = ifelse(delta[8] == 1, p.int, 0)
delta[17] ~ dbern(int[1])
int[2] = ifelse(delta[16] == 1, p.int, 0)
delta[18] ~ dbern(int[2])
delta[19] ~ dbern(p.int)
delta[20] ~ dbern(p.int)
int[3] = ifelse(delta[9] == 1, p.int, 0)
delta[21] ~ dbern(int[3])
delta[22] ~ dbern(p.int)
int[4] = ifelse(delta[15] == 1, p.int, 0)
delta[23] ~ dbern(int[4])
p.int ~ dbeta(2,8)



#Error in observations
sd.mod ~ dt(0, 0.0001, 1)T(0.0000000000001,)	
tau = 1/pow(sd.mod, 2)
}"





params = c(
  "alpha",
  "beta",
  "sd.alpha",
  "delta",
  "sd.gamma",
  "sd.reg",
  "sd.cou",
  "sd.gen",
  "sd.spec",
  "sd.mod",
  "p.select",
  "p.int",
  "mod",
  "pt",
  "mu.reg",
  "pt.pred",
  "obs.site",
  "mu.gen",
  "obs.spec",
  "gov_e",
  "hdi_e",
  "re_e",
  "gn_e"
)

iter = 150000
burn = 50000

Sys.time()
mod <- jags(
  model.file = textConnection(global),
  data = jagsdata_lag10, 
  n.chains = 3,
  n.iter = iter,
  n.burnin = burn,
  n.thin = 10,
  parameters.to.save = params)

saveRDS(mod, "full_mod.rds")
