pacman::p_load(LSTbook, plotly, tidyverse, purrr)

data=read.xlsx("Original Data/CDC Natality 2022")

base_rate = mean(data$hbp_gest == "Y", na.rm=TRUE)
smpl_n=20000
# 8% of women had gestational hypertension 

# Parameters #
n_max       = 500
intrm       = 3
sims        = 1000
alpha_eff   = .975
aplha_fut   = 0.05
prior_alpha = base_rate * (smpl_n/10) + 1 #scale down prior strength by dividing
prior_beta  = (1-base_rate) * (sampl_n/10) +1 #scale down prior strength by dividing
#Power= # simulation that stop for efficacy/total

# Trial Setup #
simulate_trial = function(true_effect){
  stop = NA; n = NA
  for (i in 1:intrm){
    n_arm   = i*(n_max/intrm)
    control = rbinom(n_arm, 1, base_rate)
    treat   = rbinom(n_arm, 1, base_rate + true_effect)
    
    post_c  = sum(control) + prior_alpha
    post_nc = n_arm - sum(control) + prior_beta
    post_t  = sum(treat) + prior_alpha
    post_nt = n_arm-sum(treat)+ prior_beta
    
    prob_eff = pbeta(0.5,
      post_t, post_nt, lower.tail=FALSE)/
      (pbeta(0.5, post_t, post_nt, lower.tail = FALSE) +
         pbeta(0.5, post_c, post_nc, lower.tail = TRUE))
    if (prob_eff > alpha_eff) {stop = "eff"; n = 2*n_arm; break}
    if (prob_eff < alpha_fut) {stop = "fut";  n = 2*n_arm; break}
  }
  if(is.na(stop)) {stop = "Cont"; n= 2*n_max}
  list(decision=stop, n=n)
}

# Simulations # 

result = replicate(sims, simulate_trial(true_effect = 0.05), simplify=FALSE)
res_df = tibble(
  decision = map_chr(result, "decision"),
  sample_n = map_dbl(result, "n")
)

res_df %>% count(decision)%>% mutate(p = n/sims)
res_df %>% summarise(mean_n = mean(sample_n))


p=ggplot(res_df, aes(sample_n, fill = decision)) +
  geom_histogram(binwidth = 10, position = "stack") +
  labs(title = "Sample Size by Trial Outcome", x = "Total Sample Size") + theme_classic()
ggplotly(p)

