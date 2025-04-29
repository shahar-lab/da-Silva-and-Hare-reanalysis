library(lme4)
library(car)
library(dplyr)
rm(list=ls())
load("df.Rdata")
names(df)


#####model-based estimates ----
ch.model <- glmer(stay1_bnd ~ trn_pv*rw_pv + (trn_pv*rw_pv | subj),data = df,family = binomial(link = "logit"))
summary(ch.model)

rt.model<-lmer(rt2~trn+(trn|subj),data=df)
summary(rt.model)
Anova(rt.model)

mb1<-ranef(ch.model)$subj[,'trn_pv:rw_pv']*-1   #made sure the direction is accurate by running dcast(df,subj  ~ rw_pv+trn_pv ,mean, value.var = c('stay1_bnd')) and then mb1=((mb1[,4]-mb1[,5])-(mb1[,2]-mb1[,3]))
mb2<-ranef(rt.model)$subj[,'trn']

library(ggplot2)
ggplot(data.frame(mb1,mb2), aes(x=mb1, y=mb2)) +
  geom_point(size=2,alpha=0.5,col='orange')+
  geom_smooth(method=lm)+
  ylab('first-stage choice')+
  xlab('second-stage reaction-times')

cor.test(mb1,mb2)

####outcome-irrelevant response-key
key.modelnull <- glmer(stay2_key ~ 1 + (rw_pv | subj),data = df[df$stg2_stat_rep==F,],family = binomial(link = "logit"))
key.model <- glmer(stay2_key ~ rw_pv + (rw_pv | subj),data = df[df$stg2_stat_rep==F,],family = binomial(link = "logit"))
summary(key.model)
anova(key.modelnull,key.model)




x<-
  df%>%
  filter(stg2_stat_rep==F)%>%
  mutate(stay2_key=stay2_key*1)%>%
  group_by(subj,rw_pv)%>%
  summarise(pStay_key=mean(stay2_key))%>%
  pivot_wider(names_from = 'rw_pv',values_from='pStay_key')

colMeans(x[,2:3])



key<-ranef(key.model)$subj[,'rw_pv']
plot(mb1,mb2)
cor.test(rowMeans(cbind(mb1,mb2)),key)

hist(key)
sum(key>0)/24

#### outcome-irrelevant ----------


library(brms)
f_model = stay2_key ~ rw_pv + (rw_pv | subj)

#set priors
get_prior(f_model, data = df[df$stg2_stat_rep==F,], family = bernoulli("logit")) 

myprior = c(
  prior(normal(0, 4),  class = Intercept),
  prior(normal(0, 4),  class = b        , coef = 'rw_pv')
)


#compile
model_compile = brm(f_model, 
                    data = df[df$stg2_stat_rep==F,],
                    chains = 0, 
                    prior = myprior, 
                    family = bernoulli("logit"),
                    backend='cmdstan')

#sample
model<-update(model_compile , 
              newdata   = df ,
              warmup = 3000,
              iter   = 5000,    
              cores  = 4,
              chains = 4,
              prior = myprior,
              family = bernoulli("logit"),
              backend='cmdstan')

bayestestR::describe_posterior(model, rope_range = c(-0.05,+0.05))
brms::conditional_effects(model)
save(model,file='./stay2key~reward_pv.rdata')

#plot posterior
library(ggdist)
params = insight::get_parameters(model)  
names(params)
params |>
  select(b_rw_pv) |>
  ggplot(aes(x = b_rw_pv)) +
  stat_halfeye() +
  xlab(expression(beta ~""["reward"])) +  
  xlim(-1,1)+
  ggtitle("")+
  theme_classic()+
  theme(axis.text.y  = element_blank(), 
        axis.ticks.y = element_blank())+
  ylab('density')


#### stay_key2_to1 ~reward ----
df |>
  group_by(rw_pv) |>
  summarise(mean(stay_key2to1))


##### stay_key2 ~ reward -----
df |> 
  mutate(stay_ch2 = lag(ch2) == ch2) |>
  filter(stg2_stat_rep) |>
  group_by(pair_rep2,
           rw_pv) |>
  summarise(mean(stay_ch2), n())
