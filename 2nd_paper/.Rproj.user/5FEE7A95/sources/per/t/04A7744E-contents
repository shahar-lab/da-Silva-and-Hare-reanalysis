rm(list=ls())
df = read.csv('./fmri_magic_carpet-main/fmri_magic_carpet-main/code/analysis/beh_noslow.csv')
names(df)
library(dplyr)
df = 
  df |>
  mutate(reward        = factor(reward, levels = c(0,1), labels = c("unrewarded", "rewarded")),
         reward_oneback = lag(reward),
         transition         = factor(common, levels = c(0,1), labels = c("rare", "common")),
         transition_oneback = lag(transition),
         state2 = final_state,
         mapping_state1 = isymbol_lft,
         mapping_state2 = fsymbol_lft,
         key1 = (choice1==isymbol_lft)*1, #1 for left, 0 for right
         key2 = (choice2==fsymbol_lft)*1, #1 for left, 0 for right
         stay_ch1       = lag(choice1)==choice1,
         stay_ch2       = lag(choice2)==choice2,
         stay_key1      = (lag(key1)==key1)*1, 
         stay_key2      = (lag(key2)==key2)*1, 
         stay_key2_to_1 = (lag(key2)==key1)*1, #check coding!!
         state2_repeat  = (lag(state2) == state2),
         state2_mapping_repeat = mapping_state2 == lag(mapping_state2)
         )
##### stay_key2_to_1 ~ reward -----
df |>
  group_by(condition,
           reward_oneback) |>
  summarise(mean(stay_key2_to_1))



f_model = stay_key2_to_1 ~ reward_oneback*condition + (reward_oneback | participant)
get_prior(f_model, data = df, family = bernoulli("logit")) 

myprior = c(
  prior(normal(0, 4),  class = Intercept),
  prior(normal(0, 4),  class = b        , coef = 'conditionstory'),
  prior(normal(0, 4),  class = b        , coef = 'reward_onebackrewarded'),
  prior(normal(0, 4),  class = b        , coef = 'reward_onebackrewarded:conditionstory')
)


#compile
model_compile = brm(f_model, data = df ,chains = 0, family = bernoulli("logit"), prior = myprior, backend='cmdstan')

#sample
model<-update(model_compile , 
              newdata   = df,
              warmup = 3000,
              iter   = 5000,    
              cores  = 4,
              chains = 4,
              #prior = myprior,
              #family = bernoulli("logit"),
              backend='cmdstan')
#save(model,file='./stay_key2_to_1 ~ reward_onebackXcondition.rdata')

#examine your model
load('./stay_key2_to_1 ~ reward_onebackXcondition.rdata')
bayestestR::describe_posterior(model, rope_range = c(-0.1,+0.1))
brms::conditional_effects(model)

#plot posterior
library(ggdist)
library(ggplot2)
library(emmeans)
library(tidybayes)

model |>
  emmeans(~ reward_oneback,
          at = list(condition = 'story'),
          epred = T,
          level = 0.90,
          re_formula = NA) |>
  contrast(method = 'revpairwise') |>
  gather_emmeans_draws()|>
  ggplot(aes(x=.value))+
  stat_halfeye()+
  xlab(expression(beta ~""["reward (only story)"])) +  
  xlim(-0.5,0.5)+
  ggtitle("")+
  theme_classic()+
  theme(axis.text.y  = element_blank(), 
        axis.ticks.y = element_blank())+
  ylab('density')

params = insight::get_parameters(model)  
names(params)
data.frame(x = params[,'b_reward_onebackrewarded:conditionstory'])|>
  ggplot(aes(x = x)) +
  stat_halfeye() +
  xlab(expression(beta ~""["reward X condition"])) +  
  xlim(-0.5,0.5)+
  ggtitle("")+
  theme_classic()+
  theme(axis.text.y  = element_blank(), 
        axis.ticks.y = element_blank())+
  ylab('density')





#### stay ~ reward X transition ----
df |> 
  group_by(condition,
           transition_oneback,
           reward_oneback) |>
  summarise(mean(stay_ch1))


##### stay_key2 ~ reward -----
df |> 
  filter(!state2_repeat) |>
  group_by(condition,
           reward_oneback) |>
  summarise(mean(stay_key2))

library(brms)

f_model = stay_key2 ~ reward_oneback*condition + (reward_oneback | participant)
get_prior(f_model, data = df |> filter(!state2_repeat), family = bernoulli("logit")) 

myprior = prior(normal(0, 4), class = b)+
          prior(normal(0, 5), class = Intercept)
# myprior = c(
#   prior(normal(0, 4),  class = Intercept),
#   prior(normal(0, 4),  class = b        , coef = 'conditionstory'),
#   prior(normal(0, 4),  class = b        , coef = 'reward_onebackrewarded'),
#   prior(normal(0, 4),  class = b        , coef = 'reward_onebackrewarded:conditionstory')
# )
# 

#compile
model_compile = brm(f_model, data = df |> filter(!state2_repeat) ,chains = 0, 
                    family = bernoulli("logit"),
                    prior = myprior, backend='cmdstan')

prior_summary(model_compile) 

#sample
model<-update(model_compile , 
              newdata   = df |> filter(!state2_repeat) ,
              warmup = 3000,
              iter   = 5000,    
              cores  = 4,
              chains = 4,
              #prior = myprior,
              #family = bernoulli("logit"),
              backend='cmdstan')


#save(model,file='./stay_key2 ~ reward_onebackXcondition.rdata')

#examine your model
load('./stay_key2 ~ reward_onebackXcondition.rdata')
bayestestR::describe_posterior(model, rope_range = c(-0.1,+0.1))
brms::conditional_effects(model)



#plot posterior
library(ggdist)
library(ggplot2)
library(emmeans)
library(tidybayes)

model |>
  emmeans(~ reward_oneback,
          at = list(condition = 'story'),
          epred = T,
          level = 0.89,
          re_formula = NA) |>
  contrast(method = 'pairwise') |>
  gather_emmeans_draws()|>
  ggplot(aes(x=.value))+
  stat_halfeye()+
  xlab(expression(beta ~""["reward (only story)"])) +  
  xlim(-0.5,0.5)+
  ggtitle("")+
  theme_classic()+
  theme(axis.text.y  = element_blank(), 
        axis.ticks.y = element_blank())+
  ylab('density')
  
params = insight::get_parameters(model)  
names(params)
data.frame(x = params[,'b_reward_onebackrewarded:conditionstory'])|>
  ggplot(aes(x = x)) +
  stat_halfeye() +
  xlab(expression(beta ~""["reward X condition"])) +  
  xlim(-0.5,0.5)+
  ggtitle("")+
  theme_classic()+
  theme(axis.text.y  = element_blank(), 
        axis.ticks.y = element_blank())+
  ylab('density')



##### stay_key2 ~ reward -----
df |> 
  filter(state2_repeat) |>
  group_by(condition,
           state2_mapping_repeat,
           reward_oneback) |>
  summarise(mean(stay_ch2))


f_model = stay_ch2 ~ reward_oneback*state2_mapping_repeat*condition + (reward_oneback*state2_mapping_repeat | participant)
get_prior(f_model, data = df, family = bernoulli("logit")) 

myprior = c(
  prior(normal(0, 4),  class = Intercept),
  prior(normal(0, 4),  class = b        , coef = 'conditionstory'),
  prior(normal(0, 4),  class = b        , coef = 'reward_onebackrewarded'),
  prior(normal(0, 4),  class = b        , coef = 'reward_onebackrewarded:conditionstory'),
  prior(normal(0, 4),  class = b        , coef = 'reward_onebackrewarded:state2_mapping_repeatTRUE'),
  prior(normal(0, 4),  class = b        , coef = 'reward_onebackrewarded:state2_mapping_repeatTRUE:conditionstory'),
  prior(normal(0, 4),  class = b        , coef = 'state2_mapping_repeatTRUE'),
  prior(normal(0, 4),  class = b        , coef = 'state2_mapping_repeatTRUE:conditionstory')
  
)


#compile
model_compile = brm(f_model, data = df ,chains = 0, family = bernoulli("logit"), prior = myprior, backend='cmdstan')

#sample
model<-update(model_compile , 
              newdata   = df,
              warmup = 3000,
              iter   = 5000,    
              cores  = 4,
              chains = 4,
              #prior = myprior,
              #family = bernoulli("logit"),
              backend='cmdstan')

#examine your model
##load('./stay_ch2 ~ reward_oneback*state2_mapping_repeat*conditionrdata')
bayestestR::describe_posterior(model, rope_range = c(-0.1,+0.1))
brms::conditional_effects(model)
conditional_effects(model,effects = "reward_oneback:state2_mapping_repeat", 
                    conditions = make_conditions(model, "condition"))

# Plot the conditional effects
plot(effects, points = TRUE) 
##save(model,file='./stay_ch2 ~ reward_onebackXstate2_mapping_repeatXcondition.rdata')
#plot posterior
library(ggdist)
library(ggplot2)
params = insight::get_parameters(model)  
names(params)
data.frame(x = params[,'b_reward_onebackrewarded:state2_mapping_repeatTRUE:conditionstory'])|>
  ggplot(aes(x = x)) +
  stat_halfeye() +
  xlab(expression(beta ~""["reward X mapping_switch X condition"])) +  
  xlim(-0.5,0.5)+
  ggtitle("")+
  theme_classic()+
  theme(axis.text.y  = element_blank(), 
        axis.ticks.y = element_blank())+
  ylab('density')




#### mb - mmf correlation ----
x = coef(model)$participant
head(x)
