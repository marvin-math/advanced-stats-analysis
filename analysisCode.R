library(tidyverse)
library(tidyr)
library("lavaan")
library(semPlot)
library(dplyr)
library(GPArotation)
library(MVN)
library(ggplot2)
library(buildmer)
library(brms)
library(BayesFactor)
library(jtools)
library(olsrr)



df_exp <- read.csv(file = '/Users/marvinmathony/Documents/UCL/AdvancedStats/data_49_experiment.csv')
df_pilot <- read.csv(file = '/Users/marvinmathony/Documents/UCL/AdvancedStats/data_49_pilot.csv')
df_pilot <- drop_na(df_pilot)

##### Hypothesis 1 #####

print("Position of missing values by column wise")
sapply(df_pilot, function(x) which(is.na(x)))
sapply(df_exp, function(x) which(is.na(x)))
# no need to exclude any data in pilot experiment

#descriptive stats
sum(df_pilot$gender==1) + sum(df_pilot$gender==2)
sum(df_pilot$gender==3)

# reverse code negative questions
#define columns to reverse code
reverse_cols = c("rel_identity_8", "rel_identity_9")
#reverse code the columns
df_pilot[ , reverse_cols] = 8 - df_pilot[ , reverse_cols]
constructs_df <- df_pilot %>% select(starts_with(c("se_", "rel_", "responsive", "intimate_", "satisfact_", "relation_length", "relation_type")))

#normality test
NormTest <- mvn(data = constructs_df, mvnTest = "royston", multivariatePlot = "qq")
NormTest

#descriptive stats
min(df_pilot$age, na.rm = TRUE)
max(df_pilot$age,na.rm = TRUE)
mean(df_pilot$age,na.rm = TRUE)
sd(df_pilot$age,na.rm = TRUE)

  ###### EFA ####
# no measurement errors
set.seed(20221221)
psych::fa.parallel(constructs_df, fa="fa")
# the analysis suggests 5 distinct factors for the data (corresponds with the number of
# questionnaires)

# exploratory factor model
exp_analysis <- psych::fa(constructs_df, nfactors = 5, fm="mle", rotate="oblimin")
exp_analysis
export_summs(exp_analysis,scale = TRUE, to.file = "docx", file.name = "test3.docx")

loadings(exp_analysis, cutoff = 0.32)
# factors are ordered by how much variance they explain
# note from lecture: Only explain regression factors that are larger than .10
# -> the results point towards the fact that shared experience is a distinct 
# psychological construct


###### CFA #####
## SHARED EXPERIENCE
data_se <- df_pilot %>% select(starts_with("se_"))
cfa_shared_exp <- '
  # factor specification
    SharedExp =~ se_share + se_antic + se_sync + se_persp + se_sim + se_realc + se_realev + se_cert
'
cfa_model_se <- lavaan::cfa(cfa_shared_exp, data=data_se)
cfa_model_se
summary(cfa_model_se, fit.measures=TRUE)
# in total, this model is not satisfactory
semPlot::semPaths(cfa_model_se, layout="tree", sizeMan=2, residuals=TRUE, rotation=1, whatLabels = "est", nCharNodes = 5, normalize=TRUE, width=12, height=6)

# modification indices:
lavaan::modificationIndices(cfa_model_se, minimum.value = 10, sort=TRUE)

# reestimate after modification indices
cfa_shared_exp_mod <- '
  # factor specification
    SharedExp =~ se_share + se_antic + se_sync + se_persp + se_sim + se_realc + se_realev + se_cert
se_antic ~~ se_sync
se_realev ~~ se_cert
se_realc ~~ se_realev
'
cfa_model_se_mod <- lavaan::cfa(cfa_shared_exp_mod, data=data_se)
summary(cfa_model_se_mod, fit.measures=TRUE)
fitted(cfa_model_se_mod)
#this model fits well
semPlot::semPaths(cfa_model_se_mod, layout="tree", sizeMan=6, residuals=TRUE, rotation=1, whatLabels = "est", nCharNodes = 5, normalize=TRUE, width=12, height=6)

# modification indices:
lavaan::modificationIndices(cfa_model_se_mod, minimum.value = 10, sort=TRUE)
# no modification indices left 



##### Hypothesis 2 #####
### composite scores
cmp_SE <- df_pilot %>% select(starts_with("se_"))
SE <- rowMeans(cmp_SE, na.rm=TRUE)

cmp_rel <- df_pilot %>% select(starts_with("rel_"))
rel_ident <- rowMeans(cmp_rel, na.rm=TRUE)

cmp_resp <- df_pilot %>% select(starts_with("responsive_"))
responsive <- rowMeans(cmp_resp, na.rm=TRUE)

cmp_int <- df_pilot %>% select(starts_with("intimate_"))
intimacy <- rowMeans(cmp_int, na.rm=TRUE)

cmp_sat <- df_pilot %>% select(starts_with("satisfact_"))
satisfact <- rowMeans(cmp_sat, na.rm=TRUE)

pilot_cmp <- cbind(SE,rel_ident,responsive,intimacy,satisfact)
pilot_cmp <- as.data.frame(pilot_cmp)

ggpairs(pilot_cmp)
### SEM with regression
gsem_spec <- '
  # factor specification
    SharedExp =~ se_share + se_antic + se_sync + se_persp + se_sim + se_realc + se_realev + se_cert
    RelIdent =~  rel_identity_1 + rel_identity_2 + rel_identity_3 + rel_identity_4 + rel_identity_5+
    rel_identity_6 + rel_identity_7 + rel_identity_8 + rel_identity_9 + rel_identity_10 + rel_identity_11
    PartResp =~  responsive_1 + responsive_2 + responsive_3 + responsive_4 + responsive_5 + responsive_6 +
    responsive_7 + responsive_8 + responsive_9 + responsive_10 + responsive_11 + responsive_12 +
    responsive_13 + responsive_14 + responsive_15 + responsive_16 + responsive_17
    Intimacy =~ intimate_1 + intimate_2 + intimate_3
    RelSat =~ satisfact_1 + satisfact_2 + satisfact_3 + satisfact_4 + satisfact_5 + satisfact_6

    
    RelSat ~ Intimacy + PartResp + SharedExp + RelIdent + relation_length
    

se_antic ~~ se_sync
se_realev ~~ se_cert
se_realc ~~ se_realev

'
gsem_mod <- lavaan::sem(gsem_spec, data=constructs_df)
summary(gsem_mod, fit.measures = TRUE, ci = TRUE)
#export_summs(gsem_mod,scale = TRUE, to.file = "docx", file.name = "test.docx")

semPlot::semPaths(gsem_mod, layout="tree", sizeMan=2, residuals=TRUE, rotation=1, whatLabels = "est", nCharNodes = 5, normalize=TRUE, width=12, height=6, label.cex = 1.1)
semPlot::semPaths(gsem_mod, whatLabels = "est", nCharNodes = 0, label.cex = 1.1)


## without covariance
gsem_spec_no_cov <- '
  # factor specification
    SharedExp =~ se_share + se_antic + se_sync + se_persp + se_sim + se_realc + se_realev + se_cert
    RelIdent =~  rel_identity_1 + rel_identity_2 + rel_identity_3 + rel_identity_4 + rel_identity_5+
    rel_identity_6 + rel_identity_7 + rel_identity_8 + rel_identity_9 + rel_identity_10 + rel_identity_11
    PartResp =~  responsive_1 + responsive_2 + responsive_3 + responsive_4 + responsive_5 + responsive_6 +
    responsive_7 + responsive_8 + responsive_9 + responsive_10 + responsive_11 + responsive_12 +
    responsive_13 + responsive_14 + responsive_15 + responsive_16 + responsive_17
    Intimacy =~ intimate_1 + intimate_2 + intimate_3
    RelSat =~ satisfact_1 + satisfact_2 + satisfact_3 + satisfact_4 + satisfact_5 + satisfact_6

    
    SharedExp ~~ 0*RelIdent + 0*PartResp + 0*Intimacy 
    RelSat ~ Intimacy + PartResp + SharedExp + RelIdent 


se_antic ~~ se_sync
se_realev ~~ se_cert
se_realc ~~ se_realev

'
gsem_mod_no_cov <- lavaan::sem(gsem_spec_no_cov, data=constructs_df)

anova(gsem_mod_no_cov, gsem_mod)
# add relationship length as covariate and exogenous variable

## exploratory analysis
exploratory_df <- pilot_cmp
exploratory_df$relation_length <- df_pilot$relation_length
exploratory_df$relation_type <- df_pilot$relation_type

explor_sem <- lm(satisfact ~ SE+ rel_ident + responsive + intimacy, data = pilot_cmp)
summary(explor_sem)

explor_sem2 <- lm(satisfact ~ SE+ rel_ident + responsive + intimacy + relation_length + relation_type, data = exploratory_df)
summary(explor_sem2)

modoverall_expl <- car::vif(explor_sem)
modoverall_expl_tolerance <- 1/modoverall_expl
modoverall_expl_tolerance
summary(cooks.distance(explor_sem))


####### Hypothesis 3 ########
#descriptive stats
min(df_exp$age, na.rm = TRUE)
max(df_exp$age,na.rm = TRUE)
mean(df_exp$age,na.rm = TRUE)
sd(df_exp$age,na.rm = TRUE)

sum(df_exp$gender==1) 
sum(df_exp$gender==2)
sum(df_exp$gender==3)

sum(df_exp$condition=="high") 
sum(df_exp$condition=="low") 

#CHECK
df_exp = drop_na(df_exp)
exp_data <- df_exp[, c("condition","dyad", "shared_view_1", "shared_view_2", "shared_view_3", "se")]
exp_data$condition <- unclass(exp_data$condition)
exp_data$condition <- as.factor(exp_data$condition)
exp_data$dyad <- as.factor(exp_data$dyad)
contrasts(exp_data$condition) <- c(1/2, -1/2)
contrasts(exp_data$condition)

# composite scores
exp_data$shared_overall <- rowMeans(exp_data[,c('shared_view_1', 'shared_view_2', 'shared_view_3')], na.rm=TRUE)
ggpairs(exp_data[,c("shared_view_1","shared_view_2","shared_view_3")])
sdamr::plot_raincloud(exp_data, shared_overall, group = condition) #+ facet_wrap(exp_data$condition)

###### buildmer ####
max_mod <- buildmer(center(shared_overall) ~ condition*center(se) + (center(se)|dyad), data=exp_data, buildmerControl = buildmerControl(include = ~ condition:center(se) + (1|dyad)))
mod_mixed <- afex::mixed(shared_overall ~ condition*center(se) + (1|dyad), data = exp_data, method="KR", check_contrasts = FALSE)
anova(mod_mixed)
summary(mod_mixed)
mod_mixed
confint(mod_mixed$full_model, method="boot")

emmeans::emmeans(mod_mixed, specs = pairwise ~ condition*center(se))

summary(mod_mixed)
confint(mod_mixed$full_model, oldNames=FALSE)
# plot model
exp_data$predicted <- predict(mod_mixed$full_model)

###### plot ####

ggplot(exp_data,aes(se, shared_overall, group=dyad, col=condition)) + 
  geom_smooth(aes(y=predicted), size=0.8, method="lm") +
  geom_point() + 
  theme_bw() +
  xlab("Shared Experience") + 
  ylab("Shared View")+
  scale_fill_discrete(breaks=c(1,2),labels=c("High","Low"))

###### model assumptions ####
# residual vs. predicted plot
ggplot(data.frame(predicted=predict(mod_mixed$full_model),residual=residuals(mod_mixed$full_model)),aes(x=predicted,y=residual)) + geom_point() + geom_hline(yintercept=0,lty=3)
# histogram of errors
ggplot(data.frame(residual=residuals(mod_mixed$full_model)),aes(x=residual)) + geom_histogram(colour="black",bins=20)
# qqplot
ggplot(tdat,aes(sample=residuals(mod_mixed$full_model))) + stat_qq() + stat_qq_line()
# histogram of estimated random effects
raneff <- lme4::ranef(mod_mixed$full_model)
hist(raneff$dyad$'(Intercept)')

# some skew, but not horrible
modoverall_vif <- car::vif(mod_mixed$full_model)
modoverall_tolerance <- 1/modoverall_vif
modoverall_tolerance
# no problem with tolerance/multicollinearity
summary(cooks.distance(mod_mixed$full_model))
# no problem with outliers

###### robustness ####
# lm
mod_mixed <- lm(shared_overall ~ condition+se, data = exp_data)
mod_mixed
summary(mod_mixed)
confint(mod_mixed, oldNames=FALSE)

# mixed only without interaction
mod_mixed_robst <- afex::mixed(shared_overall ~ condition+center(se) + (1|dyad), data = exp_data, method="KR", check_contrasts = FALSE)
anova(mod_mixed_robst)
summary(mod_mixed_robst)
confint(mod_mixed_robst$full_model, method="boot")

# lm including interaction
mod_ <- lm(shared_overall ~ condition*center(se), data = exp_data)
mod_mixed
summary(mod_mixed)
confint(mod_mixed, oldNames=FALSE)


##### Hypothesis 4 #####

hyp4_data <- df_exp[, c("id","dyad", "condition", "dec_joint_persp", "dec_effort_joint", "dec_participation", "se")]
hyp4_data$condition <- as.factor(exp_data$condition)
contrasts(hyp4_data$condition)
contrasts(hyp4_data$condition) <- c(-1/2, 1/2)

hyp4_data$dec_overall <- rowMeans(hyp4_data[,c("dec_joint_persp", "dec_effort_joint", "dec_participation")], na.rm=TRUE)

### Bayesian approach ###
mod2_priors <- c(# intercept
  prior(student_t(3, 4, 1), class = Intercept), 
  # slopes
  prior(normal(0.5, 0.5), class = b, coef = "condition1"),
  prior(normal(0, 0.5), class = b, coef = "centerse"),
  prior(normal(0, 0.5), class = b, coef = "condition1:centerse"),
  # error SD
  prior(exponential(1), class = sigma))

hyp4_dyad <- aggregate(.~dyad,hyp4_data,mean)
hyp4_dyad$condition = as.factor(hyp4_dyad$condition)
contrasts(hyp4_dyad$condition) <- c(-1/2, 1/2)

bayesian_hyp4_dyad <- brms::brm(dec_overall ~ condition*center(se), data = hyp4_dyad, seed=27122022, sample_prior=TRUE,prior = mod2_priors,
                           save_pars = save_pars(all = TRUE))
summary(bayesian_hyp4_dyad)
plot(bayesian_hyp4_dyad)
mcmc_plot(bayesian_hyp4_dyad, type="acf", variable=c("b_Intercept", "b_condition1", "b_centerse","b_condition1:centerse", "sigma"))
launch_shinystan(mod_bayesian_hyp4_dyad)
brms::as_draws_df(bayesian_hyp4_dyad)

###### compare to model with point-prior interaction effect ####
mod2_priors_no_inter <- c(# intercept
  prior(student_t(3, 4, 1), class = Intercept), 
  # slopes
  prior(normal(0.5, 0.5), class = b, coef = "condition1"),
  prior(normal(0, 0.5), class = b, coef = "centerse"),
  prior(constant(0), class = b, coef = "condition1:centerse"),
  # error SD
  prior(exponential(1), class = sigma))
bayesian_hyp4_dyad_no_inter <- brms::brm(dec_overall ~ condition*center(se), data = hyp4_dyad, seed=02012023, sample_prior=TRUE,prior = mod2_priors_no_inter,
                                         save_pars = save_pars(all = TRUE))
summary(bayesian_hyp4_dyad_no_inter)
marg_lik_mod_hyp4_dyad_no_inter <- brms::bridge_sampler(bayesian_hyp4_dyad_no_inter)
bridgesampling::bf(marg_lik_mod_hyp4_dyad, marg_lik_mod_hyp4_dyad_no_inter)





###### post-hoc analysis per group #####
se_mean <- mean(hyp4_dyad$se)
se_sd <- sd(hyp4_dyad$se)
hyp4_dyad2 <- hyp4_dyad
hyp4_dyad2$SE_group <- NA
hyp4_dyad2$SE_group[hyp4_dyad$se <= (se_mean - se_sd)] <- "below"
hyp4_dyad2$SE_group[hyp4_dyad$se >= (se_mean + se_sd)] <- "above"
hyp4_dyad2$SE_group <- as.factor(hyp4_dyad2$SE_group)
contrasts(hyp4_dyad2$SE_group) <- c(-1/2, 1/2)
contrasts(hyp4_dyad2$SE_group)
hyp4_dyad2 <- drop_na(hyp4_dyad2)
hyp4_dyad2$condition <- ifelse(hyp4_dyad2$condition == 1, "high", "low")
p<-ggplot(hyp4_dyad2, aes(x=SE_group, y=dec_overall, color=condition)) +
  geom_boxplot()+
  xlab("Shared Experience group") + 
  ylab("Joint Decision Behaviour")
p


posthoc_priors <- c(# intercept
  prior(student_t(3, 4, 1), class = Intercept), 
  # slopes
  prior(normal(0.5, 0.5), class = b, coef = "condition1"),
  prior(normal(0.5, 1), class = b, coef = "SE_group1"),
  prior(normal(-1, 1), class = b, coef = "condition1:SE_group1"),
  prior(exponential(1), class = sigma))

bayesian_hyp4_dyad_posthoc <- brms::brm(dec_overall ~ condition*SE_group, data = hyp4_dyad2, seed=27122022, sample_prior=TRUE,prior = posthoc_priors,
                                    save_pars = save_pars(all = TRUE))
summary(bayesian_hyp4_dyad_posthoc)
launch_shinystan(bayesian_hyp4_dyad_posthoc)
emmeans::emmeans(bayesian_hyp4_dyad_posthoc, specs = pairwise ~ condition:SE_group)

brms::as_draws_df(bayesian_hyp4_dyad_posthoc)

mcmc_plot(bayesian_hyp4_dyad_posthoc, type="acf", variable=c("b_Intercept", "b_condition1", "b_condition1:SE_group1", "sigma"))
plot(bayesian_hyp4_dyad_posthoc)


marg_lik_mod_hyp4_dyad_posthoc <- brms::bridge_sampler(bayesian_hyp4_dyad_posthoc)
fixef(bayesian_hyp4_dyad)[2]
launch_shinystan(bayesian_hyp4_dyad)
plot(bayesian_hyp4_dyad)

###### evidence post hoc #####
posthoc_priors_no_inter <- c(# intercept
  prior(student_t(3, 4, 1), class = Intercept), 
  # slopes
  prior(normal(0.5, 0.5), class = b, coef = "condition1"),
  prior(normal(0.5, 1), class = b, coef = "SE_group1"),
  prior(constant(0), class = b, coef = "condition1:SE_group1"),
  prior(exponential(1), class = sigma))
bayesian_hyp4_dyad_posthoc_no_inter <- brms::brm(dec_overall ~ condition*SE_group, data = hyp4_dyad2, seed=27122022, sample_prior=TRUE,prior = posthoc_priors_no_inter,
                                        save_pars = save_pars(all = TRUE))
summary(bayesian_hyp4_dyad_posthoc_no_inter)
marg_lik_mod_hyp4_dyad_posthoc_no_inter <- brms::bridge_sampler(bayesian_hyp4_dyad_posthoc_no_inter)
bridgesampling::bf(marg_lik_mod_hyp4_dyad_posthoc, marg_lik_mod_hyp4_dyad_posthoc_no_inter)
posterior(bayesian_hyp4_dyad_posthoc)






# compare to model without interaction and se:
bayesian_hyp4_dyad3 <- brms::brm(dec_overall ~ condition, data = hyp4_dyad, seed=02012023, sample_prior=TRUE,prior = mod2_priors,
                                 save_pars = save_pars(all = TRUE))
summary(bayesian_hyp4_dyad3)
marg_lik_mod_hyp4_dyad3 <- brms::bridge_sampler(bayesian_hyp4_dyad3)
bridgesampling::bf(marg_lik_mod_hyp4_dyad2, marg_lik_mod_hyp4_dyad3)

# compare to model with only intercept
bayesian_hyp4_dyad4 <- brms::brm(dec_overall ~ 1, data = hyp4_dyad, seed=02012023, sample_prior=TRUE,prior = mod2_priors,
                                 save_pars = save_pars(all = TRUE))
summary(bayesian_hyp4_dyad4)
marg_lik_mod_hyp4_dyad4 <- brms::bridge_sampler(bayesian_hyp4_dyad4)
bridgesampling::bf(marg_lik_mod_hyp4_dyad3, marg_lik_mod_hyp4_dyad4)

mcmc_plot(bayesian_hyp4_dyad, type="acf", variable=c("b_Intercept", "b_condition1", "b_centerse","b_condition1:centerse", "sigma"))
plot(bayesian_hyp4_dyad)
# no reason for concern of convergence of MCMC chain

max_mod <- buildmer(dec_overall ~ condition*center(se) + (condition*center(se)|dyad/id), data=hyp4_data)
summary(max_mod)

max_mod_dyad <- buildmer(dec_overall ~ condition*center(se) + (condition*center(se)|dyad), data=hyp4_dyad)
summary(max_mod_dyad)




## check each item individually whether behavior changes
# dec_joint_perspective
bayesian_hyp4_dyad_persp <- brms::brm(dec_joint_persp ~ condition*center(se), data = hyp4_dyad, seed=27122022, sample_prior=TRUE,prior = mod2_priors,
                                save_pars = save_pars(all = TRUE))
summary(bayesian_hyp4_dyad_persp)
contrasts(hyp4_dyad$condition)
# interaction effect
# fit indices look good
# se has negative estimate

# dec_effort_joint
bayesian_hyp4_dyad_effort <- brms::brm(dec_effort_joint ~ condition*center(se), data = hyp4_dyad, seed=27122022, sample_prior=TRUE,prior = mod2_priors,
                                      save_pars = save_pars(all = TRUE))
summary(bayesian_hyp4_dyad_effort)
# fit indices look good
# se has negative estimate
# interaction effect

# dec_participation
bayesian_hyp4_dyad_participation <- brms::brm(dec_participation ~ condition*center(se), data = hyp4_dyad, seed=27122022, sample_prior=TRUE,prior = mod2_priors,
                                      save_pars = save_pars(all = TRUE))
summary(bayesian_hyp4_dyad_participation)


bayesian_hyp4_participation <- brms::brm(dec_participation ~ condition*center(se), data = hyp4_data, seed=27122022, sample_prior=TRUE,prior = mod2_priors,
                                              save_pars = save_pars(all = TRUE))
summary(bayesian_hyp4_participation)

