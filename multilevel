# Installing and loading packages

pacotes <- c("plotly","tidyverse","reshape2","knitr","kableExtra",
             "nlme","lmtest","fastDummies","msm","lmeInfo","jtools")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}





setwd("/Users/robertorodrigues/Desktop/dsa/Solar_panel/")
tess <- read.csv("cluster_prep_level.csv", sep = ",", dec = ".", header = T)


# stderr_nlme -> function to help calculate Intraclass Correlation Coeficient - ICC

stderr_nlme <- function(model){
  if(base::class(model) != "lme"){
    base::message("Use a lme object model from nlme package")
    stop()}
  resume <- base::summary(model)
  if(base::length(base::names(model$groups))==1){
    m.type <- "HLM2"
  } else if(base::length(base::names(model$groups))==2){
    m.type <- "HLM3"
  }
  if(m.type == "HLM2"){
    vcov_matrix <- model$apVar
    logs_sd_re <- base::attr(vcov_matrix,"Pars")
    if(base::length(logs_sd_re)==2){
      stderr_tau00 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(`RE Components`=base::c("Var(v0j)","Var(e)"),
                                  `Variance Estimatives`= base::c(base::exp(logs_sd_re)[[1]]^2,
                                                                  base::exp(logs_sd_re[[2]])^2),
                                  `Std Err.`=base::c(stderr_tau00,
                                                     stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                            base::exp(logs_sd_re[[2]])^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                                                               base::exp(logs_sd_re[[2]])^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    }
    else{
      stderr_tau00 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_tau01 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x4)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(Components=base::c("Var(v0j)","Var(v1j)","Var(e)"),
                                  Estimatives= base::c(base::exp(logs_sd_re)[[1]]^2,
                                                       base::exp(logs_sd_re[[2]])^2,
                                                       base::exp(logs_sd_re[[4]])^2),
                                  Std_Err=base::c(stderr_tau00,
                                                  stderr_tau01,
                                                  stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                            base::exp(logs_sd_re[[2]])^2/stderr_tau01,
                                            base::exp(logs_sd_re[[4]])^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                                                               base::exp(logs_sd_re[[2]])^2/stderr_tau01,
                                                                               base::exp(logs_sd_re[[4]])^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    }
  }
  if(m.type == "HLM3"){
    vcov_matrix <- model$apVar
    logs_sd_re <-  base::attr(vcov_matrix,"Pars")
    if(base::length(logs_sd_re) == 3){
      stderr_tau_r000 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_tau_u000 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x3)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(Components=base::c("Var(t00k)","Var(v0jk)","Var(e)"),
                                  Estimatives=base::c(base::exp(logs_sd_re)[[2]]^2,
                                                      base::exp(logs_sd_re)[[1]]^2,
                                                      base::exp(logs_sd_re)[[3]]^2),
                                  Std_Err=base::c(stderr_tau_u000,
                                                  stderr_tau_r000,
                                                  stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[2]]^2/stderr_tau_u000,
                                            base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                            base::exp(logs_sd_re)[[3]]^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[2]]^2/stderr_tau_u000,
                                                                               base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                                                               base::exp(logs_sd_re)[[3]]^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    } 
    else{
      stderr_tau_r000 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_tau_r100 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      stderr_tau_u000 <- msm::deltamethod(~exp(x4)^2,logs_sd_re,vcov_matrix)
      stderr_tau_u100 <- msm::deltamethod(~exp(x5)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x7)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(`RE_Components`=base::c("Var(t00k)","Var(t10k)",
                                                          "Var(v0jk)","Var(v1jk)",
                                                          "Var(e)"),
                                  `Variance Estimatives`=base::c(base::exp(logs_sd_re)[[4]]^2,
                                                                 base::exp(logs_sd_re)[[5]]^2,
                                                                 base::exp(logs_sd_re)[[1]]^2,
                                                                 base::exp(logs_sd_re)[[2]]^2,
                                                                 base::exp(logs_sd_re)[[7]]^2),
                                  `Std Err.`=base::c(stderr_tau_u000,
                                                     stderr_tau_u100,
                                                     stderr_tau_r000,
                                                     stderr_tau_r100,
                                                     stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[4]]^2/stderr_tau_u000,
                                            base::exp(logs_sd_re)[[5]]^2/stderr_tau_u100,
                                            base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                            base::exp(logs_sd_re)[[2]]^2/stderr_tau_r100,
                                            base::exp(logs_sd_re)[[7]]^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[4]]^2/stderr_tau_u000,
                                                                               base::exp(logs_sd_re)[[5]]^2/stderr_tau_u100,
                                                                               base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                                                               base::exp(logs_sd_re)[[2]]^2/stderr_tau_r100,
                                                                               base::exp(logs_sd_re)[[7]]^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    }
  }
}

tess <- tess %>% dplyr::select(Country.Name, Year, Generation, everything())


# Visualizing the dataset
tess %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

# Descriptive statistics
summary(tess)



# time evolution of the energy generated by countries - graph 
ggplotly(
  tess %>%
    ggplot(aes(x = Year, y = Generation, group = 1, label = Country.Name)) +
    geom_point(color = "gold", size = 2, alpha = 0.6) +
    geom_smooth(color = "#440154FF", method = "lm", se = F, size = 2) +
    labs(x = "Year",
         y = "Solar Energy Generation") +
    theme_bw()
)



################################################################################
#               ESTIMATING NULL MODEL HLM3  - STEP-UP STRATEGY           #
################################################################################

#Estimação do modelo nulo (função lme do pacote nlme)
model_nulo_hlm3 <- lme(fixed = Generation ~ 1,
                       random = list(Cluster = ~1, Country.Name = ~1),
                       data = tess,
                       method = "REML")

#Parâmetros do modelo
summary(model_nulo_hlm3)

#Erros-padrão do modelo por meio da função 'stderr_nlme' desenvolvida
stderr_nlme(model_nulo_hlm3)


################################################################################
#                      HLM3 FAILED - TRY HLM2                        #
################################################################################

#Estimação do modelo nulo (função lme do pacote nlme)
model_nulo_hlm2 <- lme(fixed = Generation ~ 1, 
                       random = ~ 1 | Country.Name,
                       data = tess,
                       method = "REML")

#Parameters
summary(model_nulo_hlm2)

#ICC
stderr_nlme(model_nulo_hlm2)



################################################################################


#Estimação do modelo com Interceptos Aleatórios
model_intercept_hlm2 <- lme(fixed = Generation ~ Year,
                            random = ~ 1 | Country.Name,
                            data = tess,
                            method = "REML")



#Parameters
summary(model_intercept_hlm2)

#ICC
stderr_nlme(model_intercept_hlm2)



################################################################################


#Estimação do modelo com Interceptos e Inclinações Aleatórios
model_intercept_inclin_hlm2 <- lme(fixed = Generation ~ Year,
                                   random = ~ Year | Country.Name,
                                   data = tess,
                                   method = "REML")

#Parameters
summary(model_intercept_inclin_hlm2)

#ICC
stderr_nlme(model_intercept_inclin_hlm2)

################################################################################

#Estimating final model  
model_final_hlm2 <- lme(fixed = Generation ~ Year
                        + Access.to.clean.fuels.and.technologies.for.cooking....of.population.
                        + Access.to.electricity....of.population.
                        + Adolescent.fertility.rate..births.per.1.000.women.ages.15.19.
                        + Age.dependency.ratio....of.working.age.population.
                        + Air.transport..passengers.carried
                        + Armed.forces.personnel....of.total.labor.force.
                        + Birth.rate..crude..per.1.000.people.
                        # + Commercial.bank.branches..per.100.000.adults.
                        + Compulsory.education..duration..years.
                        + Consumer.price.index..2010...100.
                        + Current.health.expenditure.per.capita..current.US..
                        + Death.rate..crude..per.1.000.people.
                        + Exports.of.goods.and.services..annual...growth.
                        + Fertility.rate..total..births.per.woman.
                        + Final.consumption.expenditure..current.US..
                        + Food.production.index..2014.2016...100.
                        + Foreign.direct.investment..net..BoP..current.US..
                        + Forest.area..sq..km.
                        + GDP..current.US..
                        + GDP.per.capita..current.US..
                        + General.government.final.consumption.expenditure..current.US..
                        + Gini.index
                        + Goods.exports..BoP..current.US..
                        # + Goods.imports..BoP..current.US..
                        + Gross.savings..current.US..
                        + High.technology.exports..current.US..
                        + Hospital.beds..per.1.000.people.
                        # + Households.and.NPISHs.Final.consumption.expenditure..current.US..
                        + Individuals.using.the.Internet....of.population.
                        + Inflation..consumer.prices..annual...
                        + Labor.force..total
                        + Land.area..sq..km.
                        + Life.expectancy.at.birth..total..years.
                        + Military.expenditure..current.USD.
                        + Mobile.cellular.subscriptions
                        + Net.trade.in.goods..BoP..current.US..
                        + New.businesses.registered..number.
                        + Patent.applications..nonresidents
                        + Patent.applications..residents
                        + People.practicing.open.defecation....of.population.
                        + People.using.at.least.basic.drinking.water.services....of.population.
                        + Population.in.urban.agglomerations.of.more.than.1.million
                        + Population..total
                        + Procedures.to.build.a.warehouse..number.
                        + Procedures.to.register.property..number.
                        + Renewable.energy.consumption....of.total.final.energy.consumption.
                        + Research.and.development.expenditure....of.GDP.
                        + Rural.population
                        + Sex.ratio.at.birth..male.births.per.female.births.
                        + Surface.area..sq..km.
                        + Time.required.to.get.electricity..days.
                        + Unemployment..total....of.total.labor.force...modeled.ILO.estimate.
                        + Unemployment..total....of.total.labor.force...national.estimate.
                        # + Urban.population
                        + Year:Access.to.clean.fuels.and.technologies.for.cooking....of.population.
                        + Year:Access.to.electricity....of.population.
                        + Year:Adolescent.fertility.rate..births.per.1.000.women.ages.15.19.
                        + Year:Age.dependency.ratio....of.working.age.population.
                        + Year:Air.transport..passengers.carried
                        + Year:Armed.forces.personnel....of.total.labor.force.
                        + Year:Birth.rate..crude..per.1.000.people.
                        # + Year:Commercial.bank.branches..per.100.000.adults.
                        + Year:Compulsory.education..duration..years.
                        + Year:Consumer.price.index..2010...100.
                        + Year:Current.health.expenditure.per.capita..current.US..
                        + Year:Death.rate..crude..per.1.000.people.
                        + Year:Exports.of.goods.and.services..annual...growth.
                        + Year:Fertility.rate..total..births.per.woman.
                        + Year:Final.consumption.expenditure..current.US..
                        + Year:Food.production.index..2014.2016...100.
                        + Year:Foreign.direct.investment..net..BoP..current.US..
                        + Year:Forest.area..sq..km.
                        + Year:GDP..current.US..
                        + Year:GDP.per.capita..current.US..
                        + Year:General.government.final.consumption.expenditure..current.US..
                        + Year:Gini.index
                        + Year:Goods.exports..BoP..current.US..
                        # + Year:Goods.imports..BoP..current.US..
                        + Year:Gross.savings..current.US..
                        + Year:High.technology.exports..current.US..
                        + Year:Hospital.beds..per.1.000.people.
                        # + Year:Households.and.NPISHs.Final.consumption.expenditure..current.US..
                        + Year:Individuals.using.the.Internet....of.population.
                        + Year:Inflation..consumer.prices..annual...
                        + Year:Labor.force..total
                        + Year:Land.area..sq..km.
                        + Year:Life.expectancy.at.birth..total..years.
                        + Year:Military.expenditure..current.USD.
                        + Year:Mobile.cellular.subscriptions
                        + Year:Net.trade.in.goods..BoP..current.US..
                        + Year:New.businesses.registered..number.
                        + Year:Patent.applications..nonresidents
                        + Year:Patent.applications..residents
                        + Year:People.practicing.open.defecation....of.population.
                        + Year:People.using.at.least.basic.drinking.water.services....of.population.
                        + Year:Population.in.urban.agglomerations.of.more.than.1.million
                        + Year:Population..total
                        + Year:Procedures.to.build.a.warehouse..number.
                        + Year:Procedures.to.register.property..number.
                        + Year:Renewable.energy.consumption....of.total.final.energy.consumption.
                        + Year:Research.and.development.expenditure....of.GDP.
                        + Year:Rural.population
                        + Year:Sex.ratio.at.birth..male.births.per.female.births.
                        + Year:Surface.area..sq..km.
                        + Year:Time.required.to.get.electricity..days.
                        + Year:Unemployment..total....of.total.labor.force...modeled.ILO.estimate.
                        + Year:Unemployment..total....of.total.labor.force...national.estimate.
                        # + Year:Urban.population
                        ,
                        
                        random = ~ Year | Country.Name,
                        data = tess,
                        method = "REML")

#Parameters
summary(model_final_hlm2)

#ICC
stderr_nlme(model_final_hlm2)


# Random intercept and slopes by country
# for final model HLM2

v2_final <- data.frame(model_final_hlm2[["coefficients"]][["random"]][["Country.Name"]]) %>%
  rename(v00 = 1,
         v10 = 2)
v2_final$Country.Name <- c(1:70)
v2_final$Country.Name <- as.factor(v2_final$Country.Name)

v2_final %>% 
  select(Country.Name, everything()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)
