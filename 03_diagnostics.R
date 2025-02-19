#posterior predicitve checks
load("mod_cent.RData")
load("mod_std.RData")
load("mod_org.RData")

launch_shinystan(mod_std)
launch_shinystan(mod_cent)
launch_shinystan(mod_org)

#pp_checks
pp_check(mod_cent, resp = "spring")
pp_check(mod_cent, resp = "fall")
pp_check(mod_cent, resp = "success")

pp_check(mod_cent, type = "stat_grouped", stat = "mean", group = "yr", resp = "spring")
pp_check(mod_cent, type = "stat_grouped", stat = "mean", group = "yr", resp = "success")
pp_check(mod_cent, type = "stat_grouped", stat = "mean", group = "yr", resp = "fall")

pp_check(mod_cent, type = "stat_2d", stat = c("max", "min"), resp = "spring")
pp_check(mod_cent, type = "stat_2d", stat = c("max", "min"), resp = "success")
pp_check(mod_cent, type = "stat_2d", stat = c("max", "min"), resp = "fall")

pp_check(mod_cent, resp = "spring", type = "ribbon_grouped", x = "prev_fall_cent", group = "yr")
pp_check(mod_cent, resp = "spring", type = "ribbon", x = "prev_fall_cent")
pp_check(mod_cent, resp = "spring", type = "ribbon", x = "winter_dens_cent")
pp_check(mod_cent, resp = "spring", type = "ribbon_grouped", x = "winter_dens_cent", group = "yr")
pp_check(mod_cent, resp = "spring", type = "ribbon", x = "max_snow_m_cent")
pp_check(mod_cent, resp = "spring", type = "ribbon_grouped", x = "max_snow_m_cent", group = "yr")

pp_check(mod_cent, resp = "fall", type = "ribbon_grouped", x = "prev_fall_cent", group = "yr")
pp_check(mod_cent, resp = "fall", type = "ribbon", x = "prev_fall_cent")
pp_check(mod_cent, resp = "fall", type = "ribbon_grouped", x = "spring_std", group = "yr")
pp_check(mod_cent, resp = "fall", type = "ribbon", x = "prev_fall_cent")
pp_check(mod_cent, resp = "fall", type = "ribbon", x = "spring_cent")
pp_check(mod_cent, resp = "fall", type = "ribbon", x = "summer_dens_cent")
pp_check(mod_cent, resp = "fall", type = "ribbon", x = "max_snow_m_cent")
pp_check(mod_cent, resp = "fall", type = "ribbon", x = "spr_ten_cent")
pp_check(mod_cent, resp = "fall", type = "ribbon", x = "max_hundred_cent")

pp_check(mod_cent, resp = "success", type = "ribbon", x = "spring_cent")
pp_check(mod_cent, resp = "success", type = "ribbon", x = "winter_dens_cent")
pp_check(mod_cent, resp = "success", type = "ribbon", x = "max_snow_m_cent")
pp_check(mod_cent, resp = "success", type = "ribbon", x = "spr_ten_cent")
pp_check(mod_cent, resp = "success", type = "ribbon", x = "max_hundred_cent")

pp_check(mod_cent, type = "error_scatter_avg_vs_x", size = 1.1, x = "prev_fall_cent", resp = "spring")
pp_check(mod_cent, type = "error_scatter_avg_vs_x", size = 1.1, x = "winter_dens_cent", resp = "spring")
pp_check(mod_cent, type = "error_scatter_avg_vs_x", size = 1.1, x = "max_snow_m_cent", resp = "spring")

pp_check(mod_cent, type = "error_scatter_avg_vs_x", size = 1.1, x = "winter_dens_cent", resp = "success")
pp_check(mod_cent, type = "error_scatter_avg_vs_x", size = 1.1, x = "max_snow_m_cent", resp = "success")
pp_check(mod_cent, type = "error_scatter_avg_vs_x", size = 1.1, x = "spr_ten_cent", resp = "success")
pp_check(mod_cent, type = "error_scatter_avg_vs_x", size = 1.1, x = "spring_cent", resp = "success")

pp_check(mod_cent, type = "error_scatter_avg_vs_x", size = 1.1, x = "max_hundred_cent", resp = "success")

pp_check(mod_cent, type = "error_scatter_avg_vs_x", size = 1.1, x = "prev_fall_cent", resp = "fall")
pp_check(mod_cent, type = "error_scatter_avg_vs_x", size = 1.1, x = "spring_cent", resp = "fall")
pp_check(mod_cent, type = "error_scatter_avg_vs_x", size = 1.1, x = "max_snow_m_cent", resp = "fall")
pp_check(mod_cent, type = "error_scatter_avg_vs_x", size = 1.1, x = "summer_dens_cent", resp = "fall")
pp_check(mod_cent, type = "error_scatter_avg_vs_x", size = 1.1, x = "spr_ten_cent", resp = "fall")
pp_check(mod_cent, type = "error_scatter_avg_vs_x", size = 1.1, x = "max_hundred_cent", resp = "fall")

pp_check(mod_cent, type = "stat", stat = 'median', nsamples = 1000, resp = "spring")
pp_check(mod_cent, type = "stat", stat = 'median', nsamples = 1000, resp = "fall")

pp_check(mod_cent, type = "stat", stat = 'mean', nsamples = 1000, resp = "spring")
pp_check(mod_cent, type = "stat", stat = 'mean', nsamples = 1000, resp = "fall")
pp_check(mod_cent, type = "stat", stat = 'mean', nsamples = 1000, resp = "success")

pp_check(mod_cent, type = "stat", stat = 'sd', nsamples = 1000, resp = "spring")
pp_check(mod_cent, type = "stat", stat = 'sd', nsamples = 1000, resp = "fall")
pp_check(mod_cent, type = "stat", stat = 'sd', nsamples = 1000, resp = "success")

pp_check(mod_cent, type = "stat", stat = 'min', nsamples = 1000, resp = "spring")
problem_1 <- pp_check(mod_cent, type = "stat", stat = 'min', nsamples = 1000, resp = "fall")

pp_check(mod_cent, type = "stat", stat = 'max', nsamples = 1000, resp = "spring")
problem_2 <- pp_check(mod_cent, type = "stat", stat = 'max', nsamples = 1000, resp = "fall")

pp_pred <- posterior_predict(mod_org.logit,ndraws = 50)


ppc_winter_dens_overlay(y = data$spring, yrep = pp_pred[,,"spring"])
ppc_boxplot(y = data$spring, yrep = pp_pred[,,"spring"])
ppc_ecdf_overlay(y = data$spring, yrep = pp_pred[,,"spring"])
ppc_error_scatter_avg(y = data$spring, yrep = pp_pred[,,"spring"])
ppc_intervals(y = data$spring, yrep = pp_pred[,,"spring"])
ppc_ribbon(y = data$spring, yrep = pp_pred[,,"spring"])
ppc_scatter_avg(y = data$spring, yrep = pp_pred[,,"spring"])
ppc_stat(y = data$spring, yrep = pp_pred[,,"spring"])
ppc_stat_2d(y = data$spring, yrep = pp_pred[,,"spring"])
pp_check(mod_org.logit, resp = "spring") 
pp_check(mod_org.logit, resp = "spring", type = "error_hist", ndraws = 11)
pp_check(mod_org.logit, resp = "spring", type = "stat_2d")
pp_check(mod_org.logit, resp = "spring", type = "loo_pit")


ppc_summer_dens_overlay(y=data$fall, yrep=pp_pred[,,"fall"])
ppc_boxplot(y=data$fall, yrep=pp_pred[,,"fall"])
ppc_ecdf_overlay(y=data$fall, yrep=pp_pred[,,"fall"])
ppc_error_scatter_avg(y=data$fall, yrep=pp_pred[,,"fall"])
ppc_intervals(y=data$fall, yrep=pp_pred[,,"fall"])
ppc_ribbon(y=data$fall, yrep=pp_pred[,,"fall"])
ppc_scatter_avg(y=data$fall, yrep=pp_pred[,,"fall"])
ppc_stat(y=data$fall, yrep=pp_pred[,,"fall"])
ppc_stat_2d(y=data$fall, yrep=pp_pred[,,"fall"])
pp_check(mod_org.logit, resp = "fall")  
pp_check(mod_org.logit, resp = "fall", type = "error_hist", ndraws = 11)
pp_check(mod_org.logit, resp = "fall", type = "stat_2d")
pp_check(mod_org.logit, resp = "fall", type = "loo_pit")


ppc_summer_dens_overlay(y=data$fall, yrep=pp_pred[,,"fall"])
ppc_winter_dens_overlay(y=data$fall, yrep=pp_pred[,,"success"])
ppc_bars(y=data$success, yrep=pp_pred[,,"success"])
ppc_boxplot(y=data$success, yrep=pp_pred[,,"success"])
 
load("mod_std.RData")
loo_mod_std  <- loo(mod_std) #checking loo
fixef(mod_std)
loo_cent<- loo(mod_cent)
loo_org <- loo(mod_org.logit)
print(mod_org.logit)

#Check observations wioth high pareto K
pareto_k_ids(loo_mod_std, threshold = 0.7)
pareto_k_ids(loo_org, threshold = 0.7)
pareto_k_ids(loo_cent, threshold = 0.7)

paretok1 <- data %>% 
  filter(individ_dbid == 5142 & yr == 2005) #std
paretok2 <- data  %>% 
  filter(individ_dbid == 5228 & yr == 2006) #std
paretok3 <- data  %>% 
  filter(individ_dbid == 5288 & yr == 2007) #std
paretok4 <- data  %>% 
  filter(individ_dbid == 5266 & yr == 2015) #std
paretok5 <- data  %>% 
  filter(individ_dbid == 5054 & yr == 2016) #std
paretok6 <- data  %>% 
  filter(individ_dbid == 10617 & yr == 2015) #std
paretok7 <- data %>% 
  filter(individ_dbid ==5030 & yr == 2004)
paretok8 <- data  %>% 
  filter(individ_dbid == 5030 & yr == 2005)
paretok9 <- data  %>% 
  filter(individ_dbid == 5314 & yr == 2008)
paretok10 <- data %>% 
  filter(individ_dbid == 5376 & yr == 2006)

paretok <- rbind(paretok1, paretok2, paretok3, paretok4, paretok5, paretok6, #paretok7, paretok8, paretok9
                 )
paretok <- paretok%>%
  purrr::modify_at(c("individ_dbid"), factor)

spring_winter_dens <- ggplot(data, aes(winter_dens, spring)) + geom_point(alpha = 0.1) +
  geom_point(data = paretok, aes(x = winter_dens, y = spring, color = individ_dbid), show.legend = FALSE, size =2)+
  theme_gray(base_size = 8)

spring_snow <- ggplot(data, aes(max_snow_m, spring)) + geom_point(alpha = 0.1) +
  geom_point(data = paretok, aes(x = max_snow_m, y = spring, color = individ_dbid), size =2, show.legend = FALSE)+
  theme_gray(base_size = 8)

spring_prev_fall <- ggplot(data, aes(prev_fall, spring)) + geom_point(alpha = 0.1) +
  geom_point(data = paretok, aes(x = prev_fall, y = spring, color = individ_dbid), size =2, show.legend = FALSE) +
  theme_gray(base_size = 8)

fall_summer_dens <- ggplot(data, aes(summer_dens, fall)) + geom_point(alpha = 0.1) +
  geom_point(data = paretok, aes(x = summer_dens, y = fall, color = individ_dbid), size =2, show.legend = FALSE)+
  theme_gray(base_size = 8)

fall_snow <- ggplot(data, aes(max_snow_m, fall)) + geom_point(alpha = 0.1) +
  geom_point(data = paretok, aes(x = max_snow_m, y = fall, color = individ_dbid), size =2, show.legend = FALSE) +
  theme_gray(base_size = 8)

fall_prev_fall <- ggplot(data, aes(prev_fall, fall)) + geom_point(alpha = 0.1) +
  geom_point(data = paretok, aes(x = prev_fall, y = fall, color = individ_dbid), size =2,  show.legend = FALSE) +
  theme_gray(base_size = 8)

fall_spr_ten <- ggplot(data, aes(spr_ten, fall)) + geom_point(alpha = 0.1) +
  geom_point(data = paretok, aes(x = spr_ten, y = fall, color = individ_dbid), size =2,  show.legend = FALSE) +
  theme_gray(base_size = 8)

fall_prev_fall <- ggplot(data, aes(prev_fall, fall)) + geom_point(alpha = 0.1) +
  geom_point(data = paretok, aes(x = prev_fall, y = fall, color = individ_dbid), size =2,  show.legend = FALSE) +
  theme_gray(base_size = 8)

fall_spring <- ggplot(data, aes(spring, fall)) + geom_point(alpha = 0.1) +
  geom_point(data = paretok, aes(x = spring, y = fall, color = individ_dbid), size =2,  show.legend = FALSE) +
  theme_gray(base_size = 8)

fall_max_hundred <- ggplot(data, aes(max_hundred, fall)) + geom_point(alpha = 0.1) +
  geom_point(data = paretok, aes(x = max_hundred, y = fall, color = individ_dbid), size =2,  show.legend = FALSE)+
  theme_gray(base_size = 8)


success_winter_dens <- ggplot(data, aes(winter_dens, success)) + geom_point(alpha = 0.1) +
  geom_point(data = paretok, aes(x = winter_dens, y = success, color = individ_dbid), size =2,  show.legend = FALSE)+
  theme_gray(base_size = 8)
success_snow <- ggplot(data, aes(max_snow_m, success)) + geom_point(alpha = 0.1) +
  geom_point(data = paretok, aes(x = max_snow_m, y = success, color = individ_dbid), size =2,  show.legend = FALSE)+
  theme_gray(base_size = 8)
success_spr_ten <- ggplot(data, aes(spr_ten, success)) + geom_point(alpha = 0.1) +
  geom_point(data = paretok, aes(x = spr_ten, y = success, color = individ_dbid), size =2,  show.legend = FALSE)+
  theme_gray(base_size = 8)
success_spring <- ggplot(data, aes(spring, success)) + geom_point(alpha = 0.1) +
  geom_point(data = paretok, aes(x = spring, y = success, color = individ_dbid), size =2,  show.legend = FALSE)+
  theme_gray(base_size = 8)
success_max_hundred <- ggplot(data, aes(max_hundred, success)) + geom_point(alpha = 0.1) +
  geom_point(data = paretok, aes(x = max_hundred, y = success, color = individ_dbid), size =2,  show.legend = FALSE) +
  theme_gray(base_size = 8)

spring_pareto<- grid.arrange( spring_winter_dens, spring_snow, spring_prev_fall)
fall_pareto <- grid.arrange(fall_prev_fall, fall_summer_dens, fall_snow, fall_spring, fall_spr_ten, fall_max_hundred)
success_pareto <- grid.arrange(success_winter_dens, success_snow, success_spring, success_spr_ten, success_max_hundred)


