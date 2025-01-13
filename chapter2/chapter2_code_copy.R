library(dplyr)
library(ggplot2)
library(viridis) # colors
library(tidyr) # pivot_longer()
library(survival)
library(stargazer)
library(xtable) # for zph table


#library(texreg)
#library(xtable)
#library(data.table)
#library(survminer)
#library(jtools)# for coefficient plots

ma_df <- read.csv('/Data_7aug2024.csv')

# create start and stop
ma_df$start <- ma_df$duration-1
ma_df$stop <- ma_df$duration

# creating different event end variables
ma_df$force <- ifelse(ma_df$endstr == 1, 1, 0)
ma_df$splinter <- ifelse(ma_df$endstr == 2, 1, 0)
ma_df$vpp <- ifelse(ma_df$endstr == 3, 1, 0) # victory/pol process
ma_df$merge <- ifelse(ma_df$endstr == 4, 1, 0)
ma_df$inactive <- ifelse(ma_df$endstr == 5, 1, 0) # disappeared


# cumulative deaths, NAs to zeroes
ma_df <- ma_df %>%
  group_by(gid) %>%
  arrange(gid, year) %>%
  mutate(csum_deaths = cumsum(ifelse(is.na(total_deaths), 0, total_deaths)))
# death w/ decay, NAs to zeroes
ma_df <- ma_df %>%
  group_by(gid) %>%
  arrange(gid, year) %>%
  mutate(dec_deaths = purrr::accumulate(
    replace(total_deaths, is.na(total_deaths), 0), # NAs to 0
    ~ .x * .75 + .y, .init = 0)[-1] # self * .75 + total_deaths, remove initial val
  )
# check over
print(dplyr::select(ma_df, c(gid, year, total_deaths, csum_deaths, dec_deaths)), n = 50)

# create log vars 
ma_df$logtotaldeaths <- log2(ma_df$total_deaths + 1)
ma_df$logtotaldeathssq <- log2(ma_df$total_deaths + 1)^2
ma_df$logcsumdeaths <- log2(ma_df$csum_deaths + 1)
ma_df$logcsumdeathssq <- log2(ma_df$csum_deaths + 1)^2
ma_df$logdecdeaths <- log2(ma_df$dec_deaths + 1)
ma_df$logdecdeathssq <- log2(ma_df$dec_deaths + 1)^2


# vdem v2x_polyarchy
vdem_df <- read.csv("dsc_111120 copy.csv")
# create gid_year var for both datasets
vdem_df$year_gid_id <- paste(vdem_df$iyear, vdem_df$gid, sep = ".")
vdem_df <- dplyr::select(vdem_df, c(year_gid_id, vdem, vdem_final))
ma_df$year_gid_id <- paste(ma_df$year, ma_df$gid, sep = ".")
# merge
ma_df <- merge(ma_df, vdem_df)

# multiply ethnic, vdem_final, tropics by 100 to get them out of percent
ma_df$ethnic100 <- ma_df$ethnic * 100
ma_df$tropics100 <- ma_df$tropics * 100
ma_df$vdem100 <- ma_df$vdem_final * 100
ma_df$diversity100 <- ma_df$diversity * 100
ma_df$shr_trans100 <- ma_df$shr_trans * 100

# dataset with inactive removed
########################
ma_df_noinact <- ma_df %>%
  group_by(gid) %>%
  arrange(gid, year) %>%
  filter(!any(endstr == 5))

# plots
########################################################

# plotting DV
# eliminate active group-years, turn to factor var
endstr_cat <- ma_df %>%
  ungroup() %>%
  select(endstr) %>%
  filter(endstr != 0 )
endstr_order <- as.factor(c(3, 1, 2, 5, 4))

# plot end type factor var
ggplot(endstr_cat, aes(x = factor(endstr, endstr_order))) +
  geom_bar(aes(fill = factor(endstr, endstr_order))) +
  scale_fill_manual(values = viridis(5)) + # colors
  scale_x_discrete(labels = c("Victory/Political", "Force", "Splinter", "Inactive", "Merge")) + # bar labels
  theme(legend.position = "none") + # remove legend
  ggtitle("Ways of Ending") +
  theme(plot.title = element_blank(), 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_blank()) +
  labs(x = "End Type", y = "Count")

# how many active in 2016
length(which(ma_df$year == 2016 & ma_df$end==0))

# plotting main IVs
# make data long. will enable to plot side by side
expvars_long <- ma_df %>%
  mutate(log2_total_deaths = log2(total_deaths + 1),
         log2_csum_deaths = log2(csum_deaths + 1),
         log2_dec_deaths = log2(dec_deaths + 1)) %>%
  ungroup() %>%
  select(log2_total_deaths, log2_csum_deaths, log2_dec_deaths) %>%
  pivot_longer(cols = c(log2_total_deaths, log2_csum_deaths, log2_dec_deaths),
               names_to = "deaths_var", values_to = "value")

ggplot(expvars_long, aes(x = value, color = deaths_var, fill = deaths_var)) +
  geom_histogram(binwidth = .7, alpha = .6) +
  scale_fill_manual(values = c("#440154FF", "#2A788EFF", "#7AD151FF")) +
  scale_color_manual(values = c("#440154FF", "#2A788EFF", "#7AD151FF")) +
  facet_wrap(~deaths_var, # side by side
             labeller = labeller( # change each grid
               deaths_var = c(
                 "log2_csum_deaths" = "Cumulative Fatalities",
                 "log2_dec_deaths" = "Fatalities with Yearly Decay",
                 "log2_total_deaths" = "Yearly Fatalities"
               )
             )) +
  theme(legend.position = "none", # remove legend
        strip.text = element_text(size = 12)) + 
  labs(x = "Number of Fatalities (Logged)", y = "Count")


# plotting survival times
# make subsetted df of the end types being analyzed
duration_df <- ma_df %>%
  ungroup() %>%
  filter(endstr %in% c(1, 2, 3)) %>%
  select(gid, endstr, duration) %>%
  mutate(endstr_word = case_when(
    endstr == 1 ~ "Force",
    endstr == 2 ~ "Splinter",
    endstr == 3 ~ "Victory/Political"
  ))
# figure out bin width
(max(duration_df$duration) - min(duration_df$duration))/30
# plot
ggplot(duration_df, aes(x = duration, color = endstr_word, fill = endstr_word)) +
  geom_histogram(binwidth = 1.4, alpha = .6) +
  scale_fill_manual(values = c("#440154FF", "#2A788EFF", "#7AD151FF")) +
  scale_color_manual(values = c("#440154FF", "#2A788EFF", "#7AD151FF")) +
  facet_wrap(~endstr_word, # side by side
             labeller = labeller( # change each grid
               endstr_word = c(
                 "Force" = "Force",
                 "Splinter" = "Splinter",
                 "Victory/Political" = "Victory/Political"
               ))) +
  theme(legend.position = "none", # remove legend
        strip.text = element_text(size = 12)) + 
  labs(x = "Duration", y = "Count")



# -------------------------------------------------------------------------------------------
# main analysis. no inactive groups
##########################
seed <- 9986000
set.seed(seed)

# survival objects
### use Surv inside coxph otherwise issues
#vpp_S <- Surv(time = ma_df_noinact$start, time2 = ma_df_noinact$stop, event = ma_df_noinact$vpp)
#splinter_S <- Surv(time = ma_df_noinact$start, time2 = ma_df_noinact$stop, event = ma_df_noinact$splinter)
#force_S <- Surv(time = ma_df_noinact$start, time2 = ma_df_noinact$stop, event = ma_df_noinact$force)


# year, group, no country
vpp_yg <- coxph(Surv(time = start, time2 = stop, event = vpp) ~ 
                  logtotaldeaths + logtotaldeathssq + left + right + nat + ercsr + 
                  pch + tch + diversity100 + shr_trans100 + mul_bases, 
                data = ma_df_noinact, na.action = na.omit, method="efron")
force_yg <- coxph(Surv(time = start, time2 = stop, event = force) ~ 
                    logtotaldeaths + logtotaldeathssq + left + right + nat + ercsr + 
                    pch + tch + diversity100 + shr_trans100 + mul_bases, 
               data = ma_df_noinact, na.action = na.omit, method="efron")
splinter_yg <- coxph(Surv(time = start, time2 = stop, event = splinter) ~ 
                       logtotaldeaths + logtotaldeathssq + left + right + nat + 
                       ercsr + pch + tch + diversity100 + shr_trans100 + mul_bases, 
                 data = ma_df_noinact, na.action = na.omit, method="efron")
# decay, group, no country# decay, group, no countrysplinter
vpp_dg <- coxph(Surv(time = start, time2 = stop, event = vpp) ~ 
                  logdecdeaths + logdecdeathssq + left + right + nat + ercsr + 
                  pch + tch + diversity100 + shr_trans100 + mul_bases, 
               data = ma_df_noinact, na.action = na.omit, method="efron")
force_dg <- coxph(Surv(time = start, time2 = stop, event = force) ~ 
                    logdecdeaths + logdecdeathssq + left + right + nat + ercsr + 
                    pch + tch + diversity100 + shr_trans100 + mul_bases, 
                 data = ma_df_noinact, na.action = na.omit, method="efron")
splinter_dg <- coxph(Surv(time = start, time2 = stop, event = splinter) ~ 
                       logdecdeaths + logdecdeathssq + left + right + nat + ercsr + 
                       pch + tch + diversity100 + shr_trans100 + mul_bases, 
                    data = ma_df_noinact, na.action = na.omit, method="efron")
# cumulative, group, no country
vpp_cg <- coxph(Surv(time = start, time2 = stop, event = vpp) ~ 
                  logcsumdeaths + logcsumdeathssq + left + right + nat + ercsr + 
                  pch + tch + diversity100 + shr_trans100 + mul_bases, 
                data = ma_df_noinact, na.action = na.omit, method="efron")
force_cg <- coxph(Surv(time = start, time2 = stop, event = force) ~ 
                    logcsumdeaths + logcsumdeathssq + left + right + nat + ercsr + 
                    pch + tch + diversity100 + shr_trans100 + mul_bases, 
                  data = ma_df_noinact, na.action = na.omit, method="efron")
splinter_cg <- coxph(Surv(time = start, time2 = stop, event = splinter) ~ 
                       logcsumdeaths + logcsumdeathssq + left + right + nat + ercsr + 
                       pch + tch + diversity100 + shr_trans100 + mul_bases, 
                     data = ma_df_noinact, na.action = na.omit, method="efron")


# year, group, country
vpp_ygc <- coxph(Surv(time = start, time2 = stop, event = vpp) ~ 
                   logtotaldeaths + logtotaldeathssq + left + right + nat + ercsr + 
                   pch + tch + diversity100 + shr_trans100 + mul_bases + log2(pop) + 
                   log2(gdppercapita) + vdem100 + ethnic100 + tropics100 + log2(elevation) +
                   EAP + ECA + LAC + NA. + SAS + SSA, 
                data = ma_df_noinact, na.action = na.omit, method="efron")
force_ygc <- coxph(Surv(time = start, time2 = stop, event = force) ~ 
                     logtotaldeaths + logtotaldeathssq + left + right + nat + ercsr + 
                     pch + tch + diversity100 + shr_trans100 + mul_bases + log2(pop) + 
                     log2(gdppercapita) + vdem100 + ethnic100 + tropics100 + log2(elevation) +
                     EAP + ECA + LAC + NA. + SAS + SSA, 
                    data = ma_df_noinact, na.action = na.omit, method="efron")
splinter_ygc <- coxph(Surv(time = start, time2 = stop, event = splinter) ~ 
                        logtotaldeaths + logtotaldeathssq + left + right + nat + 
                        ercsr + pch + tch + diversity100 + shr_trans100 + mul_bases + 
                        log2(pop) + log2(gdppercapita) + vdem100  + ethnic100 + 
                        tropics100 + log2(elevation) + EAP + ECA + LAC + NA. + SAS + SSA, 
                       data = ma_df_noinact, na.action = na.omit, method="efron")
# decay, group & country
vpp_dgc <- coxph(Surv(time = start, time2 = stop, event = vpp) ~ 
                   logdecdeaths + logdecdeathssq + left + right + nat + ercsr + 
                   pch + tch + diversity100 + shr_trans100 + mul_bases + log2(pop) +  
                   log2(gdppercapita) + vdem100 + ethnic100 + tropics100 + log2(elevation) +
                   EAP + ECA + LAC + NA. + SAS + SSA, 
                   data = ma_df_noinact, na.action = na.omit, method="efron", x = TRUE)
force_dgc <- coxph(Surv(time = start, time2 = stop, event = force) ~ 
                     logdecdeaths + logdecdeathssq + left + right + nat + ercsr + 
                     pch + tch + diversity100 + shr_trans100 + mul_bases + log2(pop) + 
                     log2(gdppercapita) + vdem100 + ethnic100 + tropics100 + 
                     log2(elevation) + EAP + ECA + LAC + NA. + SAS + SSA, 
                     data = ma_df_noinact, na.action = na.omit, method="efron", x = TRUE)
splinter_dgc <- coxph(Surv(time = start, time2 = stop, event = splinter) ~ 
                        logdecdeaths + logdecdeathssq + left + right + nat + ercsr + 
                        pch + tch + diversity100 + shr_trans100 + mul_bases + log2(pop) + 
                        log2(gdppercapita) + vdem100 + ethnic100 + tropics100 + log2(elevation) +
                        EAP + ECA + LAC + NA. + SAS + SSA, 
                        data = ma_df_noinact, na.action = na.omit, method="efron", x = TRUE)
# cumulative, group & country
vpp_cgc <- coxph(Surv(time = start, time2 = stop, event = vpp) ~ 
                   logcsumdeaths + logcsumdeathssq + left + right + nat + ercsr + 
                   pch + tch + diversity100 + shr_trans100 + mul_bases + log2(pop) +  
                   log2(gdppercapita) + vdem100 + ethnic100 + tropics100 + log2(elevation) +
                   EAP + ECA + LAC + NA. + SAS + SSA, 
                   data = ma_df_noinact, na.action = na.omit, method="efron", x = TRUE)
force_cgc <- coxph(Surv(time = start, time2 = stop, event = force) ~ 
                     logcsumdeaths + logcsumdeathssq + left + right + nat + ercsr + 
                     pch + tch + diversity100 + shr_trans100 + mul_bases + log2(pop) + 
                     log2(gdppercapita) + vdem100 + ethnic100 + tropics100 + log2(elevation) +
                     EAP + ECA + LAC + NA. + SAS + SSA, 
                     data = ma_df_noinact, na.action = na.omit, method="efron", x = TRUE)
splinter_cgc <- coxph(Surv(time = start, time2 = stop, event = splinter) ~ 
                        logcsumdeaths + logcsumdeathssq + left + right + nat + 
                        ercsr + pch + tch + diversity100 + shr_trans100 + mul_bases + 
                        log2(pop) + log2(gdppercapita) + vdem100 + ethnic100 + 
                        tropics100 + log2(elevation) + EAP + ECA + LAC + NA. + 
                        SAS + SSA, 
                        data = ma_df_noinact, na.action = na.omit, method="efron", x = TRUE)



# model summaries for latex
# function to turn coef into pct on hazard
pct_on_hz <- function(model) {(summary(model)$coef[,2] - 1) * 100} 
stde_trans <- function(model) {summary(model)$coef[,3] * summary(model)$coef[,2] * 100}
year_models_list <- list(vpp_yg, force_yg, splinter_yg, vpp_ygc, force_ygc, splinter_ygc)
decay_models_list <- list(vpp_dg, force_dg, splinter_dg, vpp_dgc, force_dgc, splinter_dgc)
csum_models_list <- list(vpp_cg, force_cg, splinter_cg, vpp_cgc, force_cgc, splinter_cgc)

# yearly
stargazer(year_models_list,
          coef = lapply(year_models_list, pct_on_hz),
          se = lapply(year_models_list, stde_trans),
          t.auto = FALSE, p.auto = FALSE, type = "html", out="year_models_table.html", 
          out.header = TRUE, font.size = "small",
          column.labels=c("Victory/Pol.","Force","Splinter","Victory/Pol.","Force","Splinter"),
          covariate.labels = c("Fatalities (log)", "Fatalities Sq. (log)", "Left", "Right", 
                               "Nationalist", "Regime", "Policy", "Territory",
                               "Attack Diversity", "Share Trans. Terr.", "Multiple Bases",
                               "Pop (log)", "GDP/Pop (log)", "Democracy", "Ethnic Frac.",
                               "Tropics", "Elevation (log)", "East Asia \\& Pacific",
                               "Europe \\& Central Asia", "Latin Am. \\& Caribbean",
                               "North America", "South Asia", "Sub-Saharan Africa"))
browseURL("year_models_table.html") # using html much later for easy compare w/ checking code + paper tables

stargazer(decay_models_list,
          coef = lapply(decay_models_list, pct_on_hz),
          se = lapply(decay_models_list, stde_trans),
          t.auto = FALSE, p.auto = FALSE, type = "html", out="decay_models_table.html", 
          out.header = TRUE, font.size = "small",
          column.labels=c("Victory/Pol.","Force","Splinter","Victory/Pol.","Force","Splinter"),
          covariate.labels = c("Fatalities (log)", "Fatalities Sq. (log)", "Left", "Right", 
                               "Nationalist", "Regime", "Policy", "Territory",
                               "Attack Diversity", "Share Trans. Terr.", "Multiple Bases",
                               "Pop (log)", "GDP/Pop (log)", "Democracy", "Ethnic Frac.",
                               "Tropics", "Elevation (log)", "East Asia \\& Pacific",
                               "Europe \\& Central Asia", "Latin Am. \\& Caribbean",
                               "North America", "South Asia", "Sub-Saharan Africa"))

browseURL("decay_models_table.html")

# cumulative models list
stargazer(csum_models_list,
          coef = lapply(csum_models_list, pct_on_hz),
          se = lapply(csum_models_list, stde_trans),
          t.auto = FALSE, p.auto = FALSE, type = "html", out="csum_models_table.html", 
          out.header = TRUE, font.size = "small",
          column.labels=c("Victory/Pol.","Force","Splinter","Victory/Pol.","Force","Splinter"),
          covariate.labels = c("Fatalities (log)", "Fatalities Sq. (log)", "Left", "Right", 
                               "Nationalist", "Regime", "Policy", "Territory",
                               "Attack Diversity", "Share Trans. Terr.", "Multiple Bases",
                               "Pop (log)", "GDP/Pop (log)", "Democracy", "Ethnic Frac.",
                               "Tropics", "Elevation (log)", "East Asia \\& Pacific",
                               "Europe \\& Central Asia", "Latin Am. \\& Caribbean",
                               "North America", "South Asia", "Sub-Saharan Africa"))
browseURL("csum_models_table.html")

# plots w/ cumulative fatalities
###############################
library(contsurvplot)
library(gridExtra)
set.seed(seed)

# need to rerun with the variable being squared as opposed to already defined
# so that r treats it as the same variable
vpp_cgc_plots2 <- coxph(Surv(time = start, time2 = stop, event = vpp) ~ 
                   logcsumdeaths + I(logcsumdeaths^2) + left + right + nat + ercsr + 
                   pch + tch + diversity100 + shr_trans100 + mul_bases + log2(pop) +  
                   log2(gdppercapita) + vdem100 + ethnic100 + tropics100 + log2(elevation) +
                   EAP + ECA + LAC + NA. + SAS + SSA, 
                 data = ma_df_noinact, na.action = na.omit, method="efron", x = TRUE)
force_cgc_plots2 <- coxph(Surv(time = start, time2 = stop, event = force) ~ 
                     logcsumdeaths + I(logcsumdeaths^2) + left + right + nat + ercsr + 
                     pch + tch + diversity100 + shr_trans100 + mul_bases + log2(pop) + 
                     log2(gdppercapita) + vdem100 + ethnic100 + tropics100 + log2(elevation) +
                     EAP + ECA + LAC + NA. + SAS + SSA, 
                   data = ma_df_noinact, na.action = na.omit, method="efron", x = TRUE)
force_ygc_plots <- coxph(Surv(time = start, time2 = stop, event = force) ~ 
                           logtotaldeaths + I(logtotaldeaths^2) + left + right + nat + ercsr + 
                           pch + tch + diversity100 + shr_trans100 + mul_bases + log2(pop) + 
                           log2(gdppercapita) + vdem100 + ethnic100 + tropics100 + log2(elevation) +
                           EAP + ECA + LAC + NA. + SAS + SSA, 
                         data = ma_df_noinact, na.action = na.omit, method="efron", x = TRUE)

# gives a heat matrix. heat map was used instead in paper
# matrix numbers will look huge depending on plot area of IDE but look different/smaller when saved
# end type = force
plot_surv_matrix(time = "stop", status = "force", variable = "logcsumdeaths",
                 data = ma_df_noinact, model = force_cgc_plots2,
                 n_col=18, n_row=18, number_size = 8, border_size = .01,
                 start_color = viridis(1), end_color = viridis(6),
                 number_color = "azure1", xlab = "Time (Years)",
                 cif=TRUE, ylab = "Cumulative Fatalities (Logged)") +
  # changing axis text sizes
  theme(axis.title.y = element_text(size=25), axis.text.y = element_text(size=25), 
        axis.title.x = element_text(size=25), axis.text.x = element_text(size=25)) +
  scale_y_continuous(breaks = c(seq(0,14,by=2)),expand=c(0,0))


# same but heatmap
plot_surv_heatmap(time = "stop", status = "force", variable = "logcsumdeaths",
                  data = ma_df_noinact, model = force_cgc_plots2,
                  #n_col=18, n_row=18, number_size = 8, border_size = .01,
                  start_color = viridis(1), end_color = viridis(6),
                  number_color = "azure1", xlab = "Time (Years)",
                  cif=TRUE, ylab = "Cumulative Fatalities (Logged)", 
                  legend.title = "CIF: End\nby Force") +
  theme(axis.title.y = element_text(size=25), axis.text.y = element_text(size=25), 
        axis.title.x = element_text(size=25), axis.text.x = element_text(size=25),
        legend.text = element_text(size=18), legend.title = element_text(size=18)) +
  scale_y_continuous(breaks = c(seq(0,14,by=2)),expand=c(0,0))


# end type victory/political. heat matrix
plot_surv_matrix(time = "stop", status = "vpp", variable = "logcsumdeaths",
                 data = ma_df_noinact, model = vpp_cgc_plots2,
                 n_col=18, n_row=18, number_size = 8, border_size = .01,
                 start_color = viridis(1), end_color = viridis(6),
                 number_color = "azure1", xlab = "Time (Years)",
                 cif=TRUE, ylab = "Cumulative Fatalities (Logged)") +
  theme(axis.title.y = element_text(size=25), axis.text.y = element_text(size=25), 
        axis.title.x = element_text(size=25), axis.text.x = element_text(size=25)) +
  scale_y_continuous(breaks = c(seq(0,14,by=2)),expand=c(0,0))

# heat map
plot_surv_heatmap(time = "stop", status = "vpp", variable = "logcsumdeaths",
                  data = ma_df_noinact, model = vpp_cgc_plots2,
                  #n_col=18, n_row=18, number_size = 8, border_size = .01,
                  start_color = viridis(1), end_color = viridis(6),
                  number_color = "azure1", xlab = "Time (Years)",
                  cif=TRUE, ylab = "Cumulative Fatalities (Logged)",
                  legend.title = "CIF: End\nby Victory") +
  theme(axis.title.y = element_text(size=25), axis.text.y = element_text(size=25), 
        axis.title.x = element_text(size=25), axis.text.x = element_text(size=25),
        legend.text = element_text(size=18), legend.title = element_text(size=18)) +
  scale_y_continuous(breaks = c(seq(0,14,by=2)),expand=c(0,0))



# using grid.arrange to arrange surv lines side by side. victory/politics and force
grid.arrange(
  plot_surv_lines(time = "stop", status = "vpp", variable = "logcsumdeaths",
                  data = ma_df_noinact,
                  model = force_cgc_plots2,
                  start_color = viridis(1),end_color = viridis(6),
                  horizon = c(seq(0,14,2)),
                  discrete = TRUE, cif = TRUE,
                  ylab = "Cumulative Incidence Function: End by Victory/Political",
                  xlab="Time (Years)") +
    scale_color_viridis(discrete = TRUE, option = "D") +
    guides(linetype = guide_legend(override.aes = list(size = 3))) +
    theme(axis.title.y = element_text(size=15), axis.text.y = element_text(size=15), 
          legend.position = "none",
          axis.title.x = element_text(size=15), axis.text.x = element_text(size=15)),
         
  # next plot in the grid arrangement
  plot_surv_lines(time = "stop", status = "force", variable = "logcsumdeaths",
                  data = ma_df_noinact,
                  model = force_cgc_plots2,
                  start_color = viridis(1),end_color = viridis(6),
                  horizon = c(seq(0,14,2)),
                  discrete = TRUE, cif = TRUE,
                  legend.title = "Cumulative \nFatalities \n(Logged)",
                  ylab = "Cumulative Incidence Function: End by Force",
                  xlab="Time (Years)")+
    scale_color_viridis(discrete = TRUE, option = "D") +
    guides(linetype = guide_legend(override.aes = list(size = 3))) +
    theme(axis.title.y = element_text(size=15), axis.text.y = element_text(size=15), 
          legend.text = element_text(size=17), legend.title = element_text(size=15),
          legend.key.size = unit(1.25, "cm"), legend.spacing.y = unit(.25,"cm"),
          axis.title.x = element_text(size=15), axis.text.x = element_text(size=15)),
  # number of columns in grid arrangement
  ncol=2)


# yearly matrix. not in paper
plot_surv_matrix(time = "stop", status = "force", variable = "logtotaldeaths",
                 data = ma_df_noinact, model = force_ygc_plots,
                 n_col=18, n_row=18, number_size = 8, border_size = .01,
                 start_color = viridis(1), end_color = viridis(6),
                 number_color = "azure1", xlab = "Time (Years)",
                 cif=TRUE, ylab = "Cumulative Fatalities (Logged)") +
  theme(axis.title.y = element_text(size=25), axis.text.y = element_text(size=25), 
        axis.title.x = element_text(size=25), axis.text.x = element_text(size=25)) +
  scale_y_continuous(breaks = c(seq(0,14,by=2)),expand=c(0,0))



# inactive included for appendix
###########################################
seed <- 9986000
set.seed(seed)


# year, group, no country
vpp_yg_inact <- coxph(Surv(time = start, time2 = stop, event = vpp) ~ 
                  logtotaldeaths + logtotaldeathssq + left + right + nat + ercsr + 
                  pch + tch + diversity100 + shr_trans100 + mul_bases, 
                data = ma_df, na.action = na.omit, method="efron")
force_yg_inact <- coxph(Surv(time = start, time2 = stop, event = force) ~ 
                    logtotaldeaths + logtotaldeathssq + left + right + nat + ercsr + 
                    pch + tch + diversity100 + shr_trans100 + mul_bases, 
                  data = ma_df, na.action = na.omit, method="efron")
splinter_yg_inact <- coxph(Surv(time = start, time2 = stop, event = splinter) ~ 
                       logtotaldeaths + logtotaldeathssq + left + right + nat + 
                       ercsr + pch + tch + diversity100 + shr_trans100 + mul_bases, 
                     data = ma_df, na.action = na.omit, method="efron")
# decay, group, no country# decay, group, no countrysplinter
vpp_dg_inact <- coxph(Surv(time = start, time2 = stop, event = vpp) ~ 
                  logdecdeaths + logdecdeathssq + left + right + nat + ercsr + 
                  pch + tch + diversity100 + shr_trans100 + mul_bases, 
                data = ma_df, na.action = na.omit, method="efron")
force_dg_inact <- coxph(Surv(time = start, time2 = stop, event = force) ~ 
                    logdecdeaths + logdecdeathssq + left + right + nat + ercsr + 
                    pch + tch + diversity100 + shr_trans100 + mul_bases, 
                  data = ma_df, na.action = na.omit, method="efron")
splinter_dg_inact <- coxph(Surv(time = start, time2 = stop, event = splinter) ~ 
                       logdecdeaths + logdecdeathssq + left + right + nat + ercsr + 
                       pch + tch + diversity100 + shr_trans100 + mul_bases, 
                     data = ma_df, na.action = na.omit, method="efron")
# cumulative, group, no country
vpp_cg_inact <- coxph(Surv(time = start, time2 = stop, event = vpp) ~ 
                  logcsumdeaths + logcsumdeathssq + left + right + nat + ercsr + 
                  pch + tch + diversity100 + shr_trans100 + mul_bases, 
                data = ma_df, na.action = na.omit, method="efron")
force_cg_inact <- coxph(Surv(time = start, time2 = stop, event = force) ~ 
                    logcsumdeaths + logcsumdeathssq + left + right + nat + ercsr + 
                    pch + tch + diversity100 + shr_trans100 + mul_bases, 
                  data = ma_df, na.action = na.omit, method="efron")
splinter_cg_inact <- coxph(Surv(time = start, time2 = stop, event = splinter) ~ 
                       logcsumdeaths + logcsumdeathssq + left + right + nat + ercsr + 
                       pch + tch + diversity100 + shr_trans100 + mul_bases, 
                     data = ma_df, na.action = na.omit, method="efron")


# year, group, country
vpp_ygc_inact <- coxph(Surv(time = start, time2 = stop, event = vpp) ~ 
                   logtotaldeaths + logtotaldeathssq + left + right + nat + ercsr + 
                   pch + tch + diversity100 + shr_trans100 + mul_bases + log2(pop) + 
                   log2(gdppercapita) + vdem100 + ethnic100 + tropics100 + log2(elevation) +
                   EAP + ECA + LAC + NA. + SAS + SSA, 
                 data = ma_df, na.action = na.omit, method="efron")
force_ygc_inact <- coxph(Surv(time = start, time2 = stop, event = force) ~ 
                     logtotaldeaths + logtotaldeathssq + left + right + nat + ercsr + 
                     pch + tch + diversity100 + shr_trans100 + mul_bases + log2(pop) + 
                     log2(gdppercapita) + vdem100 + ethnic100 + tropics100 + log2(elevation) +
                     EAP + ECA + LAC + NA. + SAS + SSA, 
                   data = ma_df, na.action = na.omit, method="efron")
splinter_ygc_inact <- coxph(Surv(time = start, time2 = stop, event = splinter) ~ 
                        logtotaldeaths + logtotaldeathssq + left + right + nat + 
                        ercsr + pch + tch + diversity100 + shr_trans100 + mul_bases + 
                        log2(pop) + log2(gdppercapita) + vdem100  + ethnic100 + 
                        tropics100 + log2(elevation) + EAP + ECA + LAC + NA. + SAS + SSA, 
                      data = ma_df, na.action = na.omit, method="efron")
# decay, group & country
vpp_dgc_inact <- coxph(Surv(time = start, time2 = stop, event = vpp) ~ 
                   logdecdeaths + logdecdeathssq + left + right + nat + ercsr + 
                   pch + tch + diversity100 + shr_trans100 + mul_bases + log2(pop) +  
                   log2(gdppercapita) + vdem100 + ethnic100 + tropics100 + log2(elevation) +
                   EAP + ECA + LAC + NA. + SAS + SSA, 
                 data = ma_df, na.action = na.omit, method="efron", x = TRUE)
force_dgc_inact <- coxph(Surv(time = start, time2 = stop, event = force) ~ 
                     logdecdeaths + logdecdeathssq + left + right + nat + ercsr + 
                     pch + tch + diversity100 + shr_trans100 + mul_bases + log2(pop) + 
                     log2(gdppercapita) + vdem100 + ethnic100 + tropics100 + 
                     log2(elevation) + EAP + ECA + LAC + NA. + SAS + SSA, 
                   data = ma_df, na.action = na.omit, method="efron", x = TRUE)
splinter_dgc_inact <- coxph(Surv(time = start, time2 = stop, event = splinter) ~ 
                        logdecdeaths + logdecdeathssq + left + right + nat + ercsr + 
                        pch + tch + diversity100 + shr_trans100 + mul_bases + log2(pop) + 
                        log2(gdppercapita) + vdem100 + ethnic100 + tropics100 + log2(elevation) +
                        EAP + ECA + LAC + NA. + SAS + SSA, 
                      data = ma_df, na.action = na.omit, method="efron", x = TRUE)
# cumulative, group & country
vpp_cgc_inact <- coxph(Surv(time = start, time2 = stop, event = vpp) ~ 
                   logcsumdeaths + logcsumdeathssq + left + right + nat + ercsr + 
                   pch + tch + diversity100 + shr_trans100 + mul_bases + log2(pop) +  
                   log2(gdppercapita) + vdem100 + ethnic100 + tropics100 + log2(elevation) +
                   EAP + ECA + LAC + NA. + SAS + SSA, 
                 data = ma_df, na.action = na.omit, method="efron", x = TRUE)
force_cgc_inact <- coxph(Surv(time = start, time2 = stop, event = force) ~ 
                     logcsumdeaths + logcsumdeathssq + left + right + nat + ercsr + 
                     pch + tch + diversity100 + shr_trans100 + mul_bases + log2(pop) + 
                     log2(gdppercapita) + vdem100 + ethnic100 + tropics100 + log2(elevation) +
                     EAP + ECA + LAC + NA. + SAS + SSA, 
                   data = ma_df, na.action = na.omit, method="efron", x = TRUE)
splinter_cgc_inact <- coxph(Surv(time = start, time2 = stop, event = splinter) ~ 
                        logcsumdeaths + logcsumdeathssq + left + right + nat + 
                        ercsr + pch + tch + diversity100 + shr_trans100 + mul_bases + 
                        log2(pop) + log2(gdppercapita) + vdem100 + ethnic100 + 
                        tropics100 + log2(elevation) + EAP + ECA + LAC + NA. + 
                        SAS + SSA, 
                      data = ma_df, na.action = na.omit, method="efron", x = TRUE)



# model summaries for latex
# function to turn coef into pct on hazard
pct_on_hz <- function(model) {(summary(model)$coef[,2] - 1) * 100} 
stde_trans <- function(model) {summary(model)$coef[,3] * summary(model)$coef[,2] * 100}
year_models_list_inact <- list(vpp_yg_inact, force_yg_inact, splinter_yg_inact, vpp_ygc_inact, force_ygc_inact, splinter_ygc_inact)
decay_models_list_inact <- list(vpp_dg_inact, force_dg_inact, splinter_dg_inact, vpp_dgc_inact, force_dgc_inact, splinter_dgc_inact)
csum_models_list_inact <- list(vpp_cg_inact, force_cg_inact, splinter_cg_inact, vpp_cgc_inact, force_cgc_inact, splinter_cgc_inact)

# yearly
stargazer(year_models_list_inact,
          coef = lapply(year_models_list_inact, pct_on_hz),
          se = lapply(year_models_list_inact, stde_trans),
          t.auto = FALSE, p.auto = FALSE, type = "html", out="year_inact.html", 
          out.header = TRUE, font.size = "small",
          column.labels=c("Victory/Pol.","Force","Splinter","Victory/Pol.","Force","Splinter"),
          covariate.labels = c("Fatalities (log)", "Fatalities Sq. (log)", "Left", "Right", 
                               "Nationalist", "Regime", "Policy", "Territory",
                               "Attack Diversity", "Share Trans. Terr.", "Multiple Bases",
                               "Pop (log)", "GDP/Pop (log)", "Democracy", "Ethnic Frac.",
                               "Tropics", "Elevation (log)", "East Asia \\& Pacific",
                               "Europe \\& Central Asia", "Latin Am. \\& Caribbean",
                               "North America", "South Asia", "Sub-Saharan Africa"),
          omit.stat = c("rsq", "max.rsq", "lr", "wald"))

browseURL("year_inact.html") 

stargazer(decay_models_list_inact,
          coef = lapply(decay_models_list_inact, pct_on_hz),
          se = lapply(decay_models_list_inact, stde_trans),
          t.auto = FALSE, p.auto = FALSE, type = "html", out="dec_inact.html", 
          out.header = TRUE, font.size = "small",
          column.labels=c("Victory/Pol.","Force","Splinter","Victory/Pol.","Force","Splinter"),
          covariate.labels = c("Fatalities (log)", "Fatalities Sq. (log)", "Left", "Right", 
                               "Nationalist", "Regime", "Policy", "Territory",
                               "Attack Diversity", "Share Trans. Terr.", "Multiple Bases",
                               "Pop (log)", "GDP/Pop (log)", "Democracy", "Ethnic Frac.",
                               "Tropics", "Elevation (log)", "East Asia \\& Pacific",
                               "Europe \\& Central Asia", "Latin Am. \\& Caribbean",
                               "North America", "South Asia", "Sub-Saharan Africa"),
          omit.stat = c("rsq", "max.rsq", "lr", "wald"))
browseURL("dec_inact.html")
# cumulative models list
stargazer(csum_models_list_inact,
          coef = lapply(csum_models_list_inact, pct_on_hz),
          se = lapply(csum_models_list_inact, stde_trans),
          t.auto = FALSE, p.auto = FALSE, type = "html", out="csum_inact.html", 
          out.header = TRUE, font.size = "small",
          column.labels=c("Victory/Pol.","Force","Splinter","Victory/Pol.","Force","Splinter"),
          covariate.labels = c("Fatalities (log)", "Fatalities Sq. (log)", "Left", "Right", 
                               "Nationalist", "Regime", "Policy", "Territory",
                               "Attack Diversity", "Share Trans. Terr.", "Multiple Bases",
                               "Pop (log)", "GDP/Pop (log)", "Democracy", "Ethnic Frac.",
                               "Tropics", "Elevation (log)", "East Asia \\& Pacific",
                               "Europe \\& Central Asia", "Latin Am. \\& Caribbean",
                               "North America", "South Asia", "Sub-Saharan Africa"),
          omit.stat = c("rsq", "max.rsq", "lr", "wald"))
browseURL("csum_inact.html")

# gov spending
##################################################
set.seed(seed)
vpp_yg_govspend <- coxph(Surv(time = start, time2 = stop, event = vpp) ~ 
                  logtotaldeaths + logtotaldeathssq + left + right + nat + ercsr + 
                  pch + tch + diversity100 + shr_trans100 + mul_bases + GovernmentspendingGDP2010us, 
                data = ma_df_noinact, na.action = na.omit, method="efron")
force_yg_govspend <- coxph(Surv(time = start, time2 = stop, event = force) ~ 
                    logtotaldeaths + logtotaldeathssq + left + right + nat + ercsr + 
                    pch + tch + diversity100 + shr_trans100 + mul_bases + GovernmentspendingGDP2010us, 
                  data = ma_df_noinact, na.action = na.omit, method="efron")
splinter_yg_govspend <- coxph(Surv(time = start, time2 = stop, event = splinter) ~ 
                       logtotaldeaths + logtotaldeathssq + left + right + nat + 
                       ercsr + pch + tch + diversity100 + shr_trans100 + mul_bases + GovernmentspendingGDP2010us, 
                     data = ma_df_noinact, na.action = na.omit, method="efron")
# decay, group, no country# decay, group, no countrysplinter
vpp_dg_govspend <- coxph(Surv(time = start, time2 = stop, event = vpp) ~ 
                  logdecdeaths + logdecdeathssq + left + right + nat + ercsr + 
                  pch + tch + diversity100 + shr_trans100 + mul_bases + GovernmentspendingGDP2010us, 
                data = ma_df_noinact, na.action = na.omit, method="efron")
force_dg_govspend <- coxph(Surv(time = start, time2 = stop, event = force) ~ 
                    logdecdeaths + logdecdeathssq + left + right + nat + ercsr + 
                    pch + tch + diversity100 + shr_trans100 + mul_bases + GovernmentspendingGDP2010us, 
                  data = ma_df_noinact, na.action = na.omit, method="efron")
splinter_dg_govspend <- coxph(Surv(time = start, time2 = stop, event = splinter) ~ 
                       logdecdeaths + logdecdeathssq + left + right + nat + ercsr + 
                       pch + tch + diversity100 + shr_trans100 + mul_bases + GovernmentspendingGDP2010us, 
                     data = ma_df_noinact, na.action = na.omit, method="efron")
# cumulative, group, no country
vpp_cg_govspend <- coxph(Surv(time = start, time2 = stop, event = vpp) ~ 
                  logcsumdeaths + logcsumdeathssq + left + right + nat + ercsr + 
                  pch + tch + diversity100 + shr_trans100 + mul_bases + GovernmentspendingGDP2010us, 
                data = ma_df_noinact, na.action = na.omit, method="efron")
force_cg_govspend <- coxph(Surv(time = start, time2 = stop, event = force) ~ 
                    logcsumdeaths + logcsumdeathssq + left + right + nat + ercsr + 
                    pch + tch + diversity100 + shr_trans100 + mul_bases + GovernmentspendingGDP2010us, 
                  data = ma_df_noinact, na.action = na.omit, method="efron")
splinter_cg_govspend <- coxph(Surv(time = start, time2 = stop, event = splinter) ~ 
                       logcsumdeaths + logcsumdeathssq + left + right + nat + ercsr + 
                       pch + tch + diversity100 + shr_trans100 + mul_bases + GovernmentspendingGDP2010us, 
                     data = ma_df_noinact, na.action = na.omit, method="efron")


# year, group, country
vpp_ygc_govspend <- coxph(Surv(time = start, time2 = stop, event = vpp) ~ 
                   logtotaldeaths + logtotaldeathssq + left + right + nat + ercsr + 
                   pch + tch + diversity100 + shr_trans100 + mul_bases + log2(pop) + 
                   log2(gdppercapita) + vdem100 + ethnic100 + tropics100 + log2(elevation) +
                   EAP + ECA + LAC + NA. + SAS + SSA + GovernmentspendingGDP2010us, 
                 data = ma_df_noinact, na.action = na.omit, method="efron")
force_ygc_govspend <- coxph(Surv(time = start, time2 = stop, event = force) ~ 
                     logtotaldeaths + logtotaldeathssq + left + right + nat + ercsr + 
                     pch + tch + diversity100 + shr_trans100 + mul_bases + log2(pop) + 
                     log2(gdppercapita) + vdem100 + ethnic100 + tropics100 + log2(elevation) +
                     EAP + ECA + LAC + NA. + SAS + SSA + GovernmentspendingGDP2010us, 
                   data = ma_df_noinact, na.action = na.omit, method="efron")
splinter_ygc_govspend <- coxph(Surv(time = start, time2 = stop, event = splinter) ~ 
                        logtotaldeaths + logtotaldeathssq + left + right + nat + 
                        ercsr + pch + tch + diversity100 + shr_trans100 + mul_bases + 
                        log2(pop) + log2(gdppercapita) + vdem100  + ethnic100 + 
                        tropics100 + log2(elevation) + EAP + ECA + LAC + NA. + SAS + SSA + GovernmentspendingGDP2010us, 
                      data = ma_df_noinact, na.action = na.omit, method="efron")
# decay, group & country
vpp_dgc_govspend <- coxph(Surv(time = start, time2 = stop, event = vpp) ~ 
                   logdecdeaths + logdecdeathssq + left + right + nat + ercsr + 
                   pch + tch + diversity100 + shr_trans100 + mul_bases + log2(pop) +  
                   log2(gdppercapita) + vdem100 + ethnic100 + tropics100 + log2(elevation) +
                   EAP + ECA + LAC + NA. + SAS + SSA + GovernmentspendingGDP2010us, 
                 data = ma_df_noinact, na.action = na.omit, method="efron", x = TRUE)
force_dgc_govspend <- coxph(Surv(time = start, time2 = stop, event = force) ~ 
                     logdecdeaths + logdecdeathssq + left + right + nat + ercsr + 
                     pch + tch + diversity100 + shr_trans100 + mul_bases + log2(pop) + 
                     log2(gdppercapita) + vdem100 + ethnic100 + tropics100 + 
                     log2(elevation) + EAP + ECA + LAC + NA. + SAS + SSA + GovernmentspendingGDP2010us, 
                   data = ma_df_noinact, na.action = na.omit, method="efron", x = TRUE)
splinter_dgc_govspend <- coxph(Surv(time = start, time2 = stop, event = splinter) ~ 
                        logdecdeaths + logdecdeathssq + left + right + nat + ercsr + 
                        pch + tch + diversity100 + shr_trans100 + mul_bases + log2(pop) + 
                        log2(gdppercapita) + vdem100 + ethnic100 + tropics100 + log2(elevation) +
                        EAP + ECA + LAC + NA. + SAS + SSA + GovernmentspendingGDP2010us, 
                      data = ma_df_noinact, na.action = na.omit, method="efron", x = TRUE)
# cumulative, group & country
vpp_cgc_govspend <- coxph(Surv(time = start, time2 = stop, event = vpp) ~ 
                   logcsumdeaths + logcsumdeathssq + left + right + nat + ercsr + 
                   pch + tch + diversity100 + shr_trans100 + mul_bases + log2(pop) +  
                   log2(gdppercapita) + vdem100 + ethnic100 + tropics100 + log2(elevation) +
                   EAP + ECA + LAC + NA. + SAS + SSA + GovernmentspendingGDP2010us, 
                 data = ma_df_noinact, na.action = na.omit, method="efron", x = TRUE)
force_cgc_govspend <- coxph(Surv(time = start, time2 = stop, event = force) ~ 
                     logcsumdeaths + logcsumdeathssq + left + right + nat + ercsr + 
                     pch + tch + diversity100 + shr_trans100 + mul_bases + log2(pop) + 
                     log2(gdppercapita) + vdem100 + ethnic100 + tropics100 + log2(elevation) +
                     EAP + ECA + LAC + NA. + SAS + SSA + GovernmentspendingGDP2010us, 
                   data = ma_df_noinact, na.action = na.omit, method="efron", x = TRUE)
splinter_cgc_govspend <- coxph(Surv(time = start, time2 = stop, event = splinter) ~ 
                        logcsumdeaths + logcsumdeathssq + left + right + nat + 
                        ercsr + pch + tch + diversity100 + shr_trans100 + mul_bases + 
                        log2(pop) + log2(gdppercapita) + vdem100 + ethnic100 + 
                        tropics100 + log2(elevation) + EAP + ECA + LAC + NA. + 
                        SAS + SSA + GovernmentspendingGDP2010us,
                      data = ma_df_noinact, na.action = na.omit, method="efron", x = TRUE)



# model summaries for latex
# function to turn coef into pct on hazard
pct_on_hz <- function(model) {(summary(model)$coef[,2] - 1) * 100} 
stde_trans <- function(model) {summary(model)$coef[,3] * summary(model)$coef[,2] * 100}
year_models_list_govspend <- list(vpp_yg_govspend, force_yg_govspend, splinter_yg_govspend, vpp_ygc_govspend, force_ygc_govspend, splinter_ygc_govspend)
decay_models_list_govspend <- list(vpp_dg_govspend, force_dg_govspend, splinter_dg_govspend, vpp_dgc_govspend, force_dgc_govspend, splinter_dgc_govspend)
csum_models_list_govspend <- list(vpp_cg_govspend, force_cg_govspend, splinter_cg_govspend, vpp_cgc_govspend, force_cgc_govspend, splinter_cgc_govspend)

# yearly
stargazer(year_models_list_govspend,
          coef = lapply(year_models_list_govspend, pct_on_hz),
          se = lapply(year_models_list_govspend, stde_trans),
          t.auto = FALSE, p.auto = FALSE, type = "html", out="year_gov.html", 
          out.header = TRUE, font.size = "small",
          column.labels=c("Victory/Pol.","Force","Splinter","Victory/Pol.","Force","Splinter"),
          covariate.labels = c("Fatalities (log)", "Fatalities Sq. (log)", "Left", "Right", 
                               "Nationalist", "Regime", "Policy", "Territory",
                               "Attack Diversity", "Share Trans. Terr.", "Multiple Bases",
                               "Pop (log)", "GDP/Pop (log)", "Democracy", "Ethnic Frac.",
                               "Tropics", "Elevation (log)", "East Asia \\& Pacific",
                               "Europe \\& Central Asia", "Latin Am. \\& Caribbean",
                               "North America", "South Asia", "Sub-Saharan Africa", "Gov. Spending"),
          omit.stat = c("rsq", "max.rsq", "lr", "wald"))
browseURL("year_gov.html")

stargazer(decay_models_list_govspend,
          coef = lapply(decay_models_list_govspend, pct_on_hz),
          se = lapply(decay_models_list_govspend, stde_trans),
          t.auto = FALSE, p.auto = FALSE, type = "html", out="dec_gov.html", 
          out.header = TRUE, font.size = "small",
          column.labels=c("Victory/Pol.","Force","Splinter","Victory/Pol.","Force","Splinter"),
          covariate.labels = c("Fatalities (log)", "Fatalities Sq. (log)", "Left", "Right", 
                               "Nationalist", "Regime", "Policy", "Territory",
                               "Attack Diversity", "Share Trans. Terr.", "Multiple Bases",
                               "Pop (log)", "GDP/Pop (log)", "Democracy", "Ethnic Frac.",
                               "Tropics", "Elevation (log)", "East Asia \\& Pacific",
                               "Europe \\& Central Asia", "Latin Am. \\& Caribbean",
                               "North America", "South Asia", "Sub-Saharan Africa", "Gov. Spending"),
          omit.stat = c("rsq", "max.rsq", "lr", "wald"))
browseURL("dec_gov.html")
# cumulative models list
stargazer(csum_models_list_govspend,
          coef = lapply(csum_models_list_govspend, pct_on_hz),
          se = lapply(csum_models_list_govspend, stde_trans),
          t.auto = FALSE, p.auto = FALSE, type = "html", out="csum_gov.html", 
          out.header = TRUE, font.size = "small",
          column.labels=c("Victory/Pol.","Force","Splinter","Victory/Pol.","Force","Splinter"),
          covariate.labels = c("Fatalities (log)", "Fatalities Sq. (log)", "Left", "Right", 
                               "Nationalist", "Regime", "Policy", "Territory",
                               "Attack Diversity", "Share Trans. Terr.", "Multiple Bases",
                               "Pop (log)", "GDP/Pop (log)", "Democracy", "Ethnic Frac.",
                               "Tropics", "Elevation (log)", "East Asia \\& Pacific",
                               "Europe \\& Central Asia", "Latin Am. \\& Caribbean",
                               "North America", "South Asia", "Sub-Saharan Africa", "Gov. Spending"),
          omit.stat = c("rsq", "max.rsq", "lr", "wald"))
browseURL("csum_gov.html")




# proportional hazards tests of main models for appendix
###############################
cox.zph(vpp_cg, transform = "km")
cox.zph(vpp_cgc)
cox.zph(force_cg) # pass... mul_bases .081
cox.zph(force_cgc, transform = "identity") # pass... mul_bases .081
cox.zph(splinter_cg) # left and nat
cox.zph(splinter_cgc,) # nat
cox.zph(vpp_dg)
cox.zph(vpp_dgc)
cox.zph(force_dg) # pass
cox.zph(force_dgc) # log2(elevation) .056, mul_bases.074
cox.zph(splinter_dg) # left and nat
cox.zph(splinter_dgc) # left and nat
cox.zph(vpp_yg)
cox.zph(vpp_ygc)
cox.zph(force_yg) # pass
cox.zph(force_ygc) # pass
cox.zph(splinter_yg) # left and nat
cox.zph(splinter_ygc) # left and nat

# zph table w/ km
print(
xtable(
  data.frame(
    VPP_Cumulative = cox.zph(vpp_cgc)$table[,3],
    Force_Cumulative = cox.zph(force_cgc)$table[,3],
    Splinter_Cumulative = cox.zph(splinter_cgc)$table[,3],
    VPP_Decay = cox.zph(vpp_dgc)$table[,3],
    Force_Decay = cox.zph(force_dgc)$table[,3],
    Splinter_Decay = cox.zph(splinter_dgc)$table[,3],
    VPP_Yea = cox.zph(vpp_ygc)$table[,3],
    Force_Year = cox.zph(force_ygc)$table[,3],
    Splinter_Year = cox.zph(splinter_ygc)$table[,3]
    #row.names = c("Fatalities (log)", "Fatalities Sq. (log)", "Left", "Right", "Nationalist", "Regime", "Policy", "Territory", "Attack Diversity", "Share Trans. Terr", "Multiple Bases", "Pop (log)", "GDP/Pop (log)", "Democracy", "Ethnic Frac.", "Tropics", "Elevation (log)", "East Asia & Pacific", "Europe & Central Asia", "Latin Am. & Caribbean", "North America", "South Asia", "Sub-Saharan Africa", "GLOBAL")
  ),
  digits = 3),
type = "html", file = "zph.html"
)
browseURL("zph.html")



par(mfrow=c(1,2))
plot(cox.zph(vpp_ygc)[1], ylab = "Beta(t) for Logged Yearly Deaths")
plot(cox.zph(vpp_ygc)[2], ylab = "Beta(t) for Logged Yearly Deaths Squared")

par(mfrow=c(2,2))
plot(cox.zph(vpp_cgc, transform = "identity")) # using identity to see where to put the spline knots
plot(cox.zph(vpp_dgc, transform = "identity"))
plot(cox.zph(vpp_ygc, transform = "identity"))

# testing how whole categorical variable behaves in zph test (instead of dummies)
ma_df_noinact <- ma_df_noinact %>%
  mutate(
    region = case_when(
        MENA == 1 ~ "mena", EAP == 1 ~ "eap", ECA == 1 ~ "eca", LAC == 1 ~ "lac",
        NA. == 1 ~ "na.", SAS == 1 ~ "sas", SSA == 1 ~ "ssa"
        ))
ma_df_noinact$region <- as.factor(ma_df_noinact$region)
ma_df_noinact$region <- relevel(ma_df_noinact$region, ref = "mena")
ma_df_noinact <- ma_df_noinact %>%
  mutate(goal = 
      case_when(
        pch == 1 ~ "pch", sq == 1 ~ "sq", tch == 1 ~ "tch",  ercsr == 1 ~ "ercsr"
      ))
ma_df_noinact$goal <- as.factor(ma_df_noinact$goal)
ma_df_noinact$goal <- relevel(ma_df_noinact$goal, ref = "sq")
ma_df_noinact <- ma_df_noinact %>%
  mutate(orientation = 
           case_when(
             left == 1 ~ "left", right == 1 ~ "right", nat == 1 ~ "nat",  rel == 1 ~ "rel"
           ))
ma_df_noinact$orientation <- as.factor(ma_df_noinact$orientation)
ma_df_noinact$orientation <- relevel(ma_df_noinact$orientation, ref = "rel")


par(mfrow=c(1,1))
region_fit <- survfit(Surv(time = start, time2 = stop, event = vpp) ~ region, data = ma_df_noinact)
plot(region_fit, fun = "cloglog", col = 1:8)
legend("topleft", legend = levels(ma_df_noinact$region), col = 1:8, lty=1)
plot(survfit(Surv(time = start, time2 = stop, event = vpp) ~ tch, data = ma_df_noinact))

TESTCOX <- coxph(Surv(time = start, time2 = stop, event = vpp) ~ 
                   logcsumdeaths + logcsumdeathssq + orientation + goal +
                   diversity100 + shr_trans100 + mul_bases + log2(pop) +  
                   log2(gdppercapita) + vdem100 + ethnic100 + tropics100 + log2(elevation) +
                   EAP + ECA + LAC + NA. + SAS + SSA, 
                 data = ma_df_noinact, na.action = na.omit, method="efron", x = TRUE)
plot(cox.zph(TESTCOX, transform = "identity"))
# ok.... it is still each level. onto the tt and strata models

set.seed(seed)
vpp_cgc_strata <- coxph(Surv(time = start, time2 = stop, event = vpp) ~ 
                   logcsumdeaths + logcsumdeathssq + left + right + nat + strata(ercsr) + 
                   strata(pch) + tch + diversity100 + tt(shr_trans100) + strata(mul_bases) + log2(pop) +  
                   tt(log2(gdppercapita)) + vdem100 + ethnic100 + tropics100 + log2(elevation) +
                   EAP + strata(ECA) + LAC + strata(NA.) + SAS + strata(SSA), 
                   tt = function(x, t, ...) x * nsk(t, knots = c(9, 23), Boundary.knots = FALSE),
                 data = ma_df_noinact, na.action = na.omit, method="efron", x = TRUE)
summary(vpp_cgc_strata)


vpp_dgc_strata <- coxph(Surv(time = start, time2 = stop, event = vpp) ~ 
                   logdecdeaths + logdecdeathssq + left + right + nat + strata(ercsr) + 
                   strata(pch) + tch + diversity100 + tt(shr_trans100) + strata(mul_bases) + tt(log2(pop)) +  
                   tt(log2(gdppercapita)) + vdem100 + ethnic100 + tropics100 + log2(elevation) +
                   EAP + strata(ECA) + LAC + strata(NA.) + SAS + strata(SSA), 
                 tt = function(x, t, ...) x * nsk(t, knots = c(9, 23), Boundary.knots = FALSE),
                 data = ma_df_noinact, na.action = na.omit, method="efron", x = TRUE)
summary(vpp_dgc_strata)


vpp_ygc_strata <- coxph(Surv(time = start, time2 = stop, event = vpp) ~ 
                   logtotaldeaths + logtotaldeathssq + left + right + nat + strata(ercsr) + 
                   strata(pch) + tch + diversity100 + tt(shr_trans100) + strata(mul_bases) + tt(log2(pop)) + 
                   tt(log2(gdppercapita)) + vdem100 + ethnic100 + tropics100 + log2(elevation) +
                   EAP + ECA + LAC + strata(NA.) + SAS + strata(SSA), 
                   tt = function(x, t, ...) x * nsk(t, knots = c(9, 23), Boundary.knots = FALSE),
                 data = ma_df_noinact, na.action = na.omit, method="efron")
summary(vpp_ygc_strata)



# model summaries for latex
# function to turn coef into pct on hazard
pct_on_hz <- function(model) {(summary(model)$coef[,2] - 1) * 100} 
stde_trans <- function(model) {summary(model)$coef[,3] * summary(model)$coef[,2] * 100}
vpp_ttstrata_models_list <- list(vpp_cgc_strata, vpp_dgc_strata, vpp_ygc_strata)

# yearly
stargazer(vpp_ttstrata_models_list,
          coef = lapply(vpp_ttstrata_models_list, pct_on_hz),
          se = lapply(vpp_ttstrata_models_list, stde_trans),
          t.auto = FALSE, p.auto = FALSE, type = "html", out="tt_strata.html", 
          out.header = TRUE, font.size = "small",
          column.labels=c("Cumulative","Decay","Yearly"),
          omit.stat = c("rsq", "max.rsq", "lr", "wald"))
browseURL("tt_strata.html")

