# 1. readying --------------------------------------------------------------------------------------
# loading packages --
pacman::p_load(openxlsx, tidyverse, ape, rtrees, ggtree, ggExtra, Rphylopars, glmmTMB, brms, performance, emmeans, MuMIn, piecewiseSEM)

# load theme --
Sylvan <- theme(
  # 全局字体
  text = element_text(family = "serif", size = 6, colour = "#000000"),
  # 标题
  plot.title = element_text(size = 10, face = "bold"),
  plot.subtitle = element_text(size = 9),
  plot.caption = element_text(size = 8),
  # 坐标轴
  axis.title = element_text(size = 10),
  axis.title.x = element_text(margin = margin(t = 0)),
  axis.title.y = element_text(margin = margin(r = 2)),
  axis.text = element_text(size = 9, colour = "#000000"),
  axis.ticks = element_line(colour = "#000000", linewidth = 0.3),
  # axis.line = element_line(colour = "#000000", size = 0.3),
  # 网格线
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # 面板背景 & 边框
  panel.background = element_rect(fill = "#ffffff", colour = NA),
  panel.border = element_rect(fill = NA, colour = "#000000", linewidth = 0.8),
  # 图例
  legend.title = element_text(face = "bold", size = 10),
  legend.text = element_text(size = 9),
  legend.position = "right",
  legend.background = element_rect(fill = "#ffffff", colour = NA),
  legend.key = element_rect(fill = "#ffffff", colour = NA),
  # 整体边距
  plot.margin = margin(t = 2, b = 2, l = 2, r = 2, "pt")
)

# load functions --
# function 01: exported the GLMM results --
save_glmm <- function(index, count) {
  model_list <- str_c("model_", index, str_pad(1:count, 2, side = "left", pad = "0"), sep = "")
  model_save <- NULL
  for (i in 1:n_distinct(model_list)) {
    model <- summary(get(model_list[i]))
    call <- as.character(model$call)
    # mult_comp <- pairs(emmeans(get(model_list[i]), as.formula(str_c("~ ", as.character(model$call$formula[[3]][2]))), adjust = "tukey")) |> as.data.frame()
    ID <- c(1, nrow(model$coefficients$cond), length(as.character(model$varcor)[1])) # , nrow(mult_comp)
    model_save <- bind_rows(model_save, data.frame(index = index, 
                                                   formula = c(str_c(call[1], "(formula = ", call[2], ", data = ", call[3], ", family = ", call[4], ")"), rep(NA, max(ID) - 1)), 
                                                   # AIC = c(model$AICtab[1], rep(NA, max(ID) - 1)), 
                                                   # BIC = c(model$AICtab[2], rep(NA, max(ID) - 1)), 
                                                   # LogLik = c(model$AICtab[3], rep(NA, max(ID) - 1)), 
                                                   # deviance = c(model$AICtab[4], rep(NA, max(ID) - 1)), 
                                                   # df_resid = c(model$AICtab[5], rep(NA, max(ID) - 1)), 
                                                   fix_name = c(row.names(model$coefficients$cond), rep(NA, max(ID) - ID[2])), 
                                                   fix_estimate = c(model$coefficients$cond[, 1], rep(NA, max(ID) - ID[2])), 
                                                   fix_se = c(model$coefficients$cond[, 2], rep(NA, max(ID) - ID[2])), 
                                                   fix_z_value = c(model$coefficients$cond[, 3], rep(NA, max(ID) - ID[2])), 
                                                   fix_p_value = c(model$coefficients$cond[, 4], rep(NA, max(ID) - ID[2])), 
                                                   # rand_group = c("site", rep(NA, max(ID) - ID[3])), 
                                                   # rand_name = c("intercept", rep(NA, max(ID) - ID[3])), 
                                                   # rand_var = c(str_extract(as.character(model$varcor)[1], "\\d+\\.\\d+"), rep(NA, max(ID) - ID[3])), 
                                                   # rand_sd = c(NA, rep(NA, max(ID) - ID[3])), 
                                                   R2_conditional = c(r2(get(model_list[i]))[[1]], rep(NA, max(ID) - 1)), 
                                                   R2_marginal = c(r2(get(model_list[i]))[[2]], rep(NA, max(ID) - 1))
                                                   # emmeans_contrast = c(mult_comp[, 1], rep(NA, max(ID) - ID[4])), 
                                                   # emmeans_estimate = c(mult_comp[, 2], rep(NA, max(ID) - ID[4])), 
                                                   # emmeans_se = c(mult_comp[, 3], rep(NA, max(ID) - ID[4])), 
                                                   # emmeans_df = c(mult_comp[, 4], rep(NA, max(ID) - ID[4])), 
                                                   # emmeans_z_ratio = c(mult_comp[, 5], rep(NA, max(ID) - ID[4])), 
                                                   # emmeans_p_value = c(mult_comp[, 6], rep(NA, max(ID) - ID[4]))
    ))
  }
  row.names(model_save) <- NULL
  return(model_save)
}

# function 02: exported the GLM results --
save_glm <- function(index, count) {
  model_list <- str_c("model_", index, str_pad(1:count, 2, side = "left", pad = "0"), sep = "")
  model_save <- NULL
  for (i in 1:n_distinct(model_list)) {
    model <- summary(get(model_list[i]))
    call <- as.character(model$call)
    ID <- c(1, nrow(model$coefficients$cond))
    model_save <- bind_rows(model_save, data.frame(index = index, 
                                                   formula = c(str_c(call[1], "(formula = ", call[2], ", data = ", call[3], ", family = ", call[4], ")"), rep(NA, max(ID) - 1)), 
                                                   # AIC = c(model$AICtab[1], rep(NA, max(ID) - 1)), 
                                                   # BIC = c(model$AICtab[2], rep(NA, max(ID) - 1)), 
                                                   # LogLik = c(model$AICtab[3], rep(NA, max(ID) - 1)), 
                                                   # deviance = c(model$AICtab[4], rep(NA, max(ID) - 1)), 
                                                   # df_resid = c(model$AICtab[5], rep(NA, max(ID) - 1)), 
                                                   fix_name = c(row.names(model$coefficients$cond), rep(NA, max(ID) - ID[2])), 
                                                   fix_estimate = c(model$coefficients$cond[, 1], rep(NA, max(ID) - ID[2])), 
                                                   fix_se = c(model$coefficients$cond[, 2], rep(NA, max(ID) - ID[2])), 
                                                   fix_z_value = c(model$coefficients$cond[, 3], rep(NA, max(ID) - ID[2])), 
                                                   fix_p_value = c(model$coefficients$cond[, 4], rep(NA, max(ID) - ID[2]))))
  }
  row.names(model_save) <- NULL
  return(model_save)
}

# function 03: z-transform for data --
scale_z <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

# load data --
load(file = "data/data_indi.rdata")
load(file = "data/data_spec.rdata")
load(file = "data/data_comm.rdata")

# 2. data analysis ---------------------------------------------------------------------------------
# __2.1 part1: multi-stemmed vs tree-damaged -------------------------------------------------------
# ____2.1.1 Q1: individual level -------------------------------------------------------------------
data_anal <- data_indi |> mutate(damage_1 = ifelse(n_damage_1 == n_stem, 1, 0), damage_2 = ifelse(n_damage_2 == n_stem, 1, 0), damage_3 = ifelse(n_damage_3 == n_stem, 1, 0), 
  damage_4 = ifelse(n_damage_4 == n_stem, 1, 0), damage_5 = ifelse(n_damage_5 == n_stem, 1, 0)) |> separate(quadrat, into = c("site", "plot"), remove = FALSE) |> 
  mutate(bin_stem = ifelse(n_stem > 1, "M", "S"), .after = n_stem) |> mutate(bin_stem = factor(bin_stem, levels = c("S", "M"), labels = c("single-stemmed", "multi-stemmed")))
model_save <- data.frame(); model_list <- list()
link_inv <- make.link("cloglog")$linkinv

# ______(1) damage_1 -------------------------------------------------------------------------------
data_anal |> group_by(n_stem) |> select(damage_1) |> mutate(ratio = sum(damage_1)/n()) |> slice_head(n = 1) |> ggplot(aes(x = n_stem, y = ratio)) + geom_col() + Sylvan

model_01 <- glmmTMB(damage_1 ~ bin_stem + (1 | site) + (1 | species_LN), family = binomial(link = "cloglog"), data = data_anal)
model_02 <- glmmTMB(damage_1 ~ n_stem + (1 | site) + (1 | species_LN), family = binomial(link = "cloglog"), data = data_anal)
model_03 <- glmmTMB(cbind(n_damage_1, n_stem - n_damage_1) ~ n_stem + (1 | site) + (1 | species_LN), family = binomial, data = data_anal)

# model <- model_01
# summary(model); r2(model); effects::allEffects(model) |> plot(multiline = TRUE, type = "response")

plot_01 <- ggeffects::ggpredict(model_01, "bin_stem") |> tibble() |> set_names("type", "estimate", "SE", "lower", "upper", "group") |> 
  ggplot() + geom_line(aes(x = type, y = estimate, group = 1), color = "#005f80") + 
  geom_segment(aes(x = type, y = lower, xend = type, yend = upper), color = "#f48a02") + 
  geom_point(aes(x = type, y = estimate), size = 2, fill = "#ffffff", shape = 21) + 
  geom_text(aes(x = 1.5, y = max(estimate)), label = "***", size = 3.2, family = "serif") + 
  labs(x = "", y = "Probability of tree damage") + Sylvan; plot_01

models <- str_c("model_", str_pad(1:3, 2, side = "left", pad = "0")); for (i in 1:length(models)) {model <- get(models[i]); model_list <- append(model_list, list(model))}
model_save <- bind_rows(model_save, save_glmm("", 3))

# ______(2) damage_2 -------------------------------------------------------------------------------
data_anal |> group_by(n_stem) |> select(damage_2) |> mutate(ratio = sum(damage_2)/n()) |> slice_head(n = 1) |> ggplot(aes(x = n_stem, y = ratio)) + geom_col() + Sylvan

model_01 <- glmmTMB(damage_2 ~ bin_stem + (1 | site) + (1 | species_LN), family = binomial(link = "cloglog"), data = data_anal)
model_02 <- glmmTMB(damage_2 ~ n_stem + (1 | site) + (1 | species_LN), family = binomial(link = "cloglog"), data = data_anal)
model_03 <- glmmTMB(cbind(n_damage_2, n_stem - n_damage_2) ~ n_stem + (1 | site) + (1 | species_LN), family = binomial, data = data_anal)

# model <- model_01
# summary(model); r2(model); effects::allEffects(model) |> plot(multiline = TRUE, type = "response")

plot_02 <- ggeffects::ggpredict(model_01, "bin_stem") |> tibble() |> set_names("type", "estimate", "SE", "lower", "upper", "group") |> 
  ggplot() + geom_line(aes(x = type, y = estimate, group = 1), color = "#005f80", linetype = 5) + 
  geom_segment(aes(x = type, y = lower, xend = type, yend = upper), color = "#f48a02") + 
  geom_point(aes(x = type, y = estimate), size = 2, fill = "#ffffff", shape = 21) + 
  geom_text(aes(x = 1.5, y = max(estimate)), label = "n.s.", size = 3.2, family = "serif") + 
  labs(x = "", y = "Probability of tree damage") + Sylvan; plot_02

models <- str_c("model_", str_pad(1:3, 2, side = "left", pad = "0")); for (i in 1:length(models)) {model <- get(models[i]); model_list <- append(model_list, list(model))}
model_save <- bind_rows(model_save, save_glmm("", 3))

# ______(3) damage_3 -------------------------------------------------------------------------------
data_anal |> group_by(n_stem) |> select(damage_3) |> mutate(ratio = sum(damage_3)/n()) |> slice_head(n = 1) |> ggplot(aes(x = n_stem, y = ratio)) + geom_col() + Sylvan

model_01 <- glmmTMB(damage_3 ~ bin_stem + (1 | site) + (1 | species_LN), family = binomial(link = "cloglog"), data = data_anal)
model_02 <- glmmTMB(damage_3 ~ n_stem + (1 | site) + (1 | species_LN), family = binomial(link = "cloglog"), data = data_anal)
model_03 <- glmmTMB(cbind(n_damage_3, n_stem - n_damage_3) ~ n_stem + (1 | site) + (1 | species_LN), family = binomial, data = data_anal)

# model <- model_01
# summary(model); r2(model); effects::allEffects(model) |> plot(multiline = TRUE, type = "response")

plot_03 <- ggeffects::ggpredict(model_01, "bin_stem") |> tibble() |> set_names("type", "estimate", "SE", "lower", "upper", "group") |> 
  ggplot() + geom_line(aes(x = type, y = estimate, group = 1), color = "#005f80") + 
  geom_segment(aes(x = type, y = lower, xend = type, yend = upper), color = "#f48a02") + 
  geom_point(aes(x = type, y = estimate), size = 2, fill = "#ffffff", shape = 21) + 
  geom_text(aes(x = 1.5, y = max(estimate)), label = "***", size = 3.2, family = "serif") + 
  labs(x = "", y = "Probability of tree damage") + Sylvan; plot_03

models <- str_c("model_", str_pad(1:3, 2, side = "left", pad = "0")); for (i in 1:length(models)) {model <- get(models[i]); model_list <- append(model_list, list(model))}
model_save <- bind_rows(model_save, save_glmm("", 3))

# ______(4) damage_4 -------------------------------------------------------------------------------
data_anal |> group_by(n_stem) |> select(damage_4) |> mutate(ratio = sum(damage_4)/n()) |> slice_head(n = 1) |> ggplot(aes(x = n_stem, y = ratio)) + geom_col() + Sylvan

model_01 <- glmmTMB(damage_4 ~ bin_stem + (1 | site) + (1 | species_LN), family = binomial(link = "cloglog"), data = data_anal)
model_02 <- glmmTMB(damage_4 ~ n_stem + (1 | site) + (1 | species_LN), family = binomial(link = "cloglog"), data = data_anal)
model_03 <- glmmTMB(cbind(n_damage_4, n_stem - n_damage_4) ~ n_stem + (1 | site) + (1 | species_LN), family = binomial, data = data_anal)

# model <- model_01
# summary(model); r2(model); effects::allEffects(model) |> plot(multiline = TRUE, type = "response")

plot_04 <- ggeffects::ggpredict(model_01, "bin_stem") |> tibble() |> set_names("type", "estimate", "SE", "lower", "upper", "group") |> 
  ggplot() + geom_line(aes(x = type, y = estimate, group = 1), color = "#005f80") + 
  geom_segment(aes(x = type, y = lower, xend = type, yend = upper), color = "#f48a02") + 
  geom_point(aes(x = type, y = estimate), size = 2, fill = "#ffffff", shape = 21) + 
  geom_text(aes(x = 1.5, y = max(estimate)), label = "***", size = 3.2, family = "serif") + 
  labs(x = "", y = "Probability of tree damage") + Sylvan; plot_04

models <- str_c("model_", str_pad(1:3, 2, side = "left", pad = "0")); for (i in 1:length(models)) {model <- get(models[i]); model_list <- append(model_list, list(model))}
model_save <- bind_rows(model_save, save_glmm("", 3))

# ______(5) damage_5 -------------------------------------------------------------------------------
data_anal |> group_by(n_stem) |> select(damage_5) |> mutate(ratio = sum(damage_5)/n()) |> slice_head(n = 1) |> ggplot(aes(x = n_stem, y = ratio)) + geom_col() + Sylvan

model_01 <- glmmTMB(damage_5 ~ bin_stem + (1 | site) + (1 | species_LN), family = binomial(link = "cloglog"), data = data_anal)
model_02 <- glmmTMB(damage_5 ~ n_stem + (1 | site) + (1 | species_LN), family = binomial(link = "cloglog"), data = data_anal)
model_03 <- glmmTMB(cbind(n_damage_5, n_stem - n_damage_5) ~ n_stem + (1 | site) + (1 | species_LN), family = binomial, data = data_anal)

# model <- model_01
# summary(model); r2(model); effects::allEffects(model) |> plot(multiline = TRUE, type = "response")

plot_05 <- ggeffects::ggpredict(model_01, "bin_stem") |> tibble() |> set_names("type", "estimate", "SE", "lower", "upper", "group") |> 
  ggplot() + geom_line(aes(x = type, y = estimate, group = 1), color = "#005f80") + 
  geom_segment(aes(x = type, y = lower, xend = type, yend = upper), color = "#f48a02") + 
  geom_point(aes(x = type, y = estimate), size = 2, fill = "#ffffff", shape = 21) + 
  geom_text(aes(x = 1.5, y = max(estimate)), label = "***", size = 3.2, family = "serif") + 
  labs(x = "", y = "Probability of tree damage") + Sylvan; plot_05

models <- str_c("model_", str_pad(1:3, 2, side = "left", pad = "0")); for (i in 1:length(models)) {model <- get(models[i]); model_list <- append(model_list, list(model))}
model_save <- bind_rows(model_save, save_glmm("", 3))

# ______(6) save results ---------------------------------------------------------------------------
cowplot::plot_grid(plot_01, plot_02, plot_03, NULL, plot_04, plot_05, nrow = 2) + 
  annotate(geom = "text", x = ggpp::as_npc(0.17), y = ggpp::as_npc(0.98), label = "Total",  size = 3, family = "serif", fontface = "bold") + 
  annotate(geom = "text", x = ggpp::as_npc(0.50), y = ggpp::as_npc(0.98), label = "Uproot", size = 3, family = "serif", fontface = "bold") + 
  annotate(geom = "text", x = ggpp::as_npc(0.83), y = ggpp::as_npc(0.98), label = "Below",  size = 3, family = "serif", fontface = "bold") + 
  annotate(geom = "text", x = ggpp::as_npc(0.50), y = ggpp::as_npc(0.48), label = "Up",     size = 3, family = "serif", fontface = "bold") + 
  annotate(geom = "text", x = ggpp::as_npc(0.83), y = ggpp::as_npc(0.48), label = "Branch", size = 3, family = "serif", fontface = "bold")
# ggsave(file = "save/part1_indi_20260430.tiff", width = 18, height = 12, units = "cm", dpi = 300, limitsize = FALSE, bg = "#ffffff")

# save(model_list, file = "save/part1_indi_list_20260430.rdata")
# save(model_save, file = "save/part1_indi_data_20260430.rdata")
# write.xlsx(model_save, "save/part1_indi_data_20260430.xlsx")

rm(model, models, model_list, model_save, i, model_01, model_02, model_03, plot_01, plot_02, plot_03, plot_04, plot_05, data_anal)

# ____2.1.2 Q2: species level ----------------------------------------------------------------------
data_anal <- data_spec |> filter(n_tree > 17)
model_save <- data.frame(); model_list <- list()

# ______(1) damage_1 -------------------------------------------------------------------------------
# model_01 <- glmmTMB(cbind(n_damage_1, n_stem - n_damage_1) ~ avg_stem, family = binomial, data = data_anal)
model_01 <- glmmTMB(cbind(n_damage_1, n_stem - n_damage_1) ~ avg_stem, family = betabinomial, data = data_anal)
summary(model_01); effects::allEffects(model_01) |> plot(multiline = TRUE, type = "response")

data_pred <- data.frame(avg_stem = seq(min(data_anal$avg_stem), max(data_anal$avg_stem), 0.1))
data_pred$damage <- predict(model_01, newdata = data_pred, type = "response", re.form = NA)
plot_01 <- ggplot() + geom_line(data = data_pred, aes(x = avg_stem, y = damage), color = "#ff0000") + 
  geom_point(data = data_anal, aes(x = avg_stem, y = DR_damage_1), size = 0.1, alpha = 0.5) + 
  geom_text(aes(x = ggpp::as_npc(0.9), y = ggpp::as_npc(0.9)), label = "***", size = 3.2, family = "serif") + 
  Sylvan + theme(legend.position = "none"); plot_01

# ______(2) damage_2 -------------------------------------------------------------------------------
# model_02 <- glmmTMB(cbind(n_damage_2, n_stem - n_damage_2) ~ avg_stem, family = binomial, data = data_anal)
model_02 <- glmmTMB(cbind(n_damage_2, n_stem - n_damage_2) ~ avg_stem, family = betabinomial, data = data_anal)
summary(model_02); effects::allEffects(model_02) |> plot(multiline = TRUE, type = "response")

data_pred <- data.frame(avg_stem = seq(min(data_anal$avg_stem), max(data_anal$avg_stem), 0.1))
data_pred$damage <- predict(model_02, newdata = data_pred, type = "response", re.form = NA)
plot_02 <- ggplot() + geom_line(data = data_pred, aes(x = avg_stem, y = damage), color = "#ff0000") + 
  geom_point(data = data_anal, aes(x = avg_stem, y = DR_damage_2), size = 0.1, alpha = 0.5) + 
  geom_text(aes(x = ggpp::as_npc(0.9), y = ggpp::as_npc(0.9)), label = "***", size = 3.2, family = "serif") + 
  Sylvan + theme(legend.position = "none"); plot_02

# ______(3) damage_3 -------------------------------------------------------------------------------
# model_03 <- glmmTMB(cbind(n_damage_3, n_stem - n_damage_3) ~ avg_stem, family = binomial, data = data_anal)
model_03 <- glmmTMB(cbind(n_damage_3, n_stem - n_damage_3) ~ avg_stem, family = betabinomial, data = data_anal)
summary(model_03); effects::allEffects(model_03) |> plot(multiline = TRUE, type = "response")

data_pred <- data.frame(avg_stem = seq(min(data_anal$avg_stem), max(data_anal$avg_stem), 0.1))
data_pred$damage <- predict(model_03, newdata = data_pred, type = "response", re.form = NA)
plot_03 <- ggplot() + geom_line(data = data_pred, aes(x = avg_stem, y = damage), color = "#ff0000") + 
  geom_point(data = data_anal, aes(x = avg_stem, y = DR_damage_3), size = 0.1, alpha = 0.5) + 
  geom_text(aes(x = ggpp::as_npc(0.9), y = ggpp::as_npc(0.9)), label = "***", size = 3.2, family = "serif") + 
  Sylvan + theme(legend.position = "none"); plot_03

# ______(4) damage_4 -------------------------------------------------------------------------------
# model_04 <- glmmTMB(cbind(n_damage_4, n_stem - n_damage_4) ~ avg_stem, family = binomial, data = data_anal)
model_04 <- glmmTMB(cbind(n_damage_4, n_stem - n_damage_4) ~ avg_stem, family = betabinomial, data = data_anal)
summary(model_04); effects::allEffects(model_04) |> plot(multiline = TRUE, type = "response")

data_pred <- data.frame(avg_stem = seq(min(data_anal$avg_stem), max(data_anal$avg_stem), 0.1))
data_pred$damage <- predict(model_04, newdata = data_pred, type = "response", re.form = NA)
plot_04 <- ggplot() + geom_line(data = data_pred, aes(x = avg_stem, y = damage), color = "#ff0000") + 
  geom_point(data = data_anal, aes(x = avg_stem, y = DR_damage_4), size = 0.1, alpha = 0.5) + 
  geom_text(aes(x = ggpp::as_npc(0.9), y = ggpp::as_npc(0.9)), label = "*", size = 3.2, family = "serif") + 
  Sylvan + theme(legend.position = "none"); plot_04

# ______(5) damage_5 -------------------------------------------------------------------------------
# model_05 <- glmmTMB(cbind(n_damage_5, n_stem - n_damage_5) ~ avg_stem, family = binomial, data = data_anal)
model_05 <- glmmTMB(cbind(n_damage_5, n_stem - n_damage_5) ~ avg_stem, family = betabinomial, data = data_anal)
summary(model_05); effects::allEffects(model_05) |> plot(multiline = TRUE, type = "response")

data_pred <- data.frame(avg_stem = seq(min(data_anal$avg_stem), max(data_anal$avg_stem), 0.1))
data_pred$damage <- predict(model_05, newdata = data_pred, type = "response", re.form = NA)
plot_05 <- ggplot() + geom_line(data = data_pred, aes(x = avg_stem, y = damage), color = "#ff0000") + 
  geom_point(data = data_anal, aes(x = avg_stem, y = DR_damage_5), size = 0.1, alpha = 0.5) + 
  geom_text(aes(x = ggpp::as_npc(0.9), y = ggpp::as_npc(0.9)), label = "**", size = 3.2, family = "serif") + 
  Sylvan + theme(legend.position = "none"); plot_05

# ______(6) save results ---------------------------------------------------------------------------
cowplot::plot_grid(plot_01, plot_02, plot_03, NULL, plot_04, plot_05, nrow = 2) + 
  annotate(geom = "text", x = ggpp::as_npc(0.17), y = ggpp::as_npc(0.98), label = "Total",  size = 3, family = "serif", fontface = "bold") + 
  annotate(geom = "text", x = ggpp::as_npc(0.50), y = ggpp::as_npc(0.98), label = "Uproot", size = 3, family = "serif", fontface = "bold") + 
  annotate(geom = "text", x = ggpp::as_npc(0.83), y = ggpp::as_npc(0.98), label = "Below",  size = 3, family = "serif", fontface = "bold") + 
  annotate(geom = "text", x = ggpp::as_npc(0.50), y = ggpp::as_npc(0.48), label = "Up",     size = 3, family = "serif", fontface = "bold") + 
  annotate(geom = "text", x = ggpp::as_npc(0.83), y = ggpp::as_npc(0.48), label = "Branch", size = 3, family = "serif", fontface = "bold")
# ggsave(file = "save/part1_spec_20260430.tiff", width = 18, height = 12, units = "cm", dpi = 300, limitsize = FALSE, bg = "#ffffff")

models <- str_c("model_", str_pad(1:5, 2, side = "left", pad = "0")); for (i in 1:length(models)) {model <- get(models[i]); model_list <- append(model_list, list(model))}
model_save <- bind_rows(model_save, save_glm("", 5))
# save(model_list, file = "save/part1_spec_list_20260430.rdata")
# save(model_save, file = "save/part1_spec_data_20260430.rdata")
# write.xlsx(model_save, "save/part1_spec_data_20260430.xlsx")

rm(model, models, model_list, model_save, i, model_01, model_02, model_03, model_04, model_05, plot_01, plot_02, plot_03, plot_04, plot_05, data_anal, data_pred)

# ____2.1.3 Q3: community level --------------------------------------------------------------------
data_anal <- data_comm |> separate(quadrat, into = c("site", "plot"), remove = FALSE)
model_save <- data.frame(); model_list <- list()

# ______(1) damage_1 -------------------------------------------------------------------------------
model_01 <- glmmTMB(cbind(n_damage_1, n_stem - n_damage_1) ~ avg_stem + (1 | site), family = binomial, data = data_anal)
# summary(model_01); r2(model_01); effects::allEffects(model_01) |> plot(multiline = TRUE, type = "response")

data_pred <- data.frame(avg_stem = seq(min(data_anal$avg_stem), max(data_anal$avg_stem), 0.1))
data_pred$damage <- predict(model_01, newdata = data_pred, type = "response", re.form = NA)
plot_01 <- ggplot() + geom_line(data = data_pred, aes(x = avg_stem, y = damage), color = "#ff0000") + 
  geom_point(data = data_anal, aes(x = avg_stem, y = DR_damage_1), size = 0.1, alpha = 0.5) + 
  geom_text(aes(x = ggpp::as_npc(0.9), y = ggpp::as_npc(0.9)), label = "***", size = 3.2, family = "serif") + 
  Sylvan + theme(legend.position = "none"); plot_01

# ______(2) damage_2 -------------------------------------------------------------------------------
model_02 <- glmmTMB(cbind(n_damage_2, n_stem - n_damage_2) ~ avg_stem + (1 | site), family = binomial, data = data_anal)
# summary(model_02); r2(model_02); effects::allEffects(model_02) |> plot(multiline = TRUE, type = "response")

data_pred <- data.frame(avg_stem = seq(min(data_anal$avg_stem), max(data_anal$avg_stem), 0.1))
data_pred$damage <- predict(model_02, newdata = data_pred, type = "response", re.form = NA)
plot_02 <- ggplot() + geom_line(data = data_pred, aes(x = avg_stem, y = damage), color = "#ff0000") + 
  geom_point(data = data_anal, aes(x = avg_stem, y = DR_damage_2), size = 0.1, alpha = 0.5) + 
  geom_text(aes(x = ggpp::as_npc(0.9), y = ggpp::as_npc(0.9)), label = "n.s.", size = 3.2, family = "serif") + 
  Sylvan + theme(legend.position = "none"); plot_02

# ______(3) damage_3 -------------------------------------------------------------------------------
model_03 <- glmmTMB(cbind(n_damage_3, n_stem - n_damage_3) ~ avg_stem + (1 | site), family = binomial, data = data_anal)
# summary(model_03); r2(model_03); effects::allEffects(model_03) |> plot(multiline = TRUE, type = "response")

data_pred <- data.frame(avg_stem = seq(min(data_anal$avg_stem), max(data_anal$avg_stem), 0.1))
data_pred$damage <- predict(model_03, newdata = data_pred, type = "response", re.form = NA)
plot_03 <- ggplot() + geom_line(data = data_pred, aes(x = avg_stem, y = damage), color = "#ff0000") + 
  geom_point(data = data_anal, aes(x = avg_stem, y = DR_damage_3), size = 0.1, alpha = 0.5) + 
  geom_text(aes(x = ggpp::as_npc(0.9), y = ggpp::as_npc(0.9)), label = "n.s.", size = 3.2, family = "serif") + 
  Sylvan + theme(legend.position = "none"); plot_03

# ______(4) damage_4 -------------------------------------------------------------------------------
model_04 <- glmmTMB(cbind(n_damage_4, n_stem - n_damage_4) ~ avg_stem + (1 | site), family = binomial, data = data_anal)
# summary(model_04); r2(model_04); effects::allEffects(model_04) |> plot(multiline = TRUE, type = "response")

data_pred <- data.frame(avg_stem = seq(min(data_anal$avg_stem), max(data_anal$avg_stem), 0.1))
data_pred$damage <- predict(model_04, newdata = data_pred, type = "response", re.form = NA)
plot_04 <- ggplot() + geom_line(data = data_pred, aes(x = avg_stem, y = damage), color = "#ff0000") + 
  geom_point(data = data_anal, aes(x = avg_stem, y = DR_damage_4), size = 0.1, alpha = 0.5) + 
  geom_text(aes(x = ggpp::as_npc(0.9), y = ggpp::as_npc(0.9)), label = "***", size = 3.2, family = "serif") + 
  Sylvan + theme(legend.position = "none"); plot_04

# ______(5) damage_5 -------------------------------------------------------------------------------
model_05 <- glmmTMB(cbind(n_damage_5, n_stem - n_damage_5) ~ avg_stem + (1 | site), family = binomial, data = data_anal)
# summary(model_05); r2(model_05); effects::allEffects(model_05) |> plot(multiline = TRUE, type = "response")

data_pred <- data.frame(avg_stem = seq(min(data_anal$avg_stem), max(data_anal$avg_stem), 0.1))
data_pred$damage <- predict(model_05, newdata = data_pred, type = "response", re.form = NA)
plot_05 <- ggplot() + geom_line(data = data_pred, aes(x = avg_stem, y = damage), color = "#ff0000") + 
  geom_point(data = data_anal, aes(x = avg_stem, y = DR_damage_5), size = 0.1, alpha = 0.5) + 
  geom_text(aes(x = ggpp::as_npc(0.9), y = ggpp::as_npc(0.9)), label = "**", size = 3.2, family = "serif") + 
  Sylvan + theme(legend.position = "none"); plot_05

# ______(6) save results ---------------------------------------------------------------------------
cowplot::plot_grid(plot_01, plot_02, plot_03, NULL, plot_04, plot_05, nrow = 2) + 
  annotate(geom = "text", x = ggpp::as_npc(0.17), y = ggpp::as_npc(0.98), label = "Total",  size = 3, family = "serif", fontface = "bold") + 
  annotate(geom = "text", x = ggpp::as_npc(0.50), y = ggpp::as_npc(0.98), label = "Uproot", size = 3, family = "serif", fontface = "bold") + 
  annotate(geom = "text", x = ggpp::as_npc(0.83), y = ggpp::as_npc(0.98), label = "Below",  size = 3, family = "serif", fontface = "bold") + 
  annotate(geom = "text", x = ggpp::as_npc(0.50), y = ggpp::as_npc(0.48), label = "Up",     size = 3, family = "serif", fontface = "bold") + 
  annotate(geom = "text", x = ggpp::as_npc(0.83), y = ggpp::as_npc(0.48), label = "Branch", size = 3, family = "serif", fontface = "bold")
# ggsave(file = "save/part1_comm_20260430.tiff", width = 18, height = 12, units = "cm", dpi = 300, limitsize = FALSE, bg = "#ffffff")

models <- str_c("model_", str_pad(1:5, 2, side = "left", pad = "0")); for (i in 1:length(models)) {model <- get(models[i]); model_list <- append(model_list, list(model))}
model_save <- bind_rows(model_save, save_glmm("", 5))
# save(model_list, file = "save/part1_comm_list_20260430.rdata")
# save(model_save, file = "save/part1_comm_data_20260430.rdata")
# write.xlsx(model_save, "save/part1_comm_data_20260430.xlsx")

rm(model, models, model_list, model_save, i, model_01, model_02, model_03, model_04, model_05, plot_01, plot_02, plot_03, plot_04, plot_05, data_anal, data_pred)

# __2.2 part2: multi-stemmed & related-factors -----------------------------------------------------
# ____2.2.1 principal component analysis -----------------------------------------------------------
data_PCA <- data_indi |> group_by(quadrat) |> mutate(dispersal_agent = ifelse(dispersal_agent == "Anemochory", 0, 1)) |> 
  select(quadrat, SLA, LeafN, seed_mass, seed_length, seed_width, dispersal_agent, height, rdmax, woody_density, P50) |> 
  mutate(across(SLA:P50, ~ mean(.))) |> slice_head(n = 1) |> ungroup() |> column_to_rownames("quadrat")
data_plot <- FactoMineR::PCA(data_PCA, scale.unit = TRUE, ncp = 10); summary(data_plot); rm(data_PCA)

# ____2.2.2 univariate models ----------------------------------------------------------------------
data_anal <- data_indi |> separate(quadrat, into = c("site", "plot"), remove = FALSE) |> mutate(n_tree = n_stem, .after = site) |> select(quadrat:n_tree, contains("n_damage"), 
    n_stem, H:NMB, SLA:LeafN, seed_mass, seed_length:seed_width, dispersal_agent, height, rdmax, woody_density, P50, altitude, forest_age) |> group_by(quadrat) |> 
  mutate(dispersal_agent = ifelse(dispersal_agent == "Anemochory", 0, 1)) |> mutate(forest_age = case_when(forest_age == "ESF" ~ 1, forest_age == "LSF" ~ 2, TRUE ~ 3)) |> 
  mutate(across(n_tree:n_damage_5, ~ sum(.)), across(n_stem:P50, ~ mean(.))) |> slice_head(n = 1) |> ungroup() |> mutate(alti_log = log(altitude)) |> 
  left_join(data_plot$ind$coord[, 1:2] |> as_tibble(rownames = "quadrat") |> rename("Trait_PC1" = "Dim.1", "Trait_PC2" = "Dim.2"))
# save(data_anal, "data/data_anal.rdata")

plot_01 <- ggplot(data_anal, aes(x = H,               y = n_stem)) + geom_jitter(height = 0, size = 0.1) + Sylvan; plot_01
plot_02 <- ggplot(data_anal, aes(x = BA,              y = n_stem)) + geom_jitter(height = 0, size = 0.1) + Sylvan; plot_02
plot_03 <- ggplot(data_anal, aes(x = NMB,             y = n_stem)) + geom_jitter(height = 0, size = 0.1) + Sylvan; plot_03
plot_04 <- ggplot(data_anal, aes(x = SLA,             y = n_stem)) + geom_jitter(height = 0, size = 0.1) + Sylvan; plot_04
plot_05 <- ggplot(data_anal, aes(x = LeafN,           y = n_stem)) + geom_jitter(height = 0, size = 0.1) + Sylvan; plot_05
plot_06 <- ggplot(data_anal, aes(x = seed_mass,       y = n_stem)) + geom_jitter(height = 0, size = 0.1) + Sylvan; plot_06
plot_07 <- ggplot(data_anal, aes(x = seed_length,     y = n_stem)) + geom_jitter(height = 0, size = 0.1) + Sylvan; plot_07
plot_08 <- ggplot(data_anal, aes(x = seed_width,      y = n_stem)) + geom_jitter(height = 0, size = 0.1) + Sylvan; plot_08
plot_09 <- ggplot(data_anal, aes(x = dispersal_agent, y = n_stem)) + geom_jitter(height = 0, size = 0.1) + Sylvan; plot_09
plot_10 <- ggplot(data_anal, aes(x = height,          y = n_stem)) + geom_jitter(height = 0, size = 0.1) + Sylvan; plot_10
plot_11 <- ggplot(data_anal, aes(x = rdmax,           y = n_stem)) + geom_jitter(height = 0, size = 0.1) + Sylvan; plot_11
plot_12 <- ggplot(data_anal, aes(x = woody_density,   y = n_stem)) + geom_jitter(height = 0, size = 0.1) + Sylvan; plot_12
plot_13 <- ggplot(data_anal, aes(x = P50,             y = n_stem)) + geom_jitter(height = 0, size = 0.1) + Sylvan; plot_13
plot_14 <- ggplot(data_anal, aes(x = Trait_PC1,       y = n_stem)) + geom_jitter(height = 0, size = 0.1) + Sylvan; plot_14
plot_15 <- ggplot(data_anal, aes(x = Trait_PC2,       y = n_stem)) + geom_jitter(height = 0, size = 0.1) + Sylvan; plot_15
plot_16 <- ggplot(data_anal, aes(x = altitude,        y = n_stem)) + geom_jitter(height = 0, size = 0.1) + Sylvan; plot_16
plot_17 <- ggplot(data_anal, aes(x = forest_age,      y = n_stem)) + geom_jitter(height = 0, size = 0.1) + Sylvan; plot_17

cowplot::plot_grid(plot_01, plot_02, plot_03, plot_14, NULL, plot_15, plot_16, NULL, plot_17, nrow = 3)
# ggsave(file = "save/part2_univ1_20260430.tiff", width = 18, height = 18, units = "cm", dpi = 300, limitsize = FALSE, bg = "#ffffff")

cowplot::plot_grid(plot_04, plot_05, NULL, NULL, plot_06, plot_07, plot_08, plot_09, plot_10, plot_11, plot_12, plot_13, nrow = 3)
# ggsave(file = "save/part2_univ2_20260430.tiff", width = 24, height = 18, units = "cm", dpi = 300, limitsize = FALSE, bg = "#ffffff")
rm(data_plot, plot_01, plot_02, plot_03, plot_04, plot_05, plot_06, plot_07, plot_08, plot_09, plot_10, plot_11, plot_12, plot_13, plot_14, plot_15, plot_16, plot_17)

model_01 <- glmmTMB(n_stem ~ H +               (1 | site), data = data_anal)
model_02 <- glmmTMB(n_stem ~ BA +              (1 | site), data = data_anal)
model_03 <- glmmTMB(n_stem ~ NMB +             (1 | site), data = data_anal)
model_04 <- glmmTMB(n_stem ~ SLA +             (1 | site), data = data_anal)
model_05 <- glmmTMB(n_stem ~ LeafN +           (1 | site), data = data_anal)
model_06 <- glmmTMB(n_stem ~ seed_mass +       (1 | site), data = data_anal)
model_07 <- glmmTMB(n_stem ~ seed_length +     (1 | site), data = data_anal)
model_08 <- glmmTMB(n_stem ~ seed_width +      (1 | site), data = data_anal)
model_09 <- glmmTMB(n_stem ~ dispersal_agent + (1 | site), data = data_anal)
model_10 <- glmmTMB(n_stem ~ height +          (1 | site), data = data_anal)
model_11 <- glmmTMB(n_stem ~ rdmax +           (1 | site), data = data_anal)
model_12 <- glmmTMB(n_stem ~ woody_density +   (1 | site), data = data_anal)
model_13 <- glmmTMB(n_stem ~ P50 +             (1 | site), data = data_anal)
model_14 <- glmmTMB(n_stem ~ Trait_PC1 +       (1 | site), data = data_anal)
model_15 <- glmmTMB(n_stem ~ Trait_PC2 +       (1 | site), data = data_anal)
model_16 <- glmmTMB(n_stem ~ altitude +        (1 | site), data = data_anal)
model_17 <- glmmTMB(n_stem ~ forest_age +      (1 | site), data = data_anal)

# model <- model_01
# summary(model); r2(model); effects::allEffects(model) |> plot(multiline = TRUE, type = "response")

model_save <- data.frame(); model_list <- list()
models <- str_c("model_", str_pad(1:17, 2, side = "left", pad = "0")); for (i in 1:length(models)) {model <- get(models[i]); model_list <- append(model_list, list(model))}
model_save <- bind_rows(model_save, save_glmm("", 17))
# save(model_list, file = "save/part2_univ_list_20260430.rdata")
# save(model_save, file = "save/part2_univ_data_20260430.rdata")
# write.xlsx(model_save, "save/part2_univ_data_20260430.xlsx")
rm(data_anal, data_plot, model, models, model_list, model_save, i, model_01, model_02, model_03, model_04, model_05, model_06, model_07, model_08, model_09, model_10, model_11, model_12, model_13, model_14, model_15, model_16, model_17)

# ____2.2.3 multivariate models --------------------------------------------------------------------
load(file = "data/data_anal.rdata")
data_anal <- data_anal |> select(site, n_stem:NMB, Trait_PC1:Trait_PC2, altitude:forest_age) |> mutate(across(n_stem:forest_age, ~ scale_z(.)))

model_all <- glmmTMB(n_stem ~ H + BA + NMB + Trait_PC1 + Trait_PC2 + altitude + forest_age + (1 | site), data = data_anal)
summary(model_all); r2(model_all)

model_set <- dredge(model_all, trace = 2, options(na.action = "na.fail")); save(model_set, file = "save/model_set_20260430.rdata")
subset(model_set, delta < 2)
model_avg <- model.avg(model_set, delta < 2)
summary(model_avg); sw(model_avg); barplot(t(sw(model_avg)), horiz = TRUE)

rm(model_all, model_set, model_avg, data_anal)

# __2.3 part3: multi-stemmed & related-factors vs tree-damaged -------------------------------------
# ____2.3.1 structural equation model --------------------------------------------------------------
load(file = "2_data/data_anal.rdata")
data_anal <- data_anal |> select(site:n_damage_5, n_stem:NMB, Trait_PC1:Trait_PC2, altitude:forest_age) |> mutate(across(n_stem:forest_age, ~ scale_z(.))) |> 
  mutate(alti1 = altitude, altiq = scale_z(altitude*altitude))
model_save <- data.frame()

# ______(1) damage_1 -------------------------------------------------------------------------------
data_sem <- data.frame(site = data_anal$site, N = data_anal$n_tree, DR = data_anal$n_damage_1/data_anal$n_tree, alti1 = data_anal$alti1, altiq = data_anal$altiq, 
  stan = data_anal$forest_age, H = data_anal$H, BA = data_anal$BA, NMB = data_anal$NMB, PC1 = data_anal$Trait_PC1, PC2 = data_anal$Trait_PC2, stem = data_anal$n_stem)
N <- data_sem$N

# model_sem_0 <- psem(glmmTMB(DR ~   altiq + stan + stem + H + BA + NMB + PC1 + PC2 + (1 | site), data_sem, family = binomial, weights = N), 
#                     glmmTMB(stem ~ altiq + stan + H + BA + NMB + PC1 + PC2 + (1 | site), data_sem), 
#                     glmmTMB(H ~    altiq + stan + PC1 + PC2 + (1 | site), data_sem), 
#                     glmmTMB(BA ~   altiq + stan + PC1 + PC2 + (1 | site), data_sem), 
#                     glmmTMB(NMB ~  altiq + stan + PC1 + PC2 + (1 | site), data_sem), 
#                     H %~~% BA, H %~~% NMB, BA %~~% NMB); summary(model_sem_0)

# model_sem_1 <- psem(glmmTMB(DR ~   altiq + stan + stem + H + BA + NMB + PC1 + PC2 + (1 | site), data_sem, family = binomial, weights = N), 
#                     glmmTMB(stem ~ altiq + stan + H + BA + NMB + PC1 + PC2 + (1 | site), data_sem), 
#                     glmmTMB(H ~    stan + PC1 + PC2 + (1 | site), data_sem), 
#                     glmmTMB(BA ~   stan + PC1 + (1 | site), data_sem), 
#                     glmmTMB(NMB ~  altiq + stan + PC1 + (1 | site), data_sem), 
#                     H %~~% BA, H %~~% NMB, BA %~~% NMB); summary(model_sem_1)

# model_sem_2 <- psem(glmmTMB(DR ~   altiq + stan + stem + H + BA + NMB + PC1 + PC2 + (1 | site), data_sem, family = binomial, weights = N), 
#                     glmmTMB(stem ~ stan + H + BA + NMB + PC1 + (1 | site), data_sem), 
#                     glmmTMB(H ~    stan + PC1 + PC2 + (1 | site), data_sem), 
#                     glmmTMB(BA ~   stan + PC1 + (1 | site), data_sem), 
#                     glmmTMB(NMB ~  altiq + stan + PC1 + (1 | site), data_sem), 
#                     H %~~% BA, H %~~% NMB, BA %~~% NMB); summary(model_sem_2)

model_sem_3 <- psem(glmmTMB(DR ~   altiq + stan + stem + H + BA + NMB + PC1 + PC2 + (1 | site), data_sem, family = binomial, weights = N), 
                    glmmTMB(stem ~ stan + H + BA + NMB + PC1 + (1 | site), data_sem), 
                    glmmTMB(H ~    stan + PC1 + PC2 + (1 | site), data_sem), 
                    glmmTMB(BA ~   stan + PC1 + (1 | site), data_sem), 
                    glmmTMB(NMB ~  altiq + stan + PC1 + (1 | site), data_sem), 
                    H %~~% BA, H %~~% NMB, BA %~~% NMB); (model_sem <- summary(model_sem_3))

model_save <- bind_rows(model_save, bind_cols(model_sem$coefficients[, -9] |> mutate(DR = "DR_total"), 
  bind_rows(model_sem$ChiSq, data.frame(del = rep(NA, nrow(model_sem$coefficients) - 1))) |> select(!del), 
  bind_rows(model_sem$Cstat, data.frame(del = rep(NA, nrow(model_sem$coefficients) - 1))) |> select(!del), 
  bind_rows(model_sem$R2, data.frame(del = rep(NA, nrow(model_sem$coefficients) - nrow(model_sem$R2)))) |> select(!del)) |> 
  set_names("Response_1", "Predictor", "Estimate", "Std_Error", "DF", "Crit_Value", "P_Value_1", "Std_Estimate", "DR", 
    "Chisq", "df_1", "P_Value_2", "Fisher_C", "df_2", "P_Value_3", "Response_2", "family", "link", "method", "Marginal", "Conditional"))

# ______(2) damage_2 -------------------------------------------------------------------------------
data_sem <- data.frame(site = data_anal$site, N = data_anal$n_tree, DR = data_anal$n_damage_2/data_anal$n_tree, alti1 = data_anal$alti1, altiq = data_anal$altiq, 
  stan = data_anal$forest_age, H = data_anal$H, BA = data_anal$BA, NMB = data_anal$NMB, PC1 = data_anal$Trait_PC1, PC2 = data_anal$Trait_PC2, stem = data_anal$n_stem)

# model_sem_0 <- psem(glmmTMB(DR ~   altiq + stan + stem + H + BA + NMB + PC1 + PC2 + (1 | site), data_sem, family = binomial, weights = N), 
#                     glmmTMB(stem ~ altiq + stan + H + BA + NMB + PC1 + PC2 + (1 | site), data_sem), 
#                     glmmTMB(H ~    altiq + stan + PC1 + PC2 + (1 | site), data_sem), 
#                     glmmTMB(BA ~   altiq + stan + PC1 + PC2 + (1 | site), data_sem), 
#                     glmmTMB(NMB ~  altiq + stan + PC1 + PC2 + (1 | site), data_sem), 
#                     H %~~% BA, H %~~% NMB, BA %~~% NMB); summary(model_sem_0)

# model_sem_1 <- psem(glmmTMB(DR ~   altiq + stan + stem + H + BA + NMB + PC1 + PC2 + (1 | site), data_sem, family = binomial, weights = N), 
#                     glmmTMB(stem ~ altiq + stan + H + BA + NMB + PC1 + PC2 + (1 | site), data_sem), 
#                     glmmTMB(H ~    stan + PC1 + PC2 + (1 | site), data_sem), 
#                     glmmTMB(BA ~   stan + PC1 + (1 | site), data_sem), 
#                     glmmTMB(NMB ~  altiq + stan + PC1 + (1 | site), data_sem), 
#                     H %~~% BA, H %~~% NMB, BA %~~% NMB); summary(model_sem_1)

# model_sem_2 <- psem(glmmTMB(DR ~   altiq + stan + stem + H + BA + NMB + PC1 + PC2 + (1 | site), data_sem, family = binomial, weights = N), 
#                     glmmTMB(stem ~ stan + H + BA + NMB + PC1 + (1 | site), data_sem), 
#                     glmmTMB(H ~    stan + PC1 + PC2 + (1 | site), data_sem), 
#                     glmmTMB(BA ~   stan + PC1 + (1 | site), data_sem), 
#                     glmmTMB(NMB ~  altiq + stan + PC1 + (1 | site), data_sem), 
#                     H %~~% BA, H %~~% NMB, BA %~~% NMB); summary(model_sem_2)

model_sem_3 <- psem(glmmTMB(DR ~   altiq + stan + H + BA + NMB + (1 | site), data_sem, family = binomial, weights = N), 
                    glmmTMB(stem ~ stan + H + BA + NMB + PC1 + (1 | site), data_sem), 
                    glmmTMB(H ~    stan + PC1 + PC2 + (1 | site), data_sem), 
                    glmmTMB(BA ~   stan + PC1 + (1 | site), data_sem), 
                    glmmTMB(NMB ~  altiq + stan + PC1 + (1 | site), data_sem), 
                    H %~~% BA, H %~~% NMB, BA %~~% NMB); (model_sem <- summary(model_sem_3))

model_save <- bind_rows(model_save, bind_cols(model_sem$coefficients[, -9] |> mutate(DR = "DR_uproot"), 
  bind_rows(model_sem$ChiSq, data.frame(del = rep(NA, nrow(model_sem$coefficients) - 1))) |> select(!del), 
  bind_rows(model_sem$Cstat, data.frame(del = rep(NA, nrow(model_sem$coefficients) - 1))) |> select(!del), 
  bind_rows(model_sem$R2, data.frame(del = rep(NA, nrow(model_sem$coefficients) - nrow(model_sem$R2)))) |> select(!del)) |> 
  set_names("Response_1", "Predictor", "Estimate", "Std_Error", "DF", "Crit_Value", "P_Value_1", "Std_Estimate", "DR", 
    "Chisq", "df_1", "P_Value_2", "Fisher_C", "df_2", "P_Value_3", "Response_2", "family", "link", "method", "Marginal", "Conditional"))

# ______(3) damage_3 -------------------------------------------------------------------------------
data_sem <- data.frame(site = data_anal$site, N = data_anal$n_tree, DR = data_anal$n_damage_3/data_anal$n_tree, alti1 = data_anal$alti1, altiq = data_anal$altiq, 
  stan = data_anal$forest_age, H = data_anal$H, BA = data_anal$BA, NMB = data_anal$NMB, PC1 = data_anal$Trait_PC1, PC2 = data_anal$Trait_PC2, stem = data_anal$n_stem)

# model_sem_0 <- psem(glmmTMB(DR ~   altiq + stan + stem + H + BA + NMB + PC1 + PC2 + (1 | site), data_sem, family = binomial, weights = N), 
#                     glmmTMB(stem ~ altiq + stan + H + BA + NMB + PC1 + PC2 + (1 | site), data_sem), 
#                     glmmTMB(H ~    altiq + stan + PC1 + PC2 + (1 | site), data_sem), 
#                     glmmTMB(BA ~   altiq + stan + PC1 + PC2 + (1 | site), data_sem), 
#                     glmmTMB(NMB ~  altiq + stan + PC1 + PC2 + (1 | site), data_sem), 
#                     H %~~% BA, H %~~% NMB, BA %~~% NMB); summary(model_sem_0)

# model_sem_1 <- psem(glmmTMB(DR ~   altiq + stan + stem + H + BA + NMB + PC1 + PC2 + (1 | site), data_sem, family = binomial, weights = N), 
#                     glmmTMB(stem ~ altiq + stan + H + BA + NMB + PC1 + PC2 + (1 | site), data_sem), 
#                     glmmTMB(H ~    stan + PC1 + PC2 + (1 | site), data_sem), 
#                     glmmTMB(BA ~   stan + PC1 + (1 | site), data_sem), 
#                     glmmTMB(NMB ~  altiq + stan + PC1 + (1 | site), data_sem), 
#                     H %~~% BA, H %~~% NMB, BA %~~% NMB); summary(model_sem_1)

# model_sem_2 <- psem(glmmTMB(DR ~   altiq + stan + stem + H + BA + NMB + PC1 + PC2 + (1 | site), data_sem, family = binomial, weights = N), 
#                     glmmTMB(stem ~ stan + H + BA + NMB + PC1 + (1 | site), data_sem), 
#                     glmmTMB(H ~    stan + PC1 + PC2 + (1 | site), data_sem), 
#                     glmmTMB(BA ~   stan + PC1 + (1 | site), data_sem), 
#                     glmmTMB(NMB ~  altiq + stan + PC1 + (1 | site), data_sem), 
#                     H %~~% BA, H %~~% NMB, BA %~~% NMB); summary(model_sem_2)

model_sem_3 <- psem(glmmTMB(DR ~   altiq + H + BA + NMB + PC2 + (1 | site), data_sem, family = binomial, weights = N), 
                    glmmTMB(stem ~ stan + H + BA + NMB + PC1 + (1 | site), data_sem), 
                    glmmTMB(H ~    stan + PC1 + PC2 + (1 | site), data_sem), 
                    glmmTMB(BA ~   stan + PC1 + (1 | site), data_sem), 
                    glmmTMB(NMB ~  altiq + stan + PC1 + (1 | site), data_sem), 
                    H %~~% BA, H %~~% NMB, BA %~~% NMB); (model_sem <- summary(model_sem_3))

model_save <- bind_rows(model_save, bind_cols(model_sem$coefficients[, -9] |> mutate(DR = "DR_below"), 
  bind_rows(model_sem$ChiSq, data.frame(del = rep(NA, nrow(model_sem$coefficients) - 1))) |> select(!del), 
  bind_rows(model_sem$Cstat, data.frame(del = rep(NA, nrow(model_sem$coefficients) - 1))) |> select(!del), 
  bind_rows(model_sem$R2, data.frame(del = rep(NA, nrow(model_sem$coefficients) - nrow(model_sem$R2)))) |> select(!del)) |> 
  set_names("Response_1", "Predictor", "Estimate", "Std_Error", "DF", "Crit_Value", "P_Value_1", "Std_Estimate", "DR", 
    "Chisq", "df_1", "P_Value_2", "Fisher_C", "df_2", "P_Value_3", "Response_2", "family", "link", "method", "Marginal", "Conditional"))

# ______(4) damage_4 -------------------------------------------------------------------------------
data_sem <- data.frame(site = data_anal$site, N = data_anal$n_tree, DR = data_anal$n_damage_4/data_anal$n_tree, alti1 = data_anal$alti1, altiq = data_anal$altiq, 
  stan = data_anal$forest_age, H = data_anal$H, BA = data_anal$BA, NMB = data_anal$NMB, PC1 = data_anal$Trait_PC1, PC2 = data_anal$Trait_PC2, stem = data_anal$n_stem)

# model_sem_0 <- psem(glmmTMB(DR ~   altiq + stan + stem + H + BA + NMB + PC1 + PC2 + (1 | site), data_sem, family = binomial, weights = N), 
#                     glmmTMB(stem ~ altiq + stan + H + BA + NMB + PC1 + PC2 + (1 | site), data_sem), 
#                     glmmTMB(H ~    altiq + stan + PC1 + PC2 + (1 | site), data_sem), 
#                     glmmTMB(BA ~   altiq + stan + PC1 + PC2 + (1 | site), data_sem), 
#                     glmmTMB(NMB ~  altiq + stan + PC1 + PC2 + (1 | site), data_sem), 
#                     H %~~% BA, H %~~% NMB, BA %~~% NMB); summary(model_sem_0)

# model_sem_1 <- psem(glmmTMB(DR ~   altiq + stan + stem + H + BA + NMB + PC1 + PC2 + (1 | site), data_sem, family = binomial, weights = N), 
#                     glmmTMB(stem ~ altiq + stan + H + BA + NMB + PC1 + PC2 + (1 | site), data_sem), 
#                     glmmTMB(H ~    stan + PC1 + PC2 + (1 | site), data_sem), 
#                     glmmTMB(BA ~   stan + PC1 + (1 | site), data_sem), 
#                     glmmTMB(NMB ~  altiq + stan + PC1 + (1 | site), data_sem), 
#                     H %~~% BA, H %~~% NMB, BA %~~% NMB); summary(model_sem_1)

# model_sem_2 <- psem(glmmTMB(DR ~   altiq + stan + stem + H + BA + NMB + PC1 + PC2 + (1 | site), data_sem, family = binomial, weights = N), 
#                     glmmTMB(stem ~ stan + H + BA + NMB + PC1 + (1 | site), data_sem), 
#                     glmmTMB(H ~    stan + PC1 + PC2 + (1 | site), data_sem), 
#                     glmmTMB(BA ~   stan + PC1 + (1 | site), data_sem), 
#                     glmmTMB(NMB ~  altiq + stan + PC1 + (1 | site), data_sem), 
#                     H %~~% BA, H %~~% NMB, BA %~~% NMB); summary(model_sem_2)

model_sem_3 <- psem(glmmTMB(DR ~   altiq + stem + H + BA + NMB + PC1 + PC2 + (1 | site), data_sem, family = binomial, weights = N), 
                    glmmTMB(stem ~ stan + H + BA + NMB + PC1 + (1 | site), data_sem), 
                    glmmTMB(H ~    stan + PC1 + PC2 + (1 | site), data_sem), 
                    glmmTMB(BA ~   stan + PC1 + (1 | site), data_sem), 
                    glmmTMB(NMB ~  altiq + stan + PC1 + (1 | site), data_sem), 
                    H %~~% BA, H %~~% NMB, BA %~~% NMB); (model_sem <- summary(model_sem_3))

model_save <- bind_rows(model_save, bind_cols(model_sem$coefficients[, -9] |> mutate(DR = "DR_up"), 
  bind_rows(model_sem$ChiSq, data.frame(del = rep(NA, nrow(model_sem$coefficients) - 1))) |> select(!del), 
  bind_rows(model_sem$Cstat, data.frame(del = rep(NA, nrow(model_sem$coefficients) - 1))) |> select(!del), 
  bind_rows(model_sem$R2, data.frame(del = rep(NA, nrow(model_sem$coefficients) - nrow(model_sem$R2)))) |> select(!del)) |> 
  set_names("Response_1", "Predictor", "Estimate", "Std_Error", "DF", "Crit_Value", "P_Value_1", "Std_Estimate", "DR", 
    "Chisq", "df_1", "P_Value_2", "Fisher_C", "df_2", "P_Value_3", "Response_2", "family", "link", "method", "Marginal", "Conditional"))

# ______(5) damage_5 -------------------------------------------------------------------------------
data_sem <- data.frame(site = data_anal$site, N = data_anal$n_tree, DR = data_anal$n_damage_5/data_anal$n_tree, alti1 = data_anal$alti1, altiq = data_anal$altiq, 
  stan = data_anal$forest_age, H = data_anal$H, BA = data_anal$BA, NMB = data_anal$NMB, PC1 = data_anal$Trait_PC1, PC2 = data_anal$Trait_PC2, stem = data_anal$n_stem)

# model_sem_0 <- psem(glmmTMB(DR ~   alti1 + stan + stem + H + BA + NMB + PC1 + PC2 + (1 | site), data_sem, family = binomial, weights = N), 
#                     glmmTMB(stem ~ alti1 + stan + H + BA + NMB + PC1 + PC2 + (1 | site), data_sem), 
#                     glmmTMB(H ~    alti1 + stan + PC1 + PC2 + (1 | site), data_sem), 
#                     glmmTMB(BA ~   alti1 + stan + PC1 + PC2 + (1 | site), data_sem), 
#                     glmmTMB(NMB ~  alti1 + stan + PC1 + PC2 + (1 | site), data_sem), 
#                     H %~~% BA, H %~~% NMB, BA %~~% NMB); summary(model_sem_0)

# model_sem_1 <- psem(glmmTMB(DR ~   alti1 + stan + stem + H + BA + NMB + PC1 + PC2 + (1 | site), data_sem, family = binomial, weights = N), 
#                     glmmTMB(stem ~ alti1 + stan + H + BA + NMB + PC1 + PC2 + (1 | site), data_sem), 
#                     glmmTMB(H ~    alti1 + stan + PC1 + PC2 + (1 | site), data_sem), 
#                     glmmTMB(BA ~   alti1 + stan + PC1 + (1 | site), data_sem), 
#                     glmmTMB(NMB ~  alti1 + PC1 + (1 | site), data_sem), 
#                     H %~~% BA, H %~~% NMB, BA %~~% NMB); summary(model_sem_1)

# model_sem_2 <- psem(glmmTMB(DR ~   alti1 + stan + stem + H + BA + NMB + PC1 + PC2 + (1 | site), data_sem, family = binomial, weights = N), 
#                     glmmTMB(stem ~ stan + H + BA + NMB + PC1 + (1 | site), data_sem), 
#                     glmmTMB(H ~    alti1 + stan + PC1 + PC2 + (1 | site), data_sem), 
#                     glmmTMB(BA ~   alti1 + stan + PC1 + (1 | site), data_sem), 
#                     glmmTMB(NMB ~  alti1 + PC1 + (1 | site), data_sem), 
#                     H %~~% BA, H %~~% NMB, BA %~~% NMB); summary(model_sem_2)

model_sem_3 <- psem(glmmTMB(DR ~   stem + H + BA + NMB + PC2 + (1 | site), data_sem, family = binomial, weights = N), 
                    glmmTMB(stem ~ stan + H + BA + NMB + PC1 + (1 | site), data_sem), 
                    glmmTMB(H ~    alti1 + stan + PC1 + PC2 + (1 | site), data_sem), 
                    glmmTMB(BA ~   alti1 + stan + PC1 + (1 | site), data_sem), 
                    glmmTMB(NMB ~  alti1 + PC1 + (1 | site), data_sem), 
                    H %~~% BA, H %~~% NMB, BA %~~% NMB); (model_sem <- summary(model_sem_3))

model_save <- bind_rows(model_save, bind_cols(model_sem$coefficients[, -9] |> mutate(DR = "DR_branch"), 
  bind_rows(model_sem$ChiSq, data.frame(del = rep(NA, nrow(model_sem$coefficients) - 1))) |> select(!del), 
  bind_rows(model_sem$Cstat, data.frame(del = rep(NA, nrow(model_sem$coefficients) - 1))) |> select(!del), 
  bind_rows(model_sem$R2, data.frame(del = rep(NA, nrow(model_sem$coefficients) - nrow(model_sem$R2)))) |> select(!del)) |> 
  set_names("Response_1", "Predictor", "Estimate", "Std_Error", "DF", "Crit_Value", "P_Value_1", "Std_Estimate", "DR", 
    "Chisq", "df_1", "P_Value_2", "Fisher_C", "df_2", "P_Value_3", "Response_2", "family", "link", "method", "Marginal", "Conditional"))

# ______(6) save results ---------------------------------------------------------------------------
# save(model_save, file = "save/part3_SEM_data_20260430.rdata")
# write.xlsx(model_save, "save/part3_SEM_data_20260430.xlsx")
rm(data_sem, data_anal, model_sem_0, model_sem_1, model_sem_2, model_sem_3, model_sem, model_save, N, weights)

# 3. data visualization ----------------------------------------------------------------------------
# __3.1 figure 1 -----------------------------------------------------------------------------------
# ____3.1.1 Fig 1A ---------------------------------------------------------------------------------
# use PPT for drawing --

# ____3.1.2 Fig 1B ---------------------------------------------------------------------------------
# use PPT for drawing --

# ____3.1.3 Fig 1C ---------------------------------------------------------------------------------
data_anal <- data_indi |> mutate(damage_1 = ifelse(n_damage_1 == n_stem, 1, 0), damage_2 = ifelse(n_damage_2 == n_stem, 1, 0), damage_3 = ifelse(n_damage_3 == n_stem, 1, 0), 
  damage_4 = ifelse(n_damage_4 == n_stem, 1, 0), damage_5 = ifelse(n_damage_5 == n_stem, 1, 0)) |> separate(quadrat, into = c("site", "plot"), remove = FALSE) |> 
  mutate(bin_stem = ifelse(n_stem > 1, "M", "S"), .after = n_stem) |> mutate(bin_stem = factor(bin_stem, levels = c("S", "M"), labels = c("single-stemmed", "multi-stemmed")))
link_inv <- make.link("cloglog")$linkinv

model <- glmmTMB(damage_1 ~ bin_stem + (1 | site) + (1 | species_LN), family = binomial(link = "cloglog"), data = data_anal)
# summary(model); r2(model); effects::allEffects(model) |> plot(multiline = TRUE, type = "response")

plot_01 <- ggeffects::ggpredict(model, "bin_stem") |> tibble() |> set_names("type", "estimate", "SE", "lower", "upper", "group") |> 
  ggplot() + 
  geom_line(aes(x = type, y = estimate, group = 1), color = "#005f80", linewidth = 1) + 
  geom_segment(aes(x = type, y = lower, xend = type, yend = upper), color = "#f48a02", linewidth = 1) + 
  geom_point(aes(x = type, y = estimate), size = 3.6, fill = "#ffffff", shape = 21) + 
  geom_text(aes(x = 1.5, y = ggpp::as_npc(0.95)), label = "Individual level", size = (10*0.35), family = "serif") + 
  geom_text(aes(x = 2, y = ggpp::as_npc(0.85)), label = expression(italic(P)*" < 0.001***"), size = (9*0.35), family = "serif") + 
  scale_x_discrete(labels = c("single-stemmed    ", "    multi-stemmed")) + 
  scale_y_continuous(limits = c(0.04, 0.18), breaks = c(0.04, 0.11, 0.18)) + 
  labs(x = "Stem category", y = "Tree damage probability") + 
  Sylvan; plot_01

# ____3.1.4 Fig 1D ---------------------------------------------------------------------------------
data_anal <- data_spec |> filter(n_tree > 17)
model <- glmmTMB(cbind(n_damage_1, n_stem - n_damage_1) ~ avg_stem, family = betabinomial, data = data_anal)
summary(model); data_anal |> summarise(min = min(avg_stem), max = max(avg_stem))
data <- effects::allEffects(model, xlevels = list(avg_stem = seq(1.00, 2.34, 0.01)))[[1]] |> as.data.frame()

plot_02 <- ggplot() + 
  geom_point(data = data_anal, aes(x = avg_stem, y = DR_damage_1), color = "#bfbfbf", shape = 16, size = 1.6, alpha = 0.7) + 
  geom_ribbon(data = data, aes(x = avg_stem, y = fit, ymin = lower, ymax = upper), fill = "#aa71a4", alpha = 0.2) + 
  geom_line(data = data, aes(x = avg_stem, y = fit), color = "#aa71a4") + 
  scale_x_continuous(limits = c(1, 2.4), breaks = c(1, 1.7, 2.4)) + 
  scale_y_continuous(limits = c(0, 0.6), breaks = c(0, 0.3, 0.6)) + 
  labs(x = "Stem abundance", y = "Total damage ratio [%]") + 
  geom_text(aes(x = ggpp::as_npc(0.50), y = ggpp::as_npc(0.95)), label = "Species level", size = (10*0.35), family = "serif") + 
  geom_text(aes(x = ggpp::as_npc(0.73), y = ggpp::as_npc(0.85)), label = expression(italic(P)*" < 0.001***"), size = (9*0.35), family = "serif") + 
  Sylvan + 
  theme(legend.position = "none"); plot_02

# ____3.1.5 Fig 1E ---------------------------------------------------------------------------------
data_anal <- data_comm |> separate(quadrat, into = c("site", "plot"), remove = FALSE)
model <- glmmTMB(cbind(n_damage_1, n_stem - n_damage_1) ~ avg_stem + (1 | site), family = binomial, data = data_anal)
summary(model); data_anal |> summarise(min = min(avg_stem), max = max(avg_stem))
data <- effects::allEffects(model, xlevels = list(avg_stem = seq(1.00, 2.73, 0.01)))[[1]] |> as.data.frame()

plot_03 <- ggplot() + 
  geom_point(data = data_anal, aes(x = avg_stem, y = DR_damage_1), color = "#bfbfbf", shape = 16, size = 1.6, alpha = 0.7) + 
  geom_ribbon(data = data, aes(x = avg_stem, y = fit, ymin = lower, ymax = upper), fill = "#aa71a4", alpha = 0.2) + 
  geom_line(data = data, aes(x = avg_stem, y = fit), color = "#aa71a4") + 
  scale_x_continuous(limits = c(1, 2.8), breaks = c(1, 1.9, 2.8)) + 
  scale_y_continuous(limits = c(0, 0.8), breaks = c(0, 0.4, 0.8)) + 
  labs(x = "Stem abundance", y = "Total damage ratio [%]") + 
  geom_text(aes(x = ggpp::as_npc(0.50), y = ggpp::as_npc(0.95)), label = "Community level", size = (10*0.35), family = "serif") + 
  geom_text(aes(x = ggpp::as_npc(0.73), y = ggpp::as_npc(0.85)), label = expression(italic(P)*" < 0.001***"), size = (9*0.35), family = "serif") + 
  Sylvan + 
  theme(legend.position = "none"); plot_03

cowplot::plot_grid(plot_01, plot_02, plot_03, nrow = 1, align = "hv")
# ggsave(file = "save/fig_1_20260430.tiff", width = 16, height = 5, units = "cm", dpi = 300, limitsize = FALSE, bg = "#ffffff")
rm(data, data_anal, model, plot_01, plot_02, plot_03)

# __3.2 figure 2 -----------------------------------------------------------------------------------
load(file = "data/data_anal.rdata")

# ____3.2.1 Fig 2A ---------------------------------------------------------------------------------
plot_01 <- ggplot() + 
  geom_point(data = data_plot$ind$coord, aes(x = Dim.1, y = Dim.2), color = "#bfbfbf", shape = 16, alpha = 0.7) + 
  geom_vline(xintercept = 0, linetype = 5, , linewidth = 0.4) + 
  geom_hline(yintercept = 0, linetype = 5, , linewidth = 0.4) + 
  geom_segment(data = bind_cols(data_plot$var$coord), aes(x = 0, y = 0, xend = Dim.1*6, yend = Dim.2*6, color = Dim.1*Dim.1 + Dim.2*Dim.2), 
    arrow = arrow(angle = 15, length = unit(0.075, "inches"), type = "closed"), linewidth = 0.6) + 
  # ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 7), alpha = 0.8, linewidth = 1) + 
  scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07")) + 
  labs(x = "Trait PC1 (59.24%)", y = "Trait PC2 (14.43%)", title = NULL, color = NULL) + 
  annotate(geom = "text", x = -5.40, y = -0.70, label = "SLA", size = (9*0.35), family = "serif") + 
  annotate(geom = "text", x =  0.60, y =  6.00, label = "LeafN", size = (9*0.35), family = "serif") + 
  annotate(geom = "text", x =  5.90, y =  0.50, label = "SM", size = (9*0.35), family = "serif") + 
  annotate(geom = "text", x =  6.00, y = -0.30, label = "SL", size = (9*0.35), family = "serif") + 
  annotate(geom = "text", x =  5.70, y = -0.80, label = "SW", size = (9*0.35), family = "serif") + 
  annotate(geom = "text", x =  4.50, y = -3.00, label = "DM", size = (9*0.35), family = "serif") + 
  annotate(geom = "text", x =  5.30, y =  2.80, label = "Hmax", size = (9*0.35), family = "serif") + 
  annotate(geom = "text", x =  5.82, y =  1.20, label = "RDmax", size = (9*0.35), family = "serif") + 
  annotate(geom = "text", x =  5.10, y = -1.40, label = "WD", size = (9*0.35), family = "serif") + 
  annotate(geom = "text", x =  2.30, y = -1.00, label = "P50", size = (9*0.35), family = "serif") + 
  annotate(geom = "text", x =  4.00, y = -4.40, label = "Contribution", size = (10*0.35), family = "serif", fontface = "bold") + 
  scale_x_continuous(limits = c(-6, 6), breaks = c(-6, 0, 6), labels = c("−6", "0", "6")) + 
  scale_y_continuous(limits = c(-6, 6), breaks = c(-6, 0, 6), labels = c("−6", "0", "6")) + 
  Sylvan + 
  theme(legend.key.width = unit(0.5, "cm"), 
    legend.key.height = unit(0.2, "cm"), 
    legend.direction = "horizontal", 
    legend.position = c(0.8, 0.1),
    legend.background = element_blank()); plot_01

# ____3.2.2 Fig 2B ---------------------------------------------------------------------------------
plot_02 <- data_plot$var$contrib |> as_tibble(rownames = "names") |> 
  mutate(names = factor(names, levels = c("P50", "woody_density", "rdmax", "height", "dispersal_agent", "seed_width", "seed_length", "seed_mass", "LeafN", "SLA"), 
    labels = c("P50", "WD", "RDmax", "Hmax", "DM", "SW", "SL", "SM", "LeafN", "SLA"))) |> 
  ggplot(aes(x = names, y = Dim.1)) + 
  geom_col(fill = "#70a7ad", width = 0.5) + 
  geom_hline(aes(yintercept = 0)) + 
  coord_flip() + 
  labs(x = NULL, y = "Contribution [%]", title = "PC1") + 
  Sylvan + 
  theme(# axis.ticks.y = element_blank(), 
    panel.border = element_blank(), 
    plot.title = element_text(hjust = 0.5, face = "plain"), 
    axis.line.x = element_line(color = "#000000", size = 0.5)); plot_02

# ____3.2.3 Fig 2C ---------------------------------------------------------------------------------
plot_03 <- data_plot$var$contrib |> as_tibble(rownames = "names") |> 
  mutate(names = factor(names, levels = c("P50", "woody_density", "rdmax", "height", "dispersal_agent", "seed_width", "seed_length", "seed_mass", "LeafN", "SLA"), 
    labels = c("P50", "WD", "RDmax", "Hmax", "DM", "SW", "SL", "SM", "LeafN", "SLA"))) |> 
  ggplot(aes(x = names, y = Dim.2)) + 
  geom_col(fill = "#70a7ad", width = 0.5) + 
  geom_hline(aes(yintercept = 0)) + 
  coord_flip() + 
  labs(x = NULL, y = "Contribution [%]", title = "PC2") + 
  Sylvan + 
  theme(# axis.ticks.y = element_blank(), 
    panel.border = element_blank(), 
    plot.title = element_text(hjust = 0.5, face = "plain"), 
    axis.line.x = element_line(color = "#000000", size = 0.5)); plot_03

plot_02 <- cowplot::plot_grid(plot_02, plot_03, nrow = 2); plot_02
plot_01 <- cowplot::plot_grid(plot_01, plot_02, nrow = 1, rel_widths = c(2, 1)); plot_01

# ____3.2.4 Fig 2D ---------------------------------------------------------------------------------
model <- glmmTMB(n_stem ~ Trait_PC1 + (1 | site), data = data_anal)
summary(model); r2(model); data_anal |> summarise(min = min(Trait_PC1), max = max(Trait_PC1))
data <- effects::allEffects(model, xlevels = list(Trait_PC1 = seq(-4.63, 6.51, 0.01)))[[1]] |> as.data.frame()

plot_03 <- ggplot() + 
  geom_point(data = data_anal, aes(x = Trait_PC1, y = n_stem), color = "#bfbfbf", shape = 16, alpha = 0.7) + 
  geom_ribbon(data = data, aes(x = Trait_PC1, y = fit, ymin = lower, ymax = upper), fill = "#aa71a4", alpha = 0.2) + 
  geom_line(data = data, aes(x = Trait_PC1, y = fit), color = "#aa71a4") + 
  scale_x_continuous(limits = c(-5, 6.5), breaks = c(-5, 0, 5), labels = c("−5", "0", "5")) + 
  scale_y_continuous(limits = c(1, 2.6), breaks = c(1, 1.7, 2.4)) + 
  labs(x = "Trait PC1", y = "Stem abundance") + 
  annotate(geom = "text", x = ggpp::as_npc(0.70), y = ggpp::as_npc(0.90), label = expression(italic(P)*" < 0.001*** "), size = (9*0.35), family = "serif", hjust = "left") +
  Sylvan + 
  theme(legend.position = "none"); plot_03

# ____3.2.5 Fig 2E ---------------------------------------------------------------------------------
model <- glmmTMB(n_stem ~ Trait_PC2 + (1 | site), data = data_anal)
summary(model); r2(model); data_anal |> summarise(min = min(Trait_PC2), max = max(Trait_PC2))
data <- effects::allEffects(model, xlevels = list(Trait_PC2 = seq(-3.66, 5.49, 0.01)))[[1]] |> as.data.frame()

plot_04 <- ggplot() + 
  geom_point(data = data_anal, aes(x = Trait_PC2, y = n_stem), color = "#bfbfbf", shape = 16, alpha = 0.7) + 
  geom_ribbon(data = data, aes(x = Trait_PC2, y = fit, ymin = lower, ymax = upper), fill = "#aa71a4", alpha = 0.2) + 
  geom_line(data = data, aes(x = Trait_PC2, y = fit), color = "#aa71a4", linetype = 5) + 
  scale_x_continuous(limits = c(-4, 5.5), breaks = c(-4, 0, 4), labels = c("−4", "0", "4")) + 
  scale_y_continuous(limits = c(1, 2.6), breaks = c(1, 1.7, 2.4)) + 
  labs(x = "Trait PC2", y = "Stem abundance") + 
  annotate(geom = "text", x = ggpp::as_npc(0.70), y = ggpp::as_npc(0.90), label = expression(italic(P)*" = 0.289"), size = (9*0.35), family = "serif", hjust = "left") +
  Sylvan + 
  theme(legend.position = "none"); plot_04

plot_03 <- cowplot::plot_grid(plot_03, plot_04, nrow = 1); plot_03
cowplot::plot_grid(plot_01, plot_03, nrow = 2, rel_heights = c(8, 4)) + 
  annotate(geom = "text", x = ggpp::as_npc(0.02), y = ggpp::as_npc(0.980), label = "(a)", size = (10*0.35), family = "serif", fontface = "bold") + 
  annotate(geom = "text", x = ggpp::as_npc(0.68), y = ggpp::as_npc(0.980), label = "(b)", size = (10*0.35), family = "serif", fontface = "bold") + 
  annotate(geom = "text", x = ggpp::as_npc(0.68), y = ggpp::as_npc(0.650), label = "(c)", size = (10*0.35), family = "serif", fontface = "bold") + 
  annotate(geom = "text", x = ggpp::as_npc(0.02), y = ggpp::as_npc(0.318), label = "(d)", size = (10*0.35), family = "serif", fontface = "bold") + 
  annotate(geom = "text", x = ggpp::as_npc(0.52), y = ggpp::as_npc(0.318), label = "(e)", size = (10*0.35), family = "serif", fontface = "bold")
# ggsave(file = "save/fig_2_20260430.tiff", width = 16, height = 16, units = "cm", dpi = 300, limitsize = FALSE, bg = "#ffffff")
rm(data, data_anal, data_plot, model, plot_01, plot_02, plot_03, plot_04)

# __3.3 figure 3 -----------------------------------------------------------------------------------
load(file = "save/model_set_20260430.rdata")
model_avg <- model.avg(model_set, delta < 2); summary(model_avg)

data <- summary(model_avg)$coefmat.subset |> as.data.frame() |> set_names("Estimate", "SE", "Adj_SE", "Z_value", "P_value") |> 
  rownames_to_column("Var") |> modify_if( ~ is.numeric(.), ~ round(., 4)) |> filter(Var != "cond((Int))") |> mutate(Var = str_remove(Var, "cond\\(")) |> 
  mutate(Var = str_remove(Var, "\\)")) |> mutate(Var = factor(Var, levels = c("altitude", "forest_age", "H", "BA", "NMB", "Trait_PC1", "Trait_PC2"), 
    labels = c("Elevation", "Forest stand", "H", "BA", "NMB", "Tarit PC1", "Tarit PC2"))) |> mutate(Var2 = ifelse(Var == "Tarit PC2", "", as.character(Var))) |> 
  arrange(desc(Var)) |> mutate(ratio = abs(Estimate) / sum(abs(Estimate))) |> mutate(cum_ratio = cumsum(ratio)) |> 
  mutate(cum_ratio = lag(cum_ratio)) |> mutate(cum_ratio = ifelse(is.na(cum_ratio), 0, cum_ratio)) |> mutate(cum_ratio = cum_ratio + ratio / 2) |> 
  mutate(Symbol = case_when(P_value > 0.1 ~ "", P_value > 0.05 ~ "†", P_value > 0.01  ~ "*", P_value > 0.001 ~ "**", P_value <= 0.001 ~ "***")) |>  
  mutate(Var1 = str_c(Var, Symbol, sep = " ")) |> mutate(P_value = replace(P_value, P_value ==  0 , "< 0.001")); data

# ____3.3.1 Fig 3A ---------------------------------------------------------------------------------
plot_01 <- ggplot(data, aes(x = 1, y = ratio)) + 
  geom_col(aes(fill = Var)) + 
  geom_text(aes(x = 1, y = cum_ratio, label = Var2), size = (10*0.35), family = "serif") + 
  geom_hline(data = mutate(data, n = cumsum(ratio)) |> filter(n != 1), aes(yintercept = n), color = "#bfbfbf", linewidth = 0.3) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) + 
  scale_fill_manual(values = c("#66CCFF", "#4DA6D9", "#45C945", "#B3E67F", "#80C0A0", "#9575CD", "#9999CC")) + 
  labs(y = "Relative effect of estimates [%]", fill = NULL) + 
  theme_minimal(base_family = "serif") + 
  theme(plot.background = element_rect(fill = "#ffffff", color = "#ffffff"), 
        axis.title = element_text(size = 10), 
        axis.text = element_text(size = 9, colour = "#000000"), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        axis.line.y = element_line(), 
        axis.ticks.y = element_line(), 
        panel.grid = element_blank(), 
        legend.position = "none"); plot_01

# ____3.3.2 Fig 3B ---------------------------------------------------------------------------------
plot_02 <- ggplot(data, aes(x = Estimate, y = desc(Var))) + 
  geom_errorbarh(aes(xmax = Estimate + 1.96 * SE, xmin = Estimate - 1.96 * SE, color = Var), height = 0, linewidth = 6, alpha = 0.5) + 
  geom_point(aes(color = Var), shape = c(18, 16, 16, 16, 16, 16, 18), size = 5, alpha = 1) + 
  geom_text(aes(x = -0.56, y = desc(Var), label = Var1), size = (10*0.35), family = "serif", hjust = "left") + 
  geom_vline(aes(xintercept = 0)) + 
  scale_x_continuous(limits = c(-0.56, 0.56), breaks = c(-0.6, 0, 0.6), labels = c("−0.6", "0", "0.6")) + 
  scale_color_manual(values = c("#66CCFF", "#4DA6D9", "#45C945", "#B3E67F", "#80C0A0", "#9575CD", "#9999CC")) + 
  theme_bw(base_family = "serif") + 
  labs(x = "Parameter estimates", y = NULL) + 
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        axis.title = element_text(size = 10), 
        axis.text = element_text(size = 9, colour = "#000000"), 
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_line(colour = "#000000", linewidth = 0.3), 
        panel.border = element_rect(fill = NA, colour = "#000000", linewidth = 0.8), 
        legend.position = "none"); plot_02

cowplot::plot_grid(plot_01, plot_02, nrow = 1, align = "h", axis = "b", rel_widths = c(2.5, 7.5)) + 
  annotate(geom = "text", x = ggpp::as_npc(0.020), y = ggpp::as_npc(0.985), label = "(a)", size = (10*0.35), family = "serif", fontface = "bold") + 
  annotate(geom = "text", x = ggpp::as_npc(0.254), y = ggpp::as_npc(0.985), label = "(b)", size = (10*0.35), family = "serif", fontface = "bold") + 
  annotate(geom = "text", x = ggpp::as_npc(0.16), y = ggpp::as_npc(0.03), label = "Trait PC2 [1.31%]", size = (10*0.35), family = "serif") + 
  geom_segment(aes(x = ggpp::as_npc(0.16), y = ggpp::as_npc(0.074), xend = ggpp::as_npc(0.16), yend = ggpp::as_npc(0.042)), 
    arrow = arrow(angle = 15, length = unit(0.14, "cm"), type = "closed"), linewidth = 0.5) + 
  annotate(geom = "text", x = ggpp::as_npc(0.865), y = ggpp::as_npc(0.965), label = expression(italic(R)[c]^2*" = 0.53, "*italic(R)[m]^2*" = 0.28"), size = (10*0.35), family = "serif")
# ggsave(file = "save/fig_3_20260430.tiff", width = 14, height = 16, units = "cm", dpi = 300, limitsize = FALSE, bg = "#ffffff")
rm(model_avg, model_set, plot_01, plot_02, data)

# __3.4 figure 4 -----------------------------------------------------------------------------------
# ____3.4.1 Fig 4A ---------------------------------------------------------------------------------
# use PPT for drawing --

# ____3.4.2 Fig 4B ---------------------------------------------------------------------------------
load(file = "data/data_anal.rdata")

data_anal <- data_anal |> select(site:n_damage_5, n_stem:NMB, Trait_PC1:Trait_PC2, altitude:forest_age) |> mutate(across(n_stem:forest_age, ~ scale_z(.))) |> 
  mutate(alti1 = altitude, altiq = scale_z(altitude*altitude))

data_anal <- data.frame(site = data_anal$site, N = data_anal$n_tree, DR = data_anal$n_damage_1/data_anal$n_tree, alti1 = data_anal$alti1, altiq = data_anal$altiq, 
  stan = data_anal$forest_age, H = data_anal$H, BA = data_anal$BA, NMB = data_anal$NMB, PC1 = data_anal$Trait_PC1, PC2 = data_anal$Trait_PC2, stem = data_anal$n_stem)
N <- data_anal$N

model <- psem(glmmTMB(DR ~   altiq + stan + stem + H + BA + NMB + PC1 + PC2 + (1 | site), data_anal, family = binomial, weights = N), 
          glmmTMB(stem ~ stan + H + BA + NMB + PC1 + (1 | site), data_anal), 
          glmmTMB(H ~    stan + PC1 + PC2 + (1 | site), data_anal), 
          glmmTMB(BA ~   stan + PC1 + (1 | site), data_anal), 
          glmmTMB(NMB ~  altiq + stan + PC1 + (1 | site), data_anal), 
          H %~~% BA, H %~~% NMB, BA %~~% NMB); (model <- summary(model))

Response <- c("DR", "DR", "DR", "DR", "DR", "DR", "DR", "DR", "stem", "stem", "stem", "stem", "stem", "stem", "stem", "H", "H", "H", "H", "BA", "BA", "BA", "BA", "NMB", "NMB", "NMB", "NMB")
Predictor <- c("altiq", "stan", "stem", "H", "BA", "NMB", "PC1", "PC2", "altiq", "stan", "H", "BA", "NMB", "PC1", "PC2", "altiq", "stan", "PC1", "PC2", "altiq", "stan", "PC1", "PC2", "altiq", "stan", "PC1", "PC2")
data_effect <- data.frame(Response, Predictor) |> left_join(model$coefficients[, c(1:2, 7:8)]) |> mutate(Std.Estimate = ifelse(P.Value >= 0.05 | is.na(P.Value), 0, Std.Estimate)) |> 
  mutate(ID = 1:n()) |> mutate(Direct = ifelse(Response == "DR", Std.Estimate, NA)) |> 
  mutate(Indirect = ifelse(ID == 1, cur_data()[ 9, 4]*cur_data()[3, 4]+cur_data()[16, 4]*cur_data()[4, 4]+cur_data()[16, 4]*cur_data()[11, 4]*cur_data()[3, 4]+cur_data()[20, 4]*cur_data()[5, 4]+cur_data()[20, 4]*cur_data()[12, 4]*cur_data()[3, 4]+cur_data()[24, 4]*cur_data()[6, 4]+cur_data()[24, 4]*cur_data()[13, 4]*cur_data()[3, 4], NA)) |> 
  mutate(Indirect = ifelse(ID == 2, cur_data()[10, 4]*cur_data()[3, 4]+cur_data()[17, 4]*cur_data()[4, 4]+cur_data()[17, 4]*cur_data()[11, 4]*cur_data()[3, 4]+cur_data()[21, 4]*cur_data()[5, 4]+cur_data()[21, 4]*cur_data()[12, 4]*cur_data()[3, 4]+cur_data()[25, 4]*cur_data()[6, 4]+cur_data()[25, 4]*cur_data()[13, 4]*cur_data()[3, 4], Indirect)) |> 
  mutate(Indirect = ifelse(ID == 3, 0, Indirect)) |> 
  mutate(Indirect = ifelse(ID == 4, cur_data()[3, 4]*cur_data()[11, 4], Indirect)) |> 
  mutate(Indirect = ifelse(ID == 5, cur_data()[3, 4]*cur_data()[12, 4], Indirect)) |> 
  mutate(Indirect = ifelse(ID == 6, cur_data()[3, 4]*cur_data()[13, 4], Indirect)) |> 
  mutate(Indirect = ifelse(ID == 7, cur_data()[14, 4]*cur_data()[3, 4]+cur_data()[18, 4]*cur_data()[4, 4]+cur_data()[18, 4]*cur_data()[11, 4]*cur_data()[3, 4]+cur_data()[22, 4]*cur_data()[5, 4]+cur_data()[22, 4]*cur_data()[12, 4]*cur_data()[3, 4]+cur_data()[26, 4]*cur_data()[6, 4]+cur_data()[26, 4]*cur_data()[13, 4]*cur_data()[3, 4], Indirect)) |> 
  mutate(Indirect = ifelse(ID == 8, cur_data()[15, 4]*cur_data()[3, 4]+cur_data()[19, 4]*cur_data()[4, 4]+cur_data()[19, 4]*cur_data()[11, 4]*cur_data()[3, 4]+cur_data()[23, 4]*cur_data()[5, 4]+cur_data()[23, 4]*cur_data()[12, 4]*cur_data()[3, 4]+cur_data()[27, 4]*cur_data()[6, 4]+cur_data()[27, 4]*cur_data()[13, 4]*cur_data()[3, 4], Indirect)) |> 
  filter(!is.na(Direct)) |> mutate(Indirect = as.numeric(Indirect)) |> mutate(Total = Direct + Indirect)

data_effect |> pivot_longer(cols = Direct:Total, names_to = "trait", values_to = "value") |> 
  mutate(Predictor = factor(Predictor, levels = c("altiq", "stan", "PC1", "PC2", "stem", "H", "BA", "NMB"), labels = c("Elevation", "Forest stand", "Trait PC1", "Trait PC2", "Stem abundance", "H", "BA", "NMB"))) |> 
  ggplot(aes(x = Predictor, y = value, fill = trait)) + 
  geom_col(position = "dodge", width = 0.64) + 
  geom_hline(yintercept = 0, linewidth = 0.2) + 
  labs(x = NULL, y = "Standardized effects", fill = NULL) + 
  scale_fill_manual(values = c("#6cb509", "#609bef", "#a47bc2"), labels = c("Direct effects", "Indirect effects", "Total effects")) + 
  scale_y_continuous(breaks = c(-0.2, -0.1, 0, 0.1), labels = c("−0.2", "−0.1", "0", "0.1")) + 
  Sylvan + 
  theme(legend.position = c(0.84, 0.18), 
        legend.background = element_blank(), 
        legend.key.size = unit(0.25, "cm"), 
        legend.text = element_text(size = 9), 
        axis.title = element_text(size = 10), 
        axis.text = element_text(size = 9, color = "#000000"), 
        axis.ticks = element_line(color = "#000000"), 
        axis.text.x = element_text(angle = 10), 
        plot.margin = margin(t = 2, b = -4, l = 2, r = 2, "pt"))

# ggsave(file = "save/fig_4_20260430.tiff", width = 13.2, height = 4.8, units = "cm", dpi = 300, limitsize = FALSE, bg = "#ffffff")
rm(data_anal, model, N, Response, Predictor, data_effect)

# __3.5 figure S1 ----------------------------------------------------------------------------------
# use PPT for drawing --

# __3.6 figure S2 ----------------------------------------------------------------------------------
# ____3.6.1 Fig S2A --------------------------------------------------------------------------------
data_anal <- data_indi |> mutate(damage_1 = ifelse(n_damage_1 == n_stem, 1, 0), damage_2 = ifelse(n_damage_2 == n_stem, 1, 0), damage_3 = ifelse(n_damage_3 == n_stem, 1, 0), 
  damage_4 = ifelse(n_damage_4 == n_stem, 1, 0), damage_5 = ifelse(n_damage_5 == n_stem, 1, 0)) |> separate(quadrat, into = c("site", "plot"), remove = FALSE) |> 
  mutate(bin_stem = ifelse(n_stem > 1, "M", "S"), .after = n_stem) |> mutate(bin_stem = factor(bin_stem, levels = c("S", "M"), labels = c("single-stemmed", "multi-stemmed")))
link_inv <- make.link("cloglog")$linkinv

model <- glmmTMB(damage_2 ~ bin_stem + (1 | site) + (1 | species_LN), family = binomial(link = "cloglog"), data = data_anal)
# summary(model); r2(model); effects::allEffects(model) |> plot(multiline = TRUE, type = "response")

plot_01 <- ggeffects::ggpredict(model, "bin_stem") |> tibble() |> set_names("type", "estimate", "SE", "lower", "upper", "group") |> 
  ggplot() + 
  geom_line(aes(x = type, y = estimate, group = 1), color = "#005f80", linewidth = 1, linetype = 2) + 
  geom_segment(aes(x = type, y = lower, xend = type, yend = upper), color = "#f48a02", linewidth = 1) + 
  geom_point(aes(x = type, y = estimate), size = 3.6, fill = "#ffffff", shape = 21) + 
  geom_text(aes(x = 0.5, y = ggpp::as_npc(0.95)), label = "Uprooting", size = (9*0.35), family = "serif", hjust = "left") + 
  geom_text(aes(x = 2.1, y = ggpp::as_npc(0.95)), label = expression(italic(P)*" = 0.514      "), size = (9*0.35), family = "serif") + 
  scale_x_discrete(labels = c("single-stemmed  ", "  multi-stemmed")) + 
  scale_y_continuous(limits = c(0.004, 0.014), breaks = c(0.004, 0.009, 0.014)) + 
  labs(x = "Stem category", y = "Tree damage probability") + 
  Sylvan; plot_01

# ____3.6.2 Fig S2B --------------------------------------------------------------------------------
data_anal <- data_indi |> mutate(damage_1 = ifelse(n_damage_1 == n_stem, 1, 0), damage_2 = ifelse(n_damage_2 == n_stem, 1, 0), damage_3 = ifelse(n_damage_3 == n_stem, 1, 0), 
  damage_4 = ifelse(n_damage_4 == n_stem, 1, 0), damage_5 = ifelse(n_damage_5 == n_stem, 1, 0)) |> separate(quadrat, into = c("site", "plot"), remove = FALSE) |> 
  mutate(bin_stem = ifelse(n_stem > 1, "M", "S"), .after = n_stem) |> mutate(bin_stem = factor(bin_stem, levels = c("S", "M"), labels = c("single-stemmed", "multi-stemmed")))
link_inv <- make.link("cloglog")$linkinv

model <- glmmTMB(damage_3 ~ bin_stem + (1 | site) + (1 | species_LN), family = binomial(link = "cloglog"), data = data_anal)
# summary(model); r2(model); effects::allEffects(model) |> plot(multiline = TRUE, type = "response")

plot_02 <- ggeffects::ggpredict(model, "bin_stem") |> tibble() |> set_names("type", "estimate", "SE", "lower", "upper", "group") |> 
  ggplot() + 
  geom_line(aes(x = type, y = estimate, group = 1), color = "#005f80", linewidth = 1) + 
  geom_segment(aes(x = type, y = lower, xend = type, yend = upper), color = "#f48a02", linewidth = 1) + 
  geom_point(aes(x = type, y = estimate), size = 3.6, fill = "#ffffff", shape = 21) + 
  geom_text(aes(x = 0.5, y = ggpp::as_npc(0.95)), label = "Clear-bole broken", size = (9*0.35), family = "serif", hjust = "left") + 
  geom_text(aes(x = 2.1, y = ggpp::as_npc(0.95)), label = expression(italic(P)*" < 0.001***"), size = (9*0.35), family = "serif") + 
  scale_x_discrete(labels = c("single-stemmed  ", "  multi-stemmed")) + 
  scale_y_continuous(limits = c(0.003, 0.027), breaks = c(0.003, 0.015, 0.027)) + 
  labs(x = "Stem category", y = "Tree damage probability") + 
  Sylvan; plot_02

# ____3.6.3 Fig S2C --------------------------------------------------------------------------------
data_anal <- data_indi |> mutate(damage_1 = ifelse(n_damage_1 == n_stem, 1, 0), damage_2 = ifelse(n_damage_2 == n_stem, 1, 0), damage_3 = ifelse(n_damage_3 == n_stem, 1, 0), 
  damage_4 = ifelse(n_damage_4 == n_stem, 1, 0), damage_5 = ifelse(n_damage_5 == n_stem, 1, 0)) |> separate(quadrat, into = c("site", "plot"), remove = FALSE) |> 
  mutate(bin_stem = ifelse(n_stem > 1, "M", "S"), .after = n_stem) |> mutate(bin_stem = factor(bin_stem, levels = c("S", "M"), labels = c("single-stemmed", "multi-stemmed")))
link_inv <- make.link("cloglog")$linkinv

model <- glmmTMB(damage_4 ~ bin_stem + (1 | site) + (1 | species_LN), family = binomial(link = "cloglog"), data = data_anal)
# summary(model); r2(model); effects::allEffects(model) |> plot(multiline = TRUE, type = "response")

plot_03 <- ggeffects::ggpredict(model, "bin_stem") |> tibble() |> set_names("type", "estimate", "SE", "lower", "upper", "group") |> 
  ggplot() + 
  geom_line(aes(x = type, y = estimate, group = 1), color = "#005f80", linewidth = 1) + 
  geom_segment(aes(x = type, y = lower, xend = type, yend = upper), color = "#f48a02", linewidth = 1) + 
  geom_point(aes(x = type, y = estimate), size = 3.6, fill = "#ffffff", shape = 21) + 
  geom_text(aes(x = 0.5, y = ggpp::as_npc(0.95)), label = "Crown broken", size = (9*0.35), family = "serif", hjust = "left") + 
  geom_text(aes(x = 2.1, y = ggpp::as_npc(0.95)), label = expression(italic(P)*" < 0.001***"), size = (9*0.35), family = "serif") + 
  scale_x_discrete(labels = c("single-stemmed  ", "  multi-stemmed")) + 
  scale_y_continuous(limits = c(0.010, 0.076), breaks = c(0.010, 0.043, 0.076)) + 
  labs(x = "Stem category", y = "Tree damage probability") + 
  Sylvan; plot_03

# ____3.6.4 Fig S2D --------------------------------------------------------------------------------
data_anal <- data_indi |> mutate(damage_1 = ifelse(n_damage_1 == n_stem, 1, 0), damage_2 = ifelse(n_damage_2 == n_stem, 1, 0), damage_3 = ifelse(n_damage_3 == n_stem, 1, 0), 
  damage_4 = ifelse(n_damage_4 == n_stem, 1, 0), damage_5 = ifelse(n_damage_5 == n_stem, 1, 0)) |> separate(quadrat, into = c("site", "plot"), remove = FALSE) |> 
  mutate(bin_stem = ifelse(n_stem > 1, "M", "S"), .after = n_stem) |> mutate(bin_stem = factor(bin_stem, levels = c("S", "M"), labels = c("single-stemmed", "multi-stemmed")))
link_inv <- make.link("cloglog")$linkinv

model <- glmmTMB(damage_5 ~ bin_stem + (1 | site) + (1 | species_LN), family = binomial(link = "cloglog"), data = data_anal)
# summary(model); r2(model); effects::allEffects(model) |> plot(multiline = TRUE, type = "response")

plot_04 <- ggeffects::ggpredict(model, "bin_stem") |> tibble() |> set_names("type", "estimate", "SE", "lower", "upper", "group") |> 
  ggplot() + 
  geom_line(aes(x = type, y = estimate, group = 1), color = "#005f80", linewidth = 1) + 
  geom_segment(aes(x = type, y = lower, xend = type, yend = upper), color = "#f48a02", linewidth = 1) + 
  geom_point(aes(x = type, y = estimate), size = 3.6, fill = "#ffffff", shape = 21) + 
  geom_text(aes(x = 0.5, y = ggpp::as_npc(0.95)), label = "Branch broken", size = (9*0.35), family = "serif", hjust = "left") + 
  geom_text(aes(x = 2.1, y = ggpp::as_npc(0.95)), label = expression(italic(P)*" < 0.001***"), size = (9*0.35), family = "serif") + 
  scale_x_discrete(labels = c("single-stemmed  ", "  multi-stemmed")) + 
  scale_y_continuous(limits = c(0.002, 0.046), breaks = c(0.002, 0.024, 0.046)) + 
  labs(x = "Stem category", y = "Tree damage probability") + 
  Sylvan; plot_04

cowplot::plot_grid(plot_01, plot_02, plot_03, plot_04, nrow = 2) + 
  annotate(geom = "text", x = ggpp::as_npc(0.018), y = ggpp::as_npc(0.98), label = "(a)", size = (10*0.35), family = "serif", fontface = "bold") + 
  annotate(geom = "text", x = ggpp::as_npc(0.518), y = ggpp::as_npc(0.98), label = "(b)", size = (10*0.35), family = "serif", fontface = "bold") + 
  annotate(geom = "text", x = ggpp::as_npc(0.018), y = ggpp::as_npc(0.48), label = "(c)", size = (10*0.35), family = "serif", fontface = "bold") + 
  annotate(geom = "text", x = ggpp::as_npc(0.518), y = ggpp::as_npc(0.48), label = "(d)", size = (10*0.35), family = "serif", fontface = "bold")
# ggsave(file = "save/fig_S2_20260430.tiff", width = 12, height = 12, units = "cm", dpi = 300, limitsize = FALSE, bg = "#ffffff")
rm(data_anal, link_inv, model, plot_01, plot_02, plot_03, plot_04)

# __3.7 figure S3 ----------------------------------------------------------------------------------
# ____3.7.1 Fig S3A --------------------------------------------------------------------------------
data_anal <- data_spec |> filter(n_tree > 17)
model <- glmmTMB(cbind(n_damage_2, n_stem - n_damage_2) ~ avg_stem, family = betabinomial, data = data_anal)
summary(model); data_anal |> summarise(min = min(avg_stem), max = max(avg_stem))
data <- effects::allEffects(model, xlevels = list(avg_stem = seq(1.00, 2.34, 0.01)))[[1]] |> as.data.frame()

plot_01 <- ggplot() + 
  geom_point(data = data_anal, aes(x = avg_stem, y = DR_damage_2), color = "#bfbfbf", shape = 16, size = 1.6, alpha = 0.7) + 
  geom_ribbon(data = data, aes(x = avg_stem, y = fit, ymin = lower, ymax = upper), fill = "#aa71a4", alpha = 0.2) + 
  geom_line(data = data, aes(x = avg_stem, y = fit), color = "#aa71a4") + 
  scale_x_continuous(limits = c(1, 2.4), breaks = c(1, 1.7, 2.4)) + 
  scale_y_continuous(limits = c(0, 0.16), breaks = c(0.00, 0.08, 0.16)) + 
  labs(x = "Stem abundance", y = "Total damage ratio [%]") + 
  geom_text(aes(x = ggpp::as_npc(0.03), y = ggpp::as_npc(0.95)), label = "Uprooting", size = (9*0.35), family = "serif", hjust = "left") + 
  geom_text(aes(x = ggpp::as_npc(0.75), y = ggpp::as_npc(0.95)), label = expression(italic(P)*" < 0.001***"), size = (9*0.35), family = "serif") + 
  Sylvan + 
  theme(legend.position = "none"); plot_01

# ____3.7.2 Fig S3B --------------------------------------------------------------------------------
data_anal <- data_spec |> filter(n_tree > 17)
model <- glmmTMB(cbind(n_damage_3, n_stem - n_damage_3) ~ avg_stem, family = betabinomial, data = data_anal)
summary(model); data_anal |> summarise(min = min(avg_stem), max = max(avg_stem))
data <- effects::allEffects(model, xlevels = list(avg_stem = seq(1.00, 2.34, 0.01)))[[1]] |> as.data.frame()

plot_02 <- ggplot() + 
  geom_point(data = data_anal, aes(x = avg_stem, y = DR_damage_3), color = "#bfbfbf", shape = 16, size = 1.6, alpha = 0.7) + 
  geom_ribbon(data = data, aes(x = avg_stem, y = fit, ymin = lower, ymax = upper), fill = "#aa71a4", alpha = 0.2) + 
  geom_line(data = data, aes(x = avg_stem, y = fit), color = "#aa71a4") + 
  scale_x_continuous(limits = c(1, 2.4), breaks = c(1, 1.7, 2.4)) + 
  scale_y_continuous(limits = c(0, 0.24), breaks = c(0.00, 0.12, 0.24)) + 
  labs(x = "Stem abundance", y = "Total damage ratio [%]") + 
  geom_text(aes(x = ggpp::as_npc(0.03), y = ggpp::as_npc(0.95)), label = "Clear-bole broken", size = (9*0.35), family = "serif", hjust = "left") + 
  geom_text(aes(x = ggpp::as_npc(0.75), y = ggpp::as_npc(0.95)), label = expression(italic(P)*" < 0.001***"), size = (9*0.35), family = "serif") + 
  Sylvan + 
  theme(legend.position = "none"); plot_02

# ____3.7.3 Fig S3C --------------------------------------------------------------------------------
data_anal <- data_spec |> filter(n_tree > 17)
model <- glmmTMB(cbind(n_damage_4, n_stem - n_damage_4) ~ avg_stem, family = betabinomial, data = data_anal)
summary(model); data_anal |> summarise(min = min(avg_stem), max = max(avg_stem))
data <- effects::allEffects(model, xlevels = list(avg_stem = seq(1.00, 2.34, 0.01)))[[1]] |> as.data.frame()

plot_03 <- ggplot() + 
  geom_point(data = data_anal, aes(x = avg_stem, y = DR_damage_4), color = "#bfbfbf", shape = 16, size = 1.6, alpha = 0.7) + 
  geom_ribbon(data = data, aes(x = avg_stem, y = fit, ymin = lower, ymax = upper), fill = "#aa71a4", alpha = 0.2) + 
  geom_line(data = data, aes(x = avg_stem, y = fit), color = "#aa71a4") + 
  scale_x_continuous(limits = c(1, 2.4), breaks = c(1, 1.7, 2.4)) + 
  scale_y_continuous(limits = c(0, 0.36), breaks = c(0.00, 0.18, 0.36)) + 
  labs(x = "Stem abundance", y = "Total damage ratio [%]") + 
  geom_text(aes(x = ggpp::as_npc(0.03), y = ggpp::as_npc(0.95)), label = "Crown broken", size = (9*0.35), family = "serif", hjust = "left") + 
  geom_text(aes(x = ggpp::as_npc(0.75), y = ggpp::as_npc(0.95)), label = expression(italic(P)*" = 0.011*    "), size = (9*0.35), family = "serif") + 
  Sylvan + 
  theme(legend.position = "none"); plot_03

# ____3.7.4 Fig S3D --------------------------------------------------------------------------------
data_anal <- data_spec |> filter(n_tree > 17)
model <- glmmTMB(cbind(n_damage_5, n_stem - n_damage_5) ~ avg_stem, family = betabinomial, data = data_anal)
summary(model); data_anal |> summarise(min = min(avg_stem), max = max(avg_stem))
data <- effects::allEffects(model, xlevels = list(avg_stem = seq(1.00, 2.34, 0.01)))[[1]] |> as.data.frame()

plot_04 <- ggplot() + 
  geom_point(data = data_anal, aes(x = avg_stem, y = DR_damage_5), color = "#bfbfbf", shape = 16, size = 1.6, alpha = 0.7) + 
  geom_ribbon(data = data, aes(x = avg_stem, y = fit, ymin = lower, ymax = upper), fill = "#aa71a4", alpha = 0.2) + 
  geom_line(data = data, aes(x = avg_stem, y = fit), color = "#aa71a4") + 
  scale_x_continuous(limits = c(1, 2.4), breaks = c(1, 1.7, 2.4)) + 
  scale_y_continuous(limits = c(0, 0.18), breaks = c(0.00, 0.09, 0.18)) + 
  labs(x = "Stem abundance", y = "Total damage ratio [%]") + 
  geom_text(aes(x = ggpp::as_npc(0.03), y = ggpp::as_npc(0.95)), label = "Branch broken", size = (9*0.35), family = "serif", hjust = "left") + 
  geom_text(aes(x = ggpp::as_npc(0.75), y = ggpp::as_npc(0.95)), label = expression(italic(P)*" = 0.002**  "), size = (9*0.35), family = "serif") + 
  Sylvan + 
  theme(legend.position = "none"); plot_04

cowplot::plot_grid(plot_01, plot_02, plot_03, plot_04, nrow = 2) + 
  annotate(geom = "text", x = ggpp::as_npc(0.018), y = ggpp::as_npc(0.98), label = "(a)", size = (10*0.35), family = "serif", fontface = "bold") + 
  annotate(geom = "text", x = ggpp::as_npc(0.518), y = ggpp::as_npc(0.98), label = "(b)", size = (10*0.35), family = "serif", fontface = "bold") + 
  annotate(geom = "text", x = ggpp::as_npc(0.018), y = ggpp::as_npc(0.48), label = "(c)", size = (10*0.35), family = "serif", fontface = "bold") + 
  annotate(geom = "text", x = ggpp::as_npc(0.518), y = ggpp::as_npc(0.48), label = "(d)", size = (10*0.35), family = "serif", fontface = "bold")
# ggsave(file = "save/fig_S3_20260430.tiff", width = 12, height = 12, units = "cm", dpi = 300, limitsize = FALSE, bg = "#ffffff")
rm(data, data_anal, model, plot_01, plot_02, plot_03, plot_04)

# __3.8 figure S4 ----------------------------------------------------------------------------------
# ____3.8.1 Fig S4A --------------------------------------------------------------------------------
data_anal <- data_comm |> separate(quadrat, into = c("site", "plot"), remove = FALSE)
model <- glmmTMB(cbind(n_damage_2, n_stem - n_damage_2) ~ avg_stem + (1 | site), family = binomial, data = data_anal)
summary(model); data_anal |> summarise(min = min(avg_stem), max = max(avg_stem))
data <- effects::allEffects(model, xlevels = list(avg_stem = seq(1.00, 2.73, 0.01)))[[1]] |> as.data.frame()

plot_01 <- ggplot() + 
  geom_point(data = data_anal, aes(x = avg_stem, y = DR_damage_2), color = "#bfbfbf", shape = 16, size = 1.6, alpha = 0.7) + 
  geom_ribbon(data = data, aes(x = avg_stem, y = fit, ymin = lower, ymax = upper), fill = "#aa71a4", alpha = 0.2) + 
  geom_line(data = data, aes(x = avg_stem, y = fit), color = "#aa71a4", linetype = 2) + 
  # geom_smooth(data = data_anal, aes(x = avg_stem, y = DR_damage_1), method = "gam", color = "#aa71a4") + 
  scale_x_continuous(limits = c(1, 2.8), breaks = c(1, 1.9, 2.8)) + 
  scale_y_continuous(limits = c(0, 0.4), breaks = c(0.00, 0.20, 0.40)) + 
  labs(x = "Stem abundance", y = "Total damage ratio [%]") + 
  geom_text(aes(x = ggpp::as_npc(0.03), y = ggpp::as_npc(0.95)), label = "Uprooting", size = (9*0.35), family = "serif", hjust = "left") + 
  geom_text(aes(x = ggpp::as_npc(0.75), y = ggpp::as_npc(0.95)), label = expression(italic(P)*" = 0.058      "), size = (9*0.35), family = "serif") + 
  Sylvan + 
  theme(legend.position = "none"); plot_01

# ____3.8.2 Fig S4B --------------------------------------------------------------------------------
data_anal <- data_comm |> separate(quadrat, into = c("site", "plot"), remove = FALSE)
model <- glmmTMB(cbind(n_damage_3, n_stem - n_damage_3) ~ avg_stem + (1 | site), family = binomial, data = data_anal)
summary(model); data_anal |> summarise(min = min(avg_stem), max = max(avg_stem))
data <- effects::allEffects(model, xlevels = list(avg_stem = seq(1.00, 2.73, 0.01)))[[1]] |> as.data.frame()

plot_02 <- ggplot() + 
  geom_point(data = data_anal, aes(x = avg_stem, y = DR_damage_3), color = "#bfbfbf", shape = 16, size = 1.6, alpha = 0.7) + 
  geom_ribbon(data = data, aes(x = avg_stem, y = fit, ymin = lower, ymax = upper), fill = "#aa71a4", alpha = 0.2) + 
  geom_line(data = data, aes(x = avg_stem, y = fit), color = "#aa71a4", linetype = 2) + 
  # geom_smooth(data = data_anal, aes(x = avg_stem, y = DR_damage_1), method = "gam", color = "#aa71a4") + 
  scale_x_continuous(limits = c(1, 2.8), breaks = c(1, 1.9, 2.8)) + 
  scale_y_continuous(limits = c(0, 0.3), breaks = c(0.00, 0.15, 0.30)) + 
  labs(x = "Stem abundance", y = "Total damage ratio [%]") + 
  geom_text(aes(x = ggpp::as_npc(0.03), y = ggpp::as_npc(0.95)), label = "Clear-bole broken", size = (9*0.35), family = "serif", hjust = "left") + 
  geom_text(aes(x = ggpp::as_npc(0.75), y = ggpp::as_npc(0.95)), label = expression(italic(P)*" = 0.612      "), size = (9*0.35), family = "serif") + 
  Sylvan + 
  theme(legend.position = "none"); plot_02

# ____3.8.3 Fig S4C --------------------------------------------------------------------------------
data_anal <- data_comm |> separate(quadrat, into = c("site", "plot"), remove = FALSE)
model <- glmmTMB(cbind(n_damage_4, n_stem - n_damage_4) ~ avg_stem + (1 | site), family = binomial, data = data_anal)
summary(model); data_anal |> summarise(min = min(avg_stem), max = max(avg_stem))
data <- effects::allEffects(model, xlevels = list(avg_stem = seq(1.00, 2.73, 0.01)))[[1]] |> as.data.frame()

plot_03 <- ggplot() + 
  geom_point(data = data_anal, aes(x = avg_stem, y = DR_damage_4), color = "#bfbfbf", shape = 16, size = 1.6, alpha = 0.7) + 
  geom_ribbon(data = data, aes(x = avg_stem, y = fit, ymin = lower, ymax = upper), fill = "#aa71a4", alpha = 0.2) + 
  geom_line(data = data, aes(x = avg_stem, y = fit), color = "#aa71a4") + 
  # geom_smooth(data = data_anal, aes(x = avg_stem, y = DR_damage_1), method = "gam", color = "#aa71a4") + 
  scale_x_continuous(limits = c(1, 2.8), breaks = c(1, 1.9, 2.8)) + 
  scale_y_continuous(limits = c(0, 0.6), breaks = c(0.00, 0.30, 0.60)) + 
  labs(x = "Stem abundance", y = "Total damage ratio [%]") + 
  geom_text(aes(x = ggpp::as_npc(0.03), y = ggpp::as_npc(0.95)), label = "Crown broken", size = (9*0.35), family = "serif", hjust = "left") + 
  geom_text(aes(x = ggpp::as_npc(0.75), y = ggpp::as_npc(0.95)), label = expression(italic(P)*" < 0.001***"), size = (9*0.35), family = "serif") + 
  Sylvan + 
  theme(legend.position = "none"); plot_03

# ____3.8.4 Fig S4D --------------------------------------------------------------------------------
data_anal <- data_comm |> separate(quadrat, into = c("site", "plot"), remove = FALSE)
model <- glmmTMB(cbind(n_damage_5, n_stem - n_damage_5) ~ avg_stem + (1 | site), family = binomial, data = data_anal)
summary(model); data_anal |> summarise(min = min(avg_stem), max = max(avg_stem))
data <- effects::allEffects(model, xlevels = list(avg_stem = seq(1.00, 2.73, 0.01)))[[1]] |> as.data.frame()

plot_04 <- ggplot() + 
  geom_point(data = data_anal, aes(x = avg_stem, y = DR_damage_5), color = "#bfbfbf", shape = 16, size = 1.6, alpha = 0.7) + 
  geom_ribbon(data = data, aes(x = avg_stem, y = fit, ymin = lower, ymax = upper), fill = "#aa71a4", alpha = 0.2) + 
  geom_line(data = data, aes(x = avg_stem, y = fit), color = "#aa71a4") + 
  # geom_smooth(data = data_anal, aes(x = avg_stem, y = DR_damage_1), method = "gam", color = "#aa71a4") + 
  scale_x_continuous(limits = c(1, 2.8), breaks = c(1, 1.9, 2.8)) + 
  scale_y_continuous(limits = c(0, 0.3), breaks = c(0.00, 0.15, 0.30)) + 
  labs(x = "Stem abundance", y = "Total damage ratio [%]") + 
  geom_text(aes(x = ggpp::as_npc(0.03), y = ggpp::as_npc(0.95)), label = "Branch broken", size = (9*0.35), family = "serif", hjust = "left") + 
  geom_text(aes(x = ggpp::as_npc(0.75), y = ggpp::as_npc(0.95)), label = expression(italic(P)*" = 0.001**  "), size = (9*0.35), family = "serif") + 
  Sylvan + 
  theme(legend.position = "none"); plot_04

cowplot::plot_grid(plot_01, plot_02, plot_03, plot_04, nrow = 2) + 
  annotate(geom = "text", x = ggpp::as_npc(0.018), y = ggpp::as_npc(0.98), label = "(a)", size = (10*0.35), family = "serif", fontface = "bold") + 
  annotate(geom = "text", x = ggpp::as_npc(0.518), y = ggpp::as_npc(0.98), label = "(b)", size = (10*0.35), family = "serif", fontface = "bold") + 
  annotate(geom = "text", x = ggpp::as_npc(0.018), y = ggpp::as_npc(0.48), label = "(c)", size = (10*0.35), family = "serif", fontface = "bold") + 
  annotate(geom = "text", x = ggpp::as_npc(0.518), y = ggpp::as_npc(0.48), label = "(d)", size = (10*0.35), family = "serif", fontface = "bold")
# ggsave(file = "save/fig_S4_20260430.tiff", width = 12, height = 12, units = "cm", dpi = 300, limitsize = FALSE, bg = "#ffffff")
rm(data, data_anal, model, plot_01, plot_02, plot_03, plot_04)

# untitled -----------------------------------------------------------------------------------------
rm(list = ls())
