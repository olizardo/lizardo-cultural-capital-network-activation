# 03-figures.R
# Port of the figure-producing blocks in analysis-SSR-R1.do, -R2.do, -R3.do
# and figure2.do. Stata margins -> marginaleffects::predictions().

library(dplyr)
library(tidyr)
library(ggplot2)
library(nnet)
library(MASS)
library(marginaleffects)
library(here)

d <- readRDS(here("data", "analysis-gss2002.rds"))
d$omnicat <- cut(d$omnivore, breaks = c(-Inf, 0, 3, 5, Inf),
                 labels = c(NA, "1-3 Activities", "4-5 Activities",
                            "6+ Activities"))

ctrl <- "age + agesq + women + black + hisp + married + deg2 + deg3 + deg4 + deg5"
ctrl_f <- as.formula(paste("~ omnivore +", ctrl))

dir.create(here("output", "figures"), showWarnings = FALSE)
mean_prob <- function(y) mean(y == 2, na.rm = TRUE)

# ---- Figure 1 (R2/R3): predicted probability of weak-tie use ----------------
# Stata: mlogit strong_and_acqboss_vs_notie ... ; margins, at(omnivore=(0(1)7))
# predict(pr out(2)) — outcome 2 is the "strong or acquaintance/boss" category.
fit_f1 <- multinom(update(ctrl_f, strong_and_acqboss_vs_notie ~ .),
                   data = d, trace = FALSE)
p1 <- predictions(fit_f1, newdata = datagrid(omnivore = 0:7),
                  type = "probs", by = "omnivore") |>
  filter(group == 2) |>
  ggplot(aes(omnivore, estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  geom_hline(yintercept = mean_prob(d$strong_and_acqboss_vs_notie),
             linetype = "dashed") +
  annotate("text", x = 6.5, y = mean_prob(d$strong_and_acqboss_vs_notie) - 0.01,
           label = "Sample Mean", size = 3) +
  scale_x_continuous(breaks = 0:7) +
  labs(x = "Cultural Variety", y = "Predicted Probability of Using a Weak Tie") +
  theme_minimal()
ggsave(here("output", "figures", "figure1-weak-tie.pdf"), p1,
       width = 4, height = 6)

# R3 variant: by employment status and by years on the job
d <- d |>
  mutate(
    working   = as.numeric(wrkstat < 4),
    yearsjob1 = cut(yearsjob, breaks = c(0, 1, Inf),
                    labels = c("Less than an Year on Job",
                               "More than a Year on Job"))
  )

fit_f1b <- multinom(update(ctrl_f,
                strong_and_acqboss_vs_notie ~ . + omnivore:working),
                data = d, trace = FALSE)
p1b <- predictions(fit_f1b, newdata = datagrid(omnivore = 0:7,
                                               working = c(0, 1)),
                   type = "probs", by = c("omnivore", "working")) |>
  filter(group == 2) |>
  mutate(working = factor(working, labels = c("Not Employed",
                                              "Currently Employed"))) |>
  ggplot(aes(omnivore, estimate, color = working, ymin = conf.low,
             ymax = conf.high)) +
  geom_pointrange(position = position_dodge(width = 0.3)) +
  scale_x_continuous(breaks = 0:7) +
  labs(x = "Cultural Variety", y = "Predicted Probability of Using a Weak Tie",
       color = NULL) +
  theme_minimal()
ggsave(here("output", "figures", "figure1b-by-working.pdf"), p1b,
       width = 4, height = 6)

fit_f1c <- multinom(update(ctrl_f,
                strong_and_acqboss_vs_notie ~ . + omnivore:yearsjob1),
                data = filter(d, working == 1), trace = FALSE)
p1c <- predictions(fit_f1c,
                   newdata = datagrid(omnivore = 0:7,
                                      yearsjob1 = levels(factor(d$yearsjob1))),
                   type = "probs", by = c("omnivore", "yearsjob1")) |>
  filter(group == 2) |>
  ggplot(aes(omnivore, estimate, color = yearsjob1, ymin = conf.low,
             ymax = conf.high)) +
  geom_pointrange(position = position_dodge(width = 0.3)) +
  scale_x_continuous(breaks = 0:7) +
  labs(x = "Cultural Variety", y = "Predicted Probability of Using a Weak Tie",
       color = NULL) +
  theme_minimal()
ggsave(here("output", "figures", "figure1c-by-tenure.pdf"), p1c,
       width = 4, height = 6)

# ---- Figure 2 (R1/R2/figure2.do): activity-level effects and college share --
# Stata: for each activity, logit activity acq_vs_other; margins, dydx(...)
# and the same for boss_vs_other; plus mean college degree among participants.
activities <- c("seemovie", "popmusic", "readfict", "visitart", "gomusic",
                "drama", "dance")
labels <- c(Movies = "Movies", `Pop. Music` = "Pop. Music",
            `Read Fiction` = "Read Fiction",
            `Go to the Museum` = "Go to the Museum",
            `Classical Music Perf.` = "Classical Music Perf.",
            `Theater Perf.` = "Theater Perf.",
            `Dance Perf.` = "Dance Perf.")

fx <- function(dv, treat) {
  fit <- glm(as.formula(paste(dv, "~", treat)), family = binomial, data = d)
  avg_slopes(fit, variables = treat)$estimate
}

fig2 <- data.frame(
  lab     = labels,
  acq_b   = sapply(activities, fx, treat = "acq_vs_other"),
  boss_b  = sapply(activities, fx, treat = "boss_vs_other"),
  percollege = sapply(activities, function(x) mean(d$college[d[[x]] == 1]))
) |>
  mutate(lab = factor(lab, levels = rev(labels))) |>
  (\(dd) {
    p_eff <- dd |>
      dplyr::select(lab, acq_b, boss_b) |>
      pivot_longer(acq_b:boss_b, names_to = "effect", values_to = "est") |>
      ggplot(aes(est, lab, fill = effect)) +
      geom_col(position = "dodge") +
      labs(x = "Average marginal effect", y = NULL, fill = NULL) +
      theme_minimal()
    p_share <- ggplot(dd, aes(percollege, lab)) +
      geom_point() +
      labs(x = "% with college degree among participants", y = NULL) +
      theme_minimal()
    list(effects = p_eff, share = p_share)
  })()
ggsave(here("output", "figures", "figure2-activity-effects.pdf"), fig2$effects,
       width = 6, height = 4)
ggsave(here("output", "figures", "figure2-college-share.pdf"), fig2$share,
       width = 6, height = 4)

# Map display labels to activity variables
act_of <- setNames(activities, labels)

# R3 figure2.do variant: one multinomial model per activity, with the activity
# itself as the predictor of interest (mlogit outcome x age agesq ... robust;
# margins, dydx(x) predict(out(2)) / predict(out(3)))
fig2r3 <- bind_rows(lapply(seq_along(activities), function(i) {
  fit_x <- multinom(as.formula(paste("strong_vs_acq_vs_boss_vs_other ~",
                                     activities[i], "+", ctrl)),
                    data = d, trace = FALSE)
  # Stata margins dydx() averages over the estimation sample
  s <- avg_slopes(fit_x, variables = activities[i])
  data.frame(lab = labels[i], group = s$group, est = s$estimate)
})) |>
  # outcome 1 is the reference category ("other"); drop it and any NA groups
  filter(as.character(group) %in% c("2", "3")) |>
  mutate(group = factor(group, levels = c("2", "3"),
                        labels = c("Acquaintance / Boss", "Boss"))) |>
  # order activities by proportion college among participants, as in the paper
  mutate(percollege = sapply(as.character(lab), function(l) {
    x <- act_of[[l]]
    mean(d$college[d[[x]] == 1])
  })) |>
  mutate(lab = factor(as.character(lab),
                      levels = unique(names(sort(setNames(percollege,
                                                          as.character(lab))))))) |>
  ggplot(aes(est, lab)) +
  geom_col() +
  facet_wrap(~group) +
  labs(x = "Average marginal effect", y = NULL) +
  theme_minimal()
ggsave(here("output", "figures", "figure2r3-by-activity.pdf"), fig2r3,
       width = 6, height = 4)

# ---- Figure 3: weak-tie use by network size ---------------------------------
# Stata: mlogit ... omnivore c.numcntct##c.numcntct ...; margins at(numcntct)
fit_f3 <- multinom(update(ctrl_f,
              strong_and_acqboss_vs_notie ~ . + numcntct + I(numcntct^2)),
              data = d, trace = FALSE)
p3 <- predictions(fit_f3, newdata = datagrid(numcntct = seq(0, 100, 10)),
                  type = "probs", by = "numcntct") |>
  filter(group == 2) |>
  ggplot(aes(numcntct, estimate)) +
  geom_line() +
  labs(x = "N. of Contacts",
       y = "Predicted Probability of Using a Weak Tie") +
  theme_minimal()
ggsave(here("output", "figures", "figure3-network-size.pdf"), p3,
       width = 6, height = 6)

# R1 variant: over omnicat
fit_f3b <- multinom(update(ctrl_f,
               acqboss_vs_strong ~ . + numcntct + I(numcntct^2) + numcntct:omnicat),
               data = d, trace = FALSE)
p3b <- predictions(fit_f3b,
                   newdata = datagrid(numcntct = seq(0, 100, 10),
                                      grid_type = "counterfactual"),
                   type = "probs", by = c("numcntct", "omnicat")) |>
  filter(!is.na(omnicat)) |>
  ggplot(aes(numcntct, estimate, color = omnicat)) +
  geom_line() +
  labs(x = "N. of Contacts",
       y = "Predicted Probability of Using a Weak Tie",
       color = NULL) +
  theme_minimal()
ggsave(here("output", "figures", "figure3b-by-omnicat.pdf"), p3b,
       width = 6, height = 6)

cat("Figures written to output/figures/\n")
