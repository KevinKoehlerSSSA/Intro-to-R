model3 <- glm(success ~
                counterbalancing +
                top +
                middle +
                military +
                dem7 +
                lngdppc +
                chgdp +
                recent3 +
                regional3 +
                recent_rev +
                coldwar,
              data = data,
              family = "binomial")

# Clustered variance-covariance matrix
clustered_vcov <- vcovCL(model3, cluster = ~ ccode)

# Compute marginal effects with clustered SEs
mfx3 <- margins(model3, vcov = clustered_vcov)

# Convert to data frame
df <- as.data.frame(summary(mfx3))

# Add multiple confidence intervals
df <- df %>%
  mutate(
    ci90_low = AME - qnorm(0.95) * SE,
    ci90_high = AME + qnorm(0.95) * SE,
    ci95_low = AME - qnorm(0.975) * SE,
    ci95_high = AME + qnorm(0.975) * SE,
    ci99_low = AME - qnorm(0.995) * SE,
    ci99_high = AME + qnorm(0.995) * SE
  )


model_vars <- attr(model3$terms, "term.labels")
df$factor <- factor(df$factor, levels = rev(model_vars))

var_labels <- c("Counterbalancing (log)",
                               "Coup from top",
                               "Coup from middle",
                               "Military regime",
                               "Democracy",
                               "GDP/capita (log)",
                               "Change in GDP/capita",
                               "Recent successful coup",
                               "Recent regional coup",
                               "Recent revolution",
                               "Cold War")
names(var_labels) <- model_vars

df$label <- var_labels[as.character(df$factor)]
df$label <- factor(df$label, levels = var_labels[rev(model_vars)])


ggplot(df, aes(x = label, y = AME)) +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = ci99_low, ymax = ci99_high), width = 0, color = "red") +
  geom_errorbar(aes(ymin = ci95_low, ymax = ci95_high), width = 0, color = "green") +
  geom_errorbar(aes(ymin = ci90_low, ymax = ci90_high), width = 0, color = "blue") +
  coord_flip() +
  labs(title = "Average Marginal Effects (Model 3)",
       x = NULL, y = "AME") 
