library(survival)

# Load the lung dataset
data(lung)

# Create a survival object
surv_obj <- with(lung, Surv(time, status))

# Calculate the survival probabilities and cumulative hazards
surv_prob <- survfit(surv_obj)
cumhaz <- survfit(surv_obj, conf.type = "none", type = "cumhaz")

# Create a dataframe with the life table information
life_table <- data.frame(time = surv_prob$time,
                         n = surv_prob$n.event,
                         n_cum = cumsum(surv_prob$n.event),
                         surv_prob = surv_prob$surv,
                         cumhaz = cumhaz$cumhaz)

# Add columns for additional life table information
life_table$qx <- life_table$n.event / life_table$n
life_table$px <- c(1, head(life_table$surv_prob, -1))
life_table$lx <- c(nrow(life_table), tail(life_table$n_cum, -1))
life_table$d_x <- -diff(life_table$lx)
life_table$L_x <- life_table$surv_prob * life_table$lx
life_table$T_x <- cumsum(life_table$L_x)

# Print the life table
print(life_table)
