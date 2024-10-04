rm(list=ls())
gc() 

# # WD AND LIBRARIES # # # # # # # # # # # # # # # # # # # # # # # # # # 
setwd("C:\\Users\\stefa\\Documents\\Code\\ / /Vit A Inception/VAS Cost Optimization/Simulation/"); getwd()
library(openxlsx)
library(dplyr)
library(ggplot2)
library(doParallel)
library(foreach)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# Simulated Children Dataset #

province_data = read.xlsx("optimization_dataset_new.xlsx")
province_data <- province_data %>% filter(province_name != "DRC Total")

num_cores <- detectCores() - 1 
registerDoParallel(cores = num_cores)

set.seed(123)

# Parallel processing with foreach
combined_data_list <- foreach(i = 1:nrow(province_data), .combine = 'rbind', .packages = c('dplyr')) %dopar% {
  province_name <- province_data$province_name[i]
  province_id <- province_data$province_id[i]
  children_population <- round(province_data$children_population_div1k[i]) # using re-scaled by 1000
  # children_population <- round(province_data$children_population[i]) # using NOT re-scaled
  
  children_data <- data.frame(
    province_name = rep(province_name, children_population),
    province_id = rep(province_id, children_population),
    is_under_24_months = sample(c(1, 0), children_population, replace = TRUE, 
                                prob = c(province_data$under_24_prop[i], province_data$above_24_prop[i])),
    is_near_facility = sample(c(1, 0), children_population, replace = TRUE, 
                              prob = c(province_data$near_facility[i], 1 - province_data$near_facility[i]))
  )
  
  # Assign delivery type and VAS Coverage
  children_data <- children_data %>%
    rowwise() %>%
    mutate(
      is_facility_based = case_when(
        is_under_24_months == 1 ~ sample(c(1, 0), 1, 
                                         prob = c(province_data$under_24_Facility[i], 1 - province_data$under_24_Facility[i])),
        TRUE ~ sample(c(1, 0), 1, 
                      prob = c(province_data$above_24_Facility[i], 1 - province_data$above_24_Facility[i]))
      ),
      VAS_Coverage = case_when(
        is_facility_based == 1 & is_under_24_months == 1 & is_near_facility == 1 ~ sample(c(1, 0), 1, 
                                                                                          prob = c(province_data$near_under_24_VASCOV[i], 1 - province_data$near_under_24_VASCOV[i])),
        is_facility_based == 1 & is_under_24_months == 1 ~ sample(c(1, 0), 1, 
                                                                  prob = c(province_data$away_under_24_VASCOV[i], 1 - province_data$away_under_24_VASCOV[i])),
        is_facility_based == 1 & is_under_24_months == 0 & is_near_facility == 1 ~ sample(c(1, 0), 1, 
                                                                                          prob = c(province_data$near_above_24_VASCOV[i], 1 - province_data$near_above_24_VASCOV[i])),
        is_facility_based == 1 & is_under_24_months == 0 ~ sample(c(1, 0), 1, 
                                                                  prob = c(province_data$away_above_24_VASCOV[i], 1 - province_data$away_above_24_VASCOV[i])),
        is_facility_based == 0 & is_under_24_months == 1 ~ sample(c(1, 0), 1, 
                                                                  prob = c(province_data$campaign_under_24_VASCOV[i], 1 - province_data$campaign_under_24_VASCOV[i])),
        is_facility_based == 0 & is_under_24_months == 0 ~ sample(c(1, 0), 1, 
                                                                  prob = c(province_data$campaign_above_24_VASCOV[i], 1 - province_data$campaign_above_24_VASCOV[i]))
      )
    ) %>%
    ungroup()
  
  # Correcting is_campaign_based and is_facility_based
  children_data$is_campaign_based <- 1 - children_data$is_facility_based
  children_data$is_facility_based <- 1 - children_data$is_campaign_based
  
  # Correcting the is_facility_based and is_campaign_based when VAS_Coverage == 0
  children_data <- children_data %>%
    mutate(
      is_facility_based = case_when(
        VAS_Coverage == 0 ~ 0,
        TRUE ~ is_facility_based
      ),
      is_campaign_based = case_when(
        VAS_Coverage == 0 ~ 0,
        TRUE ~ is_campaign_based
      )
    )
  
  # Adding Cost_per_Child variable
  children_data <- children_data %>%
    mutate(
      Cost_per_Child = case_when(
        VAS_Coverage == 0 ~ 0,
        is_facility_based == 1 & is_under_24_months == 1 ~ 0.2006,
        is_facility_based == 1 & is_under_24_months == 0 ~ 0.2604,
        is_campaign_based == 1 & is_under_24_months == 1 ~ 0.5339,
        is_campaign_based == 1 & is_under_24_months == 0 ~ 0.5465
      )
    )
  
  # Adding the province_has_supplies variable
  children_data <- children_data %>%
    mutate(
      max_children_with_supplies = floor(province_data$`2023_vita_supplies`[i] / 2),
      province_has_supplies = if_else(is_facility_based == 1 & VAS_Coverage == 1, 1, 0),
      province_has_supplies = if_else(cumsum(province_has_supplies) <= max_children_with_supplies, province_has_supplies, 0)
    ) %>%
    select(-max_children_with_supplies)
  
  return(children_data)
}; stopImplicitCluster()

combined_data <- bind_rows(combined_data_list)

nrow(combined_data)
names(combined_data)
table(combined_data$province_name) # DRC provinces where children live
table(combined_data$is_under_24_months) # there are children under 24m (==1) and above 24m (==0)
table(combined_data$is_facility_based) # service deliver is facility
table(combined_data$is_campaign_based) # service deliver is campaign
table(combined_data$is_near_facility) # if child is near health facility (==1), if not (==0)
table(combined_data$VAS_Coverage) # if child received vitamin A (==1), if not (==0)
summary(combined_data$VAS_Coverage)
table(combined_data$Cost_per_Child) # 0.2006 = Facility based & under 24 months; 0.2604 = Facility based & above 24 months; 0.5339 = Campaign based & under 24 months; 0.5465 = Campaign based & above 24 months
table(combined_data$province_has_supplies) # randomly distribute as dummy == 1 if child is_facility_based == 1 & VAS_Coverage == 1, with a maximum top per province given by the amount of supplies of each province, assuming each child needs 2 supplies to get vas_coverage == 1 

write.xlsx(combined_data, "final_simulated_children_DRC_new.xlsx")


# Logistic Regression #
logit_A <- glm(VAS_Coverage ~ is_near_facility + province_has_supplies + is_facility_based 
                   + is_facility_based:is_near_facility + is_facility_based:province_has_supplies, 
                data = combined_data, family = "binomial"); summary(logit_A) 

# Ridge regression model
library(glmnet)
X <- model.matrix(VAS_Coverage ~ is_near_facility + province_has_supplies + 
                    is_facility_based + is_facility_based:is_near_facility + 
                    is_facility_based:province_has_supplies, 
                  data = combined_data)[,-1]
y <- combined_data$VAS_Coverage
ridge_model <- cv.glmnet(X, y, family = "binomial", alpha = 0)
coef(ridge_model, s = "lambda.min")

# MCS #
table(combined_data$Cost_per_Child) # 0.2006 = Facility based & under 24 months; 0.2604 = Facility based & above 24 months; 0.5339 = Campaign based & under 24 months; 0.5465 = Campaign based & above 24 months
cost_facility_under_24 <- 0.2006
cost_facility_over_24 <- 0.2604
cost_campaign_under_24 <- 0.5339
cost_campaign_over_24 <- 0.5465

num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)

facility_shares <- seq(0, 1, by = 0.05)
n_simulations_per_scenario <- 1000

results <- data.frame(simulation = integer(),
                      facility_share = numeric(),
                      campaign_share = numeric(),
                      near_facility_share = numeric(),
                      has_supplies_share = numeric(),
                      cost = numeric(),
                      vas_coverage = numeric())

results <- foreach(facility_share = facility_shares, .combine = rbind, .packages = 'glmnet', .export = c('ridge_model', 'combined_data', 'cost_facility_under_24', 'cost_facility_over_24', 'cost_campaign_under_24', 'cost_campaign_over_24')) %dopar% {
  temp_results <- data.frame(simulation = integer(),
                             facility_share = numeric(),
                             campaign_share = numeric(),
                             near_facility_share = numeric(),
                             has_supplies_share = numeric(),
                             cost = numeric(),
                             vas_coverage = numeric())
  
  for (i in 1:n_simulations_per_scenario) {
    
    # Simulate facility-based deliveries (randomly varying)
    simulated_is_facility_based <- rbinom(nrow(combined_data), size = 1, prob = facility_share)
    
    # Simulate supplies availability (randomly varying)
    simulated_province_has_supplies <- rbinom(nrow(combined_data), size = 1, prob = runif(1, 0, 1))
    
    # Determine campaign-based deliveries only if not facility-based
    simulated_is_campaign_based <- rbinom(nrow(combined_data), size = 1, prob = 1 - facility_share) * (1 - simulated_is_facility_based)
    
    # Keep is_near_facility fixed
    is_near_facility_fixed <- combined_data$is_near_facility
    
    # Add some random effect for more variance
    random_effect <- rnorm(nrow(combined_data), mean = 0, sd = 0.1)
    
    # Predict VAS Coverage using Ridge regression model for facility-based cases
    ridge_prediction_data <- data.frame(is_near_facility = is_near_facility_fixed,
                                        province_has_supplies = simulated_province_has_supplies,
                                        is_facility_based = simulated_is_facility_based)
    
    ridge_matrix <- model.matrix(~ is_near_facility + province_has_supplies + 
                                   is_facility_based + is_facility_based:is_near_facility + 
                                   is_facility_based:province_has_supplies, 
                                 data = ridge_prediction_data)[,-1]
    
    vas_pred <- predict(ridge_model, newx = ridge_matrix, type = "response", s = "lambda.min")
    
    # Adjust predictions for campaign-based and no delivery cases
    vas_pred <- ifelse(simulated_is_campaign_based == 1, 0.9,  # 90% coverage for campaign-based delivery
                       ifelse(simulated_is_facility_based == 0 & simulated_is_campaign_based == 0, 0, vas_pred))
    
    # Calculate cost based on facility vs. campaign delivery and age groups
    cost_estimate <- sum(simulated_is_facility_based * combined_data$is_under_24_months * cost_facility_under_24) +
      sum(simulated_is_facility_based * (1 - combined_data$is_under_24_months) * cost_facility_over_24) +
      sum(simulated_is_campaign_based * combined_data$is_under_24_months * cost_campaign_under_24) +
      sum(simulated_is_campaign_based * (1 - combined_data$is_under_24_months) * cost_campaign_over_24)
    
    # Calculate near_facility_share and has_supplies_share
    near_facility_share <- mean(is_near_facility_fixed)
    has_supplies_share <- mean(simulated_province_has_supplies)
    
    # Store results for this simulation
    temp_results <- rbind(temp_results, data.frame(simulation = i,
                                                   facility_share = facility_share,
                                                   campaign_share = 1 - facility_share,
                                                   near_facility_share = near_facility_share,
                                                   has_supplies_share = has_supplies_share,
                                                   cost = cost_estimate / nrow(combined_data),  # Average cost per child
                                                   vas_coverage = mean(vas_pred)))
  }
  
  return(temp_results)
}; stopCluster(cl)

{
# Add cost-effectiveness measure (cost per unit of VAS coverage)
results <- results %>%
  mutate(cost_effectiveness = cost / vas_coverage)
# Optimal scenario: minimum cost-effectiveness (most cost-effective)
optimal_scenario <- results %>%
  filter(cost_effectiveness == min(cost_effectiveness))
# Scenario with 50% facility and 50% campaign service delivery
scenario_50_50 <- results %>%
  filter(facility_share == 0.50)
# Summary of the 50%/50% scenario
summary_50_50 <- scenario_50_50 %>%
  summarize(avg_cost = mean(cost),
            avg_vas_coverage = mean(vas_coverage),
            avg_cost_effectiveness = mean(cost_effectiveness))
}

aggregate(cbind(cost, vas_coverage) ~ facility_share, data = results, FUN = mean)
summary(results)
print(optimal_scenario) # The optimal scenario in terms of cost-effectiveness
print(summary_50_50) # Scenario of 50%/50% service delivery (campaign vs facility)

 
# # # # # # # # # Graphs # # # # # # # # #

# Cost vs Coverage
ggplot(results, aes(x = cost, y = vas_coverage, color = factor(facility_share))) +
  geom_point(alpha = 0.6, size = 1.5) +  
  geom_point(data = scenario_50_50, aes(x = cost, y = vas_coverage), color = "red", size = 3) +
  geom_smooth(method = "lm", color = "blue", linetype = "dashed", se = FALSE) + 
  labs(title = "Cost vs VAS Coverage (50/50 highlighted)",
       x = "Cost per Child",
       y = "VAS Coverage",
       color = "Facility Share") +
  scale_color_viridis_d(option = "plasma", direction = -1) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),  
    axis.title = element_text(size = 12),
    legend.position = "right"
  )


# CE by service delivery
 target_cost_effectiveness <- median(results$cost_effectiveness)
 ggplot(results, aes(x = facility_share, y = cost_effectiveness, color = factor(facility_share))) +
   geom_line(size = 1.2, alpha = 0.8) +  
   geom_hline(yintercept = target_cost_effectiveness, linetype = "dashed", color = "red", size = 1) +  
   labs(title = "Cost-Effectiveness by Facility Share",
        x = "Facility Share",
        y = "Cost-Effectiveness (Cost per VAS Coverage)",
        color = "Facility Share") +
   scale_color_viridis_d(option = "plasma", direction = -1) + 
   theme_minimal() +
   theme(
     plot.title = element_text(size = 14, face = "bold"), 
     axis.title = element_text(size = 12),
     legend.position = "right"
   )

 
# Coverage by supplies while service delivery
 ggplot(results, aes(x = facility_share, y = vas_coverage, color = has_supplies_share)) +
    geom_point(alpha = 0.8, shape = 16) + 
    scale_color_gradientn(colours = c("red", "yellow", "green")) +
  labs(
    title = "VAS Coverage by Facility Share and Supply Availability",
    x = "Facility Share (Service Delivery by Facility)",
    y = "VAS Coverage (%)",
    color = "Supply Availability Share"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),  
    legend.position = "right",              
    legend.title = element_text(size = 12), 
    legend.text = element_text(size = 10)   
  )

 
# Heatmap of CE by service delivery
library(RColorBrewer)
results_clean <- results # na.omit(results)
results_clean <- results_clean %>%
  mutate(
    facility_share_binned = cut(facility_share, breaks = 10, include.lowest = TRUE),
    vas_coverage_binned = cut(vas_coverage, breaks = 10, include.lowest = TRUE)
  )
summary(results_clean$cost_effectiveness)
min_cost_effectiveness <- min(results_clean$cost_effectiveness, na.rm = TRUE)
max_cost_effectiveness <- 0.7
ggplot(results_clean, aes(x = facility_share_binned, y = vas_coverage_binned, fill = cost_effectiveness)) +
  geom_tile() +
  scale_fill_gradientn(
    colors = rev(brewer.pal(11, "RdYlGn")),
    limits = c(min_cost_effectiveness, max_cost_effectiveness)
  ) +
  labs(
    title = "Heatmap: Cost-Effectiveness by Facility Share and VAS Coverage",
    x = "Facility Share (Binned)",
    y = "VAS Coverage (%) (Binned)",
    fill = "Cost Effectiveness"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "right"
  )


# Scatter plot
results$total_cost <- results$cost * results$vas_coverage * nrow(results)
ggplot(results, aes(x = total_cost, y = cost_effectiveness)) +
  geom_point(alpha = 0.6, size = 1.5, color = "blue") +
  geom_smooth(method = "loess", color = "red", se = FALSE, size = 1) + 
  labs(title = "Total Cost vs Cost-Effectiveness Across Simulations",
       x = "Total Cost (USD)",
       y = "Cost-Effectiveness (Cost per VAS Coverage)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  )


# Acceptability Curves
facility_over_50 <- results %>% filter(facility_share == 1 & has_supplies_share > 0.5)
facility_under_50 <- results %>% filter(facility_share == 1 & has_supplies_share <= 0.5)
campaign_delivery <- results %>% filter(campaign_share == 1)

standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

facility_over_50$standardized_cost <- standardize(facility_over_50$total_cost)
facility_over_50$standardized_ce <- standardize(facility_over_50$cost_effectiveness)

facility_under_50$standardized_cost <- standardize(facility_under_50$total_cost)
facility_under_50$standardized_ce <- standardize(facility_under_50$cost_effectiveness)

if (length(unique(campaign_delivery$total_cost)) == 1) {
  campaign_delivery$standardized_cost <- 0
  campaign_delivery$standardized_ce <- 0
} else {
  campaign_delivery$standardized_cost <- standardize(campaign_delivery$total_cost)
  campaign_delivery$standardized_ce <- standardize(campaign_delivery$cost_effectiveness)
}

standardized_curve_data <- data.frame(
  standardized_cost = c(facility_over_50$standardized_cost, 
                        facility_under_50$standardized_cost, 
                        campaign_delivery$standardized_cost),
  standardized_ce = c(facility_over_50$standardized_ce, 
                      facility_under_50$standardized_ce, 
                      campaign_delivery$standardized_ce),
  Group = rep(c("Facility > 50% Supplies", 
                "Facility <= 50% Supplies", 
                "Campaign Delivery"), 
              times = c(nrow(facility_over_50), nrow(facility_under_50), nrow(campaign_delivery)))
)

ggplot(standardized_curve_data, aes(x = standardized_cost, y = standardized_ce, color = Group)) +
  geom_line(stat = 'summary', fun = 'mean', size = 1) + 
  labs(title = "Standardized Cost-Effectiveness vs Total Costs by Service Delivery",
       x = "Standardized Total Cost",
       y = "Standardized Cost-Effectiveness",
       color = "Service Delivery Type") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.position = "right"
  ) +
  scale_color_manual(values = c("green", "blue", "red"))


rm(list=ls())
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# Province Dataset #
province_data = read.xlsx("optimization_dataset_new.xlsx")
province_data <- province_data %>% filter(province_name != "DRC Total")
names(province_data)
province_data_2 = subset(province_data, select = -c(21, 22, 23))
province_data_2 = rename(province_data_2, VAS_Coverage_near_facility = near_under_24_VASCOV,
                                          VAS_Coverage_away_facility = away_under_24_VASCOV,
                                          VAS_Coverage_campaign = campaign_under_24_VASCOV)
province_data = province_data_2; rm(province_data_2); names(province_data)
summary(province_data)


library(car)
library(lme4)
province_data$scaled_near_facility <- scale(province_data$near_facility)
province_data$scaled_average_distance_near <- scale(province_data$average_distance_near)
province_data$scaled_2023_vita_supplies <- scale(province_data$`2023_vita_supplies`)

glmm_model1 <- glmer(VAS_Coverage_near_facility ~ scaled_near_facility + 
                       scaled_average_distance_near + scaled_2023_vita_supplies + 
                       (1 | province_name),
                     data = province_data, 
                     family = binomial(link = "logit"), 
                     weights = children_population,
                     control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(glmm_model1); vif(glmm_model1)


# Set up the simulation parameters
num_simulations <- 10000
num_provinces <- nrow(province_data)

# Store results
simulation_results <- data.frame()

# Define costs per child for each method
cost_facility_under_24 <- 0.2006
cost_facility_over_24 <- 0.2604
cost_campaign_under_24 <- 0.5339
cost_campaign_over_24 <- 0.5465

# Monte Carlo Simulation
for (i in 1:num_simulations) {
  
  # Copy province data for this simulation
  simulation_data <- province_data
  
  # Randomly allocate delivery method proportions
  simulation_data$under_24_Facility <- runif(num_provinces, 0, 1)
  simulation_data$under_24_Campaign <- 1 - simulation_data$under_24_Facility
  
  simulation_data$above_24_Facility <- runif(num_provinces, 0, 1)
  simulation_data$above_24_Campaign <- 1 - simulation_data$above_24_Facility
  
  # Calculate the number of children covered for each method and age group
  simulation_data$under_24_covered <- simulation_data$children_population * simulation_data$under_24_prop
  simulation_data$above_24_covered <- simulation_data$children_population * simulation_data$above_24_prop
  
  # Calculate total doses required for full coverage
  simulation_data$total_doses_needed <- (simulation_data$under_24_covered + simulation_data$above_24_covered) * 2
  
  # Determine VAS coverage
  simulation_data$VAS_Coverage <- ifelse(
    # If campaign is used 100%, force coverage to be around 90%
    (simulation_data$under_24_Campaign == 1 & simulation_data$above_24_Campaign == 1), 
    0.90, 
    # Otherwise, compute VAS coverage considering supplies and distance
    {
      # Impact of distance and supplies for facility-based delivery
      facility_impact <- (simulation_data$under_24_Facility + simulation_data$above_24_Facility) / 2
      supplies_impact <- ifelse(simulation_data$total_doses_needed > 0,
                                pmin(simulation_data$total_doses_needed, simulation_data$`2023_vita_supplies`) / simulation_data$total_doses_needed,
                                0) # Ensure no division by zero
      
      # Combine effects (facility, supplies) only if facility is used
      combined_impact <- facility_impact * supplies_impact
      
      # Final VAS coverage, considering the combined effect only for facility-based delivery
      (simulation_data$under_24_Campaign * 0.9) + combined_impact * (simulation_data$under_24_Facility)
    }
  )
  
  # Calculate the cost for each province
  simulation_data$cost <- (
    (simulation_data$under_24_Facility * simulation_data$under_24_covered * cost_facility_under_24) +
      (simulation_data$under_24_Campaign * simulation_data$under_24_covered * cost_campaign_under_24) +
      (simulation_data$above_24_Facility * simulation_data$above_24_covered * cost_facility_over_24) +
      (simulation_data$above_24_Campaign * simulation_data$above_24_covered * cost_campaign_over_24)
  )
  
  # Store results for this simulation
  simulation_results <- rbind(simulation_results, data.frame(Simulation = i, 
                                                             Province = simulation_data$province_name, 
                                                             VAS_Coverage = simulation_data$VAS_Coverage,
                                                             Cost = simulation_data$cost,
                                                             facility_share_under_24 = simulation_data$under_24_Facility,
                                                             campaign_share_under_24 = simulation_data$under_24_Campaign,
                                                             facility_share_above_24 = simulation_data$above_24_Facility,
                                                             campaign_share_above_24 = simulation_data$above_24_Campaign))
}

# trade-off
weight_coverage <- 0.1
weight_cost <- 0.9
simulation_results <- simulation_results %>%
  mutate(Weighted_Score = (weight_coverage * VAS_Coverage) - (weight_cost * (Cost / max(Cost)))) 

optimal_scenarios_per_province <- simulation_results %>%
  group_by(Province) %>%
  filter(Weighted_Score == max(Weighted_Score)) %>%
  select(Province, Simulation, VAS_Coverage, Cost, Weighted_Score,
         facility_share_under_24, campaign_share_under_24, 
         facility_share_above_24, campaign_share_above_24)
print(optimal_scenarios_per_province, n = 26)


ggplot(optimal_scenarios_per_province, aes(x = reorder(Province, VAS_Coverage), y = VAS_Coverage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "VAS Coverage in Optimal Scenarios by Province",
       x = "Province",
       y = "VAS Coverage") +
  theme_minimal()

ggplot(optimal_scenarios_per_province, aes(x = reorder(Province, Cost), y = Cost)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  labs(title = "Cost in Optimal Scenarios by Province",
       x = "Province",
       y = "Cost") +
  theme_minimal()




