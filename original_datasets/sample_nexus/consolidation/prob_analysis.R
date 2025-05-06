#Starting nexus environment (load nexus, dal tool box, harbinger and datasets)
source("~/janio/nexus/dev/env_start.R")


# Probability Analysis ----------------------------------------------------
#Datasets Yahoo, NAB e RARE
#Select series, labels and detection result to analysis
rare_pe_analysis <- list()
rare_pe_analysis$acc

data("yahoo_a1")
data <- yahoo_a1[[1]]
result <- result_243_arima_yh_a1[[1]]


# Probability limit
#plim = 0; plim = 0.5; plim = 0.8
plim = 0

#Select results with probability data
prob <- result$prob

#Filter by limit
prob_lim <- subset(prob, pe >= plim)

det_prob <- result$detection
det_prob$event <- 0
det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1

#Sum of events
#sum(result$detection$event, na.rm = TRUE)
sum(det_prob$event)

View(det_prob)
# Metrics and Plots -------------------------------------------------------
# evaluating the detection without probability filter
evaluation <- evaluate(result$detector,
                       result$detection$event,
                       data$event)

print(evaluation$confMatrix)

#Evaluate detection with probability filter
#print(evaluate(result$detector, det_prob$event, data$event)$confMatrix)
print(evaluate(result$detector, det_prob$event, data$event)$accuracy)
print(evaluate(result$detector, det_prob$event, data$event)$F1)


# plotting the results
#grf <- har_plot(result$detector, data$series, result$detection, data$event)
#plot(grf)

# plotting limit query detection
grf_lim <- har_plot(result$detector, data$series, det_prob, data$event)
plot(grf_lim)
