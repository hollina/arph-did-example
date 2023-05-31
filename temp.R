event_time <- function(dataset, id, time, treated, high_cap, low_cap) {
    
    id <- deparse(substitute(id))
    time <- deparse(substitute(time))
    treated <- deparse(substitute(treated))
    dataset <- deparse(substitute(dataset))
    
# Create a temporary dataset that creates "event-time" and caps this variable. 
# Never treated units get set to -1
temp = substitute(dataset)[substitute(treated) == 1, .SD[which.min(substitute(time))], by = substitute(id)][,first_time := substitute(time)][,.(substitute(id), first_time)]

temp = merge(temp, substitute(dataset), all.y = TRUE)

temp[, event_time := substitute(time) - first_time][,event_time_capped := ifelse(event_time > high_cap,  high_cap + 1, ifelse( event_time < low_cap,  low_cap - 1, event_time))][,event_time_capped := ifelse(is.na(event_time), -1, event_time_capped)]

# Create a list of unique levels of the capped event-time variable
unique_vals = levels(as.factor(temp$event_time_capped))

# Remove -1 event-time (so it will be forced to be reference group)
unique_vals = unique_vals[!(unique_vals == -1)]


# Create a dummy variable matrix. 
# Note this is strictly binary 
# You could imagine wanting NA values 
# (e.g., can you know if T-5 is really zero or one if the current substitute(time) is 2020 and the data substitute(time) is 2019?)
# Technically this should be NA IMO. 
# Fill in the 1's
for (i in unique_vals) {
    temp[event_time_capped == i, paste0('_T', i) := 1]
}
# Fill in the zeros
for (i in seq(ncol(temp)-length(unique_vals)+1,ncol(temp))) set(temp, i=which(is.na(temp[[i]])), j=i, value=0)
# Output is the dataset
return(temp)
}

test = event_time(dataset = timing_no_growing_no, 
                                      id = state_pc, 
                                      time = year, 
                                      treated = treated, 
                                      low_cap = -6, 
                                      high_cap = 6)