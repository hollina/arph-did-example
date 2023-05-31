## Run event-study regression
event_study_regression <- function(dataset, id, time, treated, y_var, dd_control_list = NULL, control_list = NULL, fixed_effects, cluster_var, high_cap, low_cap){
    
    # Format any lists. 
    if (!is.null(dd_control_list)) {
        dd_control_list_formatted <- paste0(" + ", paste(dd_control_list, collapse = " + "))
    }
    if (is.null(dd_control_list)) {
        dd_control_list_formatted <- ""
    }
    
    if (!is.null(control_list)) {
        control_list_formatted <- paste0(" + ", paste(control_list, collapse = " + "))
    }
    if (is.null(control_list)) {
        control_list_formatted <- ""
    }
    
    # Estimate model 
    model <- eval(
        substitute(
            feols(as.formula(paste0(y_var, 
                                    " ~ `",
                                    paste(grep("_T", names(dataset), value=TRUE), 
                                          collapse = "` + `"), 
                                    "`", 
                                    dd_control_list_formatted,
                                    control_list_formatted,
                                    " | ", 
                                    fixed_effects
            )
            ), 
            data = dataset,
            cluster = ~cluster_var
            )
        )
    )
    ests <- 
        broom::tidy(model)
    
    ests <- ests %>%
        add_row(term = "`_T-1`", estimate = 0, std.error = 0, .before = abs(low_cap) + 1)
    
    ests <- ests %>%
        slice(1:(abs(low_cap) + high_cap + 3))
    
    ests$x <- c(seq(low_cap - 1, high_cap + 1))
    
    output <- list(ests)
    names(output) <- c("ests")
    return(output)
    
    
}

# Make a function that plots the event-study results
plot_event_study <- function(event_study_results, high_cap, low_cap, breaks, plot_title, plot_subtitle, x_title){
    
    # Drop the caps
    event_study_results$ests <- event_study_results$ests %>%
        filter(x != high_cap + 1 & x != low_cap - 1)
    
    event_study_plot <- ggplot(event_study_results$ests) +
        geom_hline(yintercept = 0, color = "grey50", linewidth = 1, linetype = "dashed") +
        geom_vline(xintercept = -.5, color = "grey25", linewidth = 2, linetype = "longdash") +
        geom_ribbon(aes(x = x, ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), alpha = 0.2) +
        geom_line(aes(x = x, y = estimate, size = 1), color = "red", size = 1) + 
        geom_point(aes(x = x, y = estimate, size = 1), color = "white", size = 6, shape = 15) + 
        geom_point(aes(x = x, y = estimate, size = 1), color = "red", size = 5, shape = 15) + 
        theme_minimal() +
        theme(
            legend.position = "none",
            axis.text.x = element_text(size = 24), axis.text.y = element_text(size = 24),
            axis.title.x = element_text(size = 24), axis.title.y = element_text(size = 24),
            panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(),
            axis.line = element_line(colour = "black")
        ) +
        scale_x_continuous(breaks = seq(from = low_cap, to = high_cap, by = breaks)) +
        labs(
            x = x_title,
            y = "",
            title = plot_title,
            subtitle = plot_subtitle
        )
    
    return(event_study_plot)
}