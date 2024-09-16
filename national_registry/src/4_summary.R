# install.packages("ggdark")

path <- "../data/"

color0 <- "#71f9ff"
color1 <- "#3f939a"
color2 <- "#417a9a"
color3 <- "#35516d"
color4 <- "#0c1923"
ground <- "#191919"

# Applying the class function to each column in the sepsis_data data frame
column_classes <- sapply(sepsis_data, class)

# Print the classes of each column
print( column_classes)
class(sepsis_data$Age)

library(ggplot2)
library(ggdark)
library(lubridate)
library(plotly)

sepsis_data$Date_Admission <- ymd(sepsis_data$Date_Admission)
sepsis_data$Date_Discharge <- ymd(sepsis_data$Date_Discharge)
sepsis_data$Date_Follow_up <- ymd(sepsis_data$Date_Follow_up)
class(sepsis_data$Date_Admission)

# plot function ----
plot_data_by_class <- function(data, path = "../data/") {
	# Get classes of each column
	classes <- sapply(data, class)
	
	# Loop through each column and create a plot based on the data class
	plots <- list()
	for (i in seq_along(data)) {
		column_name <- names(data)[i]
		column_data <- data[[i]]
		
		print(paste(column_name, "is a", classes[i]))  # Debugging output
		
		# Check if the column is numeric or integer
		if (classes[i] %in% c("numeric", "integer")) {
			p <- ggplot(data, aes_string(x = column_name)) +
				geom_histogram(bins = 30, fill = color1, color = color3) +
				# ggtitle(paste("Histogram of", column_name)) +
				# theme_minimal() +
				dark_theme_linedraw() +
				xlab(column_name) +
				theme(plot.background = element_rect(fill = ground),
							panel.background = element_rect(fill = ground))
		} else {  # Assume character or factor for simplicity
			p <- ggplot(data, aes_string(x = column_name)) +
				geom_bar(fill = color2, color = color3) +
				# ggtitle(paste("Bar Plot of", column_name)) +
				# theme_minimal() +
				dark_theme_linedraw() +
				xlab(column_name) +
				ylab("Count") +
				theme(plot.background = element_rect(fill = ground),
							panel.background = element_rect(fill = ground),
							axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
		}
		
		# Store the plot in a list
		plots[[column_name]] <- p
		
		print("ggplot")
		# Save the plot to the specified path
		plot_path <- paste0(path, "plot_", column_name, ".png")
		ggsave(plot_path, plot = p, width = 6, height = 4)
		print(paste("Saved plot to", plot_path))
		
		print("plotly")
		# Save the Plotly plot as HTML file
		plot_path <- paste0(path, "plot_", column_name, ".html")
		saveWidget(ggplotly(p), file = plot_path, selfcontained = TRUE)
		print(paste("Saved interactive plot to", plot_path))
	}
	
	return(plots)
}

# Example usage:
sepsis_plots <- plot_data_by_class(sepsis_data)
