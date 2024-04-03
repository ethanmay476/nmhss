library(ggplot2)
library(gridExtra)

# Your dataset
# df <- data.frame(...)
data("data_start_2014")
# Start the PDF device, directing output to 'variable_summary.pdf'
pdf("variable_summary.pdf", width = 8, height = 11)

# Loop through each variable in the dataset
for (var in names(data_start_2014)) {
  data <- data.frame(value = data_start_2014[[var]])

  # Check if the variable is numeric or factor/categorical
  if (is.numeric(data_start_2014[[var]])) {
    # Create a histogram for numeric variables
    p <- ggplot(data, aes(x = value)) +
      geom_histogram(bins = 30, fill = "skyblue", color = "black") +
      theme_minimal() +
      ggtitle(paste("Summary of", var))
  } else {
    # Create a bar chart for factor/categorical variables
    p <- ggplot(data, aes(x = value)) +
      geom_bar(fill = "tomato", color = "black") +
      theme_minimal() +
      ggtitle(paste("Summary of", var)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }

  # Print the plot to the PDF
  print(p)
}

# Close the PDF device
dev.off()
