library(knitr)
library(kableExtra)

# Install TinyTeX
if (!requireNamespace("tinytex", quietly = TRUE)) install.packages("tinytex")
tinytex::install_tinytex()

# To reinstall TinyTeX if it's already installed but you're encountering issues
# tinytex::reinstall_tinytex()
Yy
data("data_start_2014_filter")



is_binary_factor <- function(column) {
  if (is.factor(column)) {
    return(all(levels(column) %in% c("0", "1")))
  } else {
    return(TRUE) # Keep non-factor columns
  }
}

# Use sapply to check each column, then filter with select
binary_data <- data_start_2014_filter %>% select(which(sapply(., is_binary_factor)))






# Assuming 'data' is your dataset
# Replace this with your actual dataset loading or preparation code
data <- data.frame(matrix(sample(0:1, 80*10, replace=TRUE), ncol=80)) # Example data

# Start the LaTeX document
latex_content <- c("\\documentclass{article}", "\\usepackage{longtable}", "\\begin{document}")

for (i in 1:(ncol(data)-1)) {
  for (j in (i+1):ncol(data)) {
    # Create a contingency table for each pair of variables
    tbl <- table(data[,i], data[,j])

    # Convert the contingency table to a data frame for kable
    tbl_df <- as.data.frame.matrix(tbl)

    # Add the table in LaTeX format to the document content
    latex_table <- kable(tbl_df, format = "latex", booktabs = TRUE, caption = paste("Variable", i, "vs Variable", j))
    latex_content <- c(latex_content, latex_table, "\\newpage")
  }
}

# End the LaTeX document
latex_content <- c(latex_content, "\\end{document}")

# Write the LaTeX code to a file
writeLines(latex_content, "tables_document.tex")

# Compile the LaTeX document to PDF
# This requires having LaTeX installed on your system
tinytex::pdflatex("tables_document.tex")

