# Install and load necessary libraries
if (!requireNamespace("rvest", quietly = TRUE)) install.packages("rvest")
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")

library(rvest)
library(stringr)
library(dplyr)

# URL of the EU regulation guidance
url <- "https://eur-lex.europa.eu/legal-content/EN/TXT/HTML/?uri=CELEX:02017R0746-20230320"

# Read the webpage content
webpage <- read_html(url)

# Extract all divs with class eli-subdivision and their IDs
article_nodes <- webpage %>% html_nodes("div.eli-subdivision")

# Initialize an empty data frame to store article contents
article_df <- data.frame(article = character(), title = character(), subtitle = character(), content = character(), stringsAsFactors = FALSE)

# Loop through each article node, extract content, and save it to the data frame
for (node in article_nodes) {
	# Extract the ID of the current article
	article_id <- node %>% html_attr("id")
	
	# Extract the title of the article
	article_title <- node %>% html_node("p.title-article-norm") %>% html_text(trim = TRUE)
	
	# Extract the sub-title (eli-title) of the article, if present
	article_subtitle <- node %>% html_node("div.eli-title p.stitle-article-norm") %>% html_text(trim = TRUE)
	
	# Extract all paragraphs and inline elements within the article
	article_content <- node %>% html_nodes(xpath = ".//p | .//div[@class='norm'] | .//span[@class='no-parag']") %>% html_text(trim = TRUE)
	
	# Combine the sub-title and content
	# article_text <- paste(paste(article_content, collapse = "\n"), sep = "\n\n")
	article_text <- paste(paste(article_content, collapse = "\n"), sep = ";;")
	
	# Add the article number, title, subtitle, and content to the data frame
	article_df <- rbind(article_df, data.frame(article = article_id, title = article_title, subtitle = article_subtitle, content = article_text, stringsAsFactors = FALSE))
}

# Display the resulting data frame
print(head(article_df))


# Save the data frame to a CSV file
write.csv(article_df, "./data/EU_Regulation_Articles.csv", row.names = FALSE)
saveRDS(article_df, "./data/EU_Regulation_Articles.Rds")
# Inform the user that the process is complete

cat("Articles have been successfully extracted and saved to the data frame.\n")


# # Install and load necessary libraries
# if (!requireNamespace("rvest", quietly = TRUE)) install.packages("rvest")
# if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
# 
# library(rvest)
# library(stringr)
# 
# # URL of the EU regulation guidance
# url <- "https://eur-lex.europa.eu/legal-content/EN/TXT/HTML/?uri=CELEX:02017R0746-20230320"
# 
# # Read the webpage content
# webpage <- read_html(url)
# 
# # Extract all divs with class eli-subdivision and their IDs
# article_nodes <- webpage %>% html_nodes("div.eli-subdivision")
# 
# # Loop through each article node, extract content, and save it
# for (node in article_nodes) {
# 	# Extract the ID of the current article
# 	article_id <- node %>% html_attr("id")
# 	
# 	# Extract the title of the article
# 	article_title <- node %>% html_node("p.title-article-norm") %>% html_text(trim = TRUE)
# 	
# 	# Extract the content of the article
# 	article_content <- node %>% html_node(xpath = "following-sibling::*[1]") %>% html_text(trim = TRUE)
# 	
# 	# Combine the title and content
# 	article_text <- paste(article_title, article_content, sep = "\n\n")
# 	
# 	# Create a file name for each article
# 	file_name <- paste0("Article_", gsub("art_", "", article_id), ".txt")
# 	
# 	# Save the article text to a file
# 	writeLines(article_text, con = file_name)
# }
# 
# # Inform the user that the process is complete
# cat("Articles have been successfully split and saved to text files.\n")
