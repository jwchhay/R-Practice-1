## libraries to use
library(rvest)
library(ggplot2)
library(stringr)


# Essentially, I just look at a website which has a table and I
# try to extract the information from there.
# The package I use can easily extract HTML tables, but
# this website doesn't have them I think.
# This thought process can be applied to practically any website that has
# data on it, but I'll just work with a table for now since it's easily 
# observable.



#### website to scrape
website <- "https://www.basketball-reference.com/leagues/NBA_stats_per_game.html"

# Find elements in page I want to extract, and turn it into something that we 
# can manipulate. I'm just trying to extract the data off the website and place it
# into R.
pattern <- "#stats a , .right , .center"
league_avg <- website %>% read_html() %>% 
  html_nodes(pattern) %>% html_text()

# league_avg is a character vector that's essentially a vector with a bunch of data
# the goal is to convert it into a data frame, a data structure that makes a lot more sense

league_average <- league_avg # copy of vector so i don't have to keep requesting info from the website

# I just want to look at the seasons where the three-point line was used, aka starting at 1979-80
match(42, league_average) # 1421 is when a new row starts, so keep everything before this
league_average <- league_average[1:1420]
head(league_average)




#### remove some content in the data
remove_pattern <- league_average[1:4] # it's the first four terms of the vector

# got the nest idea from the following link (I like using stackoverflow for these kind of scenarios):
# https://stackoverflow.com/questions/5577727/is-there-an-r-function-for-finding-the-index-of-an-element-in-a-vector
which(league_average %in% remove_pattern) 

league_average <- league_average[-which(league_average %in% remove_pattern)] 
# keep elements besides those found in the previous vector
head(league_average) 
which(league_average %in% remove_pattern) # confirming we removed those words from the vector




#### clean some data
which(league_average %in% "ORtg") # so this tells us that there are 32 columns.
league_average <- league_average[-c(((704-31):704),((1376-31):1376))] # really ugly looking way to say "remove the two latter rows"
which(league_average %in% c("Rk","ORtg")) # confirm we removed the other instances of the rows





#### convert to data frame
league_df <- matrix(league_average, ncol = 32, byrow = TRUE)
league_df <- as.data.frame(league_df, stringsAsFactors = FALSE)

league_df[1:6,1:10] # previewing the data frame

# some issues with colnames and rownames
colnames(league_df) <- league_df[1,1:32]
league_df <- league_df[-1,]
rownames(league_df) <- league_df[1:41, 1]
league_df[1:6,1:10]





#### convert some variables to character
str(league_df, list.len = 10) # just to show that things like "Age" is a character right now

league_df[] <- lapply(league_df, type.convert)
str(league_df, list.len = 10)

# I'll remove factors from some columns as well.
# (https://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters)
league_df$Season <- sapply(league_df$Season, as.character)
league_df$Lg <- sapply(league_df$Lg, as.character)
league_df$Ht <- sapply(league_df$Ht, as.character)





#### remove special characters
x <- colnames(league_df)
x # to show that there are percent symbols
x <- gsub("%", "Percent", x)
x <- gsub("3P", "ThreePoint", x)
x <- gsub("/", "Over", x)
x # now it's cleaned up
colnames(league_df) <- x






#### make plot of 3pt shooting over the years
# got help from
# https://stackoverflow.com/questions/1330989/rotating-and-spacing-axis-labels-in-ggplot2
ggplot(data = league_df, aes(x = Season, y = ThreePointA)) + 
  geom_bar(stat = "identity", fill = "red") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  xlab("NBA Season Year") + 
  ylab("Average three pointers attempted per game")
