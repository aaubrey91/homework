library(arules)

pre_data <- read.csv("maincharlesbook.csv")

pre_data$ChildBks <- ifelse(grepl("^0$", pre_data$ChildBks), NA, "ChildBks")
pre_data$YouthBks <- ifelse(grepl("^0$", pre_data$YouthBks), NA, "YouthBks")
pre_data$CookBks <- ifelse(grepl("^0$", pre_data$CookBks), NA, "CookBks")
pre_data$DoItYBks <- ifelse(grepl("^0$", pre_data$DoItYBks), NA, "DoItYBks")
pre_data$RefBks <- ifelse(grepl("^0$", pre_data$RefBks), NA, "RefBks")
pre_data$ArtBks <- ifelse(grepl("^0$", pre_data$ArtBks), NA, "ArtBks")
pre_data$GeogBks <- ifelse(grepl("^0$", pre_data$GeogBks), NA, "GeogBks")
pre_data$ItalCook <- ifelse(grepl("^0$", pre_data$ItalCook), NA, "ItalCook")
pre_data$ItalAtlas <- ifelse(grepl("^0$", pre_data$ItalAtlas), NA, "ItalAtlas")
pre_data$ItalArt <- ifelse(grepl("^0$", pre_data$ItalArt), NA, "ItalArt")
pre_data$Florence <- ifelse(grepl("^0$", pre_data$Florence), NA, "Florence")

pre_data <- pre_data[,c("ChildBks", "YouthBks", "CookBks", "DoItYBks", "RefBks", "ArtBks", "GeogBks", "ItalCook",
                        "ItalAtlas", "ItalArt", "Florence")]

write.table(pre_data, file = "maincharlesbook_clean.csv", sep=",", row.names = FALSE, col.names=FALSE)

data = read.transactions("maincharlesbook_clean.csv", format="basket", sep=",")

rules <- apriori(data, parameter = list(supp = 0.1, conf=0.8),
                 appearance=list(default="lhs", rhs="CookBks"))

inspect(rules)
