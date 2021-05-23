library(rvest)
library(stringr)

df = data.frame(
  name = as.character(),
  route = as.character(),
  difficulty = as.character(),
  distance = as.character(),
  gain = as.character(),
  star = as.character(),
  rated = as.character(),
  stringsAsFactors = FALSE
)

setwd("/Users/yroell/Documents/Classes/tdi/capstone/")
alltrails = read.csv("alltrails.csv", stringsAsFactors = FALSE)

for (i in 1:nrow(alltrails)) {
  website = read_html(alltrails$websites[i])
  name = str_to_title(str_replace_all(str_match(alltrails$websites[i], "colorado/(.*)\\?")[1,2], "-", " "))
  details = website %>%
    html_nodes(".styles-module__detailData___kQ-eK") %>%
    html_text()
  difficulty = website %>%
    html_nodes(".styles-module__diff___22Qtv") %>%
    html_text()
  ratings = website %>%
    html_nodes(".styles-module__starRatingWrapper___1h6vk") %>%
    html_children()
  star = str_split(as.character(ratings[[2]]), "\"")[[1]][4]
  rated = str_split(as.character(ratings[[5]]), "\"")[[1]][4]
  
  input = c(name, details[3], difficulty, details[1], details[2], star, rated)
  
  df[i,] = input
}

df_clust = as.data.frame(cbind(as.numeric(gsub(",", "", str_split(df$distance, " ", simplify = TRUE)[,1])), as.numeric(gsub(",", "", str_split(df$gain, " ", simplify = TRUE)[,1]))))
names(df_clust) = c("distance", "gain")
df_clust$distance = (df_clust$distance - 1.1)/(41.4 - 1.1)
df_clust$gain = (df_clust$gain - 15)/(2363 - 15)

num = 4
clust = kmeans(df_clust, num)

setwd("/Users/yroell/Documents/Classes/tdi/capstone/alltrails/best_trail/")
save(clust, df, df_clust, file = "data.RData")

pt = c(2, 130)
pt = c((pt[1] - 1.1)/(37.2 - 1.1), (pt[2] - 15)/(2363 - 15))

closest = as.matrix(dist(rbind(pt, clust$centers)))[-1,1]
clust_num = which(closest %in% min(closest))

similar = df_clust[clust$cluster == clust_num, ]
likely = as.matrix(dist(rbind(pt, similar)))[-1,1]
names(likely) = row.names(similar)

head(df[as.numeric(names(sort(likely))), ], 10)
