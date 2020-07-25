install.packages('bnlearn')
install.packages('bnviewer')
install.packages('StatMeasures')
install.packages('Metrics')
library(Metrics)
library("bnlearn")
library("bnviewer")
library("StatMeasures")

df_item <- read.csv("C:\\Users\\Jeevisha\\Desktop\\ml-100k\\ml-100k\\item.csv")
df_ratings <- read.csv("C:\\Users\\Jeevisha\\Desktop\\ml-100k\\ml-100k\\ratings.csv")

head(df_ratings)

df_item = subset(df_item, select = -c(X,release_date,video_release_date,IMDb_URL,unknown))
df_ratings = subset(df_ratings,select= -c(X,unix_timestamp))

str(df_item)
str(df_ratings)

df = merge(df_item, df_ratings, by.x = "movie_id", by.y = "movie_id", all.x = TRUE)

dim(df)


df_upd <- subset(df, select = -c(movie_title))

df_upd <- as.data.frame(lapply(df_upd,as.factor))


str(df_upd)

head(df_upd)

bn.learn.movies = bnlearn::hc(df_upd)

##Visualization##

viewer(bn.learn.movies,
       bayesianNetwork.width = "100%",
       bayesianNetwork.height = "100vh",
       bayesianNetwork.layout = "layout_on_grid",
       bayesianNetwork.title="<br><span style='font-size:18px;
                                             font-family:Arial;
                                             color:black;
                                             text-align:center;'>
                                             Proposed Bayesian Network
                                             </span>",

       node.colors = list(background = "white",
                          border = "black",
                          highlight = list(background = "#e91eba",
                                           border = "black")),
       
       node.font = list(color = "black", face="Arial"),
       
       clusters.legend.title = list(text = "Legend",
                                    style = "font-size:18px;
                                             font-family:Arial;
                                             color:black;
                                             text-align:center;"),
       
       clusters = list(
         
         list(label = "rating",
              shape = "icon",
              icon = list(code = "f1ce", size = 50, color = "#e91e63")),
         list(label = "movie_id",
              shape = "icon",
              icon = list(code = "f140", size = 50, color = "#03a9f4")),
         list(label = "Action",
              shape = "icon",
              icon = list(code = "f192", size = 50, color = "#4caf50")),
         list(label = "Adventure",
              shape = "icon",
              icon = list(code = "f10c", size = 50, color = "#ffc107")),
         list(label = "Childrens",
              shape = "icon",
              icon = list(code = "f043", size = 50, color = "#03a9f4")),
         list(label = "Comedy",
              shape = "icon",
              icon = list(code = "f043", size = 50, color = "#03a9f4")),
         list(label = "Crime",
              shape = "icon",
              icon = list(code = "f043", size = 50, color = "#03a9f4")),
         list(label = "Documentary",
              shape = "icon",
              icon = list(code = "f043", size = 50, color = "#03a9f4")),
         list(label = "Drama",
              shape = "icon",
              icon = list(code = "f043", size = 50, color = "#03a9f4")),
         list(label = "Fantasy",
              shape = "icon",
              icon = list(code = "f043", size = 50, color = "#03a9f4")),
         list(label = "Film.Noir",
              shape = "icon",
              icon = list(code = "f043", size = 50, color = "#03a9f4")),
         list(label = "Horror",
              shape = "icon",
              icon = list(code = "f043", size = 50, color = "#03a9f4")),
         list(label = "Musical",
              shape = "icon",
              icon = list(code = "f043", size = 50, color = "#03a9f4")),
         list(label = "Mystery",
              shape = "icon",
              icon = list(code = "f043", size = 50, color = "#03a9f4")),
         list(label = "Romance",
              shape = "icon",
              icon = list(code = "f043", size = 50, color = "#03a9f4")),
         list(label = "Sci.Fi",
              shape = "icon",
              icon = list(code = "f043", size = 50, color = "#03a9f4")),
         list(label = "Thriller",
              shape = "icon",
              icon = list(code = "f043", size = 50, color = "#03a9f4")),
         list(label = "War",
              shape = "icon",
              icon = list(code = "f043", size = 50, color = "#03a9f4")),
         list(label = "Western",
              shape = "icon",
              icon = list(code = "f043", size = 50, color = "#03a9f4"))
       )
)

##Model Fitting ##

training.set <- df_upd[1:80000,]

test.set <- df_upd[80000:100000,]

bayesian.fit <- bn.fit(bn.learn.movies,
                       data = training.set,
                       method="mle")

bayesian.predict <- predict(bayesian.fit,
                            "rating",
                            test.set)


real <- as.integer(test.set[,"rating"])

predict <- as.numeric(bayesian.predict)

rmse(real,predict)

final_data <- cbind(test.set,predict)

head(final_data)

