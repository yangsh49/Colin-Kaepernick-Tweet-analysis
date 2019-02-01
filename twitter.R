#title: "twitter"
#author: "Shen YANG"
#date: "11/24/2018"
#source: https://www.kaggle.com/eliasdabbas/5000-justdoit-tweets-dataset/home

setwd("~/Documents/UCI/Customer and Social Analytics/Twitter Project")

rm(list=ls())
tweets = read.csv("justdoit_tweets.csv", na.strings=c("NA","NaN", " ", "none") )
tweets$user_created_at = as.character(tweets$user_created_at)
tweets$user_default_profile_image = as.factor(tweets$user_default_profile_image)
tweets$user_default_profile = as.factor(tweets$user_default_profile)
tweets$user_geo_enabled = as.factor(tweets$user_geo_enabled)

for (i in 1:dim(tweets)[1]) {
  tweets$user_created_year[i] = as.numeric(tail(strsplit(tweets$user_created_at[i], split = " ")[[1]], n=1))
}

tweets =  mutate(tweets, user_register_year = (2018 - user_created_year))

################### Network Building ################### 

#Create network
library(igraph)
library(dplyr)
#create edge_matrix
edge_matrix = data.frame(tweets$tweet_in_reply_to_user_id, tweets$user_id)

#generate network
net = graph.data.frame(edge_matrix, directed = F)


################### Attributes ################### 

#degree_distribution
node_degree = degree(net, mode="all")
deg.dist = degree_distribution(net, cumulative = T, mode="all")
plot( x=0:max(node_degree), y=1-deg.dist, pch=19, cex=1.2, col="orange", 
      xlab="Degree", ylab="Cumulative Frequency", xlim = c(0,15))
hist(node_degree, breaks=200, main="Histogram of node degree", xlim = c(0,15))

#density
edge_density(net, loops = FALSE)

#degree centrality
centr_degree_tmax(net, mode = "all", loops = FALSE)

#definition of attributes
#closeness: the inverse of the average length of the shortest paths to/from all the other vertices
#between: the number of geodesics (shortest paths) going through a vertex or an edge
#hub_scores: a good hub represented a page that pointed to many other pages
#authority_score: a good authority represented a page that was linked by many different hubs

#degree, closeness, between, and hub/authority scores for each node
network_attributes = data.frame(
  node_name = V(net)$name,
  all_degree=degree(net, mode = "all"), 
  Closeness = closeness(net, mode = "all", weights = NA, normalized = FALSE),
  Betweenness = betweenness(net, directed = T, weights = NA),
  Hub_score = hub_score(net)$vector,
  Authority_score = authority.score(net)$vector
)

#Normalized network attributes
normalized_net_attr = data.frame(node_name = network_attributes$node_name,
                                 degree = scale(network_attributes$all_degree),
                                 Closeness = scale(network_attributes$Closeness),
                                 Betweenness = scale(network_attributes$Betweenness),
                                 Hub_score = scale(network_attributes$Hub_score))

#Define importance score (importance_score) = sum of nomalized (degree, Closeness, Betweenness and Hub_score)
normalized_net_attr = mutate(normalized_net_attr, 
                             importance_score = degree + Closeness + Betweenness + Hub_score) %>%
  mutate(n_importance_score = scale(importance_score))

#radar map
library(ggplot2)
library(ggradar)

top5_node = normalized_net_attr[c(1,2,1237,18,920),1:6]%>%
  mutate_at(vars(-node_name),funs(rescale))

top5_node$node_name = as.character(top5_node$node_name)
top5_node$node_name[1] = "1.@realDonladTrump"
top5_node$node_name[2] = "2.@Nike"
top5_node$node_name[3] = "3.@nikitaBDW"
top5_node$node_name[4] = "4.@Kaepernick7"
top5_node$node_name[5] = "5.@darronmckinney"
top5_node = top5_node[,c(1,6,2,3,4,5)]

set.seed(111)
ggradar(top5_node, legend.title = "User_id", 
        plot.title="                    Relative Attributes Value")

################### Plot the Network ################### 

#compute diameter
D = diameter(net, directed = F, weights = NA)
#find the component vertex of diameter
node_along_diameter = get_diameter(net, directed = F, weights = NA)

#color all the vertex
V(net)$color = "gray92"
#colorthe vertex on diameter
V(net)[node_along_diameter]$color = "yellow"
#colorthe vertex has top5 highest importance score
V(net)[as.character(normalized_net_attr$node_name[1])]$color = "red"
V(net)[as.character(normalized_net_attr$node_name[2])]$color = "red"
V(net)[as.character(normalized_net_attr$node_name[1237])]$color = "red"
V(net)[as.character(normalized_net_attr$node_name[18])]$color = "red"
V(net)[as.character(normalized_net_attr$node_name[920])]$color = "red"


#give size to all the vertex
V(net)$size = 0.85
#give a larger size to the vertex on diameter
V(net)[node_along_diameter]$size = 2
#give largerest size to the vertex has highest degree
V(net)[as.character(normalized_net_attr$node_name[1])]$size = 6
V(net)[as.character(normalized_net_attr$node_name[2])]$size = 6
V(net)[as.character(normalized_net_attr$node_name[1237])]$size = 2
V(net)[as.character(normalized_net_attr$node_name[18])]$size = 6
V(net)[as.character(normalized_net_attr$node_name[920])]$size = 2

# Add some specific lables and legends.
V(net)$username = ""
V(net)[as.character(normalized_net_attr$node_name[1])]$username = "@Trump"
V(net)[as.character(normalized_net_attr$node_name[2])]$username = "@Nike"
V(net)[as.character(normalized_net_attr$node_name[1237])]$username = ""
V(net)[as.character(normalized_net_attr$node_name[18])]$username = "@Kaepernick"
V(net)[as.character(normalized_net_attr$node_name[920])]$username = ""

plot(simplify(net), 
     vertex.color = V(net)$color, 
     vertex.size = V(net)$size, 
     edge.arrow.size = 0.05, 
     vertex.label = V(net)$username,
     vertex.label.dist = 1.2,
     vertex.label.color = "firebrick1",
     #vertex.label.degree = pi,
     vertex.label.cex = 1.3, 
     layout=layout.kamada.kawai,     
     main = "Tweets Network")

legend(x = 0.7, y = -0.5, 
       c("Nodes with highest importance score","Nodes along the diameter", "Normal nodes"), pch=21,
       col="#777777", pt.bg=c("red","yellow","gray"), pt.cex=2, cex = 0.75, bty="n", ncol=1, horiz = FALSE)

################### Passion regression ################### 

#Put all interested variables into the model
fit1 = glm(tweet_retweet_count ~ tweet_favorite_count + user_default_profile + user_default_profile_image + 
             user_favourites_count + user_followers_count + user_friends_count + user_geo_enabled +
             user_listed_count + user_statuses_count + user_register_year, 
           data = tweets, family = poisson())
summary(fit1)

#delete non-significant variables
fit2 = glm(tweet_retweet_count ~ tweet_favorite_count + user_default_profile + 
             user_favourites_count + user_followers_count + user_friends_count + user_geo_enabled +
             user_listed_count + user_statuses_count + user_register_year, 
           data = tweets, family = poisson())
summary(fit2)

#Model comparison -> not significant -> fit2 is better for simplicity
anova(fit1, fit2, test="Chisq")

#Multicollinearity
library(car)
vif(fit2)

#Performing the deviance goodness of fit test
with(fit2, cbind(res.deviance = deviance, df = df.residual,
                 p = pchisq(fit2$deviance, df=fit2$df.residual, lower.tail=FALSE)))

#Residual plot
par(mfrow=c(2,2))
plot(fit2)
par(mfrow=c(1,1))

# robust
library(sandwich)
cov.output = vcovHC(fit2, type = "HC0")
std.err = sqrt(diag(cov.output))
robust = cbind(Estimation = coef(fit2), "Robust Std Err" = std.err, 
               "Pr(>|z|)" = 2*pnorm(abs(coef(fit2)/std.err), lower.tail = FALSE))

#interpretation of coefficient
exp(coef(fit2))



