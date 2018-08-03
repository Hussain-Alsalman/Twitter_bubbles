#==================Twitter Followers Analysis=====================
#Setting up the envirnoment 
##-------------------------------------------- Loading Some libraries 
library("rtweet")
library("dplyr")
library("ggplot2")
library("lubridate")
library("packcircles")
#----------------------------------------------Creating Authentications-----------------------------------------------
## Creating Authentications
token <- c(
  create_token(
    app = "APP NAME",
    consumer_key = "CONSUMER KEY",
    consumer_secret = "CONSUMER SECRET",
    access_token = "ACCESS TOKEN",
    access_secret = "ACCESS SECRET"
  )
)
#Reading data 
usr <- "Your_Screen_Name"
dff <- get_followers(user = usr, token = token)

db.f <- lookup_users(users = dff$user_id, token = token)

df.packing <- circleProgressiveLayout(db.f$followers_count, sizetype = "area")
db.filter <- db.f %>% select(screen_name, followers_count,country) %>% cbind(df.packing)
plot(db.filter$followers_count, db.filter$radius)

data.gg <- circleLayoutVertices(df.packing, npoint = 50)

ggplot(data.gg) + geom_polygon(aes(x,y,group =id, fill = as.factor(id)), color = "black", alpha = 0.6)+
  geom_text(data= db.filter, aes(x ,y, size = followers_count, label = screen_name))+ 
  scale_size_continuous(range = c(1,4)) + theme_void()+ coord_equal() + theme(legend.position = "none") 

ggsave("plot.png", width = 20,height=20)
