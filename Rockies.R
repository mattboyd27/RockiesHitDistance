library(tidyverse)
library(caret)
library(splines)

# load data
rockies_hit_distance_train=read_csv("rockies_hit_distance_train.csv")
rockies_hit_distance_validation=read_csv("rockies_hit_distance_validation.csv")
rockies_hit_distance_test=read_csv("rockies_hit_distance_test.csv")

# clean data
rockies_hit_distance_train=rockies_hit_distance_train%>%
  filter(!is.na(launch_angle),!is.na(launch_speed),!is.na(hit_distance))
rockies_hit_distance_test=rockies_hit_distance_test%>%
  filter(!is.na(launch_angle),!is.na(launch_speed),!is.na(hit_distance))
rockies_hit_distance_validation=rockies_hit_distance_validation%>%
  filter(!is.na(launch_angle),!is.na(launch_speed),!is.na(hit_distance))
  # Rockies home team vs. not
rockies_hit_distance_test$home_team=ifelse(rockies_hit_distance_test$home_team=="COL","COL","Else")
rockies_hit_distance_train$home_team=ifelse(rockies_hit_distance_train$home_team=="COL",
                                            "COL","Else")
rockies_hit_distance_validation$home_team=ifelse(rockies_hit_distance_validation$home_team=="COL",
                                                 "COL","Else")
# visualize
ggplot(data=rockies_hit_distance_train,aes(y=hit_distance,x=launch_angle))+geom_point(alpha=0.5)+
  geom_smooth(se=FALSE,color="darkviolet",method="lm",formula=y~ns(x,df=7),size=1.5)+theme_bw()+
  labs(y='Hit Distance (ft)',x="Launch Angle (degrees)",
       title="Hit Distance (ft) vs. Launch Angle (degrees)",subtitle="2019 Colorado Rockies")

ggplot(data=rockies_hit_distance_train,aes(y=hit_distance,x=launch_speed))+geom_point(alpha=0.5)+
  geom_smooth(se=FALSE,color="darkviolet",formula=y~ns(x,11),size=1.5)+theme_bw()+
  labs(y='Hit Distance (ft)',x="Exit Velocity (mph)",
       title="Hit Distance (ft) vs. Exit Velocity (mph)",subtitle="2019 Colorado Rockies")


# model
model=lm(hit_distance~home_team*ns(launch_angle,7)*ns(launch_speed,11),
         data=rockies_hit_distance_train)
  # MSE on train
mean((rockies_hit_distance_train$hit_distance-fitted(model))^2)
  # MSE on validation
mean((rockies_hit_distance_validation$hit_distance-
        predict(model,newdata=rockies_hit_distance_validation))^2)
  # MSE on test
mean((rockies_hit_distance_test$hit_distance-
        predict(model,newdata=rockies_hit_distance_test))^2)
  # residuals by fitted values
ggplot()+geom_point(aes(x=fitted(model),y=rstandard(model)))+
  geom_smooth(aes(x=fitted(model),y=rstandard(model)))+theme_bw()
  
  # predict and visualize
rockies_hit_distance_test$pred=predict(model,rockies_hit_distance_test)
ggplot(data=rockies_hit_distance_test,aes(x=pred,y=hit_distance))+geom_point()+
  geom_smooth()+theme_bw()


