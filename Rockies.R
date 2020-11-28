library(tidyverse)
library(caret)
library(splines)

# load/clean data
  # training set
rockies_hit_distance_train=read_csv("rockies_hit_distance_train.csv")%>%
  filter(!is.na(launch_angle),!is.na(launch_speed),!is.na(hit_distance))%>%
  mutate(home_team=ifelse(home_team=="COL","COL","Else"))
  # validation set
rockies_hit_distance_validation=read_csv("rockies_hit_distance_validation.csv")%>%
  filter(!is.na(launch_angle),!is.na(launch_speed),!is.na(hit_distance))%>%
  mutate(home_team=ifelse(home_team=="COL","COL","Else"))
  # test set
rockies_hit_distance_test=read_csv("rockies_hit_distance_test.csv")%>%
  filter(!is.na(launch_angle),!is.na(launch_speed),!is.na(hit_distance))%>%
  mutate(home_team=ifelse(home_team=="COL","COL","Else"))

 

# visualize hit distance vs. launch angle
ggplot(data=rockies_hit_distance_train,aes(y=hit_distance,x=launch_angle))+geom_point(alpha=0.5)+
  geom_smooth(se=FALSE,color="darkviolet",method="lm",formula=y~ns(x,df=7),size=1.5)+theme_bw()+
  labs(y='Hit Distance (ft)',x="Launch Angle (degrees)",
       title="Hit Distance (ft) vs. Launch Angle (degrees)",subtitle="2019 Colorado Rockies")
# visualize hit distance vs. launch speed
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


