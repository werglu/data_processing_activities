library(ggplot2)

#Reading data
activities <- c("jogging", "standing", "downstairs", "lying", "sitting", "upstairs", "walkfast", "walkslow", "walkmod");
person <- c("026", "027", "028", "029", "030", "031", "033", "034", "036", "039", "040", "041", "042", "043", "044", "046", "047", "048", "049", "050", "051", "052", "053", "054", "055", "056", "057", "058", "059", "060", "061", "062", "063")
length(person)

df <- NULL

for (a in activities)
{
  for (p in person)
  {
    file_name = paste("E:/MGR_SEM2/PPD/Projekt_dane/wristandthigh/", p, "_", a, ".csv", sep="")
    df1 = read.csv(file_name, header=FALSE)
    df1$Id <- p
    df1$Activity <- a
    if (is.null(df))
    {
      df <- df1
    }
    else
    {
      df <- rbind(df, df1)
    }
  }
}

colnames(df)[1:6] <- c("x_wirst", "y_wirst", "z_wirst", "x_leg", "y_leg", "z_leg")
df$Activity <- factor(df$Activity)

#Analyze x_wirst for person 028

df1 <- df[df$Id == "028" & df$Activity == "jogging",]
plot(ts(df1$x_wirst), ylim=c(-8,8),  main="Jogging 028", xlab="Time [Hz]", ylab="współrzędna x nadgarstka")

df1 <- df[df$Id == "028" & df$Activity == "sitting",]
plot(ts(df1$y_wirst), ylim=c(-8,8),  main="Sitting 028", xlab="Time [Hz]", ylab="współrzędna x nadgarstka")

df1 <- df[df$Id == "028" & df$Activity == "standing",]
plot(ts(df1$y_wirst), ylim=c(-8,8), main="Standing 028", xlab="Time [Hz]", ylab="współrzędna x nadgarstka")

df1 <- df[df$Id == "028" & df$Activity == "downstairs",]
plot(ts(df1$x_wirst), ylim=c(-8,8),  main="Downstairs 028", xlab="Time [Hz]", ylab="współrzędna x nadgarstka")

df1 <- df[df$Id == "028" & df$Activity == "lying",]
plot(ts(df1$y_wirst), ylim=c(-8,8),  main="Lying 028", xlab="Time [Hz]", ylab="współrzędna x nadgarstka")

df1 <- df[df$Id == "028" & df$Activity == "upstairs",]
plot(ts(df1$y_wirst), ylim=c(-8,8), main="Upstairs 028", xlab="Time [Hz]", ylab="współrzędna x nadgarstka")

df1 <- df[df$Id == "028" & df$Activity == "walkfast",]
plot(ts(df1$y_wirst), ylim=c(-8,8),  main="Walk fast 028", xlab="Time [Hz]", ylab="współrzędna x nadgarstka")

df1 <- df[df$Id == "028" & df$Activity == "walkslow",]
plot(ts(df1$y_wirst), ylim=c(-8,8), main="Walk slow 028", xlab="Time [Hz]", ylab="współrzędna x nadgarstka")

df1 <- df[df$Id == "028" & df$Activity == "walkmod",]
plot(ts(df1$y_wirst), ylim=c(-8,8), main="Walk mod 028", xlab="Time [Hz]", ylab="współrzędna x nadgarstka")
#plot(1:nrow(df1), df1$x_wirst, type = "l" )


prcomp(df1[,1:6])
pca(df1[,1:6])


#ujemmne wartosci
#uwzglednic sklae -8 do 8
#dolozyc kolumn z czasem z zegarem 100Hz 
#dla kilku osób
#szeregi czasowe ts
