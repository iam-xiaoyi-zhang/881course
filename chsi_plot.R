library(ggplot2)

# average life expentancy: ALE
ggplot(data=MA[[3]]) + 
  geom_point(aes(x=reorder(CHSI_County_Name,ALE), y=ALE ),size=8 ,color="steelblue") +
  geom_hline(aes(yintercept=US_ALE), linetype=3, color="green", size=2 ) + 
  labs(x="County", y="Average Life Expectancy") +
  theme(text=element_text(family="serif", size=24)) +
  theme(axis.text.x=element_text(angle=90, size=12, color="black"))

ggplot(data=MA[[3]]) + 
  geom_bar(aes(x=reorder(CHSI_County_Name,-ALE),y=ALE), stat="identity", fill="steelblue") +
  geom_hline(aes(yintercept=US_ALE), linetype=3, color="green", size=2) + 
  labs(x="County", y="Average Life Expectancy") +
  theme(text=element_text(family="serif", size=24)) +
  theme(axis.text.x=element_text(angle=90, size=12, color="black"))
  
# all causes of death: All_Death
ggplot(data=MA[[3]]) + 
  geom_point(aes(x=reorder(CHSI_County_Name,All_Death), y=All_Death ),size=8 ,color="steelblue") +
  geom_hline(aes(yintercept=US_All_Death), size=2, color="green", linetype=3) +
  labs(x="County", y="All Causes of Death") +
  theme(text=element_text(family="serif", size=24)) +
  theme(axis.text.x=element_text(angle=90, size=12, color="black"))

# self rate: Health_Status
ggplot(data=MA[[3]]) + 
  geom_point(aes(x=reorder(CHSI_County_Name,Health_Status), y=Health_Status ),size=8 ,color="steelblue") +
  geom_hline(aes(yintercept=US_Health_Status), size=2, color="green", linetype=3) +
  labs(x="County", y="Health_Status") +
  theme(text=element_text(family="serif", size=24)) +
  theme(axis.text.x=element_text(angle=90, size=12, color="black"))

# Unhealthy days
ggplot(data=MA[[3]]) + 
  geom_point(aes(x=reorder(CHSI_County_Name,Unhealthy_Days), y=Unhealthy_Days),size=8 ,color="steelblue") +
  geom_hline(aes(yintercept=US_Unhealthy_Days), size=2, color="green", linetype=3) +
  labs(x="County", y="Health_Status") +
  theme(text=element_text(family="serif", size=24)) +
  theme(axis.text.x=element_text(angle=90, size=12, color="black"))

# Analyze the causes of death in Worcestor
# too many missing values

# Analyze the demographics
# population size
ggplot(data=MA[[1]], aes(x=reorder(CHSI_County_Name,Population_Size), y=Population_Size)) +
  geom_bar(stat="identity")

# population density
ggplot(data=MA[[1]], aes(x=reorder(CHSI_County_Name,Population_Density), y=Population_Density)) + 
  geom_bar(stat="identity")

# poverty percentage
ggplot(data=MA[[1]], aes(x=reorder(CHSI_County_Name,Poverty), y=Poverty)) + 
  geom_bar(stat="identity")

# white percentage
ggplot(data=MA[[1]], aes(x=reorder(CHSI_County_Name,White), y=White)) + 
  geom_bar(stat="identity")

# Minority percentage
ggplot(data=MA[[1]], aes(x=reorder(CHSI_County_Name,100-White), y=100-White)) + 
  geom_bar(stat="identity")

# <19
ggplot(data=MA[[1]], aes(x=reorder(CHSI_County_Name,Age_19_Under), y=Age_19_Under)) + 
  geom_bar(stat="identity")

# >64 (64-85 & 85) aging of population
ggplot(data=MA[[1]], aes(x=reorder(CHSI_County_Name,Age_65_84+Age_85_and_Over), y=Age_65_84+Age_85_and_Over)) + 
  geom_bar(stat="identity")

