#Importing all the libraries required
library(rvest)
require(rvest)
library(robotstxt)
library(tidyr)

#Ignore all warnings
options(warn = -1)

#To check if we have permission to extract data from a website (robotstxt package)
paths_allowed(paths = c("https://victorpetfood.com/products")) 

#Reading webpage into R (rvest package)
webpage = read_html("https://victorpetfood.com/products")
webpage

#Extracting Dog product names from the website using Selector Gadget or xpath
ProductName = webpage %>% html_nodes(".product-title a") %>% html_text()
ProductName

#Extracting Product Categories
ProductCategories = webpage %>% html_nodes(".category") %>% html_text()
ProductCategories

#Extracting nested links of each product on the webpage
ProductLinks=c(webpage %>% html_nodes(".product-title a") %>% html_attr("href"))
ProductLinks

####To visit each link and retrieve details of each product####

#Protein Percentage of each product
get_ProductProtein = function(protein){
  product_website = read_html(protein)
  protein_content = product_website %>% html_nodes(".border-bottom .chart-info:nth-child(1) .big-number") %>% 
  html_text() %>% paste(collapse = ",")
  return(protein_content)
}

#Using sapply to get result in the form of vector
ProteinPercent = sapply(ProductLinks, FUN = get_ProductProtein, USE.NAMES = FALSE)
ProteinPercent=as.numeric(gsub("[\r\n%]","",ProteinPercent), ProteinPercent)
View(ProteinPercent)


#Fat Percentage Of each product
get_ProductFat = function(fat){
  product_website = read_html(fat)
  Fat_content = product_website %>% html_nodes(".border-bottom .chart-info+ .chart-info .big-number") %>% 
  html_text() %>% paste(collapse = ",")
  return(Fat_content)
}

FatPercent = sapply(ProductLinks, FUN = get_ProductFat,USE.NAMES = FALSE) 
FatPercent=as.numeric(gsub("[\r\n%]","",FatPercent),FatPercent)
View(FatPercent)


#Kilo Calories per kg of each product
get_Caloriesperkg = function(calories_kg){
  product_website = read_html(calories_kg)
  CaloriesPerKG = product_website %>% html_nodes(".chart-cell+ .chart-cell .chart-info:nth-child(1) .big-number") %>% 
  html_text() %>% paste(collapse = ",")
  return(CaloriesPerKG)
  
}

KiloCaloriesPerKG = sapply(ProductLinks, FUN = get_Caloriesperkg,USE.NAMES = FALSE) 
KiloCaloriesPerKG=as.numeric(gsub("[\r\n]","",KiloCaloriesPerKG),KiloCaloriesPerKG)
View(KiloCaloriesPerKG)


#Kilo Calories per cup of each product
get_Caloriespercup = function(calories_cup){
  product_website = read_html(calories_cup)
  CaloriesPerCup = product_website %>% html_nodes(".chart-cell+ .chart-cell .chart-info+ .chart-info .big-number") %>% 
  html_text() %>% paste(collapse = ",")
  return(CaloriesPerCup)
  
}

KiloCaloriesPerCup = sapply(ProductLinks, FUN = get_Caloriespercup,USE.NAMES = FALSE) 
KiloCaloriesPerCup=as.numeric(gsub("[\r\n]","",KiloCaloriesPerCup),KiloCaloriesPerCup)
View(KiloCaloriesPerCup)


#Protein from Meat Sources
get_meat = function(meat){
  product_website = read_html(meat)
  MeatProtein = product_website %>% html_nodes(".nopadding-right-xs .percent-value") %>% 
  html_text() %>% paste(collapse = ",")
  return(MeatProtein)
  
}

MeatProteinPercent = sapply(ProductLinks, FUN = get_meat,USE.NAMES = FALSE) 
MeatProteinPercent=as.numeric(gsub("[\r\n%]","",MeatProteinPercent),MeatProteinPercent)
View(MeatProteinPercent)


#Protein from Plants & Veggies
get_plants = function(plants){
  product_website = read_html(plants)
  PlantsProtein = product_website %>% html_nodes(".nopadding-right-xs+ .text-center .percent-value") %>% 
  html_text() %>% paste(collapse = ",")
  return(PlantsProtein)
  
}

PlantsProteinPercent = sapply(ProductLinks, FUN = get_plants,USE.NAMES = FALSE)
PlantsProteinPercent=as.numeric(gsub("[\r\n%]","",PlantsProteinPercent),PlantsProteinPercent)
View(PlantsProteinPercent)


#Protein from Grain Sources
get_grains = function(grains){
  product_website = read_html(grains)
  GrainsProtein = product_website %>% html_nodes(".nopadding-left-xs .percent-value") %>% 
  html_text() %>% paste(collapse = ",")
  return(GrainsProtein)
  
}

GrainsProteinPercent = sapply(ProductLinks, FUN = get_grains,USE.NAMES = FALSE)
GrainsProteinPercent=as.numeric(gsub("[\r\n%]","",GrainsProteinPercent),GrainsProteinPercent)
View(GrainsProteinPercent)


#Food Form of each product
get_form = function(food_form){
  product_website = read_html(food_form)
  FoodForm = product_website %>% html_nodes(".field--name-field-food-form") %>% 
  html_text() %>% paste(collapse = ",")
  return(FoodForm)
  
}

FoodForm = sapply(ProductLinks, FUN = get_form,USE.NAMES = FALSE) 
View(FoodForm)


#Life stage applicable for each product
get_stage = function(stage){
  product_website = read_html(stage)
  Lifestage = product_website %>% html_nodes(".field--name-field-life-stage") %>% 
  html_text() %>% paste(collapse = ",")
  return(Lifestage)
  
}

LifeStage = sapply(ProductLinks, FUN = get_stage,USE.NAMES = FALSE) 
View(LifeStage)


#Special Diet for type of dogs
get_diet = function(diet){
  product_website = read_html(diet)
  SpecialDiet = product_website %>% html_nodes(".field--name-field-special-diet") %>% 
  html_text() %>% paste(collapse = ",")
  return(SpecialDiet)
  
}

SpecialDiet = sapply(ProductLinks, FUN = get_diet,USE.NAMES = FALSE) 
View(SpecialDiet)


#Creating Data Frame of all the extracted data from webpage
PetProductDetails = data.frame(ProductName,ProductCategories,ProteinPercent,FatPercent,KiloCaloriesPerKG,KiloCaloriesPerCup,MeatProteinPercent,PlantsProteinPercent,GrainsProteinPercent,FoodForm,LifeStage,SpecialDiet)

View(PetProductDetails)
write.csv(PetProductDetails,"C:/Users/Dell/Desktop/PetProductDetailsRaw.csv")
######Data Cleaning & structuring#######

#Removing Duplicate values from a cell using tidyr package
PetProductDetails=separate(PetProductDetails,col = FoodForm,into = c("new col1","new col2"),sep = ",",)
PetProductDetails=separate(PetProductDetails,col = LifeStage,into = c("new col3","new col4"),sep = ",",)
PetProductDetails=separate(PetProductDetails,col = SpecialDiet,into = c("new col5","new col6"),sep = ",",)

PetProductDetails$`new col2` <- NULL
PetProductDetails$`new col4` <- NULL
PetProductDetails$`new col6` <- NULL

#Renaming the column names
colnames(PetProductDetails)[10:12] = c("FoodForm","LifeStage","SpecialDiet")
#View(PetProductDetails)

#Making all blank values as NA
PetProductDetails[PetProductDetails == ""] <- NA
View(PetProductDetails)

#Writing dataframe into csv file
write.csv(PetProductDetails,"C:/Users/Dell/Desktop/PetProductDetails.csv")

######Data Visualization of Victor Pet food data######

#Checking class, structure, number of rows & columns of data frame
class(ProteinPercent)
str(PetProductDetails)
nrow(PetProductDetails)
ncol(PetProductDetails)


#Scatter Plot with smooth line (Fat Percentages Vs KCAL per kg)
library(ggplot2)
      ggplot(PetProductDetails, aes(x=FatPercent, y=KiloCaloriesPerKG, color=ProductName)) +
        ggtitle("Fat Percentage v/s KCAL using Confidence interval") +
        geom_point(color = "Blue") +
        geom_smooth(color= "Red")


#Basic Scatter Plot
library(hrbrthemes)
      ggplot(PetProductDetails, aes(x=ProteinPercent, y=FatPercent, color=FoodForm)) + 
         ggtitle("Protein Percentags v/s Fat Percentage") +
         geom_point(size=2) +
         expand_limits(y = 0, x = 0)+
         theme_ipsum()

#Ridgeline Plot (Protein Percent vs Product Categories in different lifestages)
library(ggridges)
      ggplot(PetProductDetails, aes(x =ProteinPercent, y = ProductCategories, fill = LifeStage)) +
        geom_density_ridges() +
        theme_ridges() + 
        theme(legend.position = "none")


#Pie Chart
library(plotrix)
food_form_count=table(PetProductDetails$FoodForm)
lab<-paste0(round(food_form_count/sum(food_form_count) * 100, 2), "%")
pie3D(food_form_count,
      labels = lab,
      main = "Food Forms",
      explode = 0.1,
      col = rainbow(length(food_form_count)))
      #legend("topright",c("Canned Food","Crunchy Dog Treats","Dry kibble")),
      legend("bottomright",c("Canned Food","Crunchy Dog Treats","Dry kibble"),cex = 0.7,
      fill = rainbow(length(food_form_count)),
      )


#Bar plot
library(ggplot2)
      ggplot(PetProductDetails,
       aes(x = ProductCategories,
       fill = ProductCategories)) + 
       ggtitle("Product Categories v/s Count") + geom_bar()

#Box plot
boxplot(PetProductDetails[,c(3,7,8,9)],
        col = c("Red","green","blue","yellow"),
        main = "ProteinPercent v/s Protein Types",
        notch = TRUE,
        varwidth = TRUE,
        xlab = "Protein Types",
        ylab = "Protein Percent",
        names = c("Protein","MeatProtein","PlantProtein","GrainProtein"))


