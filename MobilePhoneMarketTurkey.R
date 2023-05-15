Code
Libraries
library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(ggipraph)
library(car)
library(rgl)
library(scatterplot3d)
library(plotly)
library(rvest)
Preparation of the Data
library(rvest)
library(stringr)
library(data.table)
library(dplyr)
library(tidyr)


# create links
product_names_url <- "https://www.epey.com/akilli-telefonlar/e/TjtfczoxMDoiZml5YXQ6REVTQyI7="
product_names_url <- str_c(product_names_url,"/",c(1:30))
product_names_url


# Iterate over the elements of the product_names_url vector
title_html <- list()

for (i in seq_along(product_names_url)) {
  url <- product_names_url[i]
  
  # Read the HTML from the URL
  urldata <- read_html(url)
  
  # Extract the text of all the p tags in the HTML
  p_text <- urldata %>%
    html_nodes("p") %>%
    html_text()
  
  # Extract the title from the HTML
  title_html[[i]] <- html_nodes(urldata, ".urunadi")
  
  print(p_text)
  print(title_html[[i]])
}

# Extract the text from the title nodes and concatenate into a single character vector
title <- sapply(title_html, html_text)

# View the data for each URL
title

# Organize product names
title = gsub("İ","i", title)
title = gsub("\\+", "", title)
title = gsub(" ", "-", title)
title = gsub("\\(", "-", title)
title = gsub("\\)", "", title)
title = gsub("--","-",title)

title<- title[-31]
title

# Convert Product names to links
product_links <- "https://www.epey.com/akilli-telefonlar"
product_links <- str_c(product_links,"/",title)
product_links <- paste(product_links,".html",sep="")

# delete duplicates
product_links <- unique(product_links)


# create empty vectors
isim <- character()
price <- character()
specs <- character()
attributes <- character()

# forge the data
telefondatam <- data.frame()
for (link in product_links) {
  # Read the HTML from the URL
  webdata <- try(read_html(link), silent = TRUE)
  
  # Check if an error occurred
  if (inherits(webdata, "try-error")) {
    # If an error occurred, skip to the next iteration of the loop
    next
  }
  
  
  # Extract the data from the HTML
  # check the vector lengths
  isim <- html_nodes(webdata, ".baslik a") %>% html_text()
  price <- html_nodes(webdata, ".urun_fiyat") %>% html_text() %>% head(6)
  length(price) <- 6
  specs <- html_nodes(webdata, "#id14 a , #id21 a , .ozellik1870+ .cs1 a , #id19 a , #id12 a , #id1 a") %>% html_text()
  specs <- specs[1:min(length(specs), 6)]
  specs <- c(specs, rep(NA, max(0, 6 - length(specs))))
  attributes <- c("Ekran Boyutu", "Kamera Çözünürlüğü", "İşlemci Çekirdeği", "RAM", "Dahili Depolama", "Çıkış Yılı")
  
  # Extract the data-link attributes from all the a tags and keep only the first 8
  sellers <- html_nodes(webdata, 'a[rel="nofollow"]') %>% html_attr("data-link") %>% head(6)
  
  # edit sellers data since we scrapped it as link
  sellers <- gsub("^[^.]+\\.", "", sellers)
  sellers <- gsub("\\..*$", "", sellers)
  sellers <- sellers[1:min(length(sellers), 6)]
  sellers <- c(sellers, rep(NA, max(0, 6 - length(sellers))))
  
  isim_6 <- rep(isim,6)
  name <- data.frame(isim_6)
  
  # organize the price
  price <- gsub("\n","",price)
  price <- gsub("Ücretsiz Kargo","",price)
  price <- gsub("\\+ Kargo","",price)
  
  # add each piece of data to the table
  ozellik <- data.frame(specs, attributes)
  ticaret <- data.frame(price,sellers)
  genelozet <- data.frame(ozellik,ticaret,name)
  telefondatam <- rbind(telefondatam,genelozet)
  
}

data <- telefondatam
data2 <- data %>%
  select(attributes, specs) %>%  # select only the relevant columns
  group_by(attributes) %>%  # group the data by the attributes column
  mutate(row = row_number()) %>%  # create a new column with the row number
  pivot_wider(names_from = attributes, values_from = specs)

# Print the new dataframe
# Create an empty dataframe to store the repeated data
repeated_data <- data.frame()

# Loop through every row of the new_data
for (i in 1:nrow(data2)) {
  # Repeat the current row 6 times
  temp_data <- data.frame(matrix(nrow = 1, ncol = ncol(data2)))
  for (j in 1:6){
    temp_data[j,] <- data2[i,]
  }
  # Append the repeated rows to the repeated_data
  repeated_data <- rbind(repeated_data, temp_data)
}
telefondatam$storage <- repeated_data$X6
telefondatam$screen <- repeated_data$X2
telefondatam$date <- repeated_data$X7
telefondatam$cam <- repeated_data$X3
telefondatam$core <- repeated_data$X4
telefondatam$RAM <- repeated_data$X5
telefondatam <- select(telefondatam, -attributes,-specs)
seckindata <- telefondatam
save(seckindata,file = "seckindata.rdata")
Model Creation
library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(ggipraph)
library(car)
library(rgl)
library(scatterplot3d)
library(ggplotly)

custom_theme <- theme_classic() +
  theme(plot.background = element_rect(fill = "gray"),
        panel.grid.major = element_line(color = "lightgray"),
        panel.grid.minor = element_line(color = "lightgray"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title.x = element_text(size = 14, color = "black", face = "bold"),
        axis.title.y = element_text(size = 14, color = "black", face = "bold"),
        legend.title = element_text(size = 14, color = "black", face = "bold"),
        legend.text = element_text(size = 12, color = "black"))

Sys.setlocale(category = "LC_ALL", locale = "Turkish")

telefondatam <- seckindata
telefondatam$price <- gsub("\\.","",telefondatam$price)
telefondatam$price <- gsub("\\,",".",telefondatam$price)
telefondatam$price <- suppressWarnings(as.numeric(gsub(" TL","",telefondatam$price)))
telefondatam <- telefondatam %>% select(-1) %>% mutate(storage = suppressWarnings(as.numeric(substr(storage, 1, nchar(storage) - 3))),
                                                       screen = suppressWarnings(as.numeric(substr(screen, 1, nchar(screen) - 4))),
                                                       date = as.numeric(date),
                                                       cam = suppressWarnings(as.numeric(substr(cam, 1, nchar(cam) - 3))),
                                                       core = suppressWarnings(as.numeric(substr(core, 1, nchar(core) - 9))),
                                                       RAM = suppressWarnings(as.numeric(substr(RAM, 1, nchar(RAM) - 3))))
#### Clean wrong values from the data
telefondatam <- filter(telefondatam, RAM <= 19)


#Organization of the sellers and transformation of them to dummy variables 
sellers <- unique(telefondatam$sellers)
biggestSellers <- c("amazon", "hepsiburada", "trendyol")
techSellers <- c("teknosa", "vatanbilgisayar", "mediamarkt", "samsung", "huawei")
alternativeSellers <- sellers[!sellers %in% biggestSellers]
alternativeSellers <- alternativeSellers[!alternativeSellers %in% techSellers]
alternativeSellers


telefondatam$biggestSellersDummy <- ifelse(telefondatam$sellers %in% biggestSellers, 1, 0)
telefondatam$techSellersDummy <- ifelse(telefondatam$sellers %in% techSellers, 1, 0)
telefondatam$alternativeSellersDummy <- ifelse(telefondatam$sellers %in% alternativeSellers, 1, 0)


#Organization of the brands and transformation of them to dummy variables 
brands <- gsub("\\ .*","", telefondatam$isim_6)
telefondatam$brands <- brands
brandNames <- unique(brands)

biggestBrands <- c("Apple", "Samsung", "Huawei", "Xiaomi")
midBrands <- c("Oppo", "Redmi", "Vestel", "Asus", "Sony", "Casper", "Motorola", "Nokia")
smallBrands <- brandNames[!brandNames %in% biggestBrands]
smallBrands <- smallBrands[!smallBrands %in% midBrands]
unique(smallBrands)

telefondatam$biggestBrandsDummy <- ifelse(telefondatam$brands %in% biggestBrands, 1, 0)
telefondatam$midBrandsDummy <- ifelse(telefondatam$brands %in% midBrands, 1, 0)
telefondatam$smallBrandsDummy <- ifelse(telefondatam$brands %in% smallBrands, 1, 0)

##### General Case and sellers
model <- lm(data = telefondatam, price ~ storage + screen + cam + RAM + biggestSellersDummy + techSellersDummy)
summary(model)

#####General Case 
model2 <- lm(data = telefondatam, price ~ storage + screen + cam + RAM)
summary(model2)
######Graphs of General Case
ggplot(telefondatam, aes(price, RAM)) +
  geom_point()+geom_smooth(method="lm")+custom_theme
ggplot(telefondatam, aes(price, storage)) +
  geom_point()+geom_smooth(method="lm")+custom_theme
ggplot(telefondatam, aes(price, screen)) +
  geom_point()+geom_smooth(method="lm")+custom_theme
ggplot(telefondatam, aes(price, cam)) +
  geom_point()+geom_smooth(method="lm")+custom_theme
avPlot3d(model2,coef1="cam", coef2 = "RAM",id = TRUE, fit="robust")

#apple graphs
telefondatamApple <- telefondatam %>% filter(brands == "Apple")
ggplot(telefondatamApple, aes(RAM, price)) +
  geom_point()+geom_smooth(method="lm")+custom_theme
ggplot(telefondatamApple, aes(storage, price)) +
  geom_point()+geom_smooth(method="lm")+custom_theme
ggplot(telefondatamApple, aes(screen, price)) +
  geom_point()+geom_smooth(method="lm")+custom_theme
ggplot(telefondatamApple, aes(cam, price)) +
  geom_point()+geom_smooth(method="lm")+custom_theme

#### Apple model and sellers
modelApple <- lm(data = telefondatamApple, price ~ storage + screen + cam + RAM + biggestSellersDummy + techSellersDummy)
summary(modelApple)
###### Apple Model.
modelApple2 <- lm(data = telefondatamApple, price ~ storage + screen + cam + RAM)
##### Apple 3D graph
avPlot3d(modelApple,coef1="cam", coef2 = "RAM",id = TRUE, fit="robust")
modelApple3d <- lm(data = telefondatamApple, price ~ storage + screen + cam)

###### not apple
telefondatamNotApple <- telefondatam %>% filter(brands == "Xiaomi" | brands == "Samsung" | brands == "Huawei") 
ggplot(telefondatamNotApple, aes(RAM, price)) +
  geom_point()+geom_smooth(method="lm")+custom_theme
ggplot(telefondatamNotApple, aes(storage, price)) +
  geom_point()+geom_smooth(method="lm")+custom_theme
ggplot(telefondatamNotApple, aes(screen, price)) +
  geom_point()+geom_smooth(method="lm")+custom_theme
ggplot(telefondatamNotApple, aes(cam, price)) +
  geom_point()+geom_smooth(method="lm")+custom_theme

modelNotApple <- lm(data = telefondatamNotApple, price ~ storage + screen + cam + RAM + biggestSellersDummy + techSellersDummy)
summary(modelNotApple)

modelNotApple2 <- lm(data = telefondatamNotApple, price ~ storage + screen + cam + RAM)
summary(modelNotApple2)
avPlot3d(modelNotApple2,coef1="cam", coef2 = "RAM",id = TRUE, fit="robust")

#### Big4
telefondatambig4 <- telefondatam %>% filter(brands == "Xiaomi" | brands == "Samsung" | brands == "Huawei" | brands == "Apple") 
ggplot(telefondatambig4, aes(RAM, price)) +
  geom_point()+geom_smooth(method="lm")+custom_theme +
  facet_wrap(~brands, ncol = 2, scales = "free")
ggplot(telefondatambig4, aes(storage, price)) +
  geom_point()+geom_smooth(method="lm")+custom_theme +
  facet_wrap(~brands, ncol = 2, scales = "free")
ggplot(telefondatambig4, aes(screen, price)) +
  geom_point()+geom_smooth(method="lm")+custom_theme +
  facet_wrap(~brands, ncol = 2, scales = "free")
ggplot(telefondatambig4, aes(cam, price)) +
  geom_point()+geom_smooth(method="lm")+custom_theme +
  facet_wrap(~brands, ncol = 2, scales = "free")



####Price Range And Graph
pricerange <- group_by(telefondatam, brands)
pricerange <- filter(pricerange, brands == "Apple"| brands =="Xiaomi"|brands =="Huawei"|brands =="Samsung"|brands =="Oppo"|brands =="General")
pricerange <- filter(pricerange, price <= 25000)
ggplot(pricerange, aes(x=price, fill=brands)) +
  geom_histogram(binwidth = 100) +
  ggtitle("Range of Prices in Mobile Phones 0-25000TRY") +
  xlab("Price (in TRY)") +
  ylab("Frequency")
ggplotly(ggplot(pricerange, aes(x=price, fill=brands)) +
           geom_histogram(binwidth = 100) +
           ggtitle("Range of Prices in Oppo Products by Brand") +
           xlab("Price (in TRY)") +
           ylab("Frequency") +
           scale_fill_discrete(name = "Brand"))
##### price range sellers

sellersrange <- group_by(telefondatam,sellers)
biggsetecommercesrange <- filter(sellersrange, sellers == "amazon"|sellers== "hepsiburada"|sellers== "trendyol")
techsellersrange <- filter(sellersrange, sellers =="mediamarkt"|sellers=="teknosa"|sellers=="turkcell"|sellers=="vatanbilgisayar"|sellers=="samsung"|sellers=="huawei")
techseller <- data.frame(1:259)
techseller$price <- techsellersrange$price
techseller$seller <- rep("tech",times=259)
ecommerce<- data.frame(1:921)
ecommerce$price <- biggsetecommercesrange$price
ecommerce$seller <- rep("ecommerce",times= 921)
sellerpricerange <- data.frame(1:1180)
sellerpricerange$seller <- c(ecommerce$seller,techseller$seller)
sellerpricerange$price <- c(ecommerce$price,techseller$price)
sellerpricerange <- filter(sellerpricerange, price <= 30000)
ggplotly(ggplot(sellerpricerange, aes(x=price,fill=seller)) +
           geom_histogram(binwidth = 150) +
           ggtitle("Range of Prices and Product Quantities by Seller 0-30000TRY") +
           xlab("Price (in TRY)") +
           ylab("Frequency") +
           scale_fill_discrete(name = "Platform"))