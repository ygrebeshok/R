print("Iuliia Grebeshok")

install.packages("RColorBrewer")
install.packages("data.table")
library(RColorBrewer)
library(data.table)
library(dplyr)

setwd("/Users/ulia/Desktop/work_dir/Data\ for\ Industry")
smartphone_market_dataset <- read.csv("smartphone_category.csv")
smartphone_market_dataset

my_dataset <- data.frame(smartphone_market_dataset$Product.Name, smartphone_market_dataset$Brand, smartphone_market_dataset$Sale.Price*0.012, smartphone_market_dataset$Upc, smartphone_market_dataset$Discount.Percentage, 
                         smartphone_market_dataset$Number.Of.Ratings, smartphone_market_dataset$Number.Of.Reviews, smartphone_market_dataset$Star.Rating, smartphone_market_dataset$Ram)
class(my_dataset)

colnames(my_dataset) <- c("Product_Name", "Brand", "Sale_Price", "Mrp", "Discount_Percentage", "Number_of_Ratings", "Number_of_ Reviews", "Star_Rating", "Ram")
my_dataset

my_dataset$Company_name <- lapply(my_dataset$Brand, function(x) 
  if ((x == "LAVA") || (x == "XOLO"))  {"Lava International"} 
  else if ((x == "Alcatel") || (x == "TCL")) {"TCL Corporation"}
  else if ((x == "Apple")) {"Apple"}
  else if ((x == "ASUS") || (x == "TCL")) {"ASUS"}
  else if ((x == "BlackZone")) {"BlackZone Mobiles"}
  else if ((x == "Bluboo")) {"Huihua Exploit Technology Co"}
  else if ((x == "Brown")) {"Solid Group Inc."}
  else if ((x == "Celkon")) {"Celkon Mobiles"}
  else if ((x == "Coolpad") || (x == "EL")) {"LeEco"}
  else if ((x == "Forme")) {"ForMe Communications Technology"}
  else if ((x == "GIONEE")) {"Gionee Communication Equipment Co. Ltd"}
  else if ((x == "Google")) {"Alphabet Inc."}
  else if ((x == "Honor")) {"Shenzhen Zhixin New Information Technology"}
  else if ((x == "HPL")) {"HPL Communication Limited"}
  else if ((x == "HTC")) {"HTC Corporation"}
  else if ((x == "Huawei")) {"Huawei Technologies Co., Ltd."}
  else if ((x == "iball")) {"Iball Worldwide Private Limited"}
  else if ((x == "IKall")) {"IKall"}
  else if ((x == "Infinix") || (x == "Itel") || (x == "Tecno")) {"Transsion Holdings"}
  else if ((x == "Intex")) {"Intex Technologies"}
  else if ((x == "Infocus")) {"Foxconn"}
  else if ((x == "Ismart")) {"I-smart Mobile Technology Private Limited"}
  else if ((x == "KARA")) {"Kara Mobile"}
  else if ((x == "KARBONN")) {"Karbonn Mobiles"}
  else if ((x == "Kekai")) {"Kekai Mobiles"}
  else if ((x == "KXD")) {"KXD Group"}
  else if ((x == "LEAGOO")) {"OTEDA Group Holding Limited"}
  else if ((x == "Lenovo") || (x == "Motorola")) {"Lenovo Group Limited"}
  else if ((x == "LG")) {"LG Corporation"}
  else if ((x == "LYF")) {"Reliance Industries Limited"}
  else if ((x == "Maplin")) {"Maplin Online Ltd."}
  else if ((x == "Meizu")) {"Meizu Technology Co., Ltd."}
  else if ((x == "Mi") || (x == "MI3") || (x == "POCO") || (x == "Redmi")) {"Xiaomi Corporation"}
  else if ((x == "Micromax")) {"Micromax Informatics"}
  else if ((x == "MICROSOFT")) {"Microsoft Corporation"}
  else if ((x == "mobiistar")) {"Mobiistar India"}
  else if ((x == "Nokia")) {"HMD Global"}
  else if ((x == "Nubia")) {"Nubia Technology"}
  else if ((x == "Nuvo")) {"Nuvo Mobiles"}
  else if ((x == "OPPO") || (x == "IQOO") || (x == "ViVO") || (x == "realme")) {"BBK Electronics"}
  else if ((x == "Panasonic")) {"Panasonic Corporation"}
  else if ((x == "ringme")) {"Ringme Gadgets Private Limited"}
  else if ((x == "SAMSUNG")) {"Samsung Group"}
  else if ((x == "Seatel")) {"SEATEL"}
  else if ((x == "Spinup")) {"SPINUP"}
  else if ((x == "SuperD")) {"SuperD Co Ltd"}
  else if ((x == "Tambo")) {"Tambo Mobiles"}
  else if ((x == "TASHAN")) {"Tashan Technology"}
  else if ((x == "Telefono")) {"Telefono"}
  else if ((x == "Voto")) {"Voto Global Limited"}
  else if ((x == "Wizphone")) {"Wizard Company"}
  else if ((x == "YU")) {"YU Televentures"}
  else if ((x == "YUHO")) {"Yuho Mobile Private Limited"}
  else if ((x == "Zen")) {"Teleecare Group"}
  else if ((x == "Zoom")) {"Zoom"})

frequency_table <- table(my_dataset$Brand)
frequency_table
frequency_brand <- as.data.frame(frequency_table)
frequency_brand

freq_lava <- as.numeric(count(subset(my_dataset, Company_name == "Lava International")))
freq_tcl <- as.numeric(count(subset(my_dataset, Company_name == "TCL Corporation")))
freq_apple <- as.numeric(count(subset(my_dataset, Company_name == "Apple")))
freq_asus <- as.numeric(count(subset(my_dataset, Company_name == "ASUS")))
freq_blackzone <- as.numeric(count(subset(my_dataset, Company_name == "BlackZone Mobiles")))
freq_huihua <- as.numeric(count(subset(my_dataset, Company_name == "Huihua Exploit Technology Co")))
freq_solid <- as.numeric(count(subset(my_dataset, Company_name == "Solid Group Inc.")))
freq_celkon <- as.numeric(count(subset(my_dataset, Company_name == "Celkon Mobiles")))
freq_leeco <- as.numeric(count(subset(my_dataset, Company_name == "LeEco")))
freq_forme <- as.numeric(count(subset(my_dataset, Company_name == "ForMe Communications Technology")))
freq_gionee <- as.numeric(count(subset(my_dataset, Company_name == "Gionee Communication Equipment Co. Ltd")))
freq_alphabet <- as.numeric(count(subset(my_dataset, Company_name == "Alphabet Inc.")))
freq_shenzhen <- as.numeric(count(subset(my_dataset, Company_name == "Shenzhen Zhixin New Information Technology")))
freq_hpl <- as.numeric(count(subset(my_dataset, Company_name == "HPL Communication Limited")))
freq_htc <- as.numeric(count(subset(my_dataset, Company_name == "HTC Corporation")))
freq_huawei <- as.numeric(count(subset(my_dataset, Company_name == "Huawei Technologies Co., Ltd.")))
freq_iball <- as.numeric(count(subset(my_dataset, Company_name == "Iball Worldwide Private Limited")))
freq_ikall <- as.numeric(count(subset(my_dataset, Company_name == "IKall")))
freq_transsion <- as.numeric(count(subset(my_dataset, Company_name == "Transsion Holdings")))
freq_intex <- as.numeric(count(subset(my_dataset, Company_name == "Intex Technologies")))
freq_foxconn <- as.numeric(count(subset(my_dataset, Company_name == "Foxconn")))
freq_ismart <- as.numeric(count(subset(my_dataset, Company_name == "I-smart Mobile Technology Private Limited")))
freq_kara <- as.numeric(count(subset(my_dataset, Company_name == "Kara Mobile")))
freq_karbonn <- as.numeric(count(subset(my_dataset, Company_name == "Karbonn Mobiles")))
freq_kekai <- as.numeric(count(subset(my_dataset, Company_name == "Kekai Mobiles")))
freq_kxd <- as.numeric(count(subset(my_dataset, Company_name == "KXD Group")))
freq_oteda <- as.numeric(count(subset(my_dataset, Company_name == "OTEDA Group Holding Limited")))
freq_lenovo <- as.numeric(count(subset(my_dataset, Company_name == "Lenovo Group Limited")))
freq_lg <- as.numeric(count(subset(my_dataset, Company_name == "LG Corporation")))
freq_reliance <- as.numeric(count(subset(my_dataset, Company_name == "Reliance Industries Limited")))
freq_maplin <- as.numeric(count(subset(my_dataset, Company_name == "Maplin Online Ltd.")))
freq_meizu <- as.numeric(count(subset(my_dataset, Company_name == "Meizu Technology Co., Ltd.")))
freq_xiaomi <- as.numeric(count(subset(my_dataset, Company_name == "Xiaomi Corporation")))
freq_micromax <- as.numeric(count(subset(my_dataset, Company_name == "Micromax Informatics")))
freq_microsoft <- as.numeric(count(subset(my_dataset, Company_name == "Microsoft Corporation")))
freq_mobiistar <- as.numeric(count(subset(my_dataset, Company_name == "Mobiistar India")))
freq_hmd <- as.numeric(count(subset(my_dataset, Company_name == "HMD Global")))
freq_nubia <- as.numeric(count(subset(my_dataset, Company_name == "Nubia Technology")))
freq_nuvo <- as.numeric(count(subset(my_dataset, Company_name == "Nuvo Mobiles")))
freq_bbk <- as.numeric(count(subset(my_dataset, Company_name == "BBK Electronics")))
freq_panasonic <- as.numeric(count(subset(my_dataset, Company_name == "Panasonic Corporation")))
freq_ringme <- as.numeric(count(subset(my_dataset, Company_name == "Ringme Gadgets Private Limited")))
freq_samsung <- as.numeric(count(subset(my_dataset, Company_name == "Samsung Group")))
freq_seatel <- as.numeric(count(subset(my_dataset, Company_name == "SEATEL")))
freq_spinup <- as.numeric(count(subset(my_dataset, Company_name == "SPINUP")))
freq_superd <- as.numeric(count(subset(my_dataset, Company_name == "SuperD Co Ltd")))
freq_tambo <- as.numeric(count(subset(my_dataset, Company_name == "Tambo Mobiles")))
freq_tashan <- as.numeric(count(subset(my_dataset, Company_name == "Tashan Technology")))
freq_telefono <- as.numeric(count(subset(my_dataset, Company_name == "Telefono")))
freq_voto <- as.numeric(count(subset(my_dataset, Company_name == "Voto Global Limited")))
freq_wizard <- as.numeric(count(subset(my_dataset, Company_name == "Wizard Company")))
freq_yu <- as.numeric(count(subset(my_dataset, Company_name == "YU Televentures")))
freq_yuho <- as.numeric(count(subset(my_dataset, Company_name == "Yuho Mobile Private Limited")))
freq_teleecare <- as.numeric(count(subset(my_dataset, Company_name == "Teleecare Group")))
freq_zoom <- as.numeric(count(subset(my_dataset, Company_name == "Zoom")))

freq_market_share_vector <- c(freq_lava, freq_tcl, freq_apple, freq_asus, freq_blackzone, freq_huihua, freq_solid, freq_celkon, freq_leeco,
                              freq_forme, freq_gionee, freq_alphabet, freq_shenzhen, freq_hpl, freq_htc, freq_huawei, freq_iball, freq_ikall,
                              freq_transsion, freq_intex, freq_foxconn, freq_ismart, freq_kara, freq_karbonn, freq_kekai, freq_kxd, freq_oteda,
                              freq_lenovo, freq_lg, freq_reliance, freq_maplin, freq_meizu, freq_xiaomi, freq_micromax, freq_microsoft,
                              freq_mobiistar, freq_hmd, freq_nubia, freq_nuvo, freq_bbk, freq_panasonic, freq_ringme, freq_samsung, freq_seatel,
                              freq_spinup, freq_superd, freq_tambo, freq_tashan, freq_telefono, freq_voto, freq_wizard, freq_yu, freq_yuho, freq_teleecare, freq_zoom)

pie_labels <- c("Lava International", "TCL Corporation", "Apple", "ASUS", "BlackZone Mobiles", "Huihua Exploit Technology Co", "Solid Group Inc.",
                "Celkon Mobiles", "LeEco", "ForMe Communications Technology", "Gionee Communication Equipment Co. Ltd", "Alphabet Inc.", "Shenzhen Zhixin New Information Technology",
                "HPL Communication Limited", "HTC Corporation", "Huawei Technologies Co., Ltd.", "Iball Worldwide Private Limited", "IKall", "Transsion Holdings",
                "Intex Technologies", "Foxconn", "I-smart Mobile Technology Private Limited", "Kara Mobile", "Karbonn Mobiles", "Kekai Mobiles", "KXD Group",
                "OTEDA Group Holding Limited", "Lenovo Group Limited", "LG Corporation", "Reliance Industries Limited", "Maplin Online Ltd.", "Meizu Technology Co., Ltd.",
                "Xiaomi Corporation", "Micromax Informatics", "Microsoft Corporation", "Mobiistar India", "HMD Global", "Nubia Technology", "Nuvo Mobiles", "BBK Electronics",
                "Panasonic Corporation", "Ringme Gadgets Private Limited", "Samsung Group", "SEATEL", "SPINUP", "SuperD Co Ltd", "Tambo Mobiles", "Tashan Technology",
                "Telefono", "Voto Global Limited", "Wizard Company", "YU Televentures", "Yuho Mobile Private Limited", "Teleecare Group", "Zoom")
pie_labels

#Pie Chart

opar <- par(no.readonly = TRUE)
par(mar = c(3, 0, 5, 15))

n <- 55
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
colors <- sample(col_vector, n)

pie(freq_market_share_vector, labels = pie_labels, cex = 0.6, col = colors, main = "Indian Mobile Devices Market", cex.main = 2)
legend("topright", legend = pie_labels, col = colors, inset = c(-0.3, 0), lwd = 6, xpd = TRUE, cex = 0.5)

sum(frequency_brand$Freq)

proportion <- prop.table(frequency_brand$Freq)*100
sum(proportion)

#Barplot for Market Shares
opar <- par(no.readonly = TRUE)
par(mar = c(12, 5, 2, 2))

total_number_of_devices <- sum(freq_market_share_vector)

market_shares_table <- as.table(setNames(freq_market_share_vector,pie_labels))
market_shares_table
barplot(market_shares_table/total_number_of_devices*100, las = 2, cex.names = 0.6, ylim = c(0, 30), col = "cornflowerblue", main = "Indian Mobile Devices Market Distribution", ylab = "Market Share by Company, %")
max(market_shares_table/total_number_of_devices*100)
sort(market_shares_table/total_number_of_devices*100)

#Mean Sale Prices and Star Rating by Brand

alcatel <- subset(my_dataset, my_dataset$Brand == "Alcatel")
av_sales_price_alcatel <- mean(alcatel$Sale_Price)
av_star_rating_alcatel <- mean(alcatel$Star_Rating)

apple <- subset(my_dataset, my_dataset$Brand == "Apple")
av_sales_price_apple <- mean(apple$Sale_Price)
av_star_rating_apple <- mean(apple$Star_Rating)

asus <- subset(my_dataset, my_dataset$Brand == "ASUS")
av_sales_price_asus <- mean(asus$Sale_Price)
av_star_rating_asus <- mean(asus$Star_Rating)

blackzone <- subset(my_dataset, my_dataset$Brand == "BlackZone")
av_sales_price_blackzone <- mean(blackzone$Sale_Price)
av_star_rating_blackzone <- mean(blackzone$Star_Rating)

bluboo <- subset(my_dataset, my_dataset$Brand == "Bluboo")
av_sales_price_bluboo <- mean(bluboo$Sale_Price)
av_star_rating_bluboo <- mean(bluboo$Star_Rating)

brown <- subset(my_dataset, my_dataset$Brand == "Brown")
av_sales_price_brown <- mean(brown$Sale_Price)
av_star_rating_brown <- mean(brown$Star_Rating)

celkon <- subset(my_dataset, my_dataset$Brand == "Celkon")
av_sales_price_celkon <- mean(celkon$Sale_Price)
av_star_rating_celkon <- mean(celkon$Star_Rating)

coolpad <- subset(my_dataset, my_dataset$Brand == "Coolpad")
av_sales_price_coolpad <- mean(coolpad$Sale_Price)
av_star_rating_coolpad <- mean(coolpad$Star_Rating)

el <- subset(my_dataset, my_dataset$Brand == "EL")
av_sales_price_el <- mean(el$Sale_Price)
av_star_rating_el <- mean(el$Star_Rating)

forme <- subset(my_dataset, my_dataset$Brand == "Forme")
av_sales_price_forme <- mean(forme$Sale_Price)
av_star_rating_forme <- mean(forme$Star_Rating)

gionee <- subset(my_dataset, my_dataset$Brand == "GIONEE")
av_sales_price_gionee <- mean(gionee$Sale_Price)
av_star_rating_gionee <- mean(gionee$Star_Rating)

google <- subset(my_dataset, my_dataset$Brand == "Google")
av_sales_price_google <- mean(google$Sale_Price)
av_star_rating_google <- mean(google$Star_Rating)

honor <- subset(my_dataset, my_dataset$Brand == "Honor")
av_sales_price_honor <- mean(honor$Sale_Price)
av_star_rating_honor <- mean(honor$Star_Rating)

hpl <- subset(my_dataset, my_dataset$Brand == "HPL")
av_sales_price_hpl <- mean(hpl$Sale_Price)
av_star_rating_hpl <- mean(hpl$Star_Rating)

htc <- subset(my_dataset, my_dataset$Brand == "HTC")
av_sales_price_htc <- mean(htc$Sale_Price)
av_star_rating_htc <- mean(htc$Star_Rating)

huawei <- subset(my_dataset, my_dataset$Brand == "Huawei")
av_sales_price_huawei <- mean(huawei$Sale_Price)
av_star_rating_huawei <- mean(huawei$Star_Rating)

iball <- subset(my_dataset, my_dataset$Brand == "iball")
av_sales_price_iball <- mean(iball$Sale_Price)
av_star_rating_iball <- mean(iball$Star_Rating)

ikall <- subset(my_dataset, my_dataset$Brand == "IKall")
av_sales_price_ikall <- mean(ikall$Sale_Price)
av_star_rating_ikall <- mean(ikall$Star_Rating)

infinix <- subset(my_dataset, my_dataset$Brand == "Infinix")
av_sales_price_infinix <- mean(infinix$Sale_Price)
av_star_rating_infinix <- mean(infinix$Star_Rating)

infocus <- subset(my_dataset, my_dataset$Brand == "Infocus")
av_sales_price_infocus <- mean(infocus$Sale_Price)
av_star_rating_infocus <- mean(infocus$Star_Rating)

intex <- subset(my_dataset, my_dataset$Brand == "Intex")
av_sales_price_intex <- mean(intex$Sale_Price)
av_star_rating_intex <- mean(intex$Star_Rating)

iqoo <- subset(my_dataset, my_dataset$Brand == "IQOO")
av_sales_price_iqoo <- mean(iqoo$Sale_Price)
av_star_rating_iqoo <- mean(iqoo$Star_Rating)

ismart <- subset(my_dataset, my_dataset$Brand == "Ismart")
av_sales_price_ismart <- mean(ismart$Sale_Price)
av_star_rating_ismart <- mean(ismart$Star_Rating)

itel <- subset(my_dataset, my_dataset$Brand == "Itel")
av_sales_price_itel <- mean(itel$Sale_Price)
av_star_rating_itel <- mean(itel$Star_Rating)

kara <- subset(my_dataset, my_dataset$Brand == "KARA")
av_sales_price_kara <- mean(kara$Sale_Price)
av_star_rating_kara <- mean(kara$Star_Rating)

karbonn <- subset(my_dataset, my_dataset$Brand == "KARBONN")
av_sales_price_karbonn <- mean(karbonn$Sale_Price)
av_star_rating_karbonn <- mean(karbonn$Star_Rating)

kekai <- subset(my_dataset, my_dataset$Brand == "Kekai")
av_sales_price_kekai <- mean(kekai$Sale_Price)
av_star_rating_kekai <- mean(kekai$Star_Rating)

kxd <- subset(my_dataset, my_dataset$Brand == "KXD")
av_sales_price_kxd <- mean(kxd$Sale_Price)
av_star_rating_kxd <- mean(kxd$Star_Rating)

lava <- subset(my_dataset, my_dataset$Brand == "LAVA")
av_sales_price_lava <- mean(lava$Sale_Price)
av_star_rating_lava <- mean(lava$Star_Rating)

leagoo <- subset(my_dataset, my_dataset$Brand == "LEAGOO")
av_sales_price_leagoo <- mean(leagoo$Sale_Price)
av_star_rating_leagoo <- mean(leagoo$Star_Rating)

lenovo <- subset(my_dataset, my_dataset$Brand == "Lenovo")
av_sales_price_lenovo <- mean(lenovo$Sale_Price)
av_star_rating_lenovo <- mean(lenovo$Star_Rating)

lg <- subset(my_dataset, my_dataset$Brand == "LG")
av_sales_price_lg <- mean(lg$Sale_Price)
av_star_rating_lg <- mean(lg$Star_Rating)

lyf <- subset(my_dataset, my_dataset$Brand == "LYF")
av_sales_price_lyf <- mean(lyf$Sale_Price)
av_star_rating_lyf <- mean(lyf$Star_Rating)

maplin <- subset(my_dataset, my_dataset$Brand == "Maplin")
av_sales_price_maplin <- mean(maplin$Sale_Price)
av_star_rating_maplin <- mean(maplin$Star_Rating)

meizu <- subset(my_dataset, my_dataset$Brand == "Meizu")
av_sales_price_meizu <- mean(meizu$Sale_Price)
av_star_rating_meizu <- mean(meizu$Star_Rating)

mi <- subset(my_dataset, my_dataset$Brand == "Mi")
av_sales_price_mi <- mean(mi$Sale_Price)
av_star_rating_mi <- mean(mi$Star_Rating)

mi3 <- subset(my_dataset, my_dataset$Brand == "MI3")
av_sales_price_mi3 <- mean(mi3$Sale_Price)
av_star_rating_mi3 <- mean(mi3$Star_Rating)

micromax <- subset(my_dataset, my_dataset$Brand == "Micromax")
av_sales_price_micromax <- mean(micromax$Sale_Price)
av_star_rating_micromax <- mean(micromax$Star_Rating)

microsoft <- subset(my_dataset, my_dataset$Brand == "MICROSOFT")
av_sales_price_microsoft <- mean(microsoft$Sale_Price)
av_star_rating_microsoft <- mean(microsoft$Star_Rating)

mobiistar <- subset(my_dataset, my_dataset$Brand == "mobiistar")
av_sales_price_mobiistar <- mean(mobiistar$Sale_Price)
av_star_rating_mobiistar <- mean(mobiistar$Star_Rating)

motorola <- subset(my_dataset, my_dataset$Brand == "Motorola")
av_sales_price_motorola <- mean(motorola$Sale_Price)
av_star_rating_motorola <- mean(motorola$Star_Rating)

nokia <- subset(my_dataset, my_dataset$Brand == "Nokia")
av_sales_price_nokia <- mean(nokia$Sale_Price)
av_star_rating_nokia <- mean(nokia$Star_Rating)

nubia <- subset(my_dataset, my_dataset$Brand == "Nubia")
av_sales_price_nubia <- mean(nubia$Sale_Price)
av_star_rating_nubia <- mean(nubia$Star_Rating)

nuvo <- subset(my_dataset, my_dataset$Brand == "Nuvo")
av_sales_price_nuvo <- mean(nuvo$Sale_Price)
av_star_rating_nuvo <- mean(nuvo$Star_Rating)

oppo <- subset(my_dataset, my_dataset$Brand == "OPPO")
av_sales_price_oppo <- mean(oppo$Sale_Price)
av_star_rating_oppo <- mean(oppo$Star_Rating)

panasonic <- subset(my_dataset, my_dataset$Brand == "Panasonic")
av_sales_price_panasonic <- mean(panasonic$Sale_Price)
av_star_rating_panasonic <- mean(panasonic$Star_Rating)

poco <- subset(my_dataset, my_dataset$Brand == "POCO")
av_sales_price_poco <- mean(poco$Sale_Price)
av_star_rating_poco <- mean(poco$Star_Rating)

realme <- subset(my_dataset, my_dataset$Brand == "realme")
av_sales_price_realme <- mean(realme$Sale_Price)
av_star_rating_realme <- mean(realme$Star_Rating)

redmi <- subset(my_dataset, my_dataset$Brand == "Redmi")
av_sales_price_redmi <- mean(redmi$Sale_Price)
av_star_rating_redmi <- mean(redmi$Star_Rating)

ringme <- subset(my_dataset, my_dataset$Brand == "ringme")
av_sales_price_ringme <- mean(ringme$Sale_Price)
av_star_rating_ringme <- mean(ringme$Star_Rating)

samsung <- subset(my_dataset, my_dataset$Brand == "SAMSUNG")
av_sales_price_samsung <- mean(samsung$Sale_Price)
av_star_rating_samsung <- mean(samsung$Star_Rating)

seatel <- subset(my_dataset, my_dataset$Brand == "Seatel")
av_sales_price_seatel <- mean(seatel$Sale_Price)
av_star_rating_seatel <- mean(seatel$Star_Rating)

spinup <- subset(my_dataset, my_dataset$Brand == "Spinup")
av_sales_price_spinup <- mean(spinup$Sale_Price)
av_star_rating_spinup <- mean(spinup$Star_Rating)

superd <- subset(my_dataset, my_dataset$Brand == "SuperD")
av_sales_price_superd <- mean(superd$Sale_Price)
av_star_rating_superd <- mean(superd$Star_Rating)

tambo <- subset(my_dataset, my_dataset$Brand == "Tambo")
av_sales_price_tambo <- mean(tambo$Sale_Price)
av_star_rating_tambo <- mean(tambo$Star_Rating)

tashan <- subset(my_dataset, my_dataset$Brand == "TASHAN")
av_sales_price_tashan <- mean(tashan$Sale_Price)
av_star_rating_tashan <- mean(tashan$Star_Rating)

tcl <- subset(my_dataset, my_dataset$Brand == "TCL")
av_sales_price_tcl <- mean(tcl$Sale_Price)
av_star_rating_tcl <- mean(tcl$Star_Rating)

tecno <- subset(my_dataset, my_dataset$Brand == "Tecno")
av_sales_price_tecno <- mean(tecno$Sale_Price)
av_star_rating_tecno <- mean(tecno$Star_Rating)

telefono <- subset(my_dataset, my_dataset$Brand == "Telefono")
av_sales_price_telefono <- mean(telefono$Sale_Price)
av_star_rating_telefono <- mean(telefono$Star_Rating)

vivo <- subset(my_dataset, my_dataset$Brand == "ViVO")
av_sales_price_vivo <- mean(vivo$Sale_Price)
av_star_rating_vivo <- mean(vivo$Star_Rating)

voto <- subset(my_dataset, my_dataset$Brand == "Voto")
av_sales_price_voto <- mean(voto$Sale_Price)
av_star_rating_voto <- mean(voto$Star_Rating)

wizphone <- subset(my_dataset, my_dataset$Brand == "Wizphone")
av_sales_price_wizphone <- mean(wizphone$Sale_Price)
av_star_rating_wizphone <- mean(wizphone$Star_Rating)

xolo <- subset(my_dataset, my_dataset$Brand == "XOLO")
av_sales_price_xolo <- mean(xolo$Sale_Price)
av_star_rating_xolo <- mean(xolo$Star_Rating)

yu <- subset(my_dataset, my_dataset$Brand == "YU")
av_sales_price_yu <- mean(yu$Sale_Price)
av_star_rating_yu <- mean(yu$Star_Rating)

yuho <- subset(my_dataset, my_dataset$Brand == "YUHO")
av_sales_price_yuho <- mean(yuho$Sale_Price)
av_star_rating_yuho <- mean(yuho$Star_Rating)

zen <- subset(my_dataset, my_dataset$Brand == "Zen")
av_sales_price_zen <- mean(zen$Sale_Price)
av_star_rating_zen <- mean(zen$Star_Rating)

zoom <- subset(my_dataset, my_dataset$Brand == "Zoom")
av_sales_price_zoom <- mean(zoom$Sale_Price)
av_star_rating_zoom <- mean(zoom$Star_Rating)

av_sales_price <- c(av_sales_price_alcatel, av_sales_price_apple, av_sales_price_asus, av_sales_price_blackzone, av_sales_price_bluboo, av_sales_price_brown, av_sales_price_celkon,
                    av_sales_price_coolpad, av_sales_price_el, av_sales_price_forme, av_sales_price_gionee, av_sales_price_google, av_sales_price_honor, av_sales_price_hpl,
                    av_sales_price_htc, av_sales_price_huawei, av_sales_price_iball, av_sales_price_ikall, av_sales_price_infinix, av_sales_price_infocus, av_sales_price_intex,
                    av_sales_price_iqoo, av_sales_price_ismart, av_sales_price_itel, av_sales_price_kara, av_sales_price_karbonn, av_sales_price_kekai, av_sales_price_kxd,
                    av_sales_price_lava, av_sales_price_leagoo, av_sales_price_lenovo, av_sales_price_lg, av_sales_price_lyf, av_sales_price_maplin, av_sales_price_meizu,
                    av_sales_price_mi, av_sales_price_mi3, av_sales_price_micromax, av_sales_price_microsoft, av_sales_price_mobiistar, av_sales_price_motorola, av_sales_price_nokia,
                    av_sales_price_nubia, av_sales_price_nuvo, av_sales_price_oppo, av_sales_price_panasonic, av_sales_price_poco, av_sales_price_realme, av_star_rating_redmi,
                    av_sales_price_ringme, av_sales_price_samsung, av_sales_price_seatel, av_sales_price_spinup, av_sales_price_superd, av_sales_price_tambo, av_sales_price_tashan,
                    av_sales_price_tcl, av_sales_price_tecno, av_sales_price_telefono, av_sales_price_vivo, av_sales_price_voto, av_sales_price_wizphone, av_sales_price_xolo,
                    av_sales_price_yu, av_sales_price_yuho, av_sales_price_zen, av_sales_price_zoom)
av_sales_price

sales_price_labels <- sort(levels(factor(my_dataset$Brand)))

av_sales_price_table <- as.table(setNames(av_sales_price, sales_price_labels))
av_sales_price_table


#Average Sales Price by Mobile Device's Brand Barplot

opar <- par(no.readonly = TRUE)
par(mar = c(7, 5, 2, 2))
max(av_sales_price_table)

barplot(av_sales_price_table, las = 2, cex.names = 0.7, main = "Average Sales Price by Mobile Device's Brand", ylim = c(0, 1000), col = "cyan4", ylab = "Price")

#Average Star Rating by Brand

av_star_rating <- c(av_star_rating_alcatel, av_star_rating_apple, av_star_rating_asus, av_star_rating_blackzone, av_star_rating_bluboo, av_star_rating_brown, av_star_rating_celkon,
                    av_star_rating_coolpad, av_star_rating_el, av_star_rating_forme, av_star_rating_gionee, av_star_rating_google, av_star_rating_honor, av_star_rating_hpl,
                    av_star_rating_htc, av_star_rating_huawei, av_star_rating_iball, av_star_rating_ikall, av_star_rating_infinix, av_star_rating_infocus, av_star_rating_intex,
                    av_star_rating_iqoo, av_star_rating_ismart, av_star_rating_itel, av_star_rating_kara, av_star_rating_karbonn, av_star_rating_kekai, av_star_rating_kxd,
                    av_star_rating_lava, av_star_rating_leagoo, av_star_rating_lenovo, av_star_rating_lg, av_star_rating_lyf, av_star_rating_maplin, av_star_rating_meizu,
                    av_star_rating_mi, av_star_rating_mi3, av_star_rating_micromax, av_star_rating_microsoft, av_star_rating_mobiistar, av_star_rating_motorola, av_star_rating_nokia,
                    av_star_rating_nubia, av_star_rating_nuvo, av_star_rating_oppo, av_star_rating_panasonic, av_star_rating_poco, av_star_rating_realme, av_star_rating_redmi,
                    av_star_rating_ringme, av_star_rating_samsung, av_star_rating_seatel, av_star_rating_spinup, av_star_rating_superd, av_star_rating_tambo, av_star_rating_tashan,
                    av_star_rating_tcl, av_star_rating_tecno, av_star_rating_telefono, av_star_rating_vivo, av_star_rating_voto, av_star_rating_wizphone, av_star_rating_xolo,
                    av_star_rating_yu, av_star_rating_yuho, av_star_rating_zen, av_star_rating_zoom)

av_star_rating_table <- as.table(setNames(av_star_rating, sales_price_labels))
av_star_rating_table
max(av_star_rating_table)

opar <- par(no.readonly = TRUE)
par(mar = c(5, 4, 3, 3))
barplot(av_star_rating_table, ylim = c(0,5), las = 2, cex.names = 0.6, main = "Average Star Rating by Mobile Device's Brand", ylab = "Star Rating", col = "cyan4")

#Relationship between Sales Price and Star Rating

scatter.smooth(my_dataset$Star_Rating ~ my_dataset$Sale_Price, ylab = "Star Rating by Model", xlab = "Sale Price by Model", main = "Relationship between Sale Price and Star Rating by Model")

#Part II
#Adding Price after Discount

my_dataset$Price_after_Discount <- my_dataset$Sale_Price - (my_dataset$Discount_Percentage*my_dataset$Sale_Price/100)

barplot(my_dataset$Price_after_Discount, ylim = c(0, 2000), main = "Mobile Device Price after Discount Distribution in India", ylab = "Price after Discount", xlab = "Mobile Devices Models")
mean(my_dataset$Price_after_Discount)
median(my_dataset$Price_after_Discount)

mean(my_dataset$Sale_Price) - mean(my_dataset$Price_after_Discount)
median(my_dataset$Sale_Price) - median(my_dataset$Price_after_Discount)

