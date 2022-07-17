application <- read.delim("~/R/application.tsv", colClasses = "character")
rawinventor <- read.delim("~/R/rawinventor.tsv", colClasses = "character")
inventor <- read.delim("~/R/inventor.tsv", colClasses = "character")
pub_inventor <- read.delim("~/R/publication_inventor.tsv", colClasses = "character")

app <- application[, c(2,5)]
raw_in <- rawinventor[, c(2:5, 12)]

inventor_set <- left_join(app, raw_in, by = "document_number")

inventor_set$year <- substr(inventor_set$date, 1, 4)
inventor_set$year <- as.numeric(inventor_set$year)

inventor_set <- inventor_set[inventor_set$year > 1990, ]

patent_n <- inventor_set %>% count(document_number)

inventor_set <- left_join(inventor_set, patent_n, by = "document_number")
inventor_set$us_inventor <- ifelse(inventor_set$country == "US", 1, 0)
inventor_set$cn_inventor <- ifelse(inventor_set$country == "CN", 1, 0)


ex <- aggregate(data = inventor_set, us_inventor~name_first+name_last+year, FUN = sum)

sum_us <- aggregate(us_inventor~document_number, data=inventor_set, sum)
sum_cn <- aggregate(cn_inventor~document_number, data=inventor_set, sum)


unique_inventor_set <- inventor_set %>% distinct(document_number, .keep_all = T)
unique_inventor_set <- inventor_set[ ,c(1:8)]

unique_inventor_set <- left_join(unique_inventor_set, sum_us, by = "document_number")
unique_inventor_set <- left_join(unique_inventor_set, sum_cn, by = "document_number")

unique_inventor_set$us_proportion <- unique_inventor_set$us_inventor/unique_inventor_set$n
unique_inventor_set$cn_proportion <- unique_inventor_set$cn_inventor/unique_inventor_set$n

unique_inventor_set1 <- unique_inventor_set[, c(1, 10, 12)]

us_cn_inventor <- left_join(inventor_set, unique_inventor_set1, by = "document_number")

us_cn_inventor$us_coinventor <- us_cn_inventor$n*us_cn_inventor$us_proportion
us_cn_inventor$cn_coinventor <- us_cn_inventor$n*us_cn_inventor$cn_proportion

us_cn_inventor$us_coinventor[us_cn_inventor$country == "US"] <- 0
us_cn_inventor$cn_coinventor[us_cn_inventor$country == "CN"] <- 0

#us_cn_inventor$inventor <- paste(us_cn_inventor$name_first, us_cn_inventor$name_last)

us_cn_inventor <- read_csv("us_cn_inventor.csv", col_types = c(document_number = "charater"))
us_cn_inventor$document_number <- as.character(us_cn_inventor$document_number)
data_set <- read_csv("data_set.csv")

inventor_set <- read_csv("inventor_set.csv")
inventor_set$document_number <- as.character(inventor_set$document_number)

us_cn_inventor$inventor <- str_trim(us_cn_inventor$inventor) 
merged_data <- us_cn_inventor[, c(3, 7,8,13,14)]
merged_data$n <- merged_data$n-1

merged_data$year <- as.character(merged_data$year)
inventor_set$year <- as.character(inventor_set$year)

ex_1 <- aggregate(n~inventor_id+year, data = merged_data, FUN = sum)
ex_2 <- aggregate(us_coinventor~inventor_id+year, data = merged_data, FUN = sum)
ex_3 <- aggregate(cn_coinventor~inventor_id+year, data = merged_data, FUN = sum)

ex2 <- cbind(ex_2,ex_3[, "cn_coinventor"])

data_set$year <- as.character(data_set$year)
merged_data1 <- left_join(ex_1, ex2, by = c("inventor_id", "year"))

semi_final <- left_join(data_set, merged_data1, by = c("inventor_id", "year"))

colnames(semi_final)[6] <- "cn_coinventor"
colnames(semi_final)[4] <- "coinventor"

inventor_set$document_number <- as.character(inventor_set$document_number)

#china
china <- unique_inventor_set[unique_inventor_set$country == "CN", ]
china$collabor <- ifelse(china$us_proportion == 0, "nation_all", "collaborated_us")

total <- china %>% count(year)
coll <- china %>% count(year, collabor)
coll <- coll[coll$collabor == "collaborated_us", c(1, 3)]
total$coll <- coll$n
total$proportion <- total$coll/total$n

#japan
japan <- unique_inventor_set[unique_inventor_set$country == "JP", ]
japan$collabor <- ifelse(japan$us_proportion == 0, "nation_all", "collaborated_us")


total_jp <- japan %>% count(year)
coll_jp <- japan %>% count(year, collabor)
coll_jp <- coll_jp[coll_jp$collabor == "collaborated_us", c(1, 3)]
total_jp <- left_join(total_jp, coll_jp, by = "year")
total_jp$proportion <- total_jp$n.y/total_jp$n.x

#germany
ger <- unique_inventor_set[unique_inventor_set$country == "DE", ]
ger$collabor <- ifelse(ger$us_proportion == 0, "nation_all", "collaborated_us")


total_de <- ger %>% count(year)
coll_de <- ger %>% count(year, collabor)
coll_de <- coll_de[coll_de$collabor == "collaborated_us", c(1, 3)]
total_de <- left_join(total_de, coll_de, by = "year")
total_de$proportion <- total_de$n.y/total_de$n.x

total$nation <- "china"
total_de$nation <- "germany"
total_jp$nation <- "Japan"

total <- total[1:21, c(1,4,5)]
total_de <- total_de[4:24, c(1, 4, 5)]
total_jp <- total_jp[7:27, c(1, 4, 5)]

total <- rbind(total, total_de)
total <- rbind(total, total_jp)



ggplot(total, aes(year, proportion, group = nation, color = nation))+ 
  geom_line() + 
  ylab(label = "proportion")+ 
  scale_x_continuous(breaks=seq(min(total$year), max(total$year), 3)) + 
  ggtitle("The proportion of each nation collaborated with U.S.") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 12, color = "black")) + 
  geom_vline(xintercept = 2001, colour = "red", lty = "dashed") + 
  annotate("rect", xmin=2017, xmax=2020, ymin=0, ymax=0.1, alpha=0.2, fill="blue") +
  annotate("text", x=2018.5, y=0.08, label="Trump", size=3) +
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed") + 
  annotate("text", x=2020, y=0.1, label="Covid19", size=3)

inventor_cpc <- left_join(inventor_set, cpc, by = c("document_number" = "patent_id"))

china_inventor <- inventor_cpc[inventor_cpc$country == "CN", ]

##
inventor_list <- inventor_set %>% count(inventor_id, year)

inventor_list$inventor <- paste(inventor_list$name_first, inventor_list$name_last)

inventor_list1 <- inventor_list[, c(4:6)]

toyset_inventor_list <- inventor_list1[1:100, ]


inventor_year <- dcast(inventor_list, inventor_id ~ year, value.var = "n", fill = 0)

inventor_year1 <- inventor_year[, c(1:ncol(inventor_year)-1)]

melted <- melt(inventor_year1, id.vars = inventor)


data_set1 <- gather(inventor_year1, key = "year", value = "number of patent", "1991", "1994","1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")






ai_data <- read.delim("~/R/ai_model_predictions.tsv", colClasses = "character")
cpc <- read.delim("~/R/cpc.tsv", colClasses = "character")
cpc <- cpc[, c(1, 4, 6)]

ai_data <- ai_data[, c(1:4)]

patent_cpc <- cpc_current[, c(2,3,5)]

str(cpc)
str(patent_cpc)

colnames(cpc) <- c("patent_id", "section_id", "group_id")

cpc_set <- rbind(patent_cpc, cpc)

ai_cpc <- left_join(ai_data, cpc_set, by = c("doc_id" = "patent_id"))



ggplot(ai_cpc, aes(x = section_id))+ 
  geom_histogram() + 
  ylab(label = "proportion")+ 
  scale_x_continuous(breaks=seq(min(total$year), max(total$year), 3)) + 
  ggtitle("The proportion of each nation collaborated with U.S.") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 12, color = "black")) + 
  geom_vline(xintercept = 2001, colour = "red", lty = "dashed") + 
  annotate("rect", xmin=2017, xmax=2020, ymin=0, ymax=0.1, alpha=0.2, fill="blue") +
  annotate("text", x=2018.5, y=0.08, label="Trump", size=3) +
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed") + 
  annotate("text", x=2020, y=0.1, label="Covid19", size=3)

section_id <- levels(ai_na$section_id)

ai_na <- na.omit(ai_cpc)

ai_na$section_id <- as.factor(ai_na$section_id)

ggplot(ai_na, aes(x = section_id))+ geom_histogram()

str(data_set1)

data_set1$inventor1 <- str_trim(data_set1$inventor)

data_set1 <- data_set1[, c(2:4)]
colnames(data_set1)[3] <- "inventor"





###2022/06/02
library(readr)
library(dplyr)

inventor_set <- read_csv("inventor_set.csv")
semi_final <- read_csv("semi_final.csv")

us_inventor <- inventor_set[inventor_set$country == "US", "inventor_id"]
cn_inventor <- inventor_set[inventor_set$country == "CN", "inventor_id"]

panel_us <- semi_final[semi_final$inventor_id %in% us_inventor, ]
colnames(panel_us)[2] <- "year"
colnames(panel_us)[3] <- "n"
panel_cn <- semi_final[semi_final$inventor_id %in% cn_inventor, ]
colnames(panel_cn)[3] <- "n"
number_us <- aggregate(n ~ year, data = panel_us, FUN = sum)
number_cn <- aggregate(n ~ year, data = panel_cn, FUN = sum)

number_us$cn <- number_cn$n

ggplot(number_us, aes(x = year))+ 
  geom_line(aes(y = n), colour = "blue", size = 1)+
  geom_line(aes(y = cn), colour = "red", size = 1)+
  xlab("")+
  ggtitle("The number of patent US and China") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 12, color = "black"))

################################
toy1 <- inventor_set[inventor_set$document_number == "20060274932" | inventor_set$document_number =="20060274933" | inventor_set$document_number =="20060140472", c("inventor_id", "year", "document_number")]

toy1 %>% distinct(inventor_id)


k <- (left_join(toy1, toy1, by = c("year", "document_number")))

ex_inventor <- inventor_set[, c("inventor_id", "year", "document_number", "country")]

ex_data <- left_join(ex_inventor, ex_inventor, by = c("year", "document_number"))
ex_data_us <- ex_data[ex_data$country.y == "US", ]
ex_data_cn <- ex_data[ex_data$country.y == "CN", ]

k2 <- ex_data %>% distinct(inventor_id.x, year, inventor_id.y, .keep_all = T)
k2_us <- ex_data_us %>% distinct(inventor_id.x, year, inventor_id.y, .keep_all = T)
k2_cn<- ex_data_cn %>% distinct(inventor_id.x, year, inventor_id.y, .keep_all = T)

net_inventor_cn$number_of_unique_inventor[net_inventor_cn$inventor_id %in% cn_inventor$inventor_id] <- 0

net_inventor_us$number_of_unique_inventor[net_inventor_us$inventor_id %in% us_inventor$inventor_id] <- 0

net_inventor <- k2 %>% count(inventor_id.x, year)
net_inventor_us <- k2_us %>% count(inventor_id.x, year)
net_inventor_cn <- k2_cn %>% count(inventor_id.x, year)

colnames(net_inventor)[3] <- "number_of_unique_inventor"
colnames(net_inventor)[1] <- "inventor_id"

colnames(net_inventor_us)[3] <- "unique_us_coinventor"
colnames(net_inventor_us)[1] <- "inventor_id"

colnames(net_inventor_cn)[3] <- "unique_china_coinventor"
colnames(net_inventor_cn)[1] <- "inventor_id"

semi_final1 <- left_join(semi_final, net_inventor, by = c("inventor_id", "year"))
semi_final1$number_of_unique_inventor <- semi_final1$number_of_unique_inventor-1

semi$year <- as.character(semi$year)

semi <- left_join(semi, net_inventor_cn, by = c("inventor_id", "year"))
semi <- left_join(semi, net_inventor_us, by = c("inventor_id", "year"))




us_year_sum <- aggregate(unique_us_coinventor ~ year, data = semi, FUN = sum)
cn_year_sum <- aggregate(unique_china_coinventor ~ year, data = semi, FUN = sum)

us_year_mean <- aggregate(unique_us_coinventor ~ year, data = semi, FUN = mean)
cn_year_mean <- aggregate(unique_china_coinventor ~ year, data = semi, FUN = mean)

us_year_sum$nation <- "US"
cn_year_sum$nation <- "CN"
colnames(us_year_sum)[2] <- "n"
colnames(cn_year_sum)[2] <- "n"

us_year_mean$nation <- "US"
cn_year_mean$nation <- "CN"
colnames(us_year_mean)[2] <- "mean"
colnames(cn_year_mean)[2] <- "mean"

semi <- semi[, c(1:4,7,8, 5,6,9,10)]
colnames(semi)[4] <- "unique_coinventor"
colnames(semi)[5] <- "unique_cn_coinventor"

check <- semi[!is.na(semi$unique_us_coinventor), ] 
check <- check[check$unique_us_coinventor !=0, ]

uni_num_us <- check %>% count(year)
uni_num_us$nation <- "US"

check_cn <- semi[!is.na(semi$unique_cn_coinventor), ] 
check_cn <- check_cn[check_cn$unique_cn_coinventor !=0, ]

uni_num_cn <- check_cn %>% count(year)
uni_num_cn$nation <- "CN"

total_num <- rbind(uni_num_us, uni_num_cn)


total_sum <- rbind(us_year_sum, cn_year_sum)
total_mean <- rbind(us_year_mean, cn_year_mean)

total_sum$mean <- total_mean$mean
total_sum <- left_join(total_sum, total_num, by = c("nation", "year"))

total_sum$mean <- total_sum$n.x/total_sum$n.y
colnames(total_sum)[2] <- "n"
total_sum <- total_sum[9:nrow(total_sum), c(1:3, 5)]

#The plot of The number of coinventor collaborated with U.S. and China
{ggplot(total_sum, aes(year, n, group = nation, color = nation))+ 
  geom_line() + 
  ylab(label = "N")+ 
  xlab(label = "")+ 
  scale_x_continuous(breaks=seq(min(total_sum$year), max(total_sum$year), 3)) + 
  ggtitle("The number of coinventor collaborated with U.S. and China") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 12, color = "black")) + 
  annotate("rect", xmin=2017, xmax=2020, ymin=0, ymax=125000, alpha=0.2, fill="blue") +
  annotate("text", x=2018.5, y=100000, label="Trump", size=3) +
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed") + 
  annotate("text", x=2020, y=50000, label="Covid19", size=3) +
  theme(legend.title = element_blank())+
  theme(legend.position = c(0.11, 0.9))
}

#The plot of The average of coinventor collaborated with U.S. and China
{ggplot(total_sum, aes(year, mean, group = nation, color = nation))+ 
    geom_line() + 
    ylab(label = "Mean")+ 
    xlab(label = "")+ 
    scale_x_continuous(breaks=seq(min(total_sum$year), max(total_sum$year), 3)) + 
    ggtitle("The average of coinventor collaborated with U.S. and China") + 
    theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 12, color = "black")) + 
    annotate("rect", xmin=2017, xmax=2020, ymin=0, ymax=3, alpha=0.2, fill="blue") +
    annotate("text", x=2018.5, y=1.5, label="Trump", size=3) +
    geom_vline(xintercept = 2020, colour = "red", lty = "dashed") + 
    annotate("text", x=2020, y=3, label="Covid19", size=3) +
    theme(legend.title = element_blank())+
    theme(legend.position = c(0.11, 0.9))
}





coin <- aggregate(coinventor ~ year, data = semi_final_add, FUN = sum)
uni_coin <- aggregate(number_of_unique_inventor ~ year, data = semi_final_add, FUN = sum)

colnames(coin)[2] <- "n"
colnames(uni_coin)[2] <- "n"

coin$nation <- "coinventor"
uni_coin$nation <- "unique coinventor"

colnames(us_year)[2] <- "n"
colnames(cn_year)[2] <- "n"

total1 <- rbind(coin, uni_coin)

ggplot(total1, aes(year, n, group = nation, color = nation))+ 
  geom_line() + 
  ylab(label = "N")+ 
  scale_x_continuous(breaks=seq(min(total$year), max(total$year), 3)) + 
  ggtitle("Trend of coinventor and unique inventor") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 12, color = "black")) + 
  geom_vline(xintercept = 2001, colour = "red", lty = "dashed") + 
  annotate("rect", xmin=2017, xmax=2020, ymin=0, ymax=4000000, alpha=0.2, fill="blue") +
  annotate("text", x=2018.5, y=1000000, label="Trump", size=3) +
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed") + 
  annotate("text", x=2020, y=400000, label="Covid19", size=3) + 
  theme(legend.position = c(0.11, 0.9))+ 
  theme(legend.title = element_blank())



us_inventor <- inventor_set[inventor_set$country == "US", "inventor_id"]
us_inventor$us_flag <- 1
cn_inventor <- inventor_set[inventor_set$country == "CN", "inventor_id"]
cn_inventor$cn_flag <- 1

us_inventor <- us_inventor %>% distinct(inventor_id, .keep_all = T)
cn_inventor <- cn_inventor %>% distinct(inventor_id, .keep_all = T)

semi <- left_join(RA_data_2022_06_10, us_inventor, by = "inventor_id")
semi <- left_join(semi, cn_inventor, by = "inventor_id")

"US and CN"
"the number of chinese inventor with US inventor"
"the number of US inventor with chinese inventor"
{
us_semi <- semi[semi$us_flag == 1, ]
us_semi <- aggregate(unique_cn_coinventor ~ year, data = us_semi, FUN = sum)

cn_semi <- semi[semi$cn_flag == 1, ]
cn_semi <- aggregate(unique_us_coinventor ~ year, data = cn_semi, FUN = sum)

colnames(us_semi)[2] <- "n"
colnames(cn_semi)[2] <- "n"
us_semi$ex <- "the number of chinese inventor with US inventor"
cn_semi$ex <- "the number of US inventor with chinese inventor"

total_semi <- rbind(us_semi, cn_semi)
total_semi <- total_semi[c(1:21, 24:nrow(total_semi)), ]

us <- semi[!is.na(semi$us_flag), ]
us <- us[us$`number of patent`> 0, ]
num_us <- us %>% count(year)
num_us$ex<- "the number of chinese inventor with US inventor"

cn <- semi[!is.na(semi$cn_flag), ]
cn <- cn[cn$`number of patent`> 0, ]
num_cn <- us %>% count(year)
num_cn$ex<- "the number of US inventor with chinese inventor"

num_us_cn <- rbind(num_us, num_cn)
total_semi <- left_join(total_semi, num_us_cn , by = c("ex", "year"))
total_semi$mean <- total_semi$n.x/total_semi$n.y

total_semi <- total_semi[, c(1:3, 5)]
colnames(total_semi)[2] <- "n"

ggplot(total_semi, aes(year, n, group = ex, color = ex))+ 
  geom_line() + 
  ylab(label = "N")+ 
  xlab(label = "")+ 
  scale_x_continuous(breaks=seq(min(total_semi$year), max(total_semi$year), 3)) + 
  ggtitle("Trend of us coinventor and chinese coinventor") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 14, color = "black")) + 
  geom_vline(xintercept = 2001, colour = "red", lty = "dashed") + 
  annotate("rect", xmin=2017, xmax=2020, ymin=0, ymax=10000, alpha=0.2, fill="blue") +
  annotate("text", x=2018.5, y=5000, label="Trump", size=3) +
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed") + 
  annotate("text", x=2020, y=10000, label="Covid19", size=3)+
  theme(legend.position = "bottom") + 
  theme(legend.title = element_blank()) + 
  theme(legend.text = element_text(color = "black", size = 9, face = "italic"))

ggplot(total_semi, aes(year, mean, group = ex, color = ex))+ 
  geom_line() + 
  ylab(label = "Average")+ 
  xlab(label = "")+ 
  scale_x_continuous(breaks=seq(min(total_semi$year), max(total_semi$year), 3)) + 
  ggtitle("Trend of us coinventor and chinese coinventor") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 14, color = "black")) + 
  geom_vline(xintercept = 2001, colour = "red", lty = "dashed") + 
  annotate("rect", xmin=2017, xmax=2020, ymin=0, ymax=0.05, alpha=0.2, fill="blue") +
  annotate("text", x=2018.5, y=0.01, label="Trump", size=3) +
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed") + 
  annotate("text", x=2020, y=0.05, label="Covid19", size=3)+
  theme(legend.position = "bottom") + 
  theme(legend.title = element_blank()) + 
  theme(legend.text = element_text(color = "black", size = 9, face = "italic"))
}

"Japan"
"the number of chinese inventor with US inventor"
"the number of US inventor with chinese inventor"
{

jp_semi <- semi[!is.na(semi$jp_flag), ]
  
jp_semi_us <- aggregate(unique_us_coinventor ~ year, data = jp_semi, FUN = sum)
jp_semi_cn <- aggregate(unique_cn_coinventor ~ year, data = jp_semi, FUN = sum)

colnames(jp_semi_us)[2] <- "n"
colnames(jp_semi_cn)[2] <- "n"
jp_semi_us$ex <- "the number of US inventor with jp inventor"
jp_semi_cn$ex <- "the number of CN inventor with jp inventor"
total_semi_jp <- rbind(jp_semi_cn, jp_semi_us)

total_semi_jp <- total_semi_jp[c(1:21, 24:nrow(total_semi_jp)), ]

jp <- semi[!is.na(semi$jp_flag), ]
jp_us <- jp[!is.na(jp$unique_us_coinventor), ]
jp_us <- jp[jp_us$unique_us_coinventor > 0, ]

jp_cn <- jp[!is.na(jp$unique_cn_coinventor), ]
jp_cn <- jp_cn[jp_cn$unique_cn_coinventor > 0, ]

num_jp_us <- jp_us %>% count(year)
num_jp_cn <- jp_cn %>% count(year)


num_jp_us$ex <- "the number of US inventor with jp inventor"
num_jp_cn$ex <- "the number of CN inventor with jp inventor"

num_jp_cn_us <- rbind(num_jp_us, num_jp_cn)
total_semi_jp <- left_join(total_semi_jp, num_jp_cn_us , by = c("ex", "year"))
total_semi_jp$mean <- total_semi_jp$n.x/total_semi_jp$n.y

total_semi_jp <- total_semi_jp[, c(1:3, 5)]
colnames(total_semi_jp)[2] <- "n"

ggplot(total_semi_jp, aes(year, n, group = ex, color = ex))+ 
  geom_line() + 
  ylab(label = "N")+ 
  xlab(label = "")+ 
  scale_x_continuous(breaks=seq(min(total_semi_jp$year), max(total_semi_jp$year), 3)) + 
  ggtitle("Trend of Japan us coinventor and chinese coinventor") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 14, color = "black")) + 
  annotate("rect", xmin=2017, xmax=2020, ymin=0, ymax=10000, alpha=0.2, fill="blue") +
  annotate("text", x=2018.5, y=5000, label="Trump", size=3) +
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed") + 
  annotate("text", x=2020, y=10000, label="Covid19", size=3)+
  theme(legend.position = "bottom") + 
  theme(legend.title = element_blank()) + 
  theme(legend.text = element_text(color = "black", size = 9, face = "italic"))

ggplot(total_semi_jp, aes(year, mean, group = ex, color = ex))+ 
  geom_line() + 
  ylab(label = "Average")+ 
  xlab(label = "")+ 
  scale_x_continuous(breaks=seq(min(total_semi_jp$year), max(total_semi_jp$year), 3)) + 
  ggtitle("Trend of Japan inventor with us coinventor and chinese coinventor") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 14, color = "black")) + 
  annotate("rect", xmin=2017, xmax=2020, ymin=0, ymax=2.6, alpha=0.2, fill="blue") +
  annotate("text", x=2018.5, y=1, label="Trump", size=3) +
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed") + 
  annotate("text", x=2020, y=2, label="Covid19", size=3)+
  theme(legend.position = "bottom") + 
  theme(legend.title = element_blank()) + 
  theme(legend.text = element_text(color = "black", size = 9, face = "italic"))
}

"German"
{
de_semi <- semi[!is.na(semi$de_flag), ]
de_semi_us <- aggregate(unique_us_coinventor ~ year, data = de_semi, FUN = sum)
de_semi_cn <- aggregate(unique_cn_coinventor ~ year, data = de_semi, FUN = sum)

colnames(de_semi_us)[2] <- "n"
colnames(de_semi_cn)[2] <- "n"
de_semi_us$ex <- "the number of CN inventor with DE inventor"
de_semi_cn$ex <- "the number of US inventor with DE inventor"

total_semi_de <- rbind(de_semi_us, de_semi_cn)
total_semi_de <- total_semi_de[c(4:nrow(total_semi_de)), ]

de <- semi[!is.na(semi$de_flag), ]
de_us <- de[!is.na(de$unique_us_coinventor), ]
de_us <- de_us[de_us$unique_us_coinventor > 0, ]

de_cn <- de[!is.na(de$unique_cn_coinventor), ]
de_cn <- de_cn[de_cn$unique_cn_coinventor > 0, ]

num_de_us <- de_us %>% count(year)
num_de_cn <- de_cn %>% count(year)


num_de_us$ex <- "the number of US inventor with DE inventor"
num_de_cn$ex <- "the number of CN inventor with DE inventor"

num_de_cn_us <- rbind(num_de_us, num_de_cn)
total_semi_de <- left_join(total_semi_de, num_de_cn_us , by = c("ex", "year"))
total_semi_de$mean <- total_semi_de$n.x/total_semi_de$n.y

total_semi_de <- total_semi_de[, c(1:3, 5)]
colnames(total_semi_de)[2] <- "n"

ggplot(total_semi_de, aes(year, n, group = ex, color = ex))+ 
  geom_line() + 
  ylab(label = "N")+ 
  xlab(label = "")+ 
  scale_x_continuous(breaks=seq(min(total_semi_de$year), max(total_semi_de$year), 3)) + 
  ggtitle("Trend of DE inventor us coinventor and chinese coinventor") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 14, color = "black")) + 
  geom_vline(xintercept = 2001, colour = "red", lty = "dashed") + 
  annotate("rect", xmin=2017, xmax=2020, ymin=0, ymax=10000, alpha=0.2, fill="blue") +
  annotate("text", x=2018.5, y=5000, label="Trump", size=3) +
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed") + 
  annotate("text", x=2020, y=10000, label="Covid19", size=3)+
  theme(legend.position = "bottom") + 
  theme(legend.title = element_blank()) + 
  theme(legend.text = element_text(color = "black", size = 9, face = "italic"))
}


ggplot(total_semi_de, aes(year, mean, group = ex, color = ex))+ 
  geom_line() + 
  ylab(label = "Average")+ 
  xlab(label = "")+ 
  scale_x_continuous(breaks=seq(min(total_semi_de$year), max(total_semi_de$year), 3)) + 
  ggtitle("Trend of DE inventor us coinventor and chinese coinventor") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 14, color = "black")) + 
  geom_vline(xintercept = 2001, colour = "red", lty = "dashed") + 
  annotate("rect", xmin=2017, xmax=2020, ymin=0, ymax=60, alpha=0.2, fill="blue") +
  annotate("text", x=2018.5, y=30, label="Trump", size=3) +
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed") + 
  annotate("text", x=2020, y=50, label="Covid19", size=3)+
  theme(legend.position = "bottom") + 
  theme(legend.title = element_blank()) + 
  theme(legend.text = element_text(color = "black", size = 9, face = "italic"))

total_semi_de_us <- total_semi_de[total_semi_de$ex == "the number of US inventor with DE inventor", ]

ggplot(total_semi_de_us, aes(year, mean))+ 
  geom_line() + 
  ylab(label = "Average")+ 
  xlab(label = "")+ 
  scale_x_continuous(breaks=seq(min(total_semi_de_us$year), max(total_semi_de_us$year), 3)) + 
  ggtitle("Trend of DE inventor with us coinventor") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 14, color = "black")) + 
  annotate("rect", xmin=2017, xmax=2020, ymin=0, ymax=0.5, alpha=0.2, fill="blue") +
  annotate("text", x=2018.5, y=0.2, label="Trump", size=3) +
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed") + 
  annotate("text", x=2020, y=0.5, label="Covid19", size=3)+
  theme(legend.position = "bottom") + 
  theme(legend.title = element_blank()) + 
  theme(legend.text = element_text(color = "black", size = 9, face = "italic"))


write.csv(semi, file = "original_RA.csv")
read.csv("original_RA.csv")

semi <- original_RA

semi$coinventor <- NULL
semi$us_coinventor <- NULL
semi$cn_coinventor <- NULL

jp_inventor <- inventor_set[inventor_set$country == "JP", "inventor_id"]
de_inventor <- inventor_set[inventor_set$country == "DE", "inventor_id"]

jp_inventor$jp_flag <- 1
de_inventor$de_flag <- 1
jp_inventor <- jp_inventor %>% distinct(inventor_id, .keep_all = T)
de_inventor <- de_inventor %>% distinct(inventor_id, .keep_all = T)

semi <- left_join(semi, jp_inventor, by = "inventor_id")
semi <- left_join(semi, de_inventor, by = "inventor_id")

##06/20
write_csv(semi, file = "semi_data_with_unique_num_and_nation_flag.csv")

semi_unique <- semi[, c(1:6)]

semi_unique$us_proportion <- semi_unique$unique_us_coinventor/semi_unique$unique_coinventor
semi_unique$cn_proportion <- semi_unique$unique_cn_coinventor/semi_unique$unique_coinventor

mean_us_proportion <- aggregate(data = semi_unique, us_proportion ~ year, FUN = mean)
mean_cn_proportion <- aggregate(data = semi_unique, cn_proportion ~ year, FUN = mean)

mean_us_proportion$nation <- "US"
mean_cn_proportion$nation <- "CN"

mean_us_proportion <- mean_us_proportion[8:28, ]

colnames(mean_us_proportion)[2] <- "proportion"
colnames(mean_cn_proportion)[2] <- "proportion"

mean_proportion <- rbind(mean_us_proportion, mean_cn_proportion)

ggplot(mean_proportion, aes(year, proportion, group = nation, color = nation))+ 
  geom_line() + 
  ylab(label = "Mean proportion")+ 
  xlab(label = "")+ 
  scale_x_continuous(breaks=seq(min(mean_proportion$year), max(mean_proportion$year), 3)) + 
  ggtitle("Trend of collaboration proportion(All nations)") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 14, color = "black")) + 
  geom_vline(xintercept = 2001, colour = "red", lty = "dashed") + 
  annotate("rect", xmin=2017, xmax=2020, ymin=0, ymax=0.1, alpha=0.2, fill="blue") +
  annotate("text", x=2018.5, y=0.06, label="Trump", size=3) +
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed") + 
  annotate("text", x=2020, y=0.075, label="Covid19", size=3)+
  theme(legend.position = "bottom") + 
  theme(legend.title = element_blank()) + 
  theme(legend.text = element_text(color = "black", size = 9, face = "italic"))

semi <- cbind(semi, semi_unique[, 7:8])

semi_us <- semi[!is.na(semi$us_flag), ]
semi_us <- semi_us[semi_us$us_flag == 1, ]

semi_cn <- semi[!is.na(semi$cn_flag), ]
semi_cn <- semi_cn[semi_cn$cn_flag == 1, ]

mean_us_proportion_cn <- aggregate(data = semi_cn, us_proportion ~ year, FUN = mean)
mean_cn_proportion_us <- aggregate(data = semi_us, cn_proportion ~ year, FUN = mean)

mean_us_proportion_cn$nation <- "us proportion in chinese inventor"
mean_cn_proportion_us$nation <- "cn proportion in us inventor"

mean_us_proportion <- mean_us_proportion[8:28, ]

colnames(mean_us_proportion_cn)[2] <- "proportion"
colnames(mean_cn_proportion_us)[2] <- "proportion"

mean_proportion_cn_us <- rbind(mean_us_proportion_cn, mean_cn_proportion_us)

ggplot(mean_proportion_cn_us, aes(year, proportion, group = nation, color = nation))+ 
  geom_line() + 
  ylab(label = "Mean proportion")+ 
  xlab(label = "")+ 
  scale_x_continuous(breaks=seq(min(mean_proportion$year), max(mean_proportion$year), 3)) + 
  ggtitle("Trend of collaboration proportion") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 14, color = "black")) + 
  geom_vline(xintercept = 2001, colour = "red", lty = "dashed") + 
  annotate("rect", xmin=2017, xmax=2020, ymin=0, ymax=0.2, alpha=0.2, fill="blue") +
  annotate("text", x=2018.5, y=0.06, label="Trump", size=3) +
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed") + 
  annotate("text", x=2020, y=0.075, label="Covid19", size=3)+
  theme(legend.position = "bottom") + 
  theme(legend.title = element_blank()) + 
  theme(legend.text = element_text(color = "black", size = 9, face = "italic"))

us_patent <- unique_inventor_set[unique_inventor_set$country == "US", ]
cn_patent <- unique_inventor_set[unique_inventor_set$country == "CN", ]

## # of co-patent us only and cn only
us_patent_only <- us_patent[us_patent$us_proportion == 1, ]
cn_patent_only <- cn_patent[cn_patent$cn_proportion == 1, ]

us_num <- us_patent_only %>% count(inventor_id, year) 
cn_num <- cn_patent_only %>% count(inventor_id, year)

colnames(us_num)[2] <- "year"
colnames(cn_num)[2] <- "year"

colnames(us_num)[3] <- "only_us"
colnames(cn_num)[3] <- "only_cn"

## # of co-patent us and cn
us_patent_cn<- us_patent[us_patent$cn_proportion > 0 & us_patent$cn_proportion < 1, ]
cn_patent_us<- cn_patent[cn_patent$us_proportion > 0 & cn_patent$us_proportion < 1, ]

us_num_with_cn <- us_patent_cn %>% count(inventor_id, year)
cn_num_with_us <- cn_patent_us %>% count(inventor_id, year)

colnames(us_num_with_cn)[2] <- "year"
colnames(cn_num_with_us)[2] <- "year"

colnames(us_num_with_cn)[3] <- "with_cn"
colnames(cn_num_with_us)[3] <- "with_us"

## # of co-patent non us and non cn
us_patent_int<- us_patent[us_patent$us_proportion != 1 & us_patent$cn_proportion == 0 , ]
cn_patent_int<- cn_patent[cn_patent$cn_proportion != 1 & cn_patent$us_proportion == 0, ]

us_num_with_int <- us_patent_int %>% count(inventor_id, year)
cn_num_with_int <- cn_patent_int %>% count(inventor_id, year)

colnames(us_num_with_int)[2] <- "year"
colnames(cn_num_with_int)[2] <- "year"

colnames(us_num_with_int)[3] <- "int_cn"
colnames(cn_num_with_int)[3] <- "int_us"

semi_core <- semi[, c(1:3)]
semi_core$year <- as.character(semi_core$year)

semi_num <- left_join(semi_core, us_num, by = c("inventor_id", "year"))
semi_num <- left_join(semi_num, cn_num, by = c("inventor_id", "year"))

semi_num <- left_join(semi_num, us_num_with_cn, by = c("inventor_id", "year"))
semi_num <- left_join(semi_num, cn_num_with_us, by = c("inventor_id", "year"))

semi_num <- left_join(semi_num, us_num_with_int, by = c("inventor_id", "year"))
semi_num <- left_join(semi_num, cn_num_with_int, by = c("inventor_id", "year"))

colnames(semi_num)[8] <- "int_cn"
semi_num$sum <- semi_num$only_us + semi_num$only_cn + semi_num$with_cn + semi_num$with_us + semi_num$n + semi_num$int_us

check <- semi_num[semi_num$`number of patent` != 0, ]
sum(check$`number of patent` == check$sum)
write_csv(semi_num, file = "semi_patent_num.csv")

####coinv_non_us, #coinv_cn
coinv_non_us <- ex_data[ex_data$country.x == "US", ]
coinv_non_us <- coinv_non_us[coinv_non_us$country.y != "US", ]

coinv_non_us <- coinv_non_us %>% distinct(inventor_id.x, year, inventor_id.y, .keep_all = T)

net_inventor_non <- coinv_non_us %>% count(inventor_id.x, year)

colnames(net_inventor_non)[3] <- "coinventor_non_us"
colnames(net_inventor_non)[1] <- "inventor_id"

coinv_non_cn <- ex_data[ex_data$country.x == "US", ]
coinv_non_cn <- coinv_non_cn[coinv_non_cn$country.y != "CN", ]

coinv_non_cn <- coinv_non_cn %>% distinct(inventor_id.x, year, inventor_id.y, .keep_all = T)
coinv_non_cn <- coinv_non_cn[coinv_non_cn$country.y != "US", ]


net_inventor_non_cn <- coinv_non_cn %>% count(inventor_id.x, year)

colnames(net_inventor_non_cn)[3] <- "coinventor_non_cn"
colnames(net_inventor_non_cn)[1] <- "inventor_id"

net_inventor_non_cn$coinventor_non_cn[net_inventor_non_cn$inventor_id %in% us_inventor$inventor_id] <- 0



semi_us_2005 <- semi_us[semi_us$year >= 2005, ]

semi_us_2005 <- semi_us_2005[, c(1:12)]
semi_us_2005_07_05$year <- as.character(semi_us_2005_07_05$year)

semi_us_2005_07_05 <- left_join(semi_us_2005_07_05, net_inventor_non_cn, by = c("inventor_id", "year"))

write_csv(semi, file = "semi_06_28.csv")
write_csv(semi_us, file = "semi_us_06_28.csv")
write_csv(semi_us_2005, file = "semi_us_2005_07-05.csv")



semi_us$cn_ratio <- semi_us$unique_cn_coinventor/semi_us$coinventor_non_us
semi_us <- semi_us[semi_us$cn_ratio < 1, ]

mill <- data.frame(c("B63G","B64D", "C06B", "C06C", "C06D", "F41A", "F41C", "F41F", "F41G", "F41H", "F42B", "F42C"))
colnames(mill)[1] <- "mill_cpc"
mill$mill_flag <- 1

phar <- data.frame(c("A61K", "A61P"))
colnames(phar)[1] <- "phar_cpc"
phar$phar_flag <- 1

medi <- data.frame(c("A61B",
                     "A61C",
                     "A61D",
                     "A61F",
                     "A61G",
                     "A61H",
                     "A61J",
                     "A61L",
                     "A61M",
                     "A61N",
                     "A01K",
                     "A01N"))
colnames(medi)[1] <- "medi_cpc"
medi$medi_flag <- 1

inventor_cpc <- left_join(inventor_set[, c(1,3,7)], cpc_set, by = c("document_number" = "patent_id"))

target_cpc <- inventor_cpc %>% distinct(inventor_id, year, group_id)

target_cpc <- left_join(target_cpc, mill, by = c("group_id" = "mill_cpc"))
target_cpc <- left_join(target_cpc, phar, by = c("group_id" = "phar_cpc"))
target_cpc <- left_join(target_cpc, medi, by = c("group_id" = "medi_cpc"))

mill_num <- target_cpc[!is.na(target_cpc$mill_flag), ] %>% count(inventor_id, year, mill_flag)
phar_num <- target_cpc[!is.na(target_cpc$phar_flag), ] %>% count(inventor_id, year, phar_flag)
medi_num <- target_cpc[!is.na(target_cpc$medi_flag), ] %>% count(inventor_id, year, medi_flag)

colnames(mill_num)[4] <- "mill_num"
colnames(phar_num)[4] <- "phar_num"
colnames(medi_num)[4] <- "medi_num"

mill_num$mill_flag <- NULL
phar_num$phar_flag <- NULL
medi_num$medi_flag <- NULL

semi_num[is.na(semi_num)] <- 0

semi_num <- left_join(semi_num, mill_num, by = c("inventor_id", "year"))
semi_num <- left_join(semi_num, phar_num, by = c("inventor_id", "year"))
semi_num <- left_join(semi_num, medi_num, by = c("inventor_id", "year"))
write_csv(semi_num, file = "semi_num_cpc_flag.csv")

cn_ratio <- aggregate(cn_ratio ~ year, data = semi_us, FUN = mean)

str(cn_ratio)
cn_ratio$year <- as.numeric(cn_ratio$year)

ggplot(cn_ratio, aes(year, cn_ratio))+ 
  geom_line() + 
  ylab(label = "Mean proportion")+ 
  xlab(label = "")+ 
  scale_x_continuous(breaks=seq(min(mean_proportion$year), max(mean_proportion$year), 3)) + 
  ggtitle("CN ratio of US inventors") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 14, color = "black")) + 
  geom_vline(xintercept = 2001, colour = "red", lty = "dashed") + 
  annotate("rect", xmin=2017, xmax=2020, ymin=0, ymax=0.45, alpha=0.2, fill="blue") +
  annotate("text", x=2018.5, y=0.2, label="Trump", size=3) +
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed") + 
  annotate("text", x=2020, y=0.075, label="Covid19", size=3)+
  theme(legend.position = "bottom") + 
  theme(legend.title = element_blank()) + 
  theme(legend.text = element_text(color = "black", size = 9, face = "italic"))

semi_06_28 <- semi_06_28[!is.na(semi_06_28$unique_coinventor), ]

small <- semi_06_28[semi_06_28$unique_coinventor > 0 & semi_06_28$unique_coinventor < 4, ]
middle <- semi_06_28[semi_06_28$unique_coinventor > 3 & semi_06_28$unique_coinventor < 7, ]
large <- semi_06_28[semi_06_28$unique_coinventor > 6, ]

small_mean <- aggregate(us_proportion ~ year , data = small, FUN = mean)
middle_mean <- aggregate(us_proportion ~ year , data = middle, FUN = mean)
large_mean <- aggregate(us_proportion ~ year , data = large, FUN = mean)

small_mean$size <- "small(1 to 3)"
middle_mean$size <- "middle(3 to 6)"
large_mean$size <- "large(over 6)"

View(small_mean)

large_mean <- rbind(large_mean, middle_mean)
large_mean <- rbind(large_mean, small_mean)

ggplot(large_mean, aes(year, us_proportion, group = size, color = size))+ 
  geom_line() + 
  ylab(label = "Mean proportion")+ 
  xlab(label = "")+ 
  scale_x_continuous(breaks=seq(min(large_mean$year), max(large_mean$year), 3)) + 
  ggtitle("US proportion by coinventor size") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 14, color = "black")) + 
  geom_vline(xintercept = 2001, colour = "red", lty = "dashed") + 
  annotate("rect", xmin=2017, xmax=2020, ymin=0, ymax=0.2, alpha=0.2, fill="blue") +
  annotate("text", x=2018.5, y=0.025, label="Trump", size=3) +
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed") + 
  annotate("text", x=2020, y=0.075, label="Covid19", size=3)+
  theme(legend.position = "bottom") + 
  theme(legend.title = element_blank()) + 
  theme(legend.text = element_text(color = "black", size = 9, face = "italic"))

small_mean_cn <- aggregate(cn_proportion ~ year , data = small, FUN = mean)
middle_mean_cn <- aggregate(cn_proportion ~ year , data = middle, FUN = mean)
large_mean_cn <- aggregate(cn_proportion ~ year , data = large, FUN = mean)

small_mean_cn$size <- "small(1 t0 3)"
middle_mean_cn$size <- "middle(3 to 6)"
large_mean_cn$size <- "large(over 6)"


large_mean_cn <- rbind(large_mean_cn, middle_mean_cn)
large_mean_cn <- rbind(large_mean_cn, small_mean_cn)

ggplot(large_mean_cn, aes(year, cn_proportion, group = size, color = size))+ 
  geom_line() + 
  ylab(label = "Mean proportion")+ 
  xlab(label = "")+ 
  scale_x_continuous(breaks=seq(min(large_mean$year), max(large_mean$year), 3)) + 
  ggtitle("CN proportion by coinventor size") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 14, color = "black")) + 
  geom_vline(xintercept = 2001, colour = "red", lty = "dashed") + 
  annotate("rect", xmin=2017, xmax=2020, ymin=0, ymax=0.2, alpha=0.2, fill="blue") +
  annotate("text", x=2018.5, y=0.025, label="Trump", size=3) +
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed") + 
  annotate("text", x=2020, y=0.075, label="Covid19", size=3)+
  theme(legend.position = "bottom") + 
  theme(legend.title = element_blank()) + 
  theme(legend.text = element_text(color = "black", size = 9, face = "italic"))


for (i in 1991:2020){
  #t기 trade 데이터를 추출하고
  data <- semi_us_06_28[semi_us_06_28$year == i, ]
  #t+2기 trade 데이터를 추출합니다.
  next_data <- semi_us_06_28[semi_us_06_28$year == i+1, ]
  #두개의 데이터프레임을 origin, hs92로 병합합니다.
  data <- merge(data, next_data, by = c("inventor_id"))
  
  if(i == 1991){
    r <- data
  } else{
    r <- rbind(r, data)
  }
  print(i)
}

r <- r[, c(1,2,4,6,8)]
colnames(r)[2] <- "t"
colnames(r)[4] <- "t+1"
colnames(r)[3] <- "t_cn"
colnames(r)[5] <- "t+1_cn"

r <- r[is.na(r$t_cn) | r$t_cn == 0, ]
r <- r[!is.na(r$`t+1_cn`), ]
r <- r[r$`t+1_cn`>=1, ]

jump_cn <- r %>% count(`t+1`)

colnames(jump_cn) <- c("year", "#")

ggplot(jump_cn, aes(year, `#`))+ 
  geom_line() + 
  ylab(label = "#")+ 
  xlab(label = "")+ 
  scale_x_continuous(breaks=seq(min(jump_cn$year), max(jump_cn$year), 2)) + 
  ggtitle("# us inventors") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 14, color = "black")) + 
  geom_vline(xintercept = 2001, colour = "red", lty = "dashed") + 
  annotate("rect", xmin=2017, xmax=2020, ymin=0, ymax=3100, alpha=0.2, fill="blue") +
  annotate("text", x=2018.5, y=1500, label="Trump", size=3) +
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed") + 
  annotate("text", x=2020, y=2000, label="Covid19", size=3)+
  theme(legend.position = "bottom") + 
  theme(legend.title = element_blank()) + 
  theme(legend.text = element_text(color = "black", size = 9, face = "italic"))


###0705
semi_us_2005_07_05 <- semi_us_2005_07_05[semi_us_2005_07_05$`number of patent`>0, ]
semi_us_2005_07_05$unique_cn_coinventor[is.na(semi_us_2005_07_05$unique_cn_coinventor)] <- 0

semi_us_2005_07_05$treatment <- ifelse(semi_us_2005_07_05$unique_cn_coinventor > 0, 1, 0)

us_treatment <- semi_us_2005_07_05[semi_us_2005_07_05$treatment == 1, ]
us_control <- semi_us_2005_07_05[semi_us_2005_07_05$treatment == 0, ]

###plot1 : #coinventor
treat_coinv <- aggregate(data = us_treatment, unique_coinventor ~ year, FUN = mean)
control_coinv <- aggregate(data = us_control, unique_coinventor ~ year, FUN = mean)

treat_coinv$treatment <- "Treatment"
control_coinv$treatment <- "Control"

coinv <- rbind(treat_coinv, control_coinv)

coinv$year <- as.numeric(coinv$year)


ggplot(coinv, aes(year, unique_coinventor, group = treatment, color = treatment))+ 
  geom_line() + 
  ylab(label = "Average")+ 
  xlab(label = "")+ 
  scale_x_continuous(breaks=seq(min(coinv$year), max(coinv$year), 3)) + 
  ggtitle("Average unique coinventor of US, group by year") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 14, color = "black")) + 
  annotate("rect", xmin=2017, xmax=2020, ymin=0, ymax=13, alpha=0.2, fill="blue") +
  annotate("text", x=2018.5, y=8, label="Trump", size=3) +
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed") + 
  annotate("text", x=2020, y=5, label="Covid19", size=3)+
  theme(legend.position = c(0.1, 0.9))+  
  theme(legend.title = element_blank()) + 
  theme(legend.text = element_text(color = "black", size = 9, face = "italic"))


cn_proportion_T <- aggregate(data = us_treatment, cn_proportion ~ year, FUN = mean)
cn_proportion_C <- aggregate(data = us_control, cn_proportion ~ year, FUN = mean)

cn_proportion_T$treatment <- "Treatment"
cn_proportion_C$treatment <- "Control"

cn_proportion <- rbind(cn_proportion_T, cn_proportion_C)

cn_proportion$year <- as.numeric(cn_proportion$year)

ggplot(cn_proportion, aes(year, cn_proportion, group = treatment, color = treatment))+ 
  geom_line() + 
  ylab(label = "Average")+ 
  xlab(label = "")+ 
  scale_x_continuous(breaks=seq(min(cn_proportion$year), max(cn_proportion$year), 3)) + 
  ggtitle("Average cn proportion group by year") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 14, color = "black")) + 
  annotate("rect", xmin=2017, xmax=2020, ymin=0, ymax=0.5, alpha=0.2, fill="blue") +
  annotate("text", x=2018.5, y=0.25, label="Trump", size=3) +
  annotate("text", x=2020, y=0.4, label="Covid19", size=3)+
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed") + 
  theme(legend.position = c(0.1, 0.9))+  
  theme(legend.title = element_blank()) + 
  theme(legend.text = element_text(color = "black", size = 9, face = "italic"))

us_treatment <- left_join(us_treatment, net_inventor_us, by = c("inventor_id", "year"))
us_treatment$unique_us_coinventor <- us_treatment$unique_us_coinventor-1
us_control <- left_join(us_control, net_inventor_us, by = c("inventor_id", "year"))
us_control$unique_us_coinventor <- us_control$unique_us_coinventor-1

us_treatment$us_proportion <- us_treatment$unique_us_coinventor/us_treatment$unique_coinventor 
us_control$us_proportion <- us_control$unique_us_coinventor/us_control$unique_coinventor 

us_proportion_T <- aggregate(data = us_treatment, us_proportion ~ year, FUN = mean)
us_proportion_C <- aggregate(data = us_control, us_proportion ~ year, FUN = mean)

us_proportion_T$treatment <- "Treatment"
us_proportion_C$treatment <- "Control"

us_proportion <- rbind(us_proportion_T, us_proportion_C)

us_proportion$year <- as.numeric(us_proportion$year)

ggp
lot(us_proportion, aes(year, us_proportion, group = treatment, color = treatment))+ 
  geom_line() + 
  ylab(label = "Average")+ 
  xlab(label = "")+ 
  scale_x_continuous(breaks=seq(min(us_proportion$year), max(us_proportion$year), 3)) + 
  ggtitle("Average us proportion group by year") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 14, color = "black")) + 
  annotate("rect", xmin=2017, xmax=2020, ymin=0, ymax=1, alpha=0.2, fill="blue") +
  annotate("text", x=2018.5, y=0.5, label="Trump", size=3) +
  annotate("text", x=2020, y=0.8, label="Covid19", size=3)+
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed") + 
  theme(legend.position = c(0.1, 0.14))+  
  theme(legend.title = element_blank()) + 
  theme(legend.text = element_text(color = "black", size = 9, face = "italic"))

us_treatment <- left_join(us_treatment, net_inventor_non_cn, by = c("inventor_id", "year"))
us_treatment$coinventor_non_cn <- us_treatment$coinventor_non_cn -1 
us_control <- left_join(us_control, net_inventor_non_cn, by = c("inventor_id", "year"))
us_control$coinventor_non_cn <- us_control$coinventor_non_cn -1 

us_treatment$non_cn_proportion <- us_treatment$coinventor_non_cn/us_treatment$unique_coinventor
us_control$non_cn_proportion <- us_control$coinventor_non_cn/us_control$unique_coinventor

non_cn_proportion_T <- aggregate(data = us_treatment, non_cn_proportion ~ year, FUN = mean)
non_cn_proportion_C <- aggregate(data = us_control, non_cn_proportion ~ year, FUN = mean)

non_cn_proportion_T$treatment <- "Treatment"
non_cn_proportion_C$treatment <- "Control"

non_cn_proportion <- rbind(non_cn_proportion_T, non_cn_proportion_C)

non_cn_proportion$year <- as.numeric(non_cn_proportion$year)

ggplot(non_cn_proportion, aes(year, non_cn_proportion, group = treatment, color = treatment))+ 
  geom_line() + 
  ylab(label = "Average")+ 
  xlab(label = "")+ 
  scale_x_continuous(breaks=seq(min(non_cn_proportion$year), max(non_cn_proportion$year), 3)) + 
  ggtitle("Average non_cn proportion group by year") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 14, color = "black")) + 
  annotate("rect", xmin=2017, xmax=2020, ymin=0, ymax=0.25, alpha=0.2, fill="blue") +
  annotate("text", x=2018.5, y=0.1, label="Trump", size=3) +
  annotate("text", x=2020, y=0.15, label="Covid19", size=3)+
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed") + 
  theme(legend.position = c(0.1, 0.14))+  
  theme(legend.title = element_blank()) + 
  theme(legend.text = element_text(color = "black", size = 9, face = "italic"))


semi_num_cpc_flag <- read.delim("~/semi_num_cpc_flag", colClasses = "character")
semi_num_cpc_flag <- semi_num_cpc_flag[semi_num_cpc_flag$year >= 2005, ]

semi_num_us <-left_join(semi_num_cpc_flag, semi_06_28, by = c("inventor_id", "year"))
semi_num_us <- semi_num_us[semi_num_us$us_flag == 1, ]
semi_num_us <- na.omit(semi_num_us)


semi_num_us_T <- semi_num_us[semi_num_us$with_cn >0, ]
semi_num_us_C <- semi_num_us[semi_num_us$with_cn == 0, ]

treatment_sum<- aggregate(data = semi_num_us_T, number.of.patent ~ year, FUN=sum)
control_sum <- aggregate(data = semi_num_us_C, number.of.patent ~ year, FUN=sum)


treatment_sum$treatment <- "Treatment"
control_sum$treatment <- "Control"

patent_sum <- cbind(treatment_sum, control_sum)

colnames(patent_sum)[2] <- "the_num1"

patent_sum$treatment <- NULL
patent_sum$year <- NULL


ggplot(patent_sum, aes(x =year)) +
geom_line(aes(y = the_num1, colour = "Treatment")) + 
geom_line(aes(y = number.of.patent/10, colour = "Control"))+
scale_y_continuous(sec.axis = sec_axis(~.*10, name = "Control"))+
scale_colour_manual(values = c("blue", "red"))+
labs(y = "Treatment", x = "")+
theme(legend.position = c(0.8, 0.9))+
ggtitle("The sum of patent group by year")+
theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 14, color = "black"))+
annotate("rect", xmin=2017, xmax=2020, ymin=0, ymax=60000, alpha=0.2, fill="blue")+
annotate("text", x=2018.5, y=30000, label="Trump", size=3)+
annotate("text", x=2020, y=10000, label="Covid19", size=3)+
geom_vline(xintercept = 2020, colour = "red", lty = "dashed")+
theme(legend.position = c(0.1, 0.14))+
theme(legend.title = element_blank())+
theme(legend.text = element_text(color = "black", size = 9, face = "italic"))

treatment_only_us<- aggregate(data = semi_num_us_T, only_us ~ year, FUN=sum)
control_only_us <- aggregate(data = semi_num_us_C, only_us ~ year, FUN=sum)

patent_only <- cbind(treatment_only_us, control_only_us[, 2])

colnames(patent_only)[2] <- "Treat"
colnames(patent_only)[3] <- "Control"

ggplot(patent_only, aes(x =year)) +
  geom_line(aes(y = Treat, colour = "Treatment")) + 
  geom_line(aes(y = Control/10, colour = "Control"))+
  scale_y_continuous(sec.axis = sec_axis(~.*10, name = "Control"))+
  scale_colour_manual(values = c("blue", "red"))+
  labs(y = "Treatment", x = "")+
  theme(legend.position = c(0.8, 0.9))+
  ggtitle("The sum of us_only patent group by year")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 14, color = "black"))+
  annotate("rect", xmin=2017, xmax=2020, ymin=0, ymax=60000, alpha=0.2, fill="blue")+
  annotate("text", x=2018.5, y=30000, label="Trump", size=3)+
  annotate("text", x=2020, y=10000, label="Covid19", size=3)+
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed")+
  theme(legend.position = c(0.1, 0.14))+
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(color = "black", size = 9, face = "italic"))

treatment_with_cn <- aggregate(data = semi_num_us_T, with_cn ~ year, FUN=sum)
control_with_cn <- aggregate(data = semi_num_us_C, with_cn ~ year, FUN=sum)

patent_with_cn <- cbind(treatment_with_cn, control_with_cn[, 2])

colnames(patent_with_cn)[2] <- "Treat"
colnames(patent_with_cn)[3] <- "Control"

ggplot(patent_with_cn, aes(x =year)) +
  geom_line(aes(y = Treat, colour = "Treatment")) + 
  geom_line(aes(y = Control/10, colour = "Control"))+
  scale_y_continuous(sec.axis = sec_axis(~.*10, name = "Control"))+
  scale_colour_manual(values = c("blue", "red"))+
  labs(y = "Treatment", x = "")+
  theme(legend.position = c(0.8, 0.9))+
  ggtitle("The sum of with_cn patent group by year")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 14, color = "black"))+
  annotate("rect", xmin=2017, xmax=2020, ymin=0, ymax=10000, alpha=0.2, fill="blue")+
  annotate("text", x=2018.5, y=5000, label="Trump", size=3)+
  annotate("text", x=2020, y=2500, label="Covid19", size=3)+
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed")+
  theme(legend.position = c(0.1, 0.9))+
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(color = "black", size = 9, face = "italic"))


semi_num_us_T$inv_prop[semi_num_us_T$int_cn != 0] <- semi_num_us_T$with_cn[semi_num_us_T$int_cn != 0]/(semi_num_us_T$int_cn[semi_num_us_T$int_cn != 0])
semi_num_us_C$inv_prop <- semi_num_us_C$with_cn/semi_num_us_C$int_cn

semi_num_us_T1 <- semi_num_us_T[semi_num_us_T$inv_prop < 1, ]

treatment_prop <- aggregate(data = semi_num_us_T, inv_prop ~ year, FUN = mean)
control_prop <- aggregate(data = semi_num_us_C, inv_prop ~ year, FUN = mean)

treatment_prop$treatment <- "Treatment"
control_prop$treatment <- "Control"

patent_prop <- rbind(treatment_prop, control_prop)


ggplot(patent_prop, aes(year, inv_prop, group = treatment, color = treatment))+ 
  geom_line() + 
  ylab(label = "Average")+ 
  xlab(label = "")+ 
  scale_x_continuous(breaks=seq(min(patent_prop$year), max(patent_prop$year), 3)) + 
  ggtitle("Average #coinv of cn / #coinv of non cn group by year") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 14, color = "black")) + 
  annotate("rect", xmin=2017, xmax=2020, ymin=0, ymax=1.5, alpha=0.2, fill="blue") +
  annotate("text", x=2018.5, y=0.75, label="Trump", size=3) +
  annotate("text", x=2020, y=0.5, label="Covid19", size=3)+
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed") + 
  theme(legend.position = c(0.1, 0.14))+  
  theme(legend.title = element_blank()) + 
  theme(legend.text = element_text(color = "black", size = 9, face = "italic"))


treatment_mill <- aggregate(data = semi_num_us_T, mill_num ~ year, FUN = sum)
control_mill <- aggregate(data = semi_num_us_C, mill_num ~ year, FUN = sum)

patent_mill <- cbind(treatment_mill, control_mill[, 2])


colnames(patent_mill)[2] <- "Treat"
colnames(patent_mill)[3] <- "Control"


ggplot(patent_mill, aes(x =year)) +
  geom_line(aes(y = Treat, colour = "Treatment")) + 
  geom_line(aes(y = Control/100, colour = "Control"))+
  scale_y_continuous(sec.axis = sec_axis(~.*100, name = "Control"))+
  scale_colour_manual(values = c("blue", "red"))+
  labs(y = "Treatment", x = "")+
  theme(legend.position = c(0.8, 0.9))+
  ggtitle("The sum of millitary cpc patent group by year")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 14, color = "black"))+
  annotate("rect", xmin=2017, xmax=2020, ymin=0, ymax=70, alpha=0.2, fill="blue")+
  annotate("text", x=2018.5, y=35, label="Trump", size=3)+
  annotate("text", x=2020, y=25, label="Covid19", size=3)+
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed")+
  theme(legend.position = c(0.1, 0.9))+
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(color = "black", size = 9, face = "italic"))


treatment_medi <- aggregate(data = semi_num_us_T, medi_num ~ year, FUN = sum)
control_medi <- aggregate(data = semi_num_us_C, medi_num ~ year, FUN = sum)

patent_medi <- cbind(treatment_medi, control_medi[, 2])


colnames(patent_medi)[2] <- "Treat"
colnames(patent_medi)[3] <- "Control"


ggplot(patent_medi, aes(x =year)) +
  geom_line(aes(y = Treat, colour = "Treatment")) + 
  geom_line(aes(y = Control/50, colour = "Control"))+
  scale_y_continuous(sec.axis = sec_axis(~.*50, name = "Control"))+
  scale_colour_manual(values = c("blue", "red"))+
  labs(y = "Treatment", x = "")+
  theme(legend.position = c(0.8, 0.9))+
  ggtitle("The sum of medical cpc patent group by year")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 14, color = "black"))+
  annotate("rect", xmin=2017, xmax=2020, ymin=0, ymax=1500, alpha=0.2, fill="blue")+
  annotate("text", x=2018.5, y=750, label="Trump", size=3)+
  annotate("text", x=2020, y=500, label="Covid19", size=3)+
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed")+
  theme(legend.position = c(0.1, 0.9))+
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(color = "black", size = 9, face = "italic"))

#07/10
semi_num_us_2016 <- semi_num_us[semi_num_us$year < 2017, ]
semi_num_us$only_cn <- NULL
semi_num_us$with_us <- NULL
semi_num_us$int_us <- NULL

tr_flag <- aggregate(data = semi_num_us_2016, with_cn ~ inventor_id, FUN = sum)
tr_flag$treat <- ifelse(tr_flag$with_cn != 0, 1, 0)
tr_flag$with_cn <- NULL
semi_num_us <- left_join(semi_num_us, tr_flag, by = "inventor_id")
write.csv(semi_num_us, file = "semi_num_us_treated.csv")

semi_num_us_treated <- read_csv("semi_num_us_treated.csv", col_types = cols(...1 = col_skip()))

semi_num_us_T <- semi_num_us_treated[semi_num_us_treated$treat == 1 , ]
semi_num_us_C <- semi_num_us_treated[semi_num_us_treated$treat == 0, ]


treatment_mean<- aggregate(data = semi_num_us_T, number.of.patent ~ year, FUN=mean)
control_mean <- aggregate(data = semi_num_us_C, number.of.patent ~ year, FUN=mean)

patent_mean <- cbind(treatment_mean, control_mean)

colnames(patent_mean)[2] <- "average_treat"
colnames(patent_mean)[4] <- "average_control"

patent_mean$year <- NULL


p1 <- ggplot(patent_mean, aes(x =year)) +
  geom_line(aes(y = average_treat, colour = "Treatment")) + 
  geom_line(aes(y = average_control*5, colour = "Control"))+
  theme_bw() + 
  scale_y_continuous(sec.axis = sec_axis(~./5, name = "Control"))+
  scale_colour_manual(values = c("blue", "red"))+
  labs(y = "Treatment", x = "")+
  scale_x_continuous(breaks=seq(min(patent_mean$year), max(patent_mean$year), 3)) + 
  ggtitle("The Average of #patent group by year")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 14, color = "black"))+
  geom_rect(xmin=2017, xmax=2020, ymin=-Inf, ymax=+Inf, fill='gray', alpha=0.05) + 
  annotate("text", x=2018.5, y=1, label="Trump", size=3)+
  annotate("text", x=2020, y=0.75, label="Covid19", size=3)+
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed")+
  theme(legend.position = c(0.2, 0.2))+
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(color = "black", size = 9, face = "italic")) + 
  theme(axis.line = element_line(colour = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
  

treatment_mill_mean <- aggregate(data = semi_num_us_T, mill_num ~ year, FUN = mean)
control_mill_mean <- aggregate(data = semi_num_us_C, mill_num ~ year, FUN = mean)

patent_mill_mean <- cbind(treatment_mill_mean, control_mill_mean[, 2])


colnames(patent_mill_mean)[2] <- "Treat"
colnames(patent_mill_mean)[3] <- "Control"


p_mill1 <- ggplot(patent_mill_mean, aes(x =year)) +
  geom_line(aes(y = (Treat), colour = "Treatment")) + 
  geom_line(aes(y = (Control)*1, colour = "Control"))+
  theme_bw() + 
  scale_y_continuous(sec.axis = sec_axis(~./1, name = "Control"))+
  scale_colour_manual(values = c("blue", "red"))+
  labs(y = "Treatment", x = "")+
  scale_x_continuous(breaks=seq(min(patent_mill_mean$year), max(patent_mill_mean$year), 3)) + 
  ggtitle("The Average of #millitary patent group by year")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 14, color = "black"))+
  geom_rect(xmin=2017, xmax=2020, ymin=-Inf, ymax=+Inf, fill='gray', alpha=0.05) + 
  annotate("text", x=2018.5, y=0.003, label="Trump", size=3)+
  annotate("text", x=2020, y=0.0015, label="Covid19", size=3)+
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed")+
  theme(legend.position = c(0.2, 0.8))+
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(color = "black", size = 9, face = "italic")) + 
  theme(axis.line = element_line(colour = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
p_mill1
treatment_medi_mean <- aggregate(data = semi_num_us_T, medi_num ~ year, FUN = mean)
control_medi_mean <- aggregate(data = semi_num_us_C, medi_num ~ year, FUN = mean)

patent_medi_mean <- cbind(treatment_medi_mean, control_medi_mean[, 2])


colnames(patent_medi_mean)[2] <- "Treat"
colnames(patent_medi_mean)[3] <- "Control"

p_medi1
p_medi1 <- ggplot(patent_medi_mean, aes(x =year)) +
  geom_line(aes(y = (Treat), colour = "Treatment")) + 
  geom_line(aes(y = (Control)*2, colour = "Control"))+
  theme_bw() + 
  scale_y_continuous(sec.axis = sec_axis(~./2, name = "Control"))+
  scale_colour_manual(values = c("blue", "red"))+
  labs(y = "Treatment", x = "")+
  scale_x_continuous(breaks=seq(min(patent_medi_mean$year), max(patent_medi_mean$year), 3)) + 
  ggtitle("The Average of #medical patent group by year")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 14, color = "black"))+
  geom_rect(xmin=2017, xmax=2020, ymin=-Inf, ymax=+Inf, fill='gray', alpha=0.05) + 
  annotate("text", x=2018.5, y=0.05, label="Trump", size=3)+
  annotate("text", x=2020, y=0.025, label="Covid19", size=3)+
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed")+
  theme(legend.position = c(0.2, 0.8))+
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(color = "black", size = 9, face = "italic")) + 
  theme(axis.line = element_line(colour = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 


treatment_phar_mean <- aggregate(data = semi_num_us_T, phar_num ~ year, FUN = mean)
control_phar_mean <- aggregate(data = semi_num_us_C, phar_num ~ year, FUN = mean)

patent_phar_mean <- cbind(treatment_phar_mean, control_phar_mean[, 2])


colnames(patent_phar_mean)[2] <- "Treat"
colnames(patent_phar_mean)[3] <- "Control"

p_phar1
p_phar1 <- ggplot(patent_phar_mean, aes(x =year)) +
  geom_line(aes(y = (Treat), colour = "Treatment")) + 
  geom_line(aes(y = (Control)*5, colour = "Control"))+
  theme_bw() + 
  scale_y_continuous(sec.axis = sec_axis(~./5, name = "Control"))+
  scale_colour_manual(values = c("blue", "red"))+
  labs(y = "Treatment", x = "")+
  scale_x_continuous(breaks=seq(min(patent_phar_mean$year), max(patent_phar_mean$year), 3)) + 
  ggtitle("The Average of #pharmaceutical patent group by year")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 14, color = "black"))+
  geom_rect(xmin=2017, xmax=2020, ymin=-Inf, ymax=+Inf, fill='gray', alpha=0.05) + 
  annotate("text", x=2018.5, y=0.05, label="Trump", size=3)+
  annotate("text", x=2020, y=0.025, label="Covid19", size=3)+
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed")+
  theme(legend.position = c(0.2, 0.8))+
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(color = "black", size = 9, face = "italic")) + 
  theme(axis.line = element_line(colour = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 


treatment_with_cn_new <- aggregate(data = semi_num_us_T, with_cn ~ year, FUN=sum)
control_with_cn_new <- aggregate(data = semi_num_us_C, with_cn ~ year, FUN=sum)

patent_with_cn_new <- cbind(treatment_with_cn_new, control_with_cn_new[, 2])

colnames(patent_with_cn_new)[2] <- "Treat"
colnames(patent_with_cn_new)[3] <- "Control"

ggplot(patent_with_cn_new, aes(x =year)) +
  geom_line(aes(y = (Treat), colour = "Treatment")) + 
  geom_line(aes(y = (Control)*1, colour = "Control"))+
  theme_bw() + 
  scale_y_continuous(sec.axis = sec_axis(~./1, name = "Control"))+
  scale_colour_manual(values = c("blue", "red"))+
  labs(y = "Treatment", x = "")+
  scale_x_continuous(breaks=seq(min(patent_with_cn_new$year), max(patent_with_cn_new$year), 3)) + 
  ggtitle("The sum of #patent with cn group by year")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 14, color = "black"))+
  geom_rect(xmin=2017, xmax=2020, ymin=-Inf, ymax=+Inf, fill='gray', alpha=0.05) + 
  annotate("text", x=2018.5, y=6000, label="Trump", size=3)+
  annotate("text", x=2020, y=3000, label="Covid19", size=3)+
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed")+
  theme(legend.position = c(0.15, 0.9))+
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(color = "black", size = 9, face = "italic")) + 
  theme(axis.line = element_line(colour = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

treatment_with_cn_mean <- aggregate(data = semi_num_us_T, with_cn ~ year, FUN=sum)
control_with_cn_mean <- aggregate(data = semi_num_us_C, with_cn ~ year, FUN=sum)

patent_with_cn_mean <- cbind(treatment_with_cn_mean, control_with_cn_mean[, 2])

colnames(patent_with_cn_mean)[2] <- "Treat"
colnames(patent_with_cn_mean)[3] <- "Control"

ggplot(patent_with_cn_new, aes(x =year)) +
  geom_line(aes(y = (Treat), colour = "Treatment")) + 
  geom_line(aes(y = (Control)*1, colour = "Control"))+
  theme_bw() + 
  scale_y_continuous(sec.axis = sec_axis(~./1, name = "Control"))+
  scale_colour_manual(values = c("blue", "red"))+
  labs(y = "Treatment", x = "")+
  scale_x_continuous(breaks=seq(min(patent_with_cn_new$year), max(patent_with_cn_new$year), 3)) + 
  ggtitle("The sum of #patent with cn group by year")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 14, color = "black"))+
  geom_rect(xmin=2017, xmax=2020, ymin=-Inf, ymax=+Inf, fill='gray', alpha=0.05) + 
  annotate("text", x=2018.5, y=6000, label="Trump", size=3)+
  annotate("text", x=2020, y=3000, label="Covid19", size=3)+
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed")+
  theme(legend.position = c(0.15, 0.9))+
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(color = "black", size = 9, face = "italic")) + 
  theme(axis.line = element_line(colour = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

treatment_sum <- aggregate(data = semi_num_us_T, number.of.patent ~ year, FUN=sum)
control_sum <- aggregate(data = semi_num_us_C, number.of.patent ~ year, FUN=sum)

patent_sum <- cbind(treatment_sum, control_sum[, 2])

colnames(patent_sum)[2] <- "Treat_num"
colnames(patent_sum)[3] <- "Control_num"

patent_sum <- cbind(patent_sum, patent_with_cn_new[, 2:3])

patent_sum$tr_proportion <- patent_sum$Treat/patent_sum$Treat_num
patent_sum$control_proportion <- patent_sum$Control/patent_sum$Control_num

ggplot(patent_sum, aes(x =year)) +
  geom_line(aes(y = (tr_proportion), colour = "Treatment")) + 
  geom_line(aes(y = (control_proportion)*1, colour = "Control"))+
  theme_bw() + 
  scale_y_continuous(sec.axis = sec_axis(~./1, name = "Control"))+
  scale_colour_manual(values = c("blue", "red"))+
  labs(y = "Treatment", x = "")+
  scale_x_continuous(breaks=seq(min(patent_sum$year), max(patent_sum$year), 3)) + 
  ggtitle("#cn_patent / #patent group by year, treatment")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 14, color = "black"))+
  geom_rect(xmin=2017, xmax=2020, ymin=-Inf, ymax=+Inf, fill='gray', alpha=0.05) + 
  annotate("text", x=2018.5, y=0.15, label="Trump", size=3)+
  annotate("text", x=2020, y=0.075, label="Covid19", size=3)+
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed")+
  theme(legend.position = c(0.15, 0.9))+
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(color = "black", size = 9, face = "italic")) + 
  theme(axis.line = element_line(colour = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

patent_flag <- aggregate(data = data1, number.of.patent ~ inventor_id, FUN = sum)
patent_flag <- patent_flag$inventor_id[patent_flag$number.of.patent != 0]

semi_num_us_treated <- semi_num_us_treated[semi_num_us_treated$inventor_id %in% patent_flag, ]

data1 <- semi_num_us_treated[semi_num_us_treated$year < 2017, ]
data2 <- semi_num_us_treated[semi_num_us_treated$year > 2016, ]


dataset = list("Entire data" = semi_num_us_treated, "Period : 2005~2016" = data1, "Period : 2017~2021" = data2)
dataset

var_name <- c("#inventor","#patent", "#patent only us" , "#patent with cn", "#patent with other", "#Mil patent", "#Phar patetn", "#medi patent")
variables = list(colnames(semi_num_us_treated)[c(1, 3:9)], colnames(semi_num_us_treated)[c(1, 3:9)], colnames(semi_num_us_treated)[c(1, 3:9)])
labels = list(var_name, var_name, var_name)

colnames =  c("N","Mean", "Median", "SD", "Min", "Max")

myDescriptives = function(x) {
  if (class(x) == "character"){
    n = length(unique(x))
    return(n)
  }
  else {
    n= length(x)
    m = mean(x, na.rm = TRUE)
    md = median(x, na.rm = TRUE)
    sd = sd(x, na.rm = TRUE)
    min = min(x)
    max = max(x)
    return(c(n, m, md, sd, min, max))
  }
  
}

createDescriptiveTable(dataset,
                       summary_function = myDescriptives,
                       column_names = colnames,
                       variable_names = variables,
                       variable_labels = labels,
                       group_variable = "treat",
                       arraystretch = 2,
                       tabcolsep = 6,
                       note = "This is a very long long long long long long long long long note.",
                       title = "Descriptive statistics",
                       label = "tab:descriptive",
                       file = "example_03.tex")

write.csv(semi_num_us_treated, file = "checkindata.csv")

stargazer(semi_num_us_treated)



summary(semi_num_us_treated[semi_num_us_treated$treat == 1, ])


#07/17
for(i in 2005:2021){
  print(length(unique(semi_num_us_treated$inventor_id[semi_num_us_treated$treat == 1 & semi_num_us_treated$year == i])))
}


new_treatment_sum<- aggregate(data = semi_num_us_T, number.of.patent ~ year, FUN=sum)
new_control_sum <- aggregate(data = semi_num_us_C, number.of.patent ~ year, FUN=sum)

new_patent_sum <- cbind(new_treatment_sum, new_control_sum)

colnames(new_patent_sum)[2] <- "average_treat"
colnames(new_patent_sum)[4] <- "average_control"

new_patent_sum$year <- NULL
View(new_patent_sum)

p2 <- ggplot(new_patent_sum, aes(x =year)) +
  geom_line(aes(y = average_treat, colour = "Treatment")) + 
  geom_line(aes(y = average_control/15, colour = "Control"))+
  theme_bw() + 
  scale_y_continuous(sec.axis = sec_axis(~.*15, name = "Control"))+
  scale_colour_manual(values = c("blue", "red"))+
  labs(y = "Treatment", x = "")+
  scale_x_continuous(breaks=seq(min(patent_mean$year), max(patent_mean$year), 3)) + 
  ggtitle("The Sum of #patent group by year")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 14, color = "black"))+
  geom_rect(xmin=2017, xmax=2020, ymin=-Inf, ymax=+Inf, fill='gray', alpha=0.05) + 
  annotate("text", x=2018.5, y=20000, label="Trump", size=3)+
  annotate("text", x=2020, y=15000, label="Covid19", size=3)+
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed")+
  theme(legend.position = c(0.2, 0.2))+
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(color = "black", size = 9, face = "italic")) + 
  theme(axis.line = element_line(colour = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

p1 + p2

new_treatment_mill_sum <- aggregate(data = semi_num_us_T, mill_num ~ year, FUN = sum)
new_control_mill_sum <- aggregate(data = semi_num_us_C, mill_num ~ year, FUN = sum)

new_patent_mill_sum <- cbind(new_treatment_mill_sum, new_control_mill_sum[, 2])


colnames(new_patent_mill_sum)[2] <- "Treat"
colnames(new_patent_mill_sum)[3] <- "Control"


p_mill2 <- ggplot(new_patent_mill_sum, aes(x =year)) +
  geom_line(aes(y = (Treat), colour = "Treatment")) + 
  geom_line(aes(y = (Control)/80, colour = "Control"))+
  theme_bw() + 
  scale_y_continuous(sec.axis = sec_axis(~.*80, name = "Control"))+
  scale_colour_manual(values = c("blue", "red"))+
  labs(y = "Treatment", x = "")+
  scale_x_continuous(breaks=seq(min(new_patent_mill_sum$year), max(new_patent_mill_sum$year), 3)) + 
  ggtitle("The Sum of #millitary patent group by year")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 14, color = "black"))+
  geom_rect(xmin=2017, xmax=2020, ymin=-Inf, ymax=+Inf, fill='gray', alpha=0.05) + 
  annotate("text", x=2018.5, y=0.003, label="Trump", size=3)+
  annotate("text", x=2020, y=0.0015, label="Covid19", size=3)+
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed")+
  theme(legend.position = c(0.15, 0.8))+
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(color = "black", size = 9, face = "italic")) + 
  theme(axis.line = element_line(colour = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

new_treatment_medi_sum <- aggregate(data = semi_num_us_T, medi_num ~ year, FUN = sum)
new_control_medi_sum <- aggregate(data = semi_num_us_C, medi_num ~ year, FUN = sum)

new_patent_medi_sum <- cbind(new_treatment_medi_sum, new_control_medi_sum[, 2])


colnames(new_patent_medi_sum)[2] <- "Treat"
colnames(new_patent_medi_sum)[3] <- "Control"


p_medi2 <- ggplot(new_patent_medi_sum, aes(x =year)) +
  geom_line(aes(y = (Treat), colour = "Treatment")) + 
  geom_line(aes(y = (Control)/40, colour = "Control"))+
  theme_bw() + 
  scale_y_continuous(sec.axis = sec_axis(~.*40, name = "Control"))+
  scale_colour_manual(values = c("blue", "red"))+
  labs(y = "Treatment", x = "")+
  scale_x_continuous(breaks=seq(min(new_patent_medi_sum$year), max(new_patent_medi_sum$year), 3)) + 
  ggtitle("The Sum of #medical patent group by year")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 14, color = "black"))+
  geom_rect(xmin=2017, xmax=2020, ymin=-Inf, ymax=+Inf, fill='gray', alpha=0.05) + 
  annotate("text", x=2018.5, y=0.05, label="Trump", size=3)+
  annotate("text", x=2020, y=0.025, label="Covid19", size=3)+
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed")+
  theme(legend.position = c(0.15, 0.8))+
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(color = "black", size = 9, face = "italic")) + 
  theme(axis.line = element_line(colour = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 


new_treatment_phar_sum <- aggregate(data = semi_num_us_T, phar_num ~ year, FUN = sum)
new_control_phar_sum <- aggregate(data = semi_num_us_C, phar_num ~ year, FUN = sum)

new_patent_phar_sum <- cbind(new_treatment_phar_sum, new_control_phar_sum[, 2])


colnames(new_patent_phar_sum)[2] <- "Treat"
colnames(new_patent_phar_sum)[3] <- "Control"


p_phar2 <- ggplot(new_patent_phar_sum, aes(x =year)) +
  geom_line(aes(y = (Treat), colour = "Treatment")) + 
  geom_line(aes(y = (Control)/15, colour = "Control"))+
  theme_bw() + 
  scale_y_continuous(sec.axis = sec_axis(~.*15, name = "Control"))+
  scale_colour_manual(values = c("blue", "red"))+
  labs(y = "Treatment", x = "")+
  scale_x_continuous(breaks=seq(min(new_patent_phar_sum$year), max(new_patent_phar_sum$year), 3)) + 
  ggtitle("The average of #pharmaceutical patent group by year")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 14, color = "black"))+
  geom_rect(xmin=2017, xmax=2020, ymin=-Inf, ymax=+Inf, fill='gray', alpha=0.05) + 
  annotate("text", x=2018.5, y=0.05, label="Trump", size=3)+
  annotate("text", x=2020, y=0.025, label="Covid19", size=3)+
  geom_vline(xintercept = 2020, colour = "red", lty = "dashed")+
  theme(legend.position = c(0.15, 0.9))+
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(color = "black", size = 9, face = "italic")) + 
  theme(axis.line = element_line(colour = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

p1 + p2
p_mill1 + p_mill2
p_medi1 + p_medi2
p_phar1 + p_phar2
