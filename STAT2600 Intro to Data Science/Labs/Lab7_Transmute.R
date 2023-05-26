library(stringr)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
ExterQualDict = setNames(5:1,c("Ex", "Gd", "TA", "Fa", "Po"))
BsmtQualDict = setNames(c(100,90,80,70,60),c("Ex", "Gd", "TA", "Fa", "Po"))
houseDf = read_csv("house_price_train.csv", show_col_types = FALSE)
newHouseDf <- houseDf %>% transmute(Id = Id,
                                    LotFrontage = LotFrontage %>% replace_na(0),
                                    ExterQual = ExterQualDict[ExterQual],
                                    BsmtHeight = BsmtQualDict[BsmtQual],
                                    HasCentralAC = ifelse(CentralAir == 'Y', TRUE, FALSE),
                                    AgeWhenSold = YrSold - YearBuilt,
                                    SeasonSold = ifelse((MoSold >=11 & MoSold <=12) | (MoSold >=1 & MoSold <=2),1,ifelse((MoSold >=5 & MoSold <=8),3,2)),
                                    DateSold = ymd(paste(YrSold,str_pad(MoSold,pad="0",side="left",width="2"),"01",sep="-")))
newHouseDf %>% write_csv("lab7.csv")
