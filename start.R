library(haven)
library(car)
all_data <- read_dta("./CPDS_1960_2022_Update_2024.dta")
head(all_data)

current <- all_data[all_data$year == 2022,]
head(current)

# country - country name
# poco - postcomunist - 1 if country is postcomunist, 0 otherwise
# eu - EU member - 1 if country is EU member, 0 otherwise
# emu - EMU member - 1 if country is Economic and Monatary Union member, 0 otherwise
# gov_party - gov_party Cabinet composition (Schmidt-Index): 0-100 -> 0 - right, 100 - socio-democratic
# receipts - government receipts (% of GDP)
# realgdpgr - Growth of real GDP, percent change from previous year
# inflation - Growth of harmonised consumer price index (CPI), all items, percent change from previous year; used as a measure for inflation
# take only the relevant columns
# deficit - Annual deficit (overall balance / net lending of general government) as a percentage of GDP.
# labfopar - Total labour force as a percentage of population 15-64 (participation rate).
# unemp- Unemployment rate, percentage of civilian labour force.
# educexp_gov - General government expenditure on education (current, capital and transfers) as a percentage of GDP.
# educatt_minimal - Share of population attending no more than secondary education
cstat <- current[,c("country", "poco", "eu", "emu", "receipts", "realgdpgr", "inflation", "deficit", "labfopar", "unemp", "educexp_gov", "gov_party", "educatt_minimal")]
cstat$poco <- as.factor(cstat$poco)
cstat$eu <- as.factor(cstat$eu)
cstat$emu <- as.factor(cstat$emu)
dim(cstat)

rownames(cstat) <- data$country
cstat<-cstat[,-1]
summary(cstat)


library(FactoMineR)
library(factoextra)

res.pca <- PCA(cstat, graph = FALSE)
print(res.pca)

fviz_pca_biplot(res.pca, repel = TRUE, col.var = "black", col.ind = "black", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), legend.title = "Country", addEllipses = TRUE, ellipse.level = 0.95)

pca_res <- prcomp(cstat[, 1:4], scale. = TRUE)
summary(pca_res)
?prcomp

fviz_pca_ind(
  pca_res,
  geom.ind = "point",
  legend.title = "Countries"
)

biplot(pca_res)















