#Set snp_position.txt as working directory
setwd("~/R_Assignment")

#puts snp_position into data
snp_data <- read.table("snp_position.txt", sep ="\t", header = TRUE)
View(snp_data)
#puts fang_et_al_genotypes.txt into fang_data
fang_data <- read.table("fang_et_al_genotypes.txt",sep="\t",header=T)
View(fang_data)

#view first 6 rows of snp_data
#note the first column is SNP_ID, second is cdv_marker, third is Chromosome, 4th is position
head(snp_data)
#view first 6 rows of fang_data
#note this file needs to be transposed
head(fang_data)

#get file size of snp_data current size is 82763
file.size("snp_position.txt")
#get file size of fang_et_al, current size is 11051939
file.size("fang_et_al_genotypes.txt")

#get dimensions of snp_data 1345 rows, 1 column
dim(snp_data)
#get dimensions of fang_data 2782 rows, 1 column
dim(fang_data)

#load package: tidyverse
library(tidyverse)
install.packages("tidyverse")

#manipulate dataset to merge them

#remove undesired information from snp_data
selected_snp_data <- select(snp_data, "SNP_ID", "Chromosome", "Position")
View(selected_snp_data)
View(fang_data)

#Pull all Maize data from each data set by filter
M_fang_data <- filter(fang_data,`Group`%in% c('ZMMIL','ZMMLR','ZMMMR'))
#Pull all teosinte from each data set by filter
T_fang_data <- filter(fang_data,`Group`%in% c('ZMPBA','ZMPIL','ZMPJA'))

#transpose M_fang_data and T_fang_data
TT_fang_data <- t(T_fang_data)
TM_fang_data <- t(M_fang_data)

#Trim transposed data to remove header, installed janitor for this
install.packages("janitor")
library(janitor)
TTT_fang_data <- row_to_names(TT_fang_data, 3, remove_row = TRUE, remove_rows_above = TRUE)
TTM_fang_data <- row_to_names(TM_fang_data, 3, remove_row = TRUE, remove_rows_above = TRUE)


#arrange the columns based on SNP_ID
arranged_snp_data <- arrange(selected_snp_data, 'SNP_ID')

#combine the two datasets into a single dataset
teosinte_data <- cbind(selected_snp_data, TTT_fang_data)
maize_data <- cbind(selected_snp_data, TTM_fang_data)
View(maize_data)

#making subsets of each chromosome with missing data as ? start by cleaning data
maize_data_clean <- clean_names(maize_data)
teosinte_data_clean <- clean_names(teosinte_data)
View(maize_data_clean)
View(teosinte_data_clean)

#pull all chromosome data into each file and arrange by position
maize_chr1_data <- maize_data_clean[maize_data_clean$chromosome == 1, ]
maize_chr1_data <- arrange(maize_chr1_data,position)

maize_chr2_data <- maize_data_clean[maize_data_clean$chromosome == 2, ]
maize_chr2_data <- arrange(maize_chr2_data,position)

maize_chr3_data <- maize_data_clean[maize_data_clean$chromosome == 3, ]
maize_chr3_data <- arrange(maize_chr3_data,position)

maize_chr4_data <- maize_data_clean[maize_data_clean$chromosome == 4, ]
maize_chr4_data <- arrange(maize_chr4_data,position)

maize_chr5_data <- maize_data_clean[maize_data_clean$chromosome == 5, ]
maize_chr5_data <- arrange(maize_chr5_data,position)

maize_chr6_data <- maize_data_clean[maize_data_clean$chromosome == 6, ]
maize_chr6_data <- arrange(maize_chr6_data,position)

maize_chr7_data <- maize_data_clean[maize_data_clean$chromosome == 7, ]
maize_chr7_data <- arrange(maize_chr7_data,position)

maize_chr8_data <- maize_data_clean[maize_data_clean$chromosome == 8, ]
maize_chr8_data <- arrange(maize_chr8_data,position)

maize_chr9_data <- maize_data_clean[maize_data_clean$chromosome == 9, ]
maize_chr9_data <- arrange(maize_chr9_data,position)

maize_chr10_data <- maize_data_clean[maize_data_clean$chromosome == 10, ]
maize_chr10_data <- arrange(maize_chr10_data,position)

teosinte_chr1_data <- teosinte_data_clean[teosinte_data_clean$chromosome == 1, ]
teosinte_chr1_data <- arrange(teosinte_chr1_data,position)

teosinte_chr2_data <- teosinte_data_clean[teosinte_data_clean$chromosome == 2, ]
teosinte_chr2_data <- arrange(teosinte_chr2_data,position)

teosinte_chr3_data <- teosinte_data_clean[teosinte_data_clean$chromosome == 3, ]
teosinte_chr3_data <- arrange(teosinte_chr3_data,position)

teosinte_chr4_data <- teosinte_data_clean[teosinte_data_clean$chromosome == 4, ]
teosinte_chr4_data <- arrange(teosinte_chr4_data,position)

teosinte_chr5_data <- teosinte_data_clean[teosinte_data_clean$chromosome == 5, ]
teosinte_chr5_data <- arrange(teosinte_chr5_data,position)

teosinte_chr6_data <- teosinte_data_clean[teosinte_data_clean$chromosome == 6, ]
teosinte_chr6_data <- arrange(teosinte_chr6_data,position)

teosinte_chr7_data <- teosinte_data_clean[teosinte_data_clean$chromosome == 7, ]
teosinte_chr7_data <- arrange(teosinte_chr7_data,position)

teosinte_chr8_data <- teosinte_data_clean[teosinte_data_clean$chromosome == 8, ]
teosinte_chr8_data <- arrange(teosinte_chr8_data,position)
view(teosinte_chr8_data)
teosinte_chr9_data <- teosinte_data_clean[teosinte_data_clean$chromosome == 9, ]
teosinte_chr9_data <- arrange(teosinte_chr9_data,position)

teosinte_chr10_data <- teosinte_data_clean[teosinte_data_clean$chromosome == 10, ]
teosinte_chr10_data <- arrange(teosinte_chr10_data,position)

#making subsets of each chromosome in descending position with missing data as -

r_maize_chr1_data <- maize_data_clean[maize_data_clean$chromosome == 1, ]
r_maize_chr1_data <- arrange(maize_chr1_data,desc(position))

r_maize_chr2_data <- maize_data_clean[maize_data_clean$chromosome == 2, ]
r_maize_chr2_data <- arrange(maize_chr2_data,desc(position))

r_maize_chr3_data <- maize_data_clean[maize_data_clean$chromosome == 3, ]
r_maize_chr3_data <- arrange(maize_chr3_data,desc(position))

r_maize_chr4_data <- maize_data_clean[maize_data_clean$chromosome == 4, ]
r_maize_chr4_data <- arrange(maize_chr4_data,desc(position))

r_maize_chr5_data <- maize_data_clean[maize_data_clean$chromosome == 5, ]
r_maize_chr5_data <- arrange(maize_chr5_data,desc(position))

r_maize_chr6_data <- maize_data_clean[maize_data_clean$chromosome == 6, ]
r_maize_chr6_data <- arrange(maize_chr6_data,desc(position))

r_maize_chr7_data <- maize_data_clean[maize_data_clean$chromosome == 7, ]
r_maize_chr7_data <- arrange(maize_chr7_data,desc(position))

r_maize_chr8_data <- maize_data_clean[maize_data_clean$chromosome == 8, ]
r_maize_chr8_data <- arrange(maize_chr8_data,desc(position))

r_maize_chr9_data <- maize_data_clean[maize_data_clean$chromosome == 9, ]
r_maize_chr9_data <- arrange(maize_chr9_data,desc(position))

r_maize_chr10_data <- maize_data_clean[maize_data_clean$chromosome == 10, ]
r_maize_chr10_data <- arrange(maize_chr10_data,desc(position))

r_teosinte_chr1_data <- teosinte_data_clean[teosinte_data_clean$chromosome == 1, ]
r_teosinte_chr1_data <- arrange(teosinte_chr1_data,desc(position))

r_teosinte_chr2_data <- teosinte_data_clean[teosinte_data_clean$chromosome == 2, ]
r_teosinte_chr2_data <- arrange(teosinte_chr2_data,desc(position))

r_teosinte_chr3_data <- teosinte_data_clean[teosinte_data_clean$chromosome == 3, ]
r_teosinte_chr3_data <- arrange(teosinte_chr3_data,desc(position))

r_teosinte_chr4_data <- teosinte_data_clean[teosinte_data_clean$chromosome == 4, ]
r_teosinte_chr4_data <- arrange(teosinte_chr4_data,desc(position))

r_teosinte_chr5_data <- teosinte_data_clean[teosinte_data_clean$chromosome == 5, ]
r_teosinte_chr5_data <- arrange(teosinte_chr5_data,desc(position))

r_teosinte_chr6_data <- teosinte_data_clean[teosinte_data_clean$chromosome == 6, ]
r_teosinte_chr6_data <- arrange(teosinte_chr6_data,desc(position))

r_teosinte_chr7_data <- teosinte_data_clean[teosinte_data_clean$chromosome == 7, ]
r_teosinte_chr7_data <- arrange(teosinte_chr7_data,desc(position))

r_teosinte_chr8_data <- teosinte_data_clean[teosinte_data_clean$chromosome == 8, ]
r_teosinte_chr8_data <- arrange(teosinte_chr8_data,desc(position))

r_teosinte_chr9_data <- teosinte_data_clean[teosinte_data_clean$chromosome == 9, ]
r_teosinte_chr9_data <- arrange(teosinte_chr9_data,desc(position))

r_teosinte_chr10_data <- teosinte_data_clean[teosinte_data_clean$chromosome == 10, ]
r_teosinte_chr10_data <- arrange(teosinte_chr10_data,desc(position))

#save all data to files
#create folder maize_data and teosinte_data
dir.create(file.path("~/R_Assignment","teosinte_data"))
save(teosinte_chr1_data,file="teosinte_data/teosinte_chr1_data.Rdata")
save(teosinte_chr2_data,file="teosinte_data/teosinte_chr2_data.Rdata")
save(teosinte_chr3_data,file="teosinte_data/teosinte_chr3_data.Rdata")
save(teosinte_chr4_data,file="teosinte_data/teosinte_chr4_data.Rdata")
save(teosinte_chr5_data,file="teosinte_data/teosinte_chr5_data.Rdata")
save(teosinte_chr6_data,file="teosinte_data/teosinte_chr6_data.Rdata")
save(teosinte_chr7_data,file="teosinte_data/teosinte_chr7_data.Rdata")
save(teosinte_chr8_data,file="teosinte_data/teosinte_chr8_data.Rdata")
save(teosinte_chr9_data,file="teosinte_data/teosinte_chr9_data.Rdata")
save(teosinte_chr10_data,file="teosinte_data/teosinte_chr10_data.Rdata")
save(r_teosinte_chr10_data,file="teosinte_data/r_teosinte_chr10_data.Rdata")
save(r_teosinte_chr1_data,file="teosinte_data/r_teosinte_chr1_data.Rdata")
save(r_teosinte_chr2_data,file="teosinte_data/r_teosinte_chr2_data.Rdata")
save(r_teosinte_chr3_data,file="teosinte_data/r_teosinte_chr3_data.Rdata")
save(r_teosinte_chr4_data,file="teosinte_data/r_teosinte_chr4_data.Rdata")
save(r_teosinte_chr5_data,file="teosinte_data/r_teosinte_chr5_data.Rdata")
save(r_teosinte_chr6_data,file="teosinte_data/r_teosinte_chr6_data.Rdata")
save(r_teosinte_chr7_data,file="teosinte_data/r_teosinte_chr7_data.Rdata")
save(r_teosinte_chr8_data,file="teosinte_data/r_teosinte_chr8_data.Rdata")
save(r_teosinte_chr9_data,file="teosinte_data/r_teosinte_chr9_data.Rdata")
save(r_teosinte_chr10_data,file="teosinte_data/r_teosinte_chr10_data.Rdata")

dir.create(file.path("~/R Assignment","maize_data"))
save(maize_chr1_data,file="maize_data/maize_chr1_data.Rdata")
save(maize_chr2_data,file="maize_data/maize_chr2_data.Rdata")
save(maize_chr3_data,file="maize_data/maize_chr3_data.Rdata")
save(maize_chr4_data,file="maize_data/maize_chr4_data.Rdata")
save(maize_chr5_data,file="maize_data/maize_chr5_data.Rdata")
save(maize_chr6_data,file="maize_data/maize_chr6_data.Rdata")
save(maize_chr7_data,file="maize_data/maize_chr7_data.Rdata")
save(maize_chr8_data,file="maize_data/maize_chr8_data.Rdata")
save(maize_chr9_data,file="maize_data/maize_chr9_data.Rdata")
save(maize_chr10_data,file="maize_data/maize_chr10_data.Rdata")
save(r_maize_chr10_data,file="maize_data/r_maize_chr10_data.Rdata")
save(r_maize_chr1_data,file="maize_data/r_maize_chr1_data.Rdata")
save(r_maize_chr2_data,file="maize_data/r_maize_chr2_data.Rdata")
save(r_maize_chr3_data,file="maize_data/r_maize_chr3_data.Rdata")
save(r_maize_chr4_data,file="maize_data/r_maize_chr4_data.Rdata")
save(r_maize_chr5_data,file="maize_data/r_maize_chr5_data.Rdata")
save(r_maize_chr6_data,file="maize_data/r_maize_chr6_data.Rdata")
save(r_maize_chr7_data,file="maize_data/r_maize_chr7_data.Rdata")
save(r_maize_chr8_data,file="maize_data/r_maize_chr8_data.Rdata")
save(r_maize_chr9_data,file="maize_data/r_maize_chr9_data.Rdata")
save(r_maize_chr10_data,file="maize_data/r_maize_chr10_data.Rdata")

#Visualizing 

#this gives a bar graph counting number of SNPs per chromosome
install.packages("tidyverse")
ggplot_snp_data <- ggplot(data = snp_data) + stat_count(mapping = aes(x=Chromosome))
print(ggplot_snp_data)

#to determine number of snps on maize and number of snps on teosinte
ggplot_maize <- ggplot(data = maize_data_clean) + stat_count(mapping = aes(x=chromosome))
print(ggplot_maize)
ggplot_teosinte <- ggplot(data = teosinte_data_clean) + stat_count(mapping = aes(x=chromosome))
print(ggplot_teosinte)

#to determine homozygous to heterozygous for each sample and each group
#maize
maize_data_hh <- maize_data_clean[,-1:-3]
maize_data_hh$homozygous <- rep(x=0,times=1)
maize_data_hhh <- maize_data_hh
for (i in 1:nrow(maize_data_hh)) {
for (j in 1:ncol(maize_data_hh)) {
maize_data_hhh[i,ncol(maize_data_hhh)] = ifelse((maize_data_hh[i,j]=="A/A"|maize_data_hh[i,j]=="C/C"|maize_data_hh[i,j]=="T/T"|maize_data_hh[i,j]=="G/G"),maize_data_hhh[i,ncol(maize_data_hhh)]+1,maize_data_hhh[i,ncol(maize_data_hhh)]+0)
}
}

maize_data_hhh$missing <- rep(x=0,times=1)
for (i in 1:nrow(maize_data_hh)) {
for (j in 1:ncol(maize_data_hh)) {
maize_data_hhh[i,ncol(maize_data_hhh)] = ifelse((maize_data_hh[i,j]=="?/?"),maize_data_hhh[i,ncol(maize_data_hhh)]+1,maize_data_hhh[i,ncol(maize_data_hhh)]+0)
}
}
View(maize_data_hhh)
maize_data_hhh$heterozygous <- rep(x=0,times=1)
for (i in 1:nrow(maize_data_hh)) {
maize_data_hhh[i,ncol(maize_data_hhh)] = 1573-(maize_data_hhh[i,ncol(maize_data_hhh)-1]+maize_data_hhh[i,ncol(maize_data_hhh)-2])
}
maize_data_hhh_cut <- select(maize_data_hhh, "homozygous", "heterozygous", "missing")
maize_data_hhm <- cbind(maize_data_clean,maize_data_hhh_cut)
View(maize_data_hhm)
#homozygous
maize_data_homozygous_sum <- rep(x=0,times=10)
maize_data_homozygous_sum <- t(maize_data_homozygous_sum)
for (i in 1:10) {
maize_data_homozygous_sum[,i] = sum(maize_data_hhm[which(maize_data_hhm$chromosome==i),]$homozygous)
}

maize_data_homozygous_sum2 <- data.frame(   name=c("1","2","3","4","5","6","7","8","9","10") ,     value=t(maize_data_homozygous_sum)   )

ggplot(maize_data_homozygous_sum2, aes(x=name, y=value)) +    geom_bar(stat = "identity")

#heterozygous

maize_data_heterozygous_sum <- rep(x=0,times=10)
maize_data_heterozygous_sum <- t(maize_data_heterozygous_sum)
for (i in 1:10) {
maize_data_heterozygous_sum[,i] = sum(maize_data_hhm[which(maize_data_hhm$chromosome==i),]$heterozygous)
}

maize_data_heterozygous_sum2 <- data.frame(   name=c("1","2","3","4","5","6","7","8","9","10") ,     value=t(maize_data_heterozygous_sum)   )

ggplot(maize_data_heterozygous_sum2, aes(x=name, y=value)) +    geom_bar(stat = "identity")

#missing
maize_data_missing_sum <- rep(x=0,times=10)
maize_data_missing_sum <- t(maize_data_missing_sum)
for (i in 1:10) {
maize_data_missing_sum[,i] = sum(maize_data_hhm[which(maize_data_hhm$chromosome==i),]$missing)
}

maize_data_missing_sum2 <- data.frame(   name=c("1","2","3","4","5","6","7","8","9","10") ,     value=t(maize_data_missing_sum)   )

ggplot(maize_data_missing_sum2, aes(x=name, y=value)) +    geom_bar(stat = "identity")

#teosinte heterozygous/homozygous/missing
teosinte_data_hh <- teosinte_data_clean[,-1:-3]
teosinte_data_hh$homozygous <- rep(x=0,times=1)
teosinte_data_hhh <- teosinte_data_hh
for (i in 1:nrow(teosinte_data_hh)) {
for (j in 1:ncol(teosinte_data_hh)) {
teosinte_data_hhh[i,ncol(teosinte_data_hhh)] = ifelse((teosinte_data_hh[i,j]=="A/A"|teosinte_data_hh[i,j]=="C/C"|teosinte_data_hh[i,j]=="T/T"|teosinte_data_hh[i,j]=="G/G"),teosinte_data_hhh[i,ncol(teosinte_data_hhh)]+1,teosinte_data_hhh[i,ncol(teosinte_data_hhh)]+0)
}
}

teosinte_data_hhh$missing <- rep(x=0,times=1)
for (i in 1:nrow(teosinte_data_hh)) {
for (j in 1:ncol(teosinte_data_hh)) {
teosinte_data_hhh[i,ncol(teosinte_data_hhh)] = ifelse((teosinte_data_hh[i,j]=="?/?"),teosinte_data_hhh[i,ncol(teosinte_data_hhh)]+1,teosinte_data_hhh[i,ncol(teosinte_data_hhh)]+0)
}
}
View(teosinte_data_hhh)
teosinte_data_hhh$heterozygous <- rep(x=0,times=1)
for (i in 1:nrow(teosinte_data_hh)) {
teosinte_data_hhh[i,ncol(teosinte_data_hhh)] = 1573-(teosinte_data_hhh[i,ncol(teosinte_data_hhh)-1]+teosinte_data_hhh[i,ncol(teosinte_data_hhh)-2])
}
teosinte_data_hhh_cut <- select(teosinte_data_hhh, "homozygous", "heterozygous", "missing")
teosinte_data_hhm <- cbind(teosinte_data_clean,teosinte_data_hhh_cut)
View(teosinte_data_hhm)
#homozygous
teosinte_data_homozygous_sum <- rep(x=0,times=10)
teosinte_data_homozygous_sum <- t(teosinte_data_homozygous_sum)
for (i in 1:10) {
teosinte_data_homozygous_sum[,i] = sum(teosinte_data_hhm[which(teosinte_data_hhm$chromosome==i),]$homozygous)
}

teosinte_data_homozygous_sum2 <- data.frame(   name=c("1","2","3","4","5","6","7","8","9","10") ,     value=t(teosinte_data_homozygous_sum)   )

ggplot(teosinte_data_homozygous_sum2, aes(x=name, y=value)) +    geom_bar(stat = "identity")

#heterozygous

teosinte_data_heterozygous_sum <- rep(x=0,times=10)
teosinte_data_heterozygous_sum <- t(teosinte_data_heterozygous_sum)
for (i in 1:10) {
teosinte_data_heterozygous_sum[,i] = sum(teosinte_data_hhm[which(teosinte_data_hhm$chromosome==i),]$heterozygous)
}

teosinte_data_heterozygous_sum2 <- data.frame(   name=c("1","2","3","4","5","6","7","8","9","10") ,     value=t(teosinte_data_heterozygous_sum)   )

ggplot(teosinte_data_heterozygous_sum2, aes(x=name, y=value)) +    geom_bar(stat = "identity")

#missing
teosinte_data_missing_sum <- rep(x=0,times=10)
teosinte_data_missing_sum <- t(teosinte_data_missing_sum)
for (i in 1:10) {
teosinte_data_missing_sum[,i] = sum(teosinte_data_hhm[which(teosinte_data_hhm$chromosome==i),]$missing)
}

teosinte_data_missing_sum2 <- data.frame(   name=c("1","2","3","4","5","6","7","8","9","10") ,     value=t(teosinte_data_missing_sum)   )

ggplot(teosinte_data_missing_sum2, aes(x=name, y=value)) +    geom_bar(stat = "identity")

#my own visual
