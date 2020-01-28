install.packages("readstata13")
library(readstata13)

dat = read.dta13("guiide_master_file_anonymized_full.dta")

# ------------- Exercise 1 -------------

nrow(dat)
ncol(dat)
#42974 rows, 3332 columns

sink("summary.txt")
#run code inside sink, so it doesn't appear in the console until you close the sink
summary(dat)
sink()
#This closes the previously opened sink.

varnames = names(dat)

# the function grep("id",varnames) can be used to search varnames for ids
varnames[grep("id",varnames)]
#shows the names of columns which include "id"
#We identify anon_studentid as a potential unique identifier

#we want to make sure that the identifier we want to use is unique
nrow(dat) == length(unique(dat$anon_studentid))
#false, meaning there's some student ids which aren't unique

nrow(dat) - length(unique(dat$anon_studentid))
#21 duplicates

stuIdIn = which(duplicated(dat$anon_studentid))
#This gives the indices of second instance of the duplicate ids, as you can see:
dat$anon_studentid[1001] == dat$anon_studentid[1000]
dat$anon_studentid[10226] == dat$anon_studentid[10225]

#we could remove these from the data so that the anon_studentid can work as a unique identifier, but we leave it for now for consistency


# ------------- Exercise 2 -------------

#dat.surv = dat[c(1,grep("stud_base",varnames))]

#alternatively
library(dplyr)
dat.surv = dat %>% select(c("anon_studentid",varnames[grep("stud_base",varnames)]))
dat.admin = dat %>% select(c("anon_studentid",varnames[grep("alladmin",varnames)]))

remvars = varnames[-append(grep("stud_base",varnames),grep("alladmin",varnames))]
dat.rest =  dat[remvars]


# ------------- Exercise 3 -------------
survVarNames = names(dat.surv)
adminVarNames = names(dat.admin)
restVarNames = names(dat.rest)

#GENDER

survVarNames[grep("gender",tolower(names(dat.surv)))]
adminVarNames[grep("gender",tolower(names(dat.admin)))]
restVarNames[grep("gender",tolower(names(dat.rest)))]
#dat.gencompare = table()

dat.gencompare = dat %>% select(c("anon_studentid","stud_base_GENDER","alladmin_GENDER","GENDER"))
# can use mutate?

incGender = dat.gencompare %>% filter(stud_base_GENDER != alladmin_GENDER | stud_base_GENDER != GENDER | alladmin_GENDER != GENDER)
# This shows all inconsistent rows - 27106 entries.
consGender = dat.gencompare %>% filter(stud_base_GENDER == alladmin_GENDER & stud_base_GENDER == GENDER)
# Alternatively, this gives the 15868 entries which are consistent
#To explore further,
emptyGender = dat.gencompare %>% filter(stud_base_GENDER == "" & GENDER == "" & alladmin_GENDER == "")
nonemptyGender = dat.gencompare %>% filter(stud_base_GENDER != "" & GENDER != "" & alladmin_GENDER != "")
#The lengths of these filtered data add up to the length of the consistent data, meaning the data is only inconsistent when one or two of the gender variables are blank, but not all. This implies the data which is there is consistent, just that a lot of data is missing.


#AGE

survVarNames[grep("age",tolower(names(dat.surv)))]
adminVarNames[grep("age",tolower(names(dat.admin)))]
restVarNames[grep("age",tolower(names(dat.rest)))]
#Use "stud_base_age", "alladmin_age", and "stud_fu1_age"

dat.agecompare = dat %>% select(c("anon_studentid","stud_base_age","alladmin_age","stud_fu1_age"))

incAge1 = dat.agecompare %>% filter(stud_base_age != alladmin_age | stud_base_age != stud_fu1_age | alladmin_age != stud_fu1_age)
consAge1 = dat.agecompare %>% filter(stud_base_age == alladmin_age & stud_base_age == stud_fu1_age & alladmin_age == stud_fu1_age)
#The size of these two should total to 42974, but they don't. This is because there are many entries which take NA in one of the age variables.
isNAAge = dat.agecompare %>% filter(is.na(stud_base_age) | is.na(stud_fu1_age) | is.na(alladmin_age))
#32650 of the entries have NA somewhere
#We will define these as inconsistent only when ages are actually different, not counting NA as different from any number
inconsAge = dat.agecompare %>% filter(!((stud_base_age == alladmin_age & stud_base_age == stud_fu1_age) | (is.na(stud_base_age) & alladmin_age == stud_fu1_age) | (is.na(alladmin_age) & stud_base_age == stud_fu1_age) | (is.na(stud_fu1_age) & alladmin_age == stud_base_age) | (is.na(stud_base_age) & is.na(stud_fu1_age)) | (is.na(stud_base_age) & is.na(alladmin_age)) | (is.na(alladmin_age) & is.na(stud_fu1_age))))
nrow(inconsAge)
#There are 10350 of these inconsistent entries


#BECE

varnames[grep("bece_best",tolower(varnames))]
varnames[grep("bece_worst",tolower(varnames))]
varnames[grep("bece_likely",tolower(varnames))]

dat.bececompare = dat %>% select(c("anon_studentid","stud_base_bece_best","stud_base_bece_worst","stud_base_bece_likely"))

dat.bececompare = mutate(dat.bececompare,stud_base_bece_best<stud_base_bece_worst)
dat.bececompare = mutate(dat.bececompare,stud_base_bece_best<stud_base_bece_likely)
dat.bececompare = mutate(dat.bececompare,stud_base_bece_likely<stud_base_bece_worst)
names(dat.bececompare)[5:7] = c("incon_bece","over_bece","under_bece")
#Now we have a table of all three questions to work with

#dat.bececompare[1:15,,]
#Lets us see how this is working

inconsBece = dat.bececompare %>% filter(incon_bece)
#10433 entries have this problem
overConfBece = dat.bececompare %>% filter(over_bece)
#7669 entries have this problem
underConfBece = dat.bececompare %>% filter(under_bece)
#9974 entries have this problem
consBece = dat.bececompare %>% filter(!under_bece & !incon_bece & !over_bece)
#If we discount entries with missing values, only 133 entries are actually consistent


# ------------- Exercise 4 -------------

varnames[grep("treat",tolower(varnames))]
#Use "treatgroup"
tr_group = as.factor(dat$treatgroup)
levels(tr_group)
# This just shows there are 6 different treatment groups

genderBalance = dat %>% group_by(treatgroup) %>% count(GENDER) %>% mutate(prop = n/sum(n))
#The large treatment groups 2, 4 and 6 seem to be fairly well balanced. Groups 1, 3 and 5 are too small to count as balanced.

ageBalance1 = dat %>% group_by(treatgroup) %>% count(stud_base_age) %>% mutate(prop = n/sum(n))
ageBalance2 = dat %>% group_by(stud_base_age) %>% count(treatgroup) %>% mutate(prop = n/sum(n))
#Ages 14-21 seem to be balanced well between treatment groups 2, 4 and 6, but the other age groups are too small to be balanced well.

regionBal1 = dat %>% group_by(treatgroup) %>% count(SHSregionname) %>% mutate(prop = n/sum(n))
regionBal2 = dat %>% group_by(SHSregionname) %>% count(treatgroup) %>% mutate(prop = n/sum(n))
#Again ignoring the too small groups, the number of entries from Ashanti and U.West seem fairly well balanced for 2,4, and 6, but none of the others are.

# ------------- Exercise 5 -------------

varnames[grep("educ_want",tolower(varnames))]

educ_want = as.factor(dat$stud_base_educ_want)
levels(educ_want)
levels(educ_want)  = (c("Junior high school", "Technical or vocational training", "Senior high school", "Nursing or teacher training", "Polytechnic", "University"))

desEducGender = dat %>% select(c("GENDER")) %>% mutate(educ_want)
ggplot(desEducGender, aes(x = as.numeric(educ_want))) + geom_histogram(aes(color = GENDER), fill = "white", bins = 6)


# ------------- Exercise 6 -------------

#mychoice_1 ~ pgm
varnames[grep("mychoice",tolower(varnames))]
#Use "stud_base_mychoice_1_pgm", "stud_base_mychoice_2_pgm", "stud_base_mychoice_3_pgm", "stud_base_mychoice_4_pgm"

dat.choicepgm = dat  %>% select(c("anon_studentid","stud_base_mychoice_1_pgm", "stud_base_mychoice_2_pgm", "stud_base_mychoice_3_pgm", "stud_base_mychoice_4_pgm"))
dat.choicepgmFreq1 = dat.choicepgm %>% group_by(stud_base_mychoice_1_pgm) %>% count() %>% mutate(freq = n/42974)
dat.choicepgmFreq2 = dat.choicepgm %>% group_by(stud_base_mychoice_2_pgm) %>% count() %>% mutate(freq = n/42974)
dat.choicepgmFreq3 = dat.choicepgm %>% group_by(stud_base_mychoice_3_pgm) %>% count() %>% mutate(freq = n/42974)
dat.choicepgmFreq4 = dat.choicepgm %>% group_by(stud_base_mychoice_4_pgm) %>% count() %>% mutate(freq = n/42974)

# Use "stud_base_mychoice_1_region", "stud_base_mychoice_2_region", stud_base_mychoice_3_region", "stud_base_mychoice_4_region" 

dat.choiceReg = dat  %>% select(c("anon_studentid","stud_base_mychoice_1_region", "stud_base_mychoice_2_region", "stud_base_mychoice_3_region", "stud_base_mychoice_4_region"))
dat.choiceRegFreq1 = dat.choiceReg %>% group_by(stud_base_mychoice_1_region) %>% count() %>% mutate(freq = n/42974)
dat.choiceRegFreq2 = dat.choiceReg %>% group_by(stud_base_mychoice_2_region) %>% count() %>% mutate(freq = n/42974)
dat.choiceRegFreq3 = dat.choiceReg %>% group_by(stud_base_mychoice_3_region) %>% count() %>% mutate(freq = n/42974)
dat.choiceRegFreq4 = dat.choiceReg %>% group_by(stud_base_mychoice_4_region) %>% count() %>% mutate(freq = n/42974)

# Use stud_base_mychoice_1_bece", "stud_base_mychoice_2_bece", "stud_base_mychoice_3_bece", "stud_base_mychoice_4_bece" 

dat.choiceBece = dat  %>% select(c("anon_studentid","stud_base_mychoice_1_bece", "stud_base_mychoice_2_bece", "stud_base_mychoice_3_bece", "stud_base_mychoice_4_bece"))
dat.choiceBeceSumm1 = dat.choiceBece %>% summarise(mean(stud_base_mychoice_1_bece, na.rm = TRUE),sd(stud_base_mychoice_1_bece, na.rm = TRUE),quantile(stud_base_mychoice_1_bece, probs = 0.25, na.rm = TRUE), quantile(stud_base_mychoice_1_bece, probs = 0.75, na.rm = TRUE))
dat.choiceBeceSumm2 = dat.choiceBece %>% summarise(mean(stud_base_mychoice_2_bece, na.rm = TRUE),sd(stud_base_mychoice_2_bece, na.rm = TRUE),quantile(stud_base_mychoice_2_bece, probs = 0.25, na.rm = TRUE), quantile(stud_base_mychoice_2_bece, probs = 0.75, na.rm = TRUE))
dat.choiceBeceSumm3 = dat.choiceBece %>% summarise(mean(stud_base_mychoice_3_bece, na.rm = TRUE),sd(stud_base_mychoice_3_bece, na.rm = TRUE),quantile(stud_base_mychoice_3_bece, probs = 0.25, na.rm = TRUE), quantile(stud_base_mychoice_3_bece, probs = 0.75, na.rm = TRUE))
dat.choiceBeceSumm4 = dat.choiceBece %>% summarise(mean(stud_base_mychoice_4_bece, na.rm = TRUE),sd(stud_base_mychoice_4_bece, na.rm = TRUE),quantile(stud_base_mychoice_4_bece, probs = 0.25, na.rm = TRUE), quantile(stud_base_mychoice_4_bece, probs = 0.75, na.rm = TRUE))

#To collate and plot these
names(dat.choiceBeceSumm1) = c("mean","sd","quant25","quant75")
names(dat.choiceBeceSumm2) = c("mean","sd","quant25","quant75")
names(dat.choiceBeceSumm3) = c("mean","sd","quant25","quant75")
names(dat.choiceBeceSumm4) = c("mean","sd","quant25","quant75")
dat.choiceBeceSumm = rbind(dat.choiceBeceSumm1,dat.choiceBeceSumm2,dat.choiceBeceSumm3,dat.choiceBeceSumm4)
choicesBece = c("stud_base_mychoice_1_bece", "stud_base_mychoice_2_bece", "stud_base_mychoice_3_bece", "stud_base_mychoice_4_bece")
dat.choiceBeceSumm = cbind(choicesBece, dat.choiceBeceSumm)
names(dat.choiceBeceSumm)[1] = "Choice"

plot(x = dat.choiceBeceSumm$Choice, y = dat.choiceBeceSumm$mean)
plot(x = dat.choiceBeceSumm$Choice, y = dat.choiceBeceSumm$sd)
plot(x = dat.choiceBeceSumm$Choice, y = dat.choiceBeceSumm$quant25)
plot(x = dat.choiceBeceSumm$Choice, y = dat.choiceBeceSumm$quant75)

#Random sample of 5 students who don't have NA entries
dat.choiceBecenoNA = na.omit(dat.choiceBece)
randsamp = sample(1:4621,5)
dat.choiceBecenoNA[randsamp,,]
#2 of these students have increasing bece's, one stays roughly constant, and 2 move around
revert = function(x1,x2,x3,x4,x5){
  if (x2>=x3 & x3>=x4 & x4>=x5){return (1)}
  else {return (0)}
}
#dat.choiceBece2 = dat.choiceBece  %>% mutate(reverting = revert())
#Doesn't work

# ------------- Exercise 7 -------------

