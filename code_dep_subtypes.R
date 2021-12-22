#############################################################################################################################################################
######################## R code used to create subtypes of depression described in the study  DOI: 10.1038/s41386-021-01059-6 ###############################
#############################################################################################################################################################

#### Reference to the study:
#### Fabbri C, Pain O, Hagenaars SP, Lewis CM, Serretti A. Transcriptome-wide association study of treatment-resistant depression and depression subtypes for drug repurposing. Neuropsychopharmacology. 2021;46(10):1821-1829. doi:10.1038/s41386-021-01059-6


#### This study examined the association between treatment-resistant depression (TRD) and depression subtypes to perform a transcriptome-wide association study of TRD and associated depression subtypes (vs healthy controls) for drug repurposing


#### TRD was defined according to a previous study: 
#### Fabbri, C., Hagenaars, S.P., John, C. et al. Genetic and clinical characteristics of treatment-resistant depression using primary care records in two UK cohorts. Mol Psychiatry 26, 3363–3373 (2021). https://doi.org/10.1038/s41380-021-01062-9
#### the R code used to define TRD is available at: https://github.com/chiarafabbri/MDD_TRD_study and the TRD phenotype was returned to UK Biobank


#### subtypes of depression examined for association with TRD:

	## Depression with typical neurovegetative symptoms: individuals with a history of lifetime MDD according to the Composite International Diagnostic Interview Short Form (CIDI-SF) who reported appetite/weight decrease and insomnia during the depressive episode according to the CIDI-SF. 

	## Depression with atypical neurovegetative symptoms: individuals with a history of lifetime MDD according to the CIDI-SF who reported appetite/weight increase and hypersomnia during the depressive episode according to the CIDI-SF.

	## Depression with weight gain: individuals with a history of lifetime MDD according to the CIDI-SF who reported weight increase during the depressive episode according to the CIDI-SF. 

	## Depression with anxiety: individuals satisfying one of the following:
		# 1) A history of lifetime MDD according to the CIDI-SF and at least two anxiety traits according to the short version of the Eysenck Personality Inventory Neuroticism scale (tense/'highly strung'; worrier/anxious feelings; nervous feelings) combined with trouble falling asleep during the reported depressive episode according to the CIDI-SF. 
		# 2) At least one diagnostic code for anxious depression according to primary care records (Read v3 codes E2003, Eu412 and X00Sb). 

	## Peripartum depression: individuals satisfying one of the following:
		# 1) At least one diagnostic code for post-partum depression according to primary care records (Read v3 codes XaY2C, XE1aY and E204. and Eu32B). 
		# 2) Having reported depression possibly related to childbirth (data field 20445) and a history of lifetime MDD according to the CIDI-SF. 

	## Psychotic, seasonal and endogenous depression: At least one diagnostic code for the corresponding depression subtype according to primary care records (Read v3 codes for psychotic depression: XE1ZZ, XSGon, XE1Ze, Eu323, Eu333; Read v3 code for seasonal depression: X761L; Read v3 codes for endogenous depression: X00SR, X00SS and XM1GC). 

	## Stress-related or reactive depression: participants satisfying one of the following:
		# 1)A history of lifetime MDD according to the CIDI-SF and having reported that depression started within two months after a stressful or traumatic event (data field 20447). 
		# 2)At least one diagnostic code for reactive depression according to primary care records (Read v3 code XE1YC). 


##### subtypes associated with TRD risk were used to run TWASs of that subtype vs healthy controls (without history of a psychiatric disorder) 


#### R version: R 3.6.0


#####################################################################
######## Depression with typical neurovegetative symptoms ###########
#####################################################################

### Definition of depression according to the CIDI-SF, which was part of the Mental Health Questionnaire (MHQ):

library(read.table) # loading required R library, check if installed

MHQ_Full <- fread("MHQ", h=T, data.table=F) ## read the required data fields, data fields are indicated as f. followed by the data field number, as shown in the UK Biobank data showcase

MHQ <- MHQ_Full[!is.na(MHQ_Full$f.20499.0.0), ] # selection of individuals without missing for this item ("Ever sought or received professional help for mental distress")

## CIDI-SF for defining lifetime depression:
MHQ$CIDI.MDD.No.Info <- with(MHQ,ifelse(((is.na(f.20446.0.0) | f.20446.0.0 < 0) &
                                       (is.na(f.20441.0.0) | f.20441.0.0 < 0 )), 1, 0)) # individuals who have no information on main items needed for depression diagnosis

MHQ$CIDI.MDD.Screen <- with(MHQ,ifelse(((!is.na(f.20446.0.0) & f.20446.0.0 == 1) |
                                      (!is.na(f.20441.0.0) & f.20441.0.0 == 1)) &
                                     (!is.na(f.20436.0.0) & f.20436.0.0 > 2) &
                                     (!is.na(f.20439.0.0) & f.20439.0.0 > 1) &
                                     (!is.na(f.20440.0.0) & f.20440.0.0 > 1), 1, 0)) # threshold for severity of depressive symptoms

MHQ$CIDI.MDD.Response <- 0 # calculate total n of depressive symptoms

MHQ$CIDI.MDD.Response<-with(MHQ, ifelse(!is.na(f.20446.0.0) & f.20446.0.0 > 0, CIDI.MDD.Response + 1, CIDI.MDD.Response))
MHQ$CIDI.MDD.Response<-with(MHQ, ifelse(!is.na(f.20441.0.0) & f.20441.0.0 > 0, CIDI.MDD.Response + 1, CIDI.MDD.Response))
MHQ$CIDI.MDD.Response<-with(MHQ, ifelse(!is.na(f.20449.0.0) & f.20449.0.0 > 0, CIDI.MDD.Response + 1, CIDI.MDD.Response))
MHQ$CIDI.MDD.Response<-with(MHQ, ifelse(!is.na(f.20536.0.0) & f.20536.0.0 > 0, CIDI.MDD.Response + 1, CIDI.MDD.Response))
MHQ$CIDI.MDD.Response<-with(MHQ, ifelse(!is.na(f.20532.0.0) & f.20532.0.0 > 0, CIDI.MDD.Response + 1, CIDI.MDD.Response))
MHQ$CIDI.MDD.Response<-with(MHQ, ifelse(!is.na(f.20435.0.0) & f.20435.0.0 > 0, CIDI.MDD.Response + 1, CIDI.MDD.Response))
MHQ$CIDI.MDD.Response<-with(MHQ, ifelse(!is.na(f.20450.0.0) & f.20450.0.0 > 0, CIDI.MDD.Response + 1, CIDI.MDD.Response))
MHQ$CIDI.MDD.Response<-with(MHQ, ifelse(!is.na(f.20437.0.0) & f.20437.0.0 > 0, CIDI.MDD.Response + 1, CIDI.MDD.Response))


## PHQ9 for recent depression:
MHQ$PHQ9.No.Info<-with(MHQ,ifelse((is.na(f.20514.0.0) | f.20514.0.0 < 0) &
                                  (is.na(f.20510.0.0) | f.20510.0.0 < 0),1,0))

MHQ$PHQ9.Screen<-with(MHQ,ifelse(((!is.na(f.20514.0.0) & f.20514.0.0 >= 2) |
                                  (!is.na(f.20510.0.0) & f.20510.0.0 >= 2)) &
                                 (!is.na(PHQ9.No.Info) & PHQ9.No.Info == 0),1,0))
MHQ$PHQ9.Items <- 0

MHQ$PHQ9.Items<-with(MHQ, ifelse(!is.na(f.20514.0.0) & f.20514.0.0 >= 3, PHQ9.Items + 1, PHQ9.Items))
MHQ$PHQ9.Items<-with(MHQ, ifelse(!is.na(f.20507.0.0) & f.20507.0.0 >= 3, PHQ9.Items + 1, PHQ9.Items))
MHQ$PHQ9.Items<-with(MHQ, ifelse(!is.na(f.20510.0.0) & f.20510.0.0 >= 3, PHQ9.Items + 1, PHQ9.Items))
MHQ$PHQ9.Items<-with(MHQ, ifelse(!is.na(f.20508.0.0) & f.20508.0.0 >= 3, PHQ9.Items + 1, PHQ9.Items))
MHQ$PHQ9.Items<-with(MHQ, ifelse(!is.na(f.20517.0.0) & f.20517.0.0 >= 3, PHQ9.Items + 1, PHQ9.Items))
MHQ$PHQ9.Items<-with(MHQ, ifelse(!is.na(f.20518.0.0) & f.20518.0.0 >= 3, PHQ9.Items + 1, PHQ9.Items))
MHQ$PHQ9.Items<-with(MHQ, ifelse(!is.na(f.20519.0.0) & f.20519.0.0 >= 3, PHQ9.Items + 1, PHQ9.Items))
MHQ$PHQ9.Items<-with(MHQ, ifelse(!is.na(f.20511.0.0) & f.20511.0.0 >= 3, PHQ9.Items + 1, PHQ9.Items))
MHQ$PHQ9.Items<-with(MHQ, ifelse(!is.na(f.20513.0.0) & f.20513.0.0 >= 2, PHQ9.Items + 1, PHQ9.Items))


MHQ$PHQ9.Severity<-with(MHQ, ifelse(f.20514.0.0 < 0 | is.na(f.20514.0.0), 0, f.20514.0.0-1) +
                             ifelse(f.20507.0.0 < 0 | is.na(f.20507.0.0), 0, f.20507.0.0-1) +
                             ifelse(f.20510.0.0 < 0 | is.na(f.20510.0.0), 0, f.20510.0.0-1) +
                             ifelse(f.20508.0.0 < 0 | is.na(f.20508.0.0), 0, f.20508.0.0-1) +
                             ifelse(f.20517.0.0 < 0 | is.na(f.20517.0.0), 0, f.20517.0.0-1) +
                             ifelse(f.20518.0.0 < 0 | is.na(f.20518.0.0), 0, f.20518.0.0-1) +
                             ifelse(f.20519.0.0 < 0 | is.na(f.20519.0.0), 0, f.20519.0.0-1) +
                             ifelse(f.20513.0.0 < 0 | is.na(f.20513.0.0), 0, f.20513.0.0-1) +
                             ifelse(f.20511.0.0 < 0 | is.na(f.20511.0.0), 0, f.20511.0.0-1))


## defining depression according to the CIDI, setting as 0 those without depression according to the CIDI as well as no recent depression (PHQ9) and no self-reported depression (SRDepression and InterviewDepression):
MHQ$SRDepression<-with(MHQ, ifelse(is.na(f.20499.0.0), NA,
                            ifelse((!is.na(f.20544.0.1) & f.20544.0.1 == 11) |
                                   (!is.na(f.20544.0.2) & f.20544.0.2 == 11) |
                                   (!is.na(f.20544.0.3) & f.20544.0.3 == 11) |
                                   (!is.na(f.20544.0.4) & f.20544.0.4 == 11) |
                                   (!is.na(f.20544.0.5) & f.20544.0.5 == 11) |
                                   (!is.na(f.20544.0.6) & f.20544.0.6 == 11) |
                                   (!is.na(f.20544.0.7) & f.20544.0.7 == 11) |
                                   (!is.na(f.20544.0.8) & f.20544.0.8 == 11) |
                                   (!is.na(f.20544.0.9) & f.20544.0.9 == 11) |
                                   (!is.na(f.20544.0.10) & f.20544.0.10 == 11) |
                                   (!is.na(f.20544.0.11) & f.20544.0.11 == 11) |
                                   (!is.na(f.20544.0.12) & f.20544.0.12 == 11) |
                                   (!is.na(f.20544.0.13) & f.20544.0.13 == 11) |
                                   (!is.na(f.20544.0.14) & f.20544.0.14 == 11) |
                                   (!is.na(f.20544.0.15) & f.20544.0.15 == 11) |
                                   (!is.na(f.20544.0.16) & f.20544.0.16 == 11), 1, 0)))

InterviewDepression<-apply(MHQ[,grep("f.20002.0", colnames(MHQ))], 1, function(row) "1286" %in% row)

MHQ$InterviewDepression[c(InterviewDepression)]<-1


MHQ$Depressed.Ever<-with(MHQ, ifelse(CIDI.MDD.No.Info == 1, NA,
                              ifelse(CIDI.MDD.Screen == 1 & (!is.na(CIDI.MDD.Response) & CIDI.MDD.Response > 4), 1,
                              ifelse(CIDI.MDD.Screen == 0 &
                                     (!is.na(PHQ9.Severity) & PHQ9.Severity < 5) &
                                     (is.na(SRDepression) | (!is.na(SRDepression) & SRDepression == 0)) &
                                     (is.na(InterviewDepression) | (!is.na(InterviewDepression) & InterviewDepression == 0)), 0, NA))))

## recent depression (PHQ9):
MHQ$Depressed.Current<-with(MHQ, ifelse(is.na(Depressed.Ever) | PHQ9.No.Info == 1, NA,
                                 ifelse(Depressed.Ever == 0 | (!is.na(PHQ9.Severity) & PHQ9.Severity < 5), 0,
                                 ifelse(Depressed.Ever == 1 & (!is.na(PHQ9.Screen) & PHQ9.Screen == 1) &
                                        (!is.na(PHQ9.Items) & PHQ9.Items > 4), 1, 0))))

## past depression (CIDI-SF):
MHQ$Depressed.past<-with(MHQ, ifelse((CIDI.MDD.No.Info == 1), NA,
                              ifelse((CIDI.MDD.Screen == 1 & !is.na(CIDI.MDD.Response) & CIDI.MDD.Response > 4), 1, 0)))



## bipolar disorder 
# Smith et al definition (data field 20126)
# wider defined bipolar disorder: mania plus three manifestations, or irritability plus four manifestations, for a week or more):
# self-reported mania

MHQ$SmithBipolar<-with(MHQ, ifelse(is.na(f.20126.0.0), NA,
                            ifelse(!is.na(f.20126.0.0) & f.20126.0.0 < 3 & f.20126.0.0 > 0, 1, 0)))

MHQ$Total.Manifestations <- 0

MHQ$Total.Manifestations<-with(MHQ, ifelse(!is.na(f.20548.0.1) & f.20548.0.1 > 0, Total.Manifestations + 1, Total.Manifestations))
MHQ$Total.Manifestations<-with(MHQ, ifelse(!is.na(f.20548.0.2) & f.20548.0.2 > 0, Total.Manifestations + 1, Total.Manifestations))
MHQ$Total.Manifestations<-with(MHQ, ifelse(!is.na(f.20548.0.3) & f.20548.0.3 > 0, Total.Manifestations + 1, Total.Manifestations))
MHQ$Total.Manifestations<-with(MHQ, ifelse(!is.na(f.20548.0.4) & f.20548.0.4 > 0, Total.Manifestations + 1, Total.Manifestations))
MHQ$Total.Manifestations<-with(MHQ, ifelse(!is.na(f.20548.0.5) & f.20548.0.5 > 0, Total.Manifestations + 1, Total.Manifestations))
MHQ$Total.Manifestations<-with(MHQ, ifelse(!is.na(f.20548.0.6) & f.20548.0.6 > 0, Total.Manifestations + 1, Total.Manifestations))
MHQ$Total.Manifestations<-with(MHQ, ifelse(!is.na(f.20548.0.7) & f.20548.0.7 > 0, Total.Manifestations + 1, Total.Manifestations))
MHQ$Total.Manifestations<-with(MHQ, ifelse(!is.na(f.20548.0.8) & f.20548.0.8 > 0, Total.Manifestations + 1, Total.Manifestations))

MHQ$SRManiaBIP<-with(MHQ, ifelse(is.na(f.20499.0.0), NA,
                          ifelse((!is.na(f.20544.0.1) & f.20544.0.1 == 10) |
                                 (!is.na(f.20544.0.2) & f.20544.0.2 == 10) |
                                 (!is.na(f.20544.0.3) & f.20544.0.3 == 10) |
                                 (!is.na(f.20544.0.4) & f.20544.0.4 == 10) |
                                 (!is.na(f.20544.0.5) & f.20544.0.5 == 10) |
                                 (!is.na(f.20544.0.6) & f.20544.0.6 == 10) |
                                 (!is.na(f.20544.0.7) & f.20544.0.7 == 10) |
                                 (!is.na(f.20544.0.8) & f.20544.0.8 == 10) |
                                 (!is.na(f.20544.0.9) & f.20544.0.9 == 10) |
                                 (!is.na(f.20544.0.10) & f.20544.0.10 == 10) |
                                 (!is.na(f.20544.0.11) & f.20544.0.11 == 10) |
                                 (!is.na(f.20544.0.12) & f.20544.0.12 == 10) |
                                 (!is.na(f.20544.0.13) & f.20544.0.13 == 10) |
                                 (!is.na(f.20544.0.14) & f.20544.0.14 == 10) |
                                 (!is.na(f.20544.0.15) & f.20544.0.15 == 10) |
                                 (!is.na(f.20544.0.16) & f.20544.0.16 == 10), 1, 0)))


MHQ$Wider.Bipolar.Definition<-with(MHQ, ifelse(((is.na(f.20501.0.0) | f.20501.0.0 < 0) &
                                                (is.na(f.20502.0.0) | f.20502.0.0 < 0)), NA,
                                        ifelse(((f.20501.0.0 == 1 &
                                                MHQ$Total.Manifestations > 2) |
                                               (f.20502.0.0 == 1 &
                                                MHQ$Total.Manifestations > 3)) &
                                              !is.na(f.20492.0.0) & f.20492.0.0 == 3, 1,
                                        ifelse((!is.na(SRManiaBIP) & SRManiaBIP == 0) & (is.na(SmithBipolar) | (!is.na(SmithBipolar) & SmithBipolar == 0)), 0, NA))))


## psychotic disorders:

MHQ$SRSchizophrenia<-with(MHQ, ifelse(is.na(f.20499.0.0), NA,
                               ifelse((!is.na(f.20544.0.1) & f.20544.0.1 == 2) |
                                      (!is.na(f.20544.0.2) & f.20544.0.2 == 2) |
                                      (!is.na(f.20544.0.3) & f.20544.0.3 == 2) |
                                      (!is.na(f.20544.0.4) & f.20544.0.4 == 2) |
                                      (!is.na(f.20544.0.5) & f.20544.0.5 == 2) |
                                      (!is.na(f.20544.0.6) & f.20544.0.6 == 2) |
                                      (!is.na(f.20544.0.7) & f.20544.0.7 == 2) |
                                      (!is.na(f.20544.0.8) & f.20544.0.8 == 2) |
                                      (!is.na(f.20544.0.9) & f.20544.0.9 == 2) |
                                      (!is.na(f.20544.0.10) & f.20544.0.10 == 2) |
                                      (!is.na(f.20544.0.11) & f.20544.0.11 == 2) |
                                      (!is.na(f.20544.0.12) & f.20544.0.12 == 2) |
                                      (!is.na(f.20544.0.13) & f.20544.0.13 == 2) |
                                      (!is.na(f.20544.0.14) & f.20544.0.14 == 2) |
                                      (!is.na(f.20544.0.15) & f.20544.0.15 == 2) |
                                      (!is.na(f.20544.0.16) & f.20544.0.16 == 2), 1, 0)))

MHQ$SRPsychosisOther<-with(MHQ, ifelse(is.na(f.20499.0.0), NA,
                                ifelse((!is.na(f.20544.0.1) & f.20544.0.1 == 3) |
                                       (!is.na(f.20544.0.2) & f.20544.0.2 == 3) |
                                       (!is.na(f.20544.0.3) & f.20544.0.3 == 3) |
                                       (!is.na(f.20544.0.4) & f.20544.0.4 == 3) |
                                       (!is.na(f.20544.0.5) & f.20544.0.5 == 3) |
                                       (!is.na(f.20544.0.6) & f.20544.0.6 == 3) |
                                       (!is.na(f.20544.0.7) & f.20544.0.7 == 3) |
                                       (!is.na(f.20544.0.8) & f.20544.0.8 == 3) |
                                       (!is.na(f.20544.0.9) & f.20544.0.9 == 3) |
                                       (!is.na(f.20544.0.10) & f.20544.0.10 == 3) |
                                       (!is.na(f.20544.0.11) & f.20544.0.11 == 3) |
                                       (!is.na(f.20544.0.12) & f.20544.0.12 == 3) |
                                       (!is.na(f.20544.0.13) & f.20544.0.13 == 3) |
                                       (!is.na(f.20544.0.14) & f.20544.0.14 == 3) |
                                       (!is.na(f.20544.0.15) & f.20544.0.15 == 3) |
                                       (!is.na(f.20544.0.16) & f.20544.0.16 == 3), 1, 0)))

MHQ$SRPsychosisAny<-with(MHQ, ifelse(is.na(f.20499.0.0), NA,
                                         ifelse((!is.na(SRSchizophrenia) & SRSchizophrenia == 1) | (!is.na(SRPsychosisOther) & SRPsychosisOther == 1), 1, 0)))



## exclude bipolar, psychotic and substance use disorders from cases with depression by setting them to missing:

excl <- MHQ[(!is.na(MHQ$SRPsychosisAny) & MHQ$SRPsychosisAny == 1) | 
		(!is.na(MHQ$Wider.Bipolar.Definition) & MHQ$Wider.Bipolar.Definition == 1) |
		(!is.na(MHQ$f.20401.0.0) & MHQ$f.20401.0.0 > 0) | # ever addicted to any substance or behaviour
		(!is.na(MHQ$f.20406.0.0) & MHQ$f.20406.0.0 > 0),] # ever addicted to alcohol


MHQ$Depressed.past <- ifelse( (!is.na(MHQ$Depressed.past) & MHQ$eid %in% excl$eid), NA, MHQ$Depressed.past)

MHQ$Depressed.Ever <- ifelse( (!is.na(MHQ$Depressed.Ever) & MHQ$eid %in% excl$eid), NA, MHQ$Depressed.Ever)



#####################################################################
######### Depression with typical neurovegetative symptoms ##########
#####################################################################

MHQ$typical.past <- 0

MHQ$typical.past <- with(MHQ, ifelse(is.na(Depressed.past), NA,
                ifelse(Depressed.past == 1 & !is.na(f.20536.0.0) & f.20536.0.0 == 2, typical.past + 1, typical.past))) # Weight loss

MHQ$typical.past <- with(MHQ, ifelse(Depressed.past == 1 & (!is.na(f.20535.0.0) & f.20535.0.0 == 1 | !is.na(f.20533.0.0) & f.20533.0.0 == 1), typical.past + 1, typical.past)) # Waking too early or trouble falling asleep

MHQ$typical.past.yes <- with(MHQ, ifelse(is.na(typical.past), NA,
                ifelse((!is.na(typical.past) & typical.past == 2),1,0)))



#####################################################################
######## Depression with atypical neurovegetative symptoms ##########
#####################################################################

MHQ$atypical.past <- 0

MHQ$atypical.past <- with(MHQ, ifelse(is.na(Depressed.past), NA,
          ifelse(Depressed.past == 1 & !is.na(f.20534.0.0) & f.20534.0.0 == 1, atypical.past + 1, atypical.past))) # Sleeping too much, coded as 0 and 1

MHQ$atypical.past <- with(MHQ, ifelse(Depressed.past == 1 & !is.na(f.20536.0.0) & f.20536.0.0 == 1, atypical.past + 1, atypical.past)) # Weight change during worst episode of depression, 0=no change, 1=weight gain, 2=weight loss, 3=both gained and lost weight, <0 do not know

MHQ$atypical.past.yes <- with(MHQ, ifelse(is.na(atypical.past), NA,
          ifelse((!is.na(atypical.past) & atypical.past > 1), 1, 0)))



#####################################################################
############### Depression with weight gain #########################
#####################################################################

MHQ$weight.gain <- with(MHQ, ifelse(!is.na(Depressed.past) & Depressed.past == 1 & !is.na(f.20536.0.0) & f.20536.0.0 == 1), 1,
				ifelse((is.na(Depressed.past) | is.na(f.20536.0.0) | f.20536.0.0 < 0), NA, 0))



#####################################################################
#################### Depression with anxiety ########################
#####################################################################

## from primary care data:

dep <- fread("mood_BP_SU_codes.txt", h=T, data.table=F) # this table was extracted and annotated from the primary care table 'gp_clinical.txt', it includes diagnostic records of depressive disorders, psychotic, bipolar and substance use disorders,
# see https://github.com/chiarafabbri/MDD_TRD_study/blob/master/scripts/extract_diagn_ADs_TRD_pheno.R for information on how the table was created

anxious_dep_GP <- dep[dep$read_3 == 'E2003' |
dep$read_3 == 'Eu412' |
dep$read_3 == 'X00Sb',]

# exclusion of bipolar, psychotic and substance use disorders according to primary care records:
excl <- dep[dep$type == 'bipolar_disorder' | 
	dep$type == 'psychosis' | 
	dep$type == 'substance_abuse',]

excl <- excl[!duplicated(excl$eid),]

anxious_dep_GP <- anxious_dep_GP[!anxious_dep_GP$eid %in% excl$eid,]

dep2 <- dep[dep$type == 'depression',]
dep2 <- dep2[!duplicated(dep2$eid),]
dep2 <- dep2[!dep2$eid %in% excl$eid,] # cases of depression without comorbidities in primary care data

anxious_dep_GP <- data.frame(eid=dep2$eid, 
anxious_dep_GP=ifelse(dep2$eid %in% anxious_dep_GP$eid, 1, 0))


## from MHQ data:

# calculating n of anxiety traits, considering data fields 1990, 1980 and 1970, after setting negative values to missing:
MHQ$f.1990.0.0[MHQ$f.1990.0.0 < 0] <- NA
MHQ$f.1980.0.0[MHQ$f.1980.0.0 < 0] <- NA
MHQ$f.1970.0.0[MHQ$f.1970.0.0 < 0] <- NA

MHQ$anx <- apply(MHQ[35:37], 1, FUN <- function(x) sum(x, na.rm=T)) # CHECK and CHANGE col number to make it correspond to data fields 1990, 1980 and 1970

MHQ$anxious_dep <- ifelse((!is.na(MHQ$anx) & MHQ$anx >=2 & MHQ$f.20533.0.0 == 1 & !is.na(MHQ$f.20533.0.0)), 1,
        ifelse(is.na(MHQ$anx) | is.na(MHQ$f.20533.0.0), NA, 0)) 

anxious_dep_MHQ <- data.frame(eid=MHQ$eid, 
anxious_dep_MHQ=MHQ$anxious_dep, 
depressed.ever=MHQ$Depressed.Ever)

anxious_dep_MHQ <- anxious_dep_MHQ[(!is.na(anxious_dep_MHQ$depressed.ever) 
			& anxious_dep_MHQ$depressed.ever == 1),]


## all cases with anxious depression:

anx_all <- merge(anxious_dep_GP, 
anxious_dep_MHQ, by='eid', all=T)

anx_all$anxious_dep_all <- ifelse( (!is.na(anx_all$anxious_dep_GP) & anx_all$anxious_dep_GP == 1) |
				(!is.na(anx_all$anxious_dep) & anx_all$anxious_dep == 1), 1, ifelse(
				is.na(anx_all$anxious_dep_GP) & is.na(anx_all$anxious_dep), NA, 0))



#####################################################################
#################### Peripartum depression ##########################
#####################################################################

## from primary care data:
post_partum_dep <- dep[dep$read_3 == 'XaY2C' |
dep$read_3 == 'XE1aY' |
dep$read_3 == 'E204.'| dep$read_3 == 'Eu32B',]
post_partum_dep <- post_partum_dep[!post_partum_dep$eid %in% excl$eid,]

post_partum_dep_GP <- data.frame(eid=dep2$eid, 
post_partum_dep_GP=ifelse(dep2$eid %in% post_partum_dep$eid, 1, 0))

## depression possibly related to childbirth according to the MHQ:

dep_ever <- MHQ[MHQ$Depressed.Ever == 1 & !is.na(MHQ$Depressed.Ever),]

MHQ$peripartum_MHQ <- ifelse(!is.na(MHQ$f.20445.0.0) & MHQ$f.20445.0.0 == 1 & MHQ$eid %in% dep_ever$eid, 1, 
			ifelse(!is.na(MHQ$f.20445.0.0) & MHQ$f.20445.0.0 == 0 & MHQ$eid %in% dep_ever$eid, 0, NA))

peripartum_MHQ <- data.frame(eid=MHQ$eid,
peripartum_MHQ=MHQ$peripartum_MHQ,
depressed.ever=MHQ$Depressed.Ever)

peripartum_MHQ <- peripartum_MHQ[!is.na(peripartum_MHQ$depressed.ever) & 
peripartum_MHQ$depressed.ever == 1,]


## all cases with peripartum depression:

peripartum_all <- merge(post_partum_dep_GP, 
peripartum_MHQ, by='eid', all=T)

peripartum_all$peripartum_all <- ifelse( (!is.na(peripartum_all$post_partum_dep_GP) & peripartum_all$post_partum_dep_GP == 1) |
					(!is.na(peripartum_all$peripartum_MHQ) & peripartum_all$peripartum_MHQ == 1), 1, 0) 


#####################################################################
########### Psychotic, seasonal and endogenous depression ###########
#####################################################################

## from primary care data:
endogenous_dep <- dep[dep$read_3 == 'X00SR' |
dep$read_3 == 'X00SS' |
dep$read_3 == 'XM1GC',]
endogenous_dep <- endogenous_dep[!endogenous_dep$eid %in% excl$eid,]
endogenous_dep <- endogenous_dep[!duplicated(endogenous_dep$eid),]


seasonal_dep <- dep[dep$read_3 == 'X761L',]
seasonal_dep <- seasonal_dep[!seasonal_dep$eid %in% excl$eid,]
seasonal_dep <- seasonal_dep[!duplicated(seasonal_dep$eid),]

psychotic_dep <- dep[dep$read_3 == 'XE1ZZ' |
dep$read_3 == 'XSGon' |
dep$read_3 == 'XE1Ze' |
dep$read_3 == 'Eu323' |
dep$read_3 == 'Eu333',]
psychotic_dep <- psychotic_dep[!psychotic_dep$eid %in% excl$eid,]
psychotic_dep <- psychotic_dep[!duplicated(psychotic_dep$eid),]


#####################################################################
############## Stress-related or reactive depression ################
#####################################################################

## from primary care data:
reactive_dep <- dep[dep$read_3 == 'XE1YC',]
reactive_dep <- reactive_dep[!reactive_dep$eid %in% excl$eid,] 
reactive_dep_GP <- data.frame(eid=dep2$eid, 
reactive_dep_GP=ifelse(dep2$eid %in% reactive_dep$eid, 1, 0))

## stress-related depression according to the MHQ:

MHQ$stress_dep <- ifelse(!is.na(MHQ$f.20447.0.0) & MHQ$f.20447.0.0 == 1 & MHQ$eid %in% dep_ever$eid, 1, 
                        ifelse(!is.na(MHQ$f.20447.0.0) & MHQ$f.20447.0.0 == 0 & MHQ$eid %in% dep_ever$eid, 0, NA))

stress_dep_MHQ <- data.frame(eid=MHQ$eid,
stress_dep_MHQ=MHQ$stress_dep,
depressed.ever=MHQ$Depressed.Ever)

stress_dep_MHQ <- stress_dep_MHQ[!is.na(stress_dep_MHQ$depressed.ever) & 
stress_dep_MHQ$depressed.ever == 1,]


## all stress-related/reactive depression cases:
reactive_dep_all <- merge(reactive_dep_GP, 
stress_dep_MHQ, by='eid', all=T)

reactive_dep_all$reactive_dep_all <- ifelse( (!is.na(reactive_dep_all$reactive_dep_GP) & reactive_dep_all$reactive_dep_GP == 1) |
						(!is.na(reactive_dep_all$stress_dep_MHQ) & reactive_dep_all$stress_dep_MHQ == 1), 1, 0)




####################################################################
######################## Healthy controls ##########################
####################################################################


### Excluding from MHQ individuals with anxiety, depression, bipolar, psychotic and substance-use related disorders:

## creating diagnostic groups not previously extracted
MHQ$SROtherPhobia<-with(MHQ, ifelse(is.na(f.20499.0.0), NA,
                             ifelse((!is.na(f.20544.0.1) & f.20544.0.1 == 5) |
                                    (!is.na(f.20544.0.2) & f.20544.0.2 == 5) |
                                    (!is.na(f.20544.0.3) & f.20544.0.3 == 5) |
                                    (!is.na(f.20544.0.4) & f.20544.0.4 == 5) |
                                    (!is.na(f.20544.0.5) & f.20544.0.5 == 5) |
                                    (!is.na(f.20544.0.6) & f.20544.0.6 == 5) |
                                    (!is.na(f.20544.0.7) & f.20544.0.7 == 5) |
                                    (!is.na(f.20544.0.8) & f.20544.0.8 == 5) |
                                    (!is.na(f.20544.0.9) & f.20544.0.9 == 5) |
                                    (!is.na(f.20544.0.10) & f.20544.0.10 == 5) |
                                    (!is.na(f.20544.0.11) & f.20544.0.11 == 5) |
                                    (!is.na(f.20544.0.12) & f.20544.0.12 == 5) |
                                    (!is.na(f.20544.0.13) & f.20544.0.13 == 5) |
                                    (!is.na(f.20544.0.14) & f.20544.0.14 == 5) |
                                    (!is.na(f.20544.0.15) & f.20544.0.15 == 5) |
                                    (!is.na(f.20544.0.16) & f.20544.0.16 == 5), 1, 0)))


MHQ$SRPanicAttacks<-with(MHQ, ifelse(is.na(f.20499.0.0), NA,
                              ifelse((!is.na(f.20544.0.1) & f.20544.0.1 == 6) |
                                     (!is.na(f.20544.0.2) & f.20544.0.2 == 6) |
                                     (!is.na(f.20544.0.3) & f.20544.0.3 == 6) |
                                     (!is.na(f.20544.0.4) & f.20544.0.4 == 6) |
                                     (!is.na(f.20544.0.5) & f.20544.0.5 == 6) |
                                     (!is.na(f.20544.0.6) & f.20544.0.6 == 6) |
                                     (!is.na(f.20544.0.7) & f.20544.0.7 == 6) |
                                     (!is.na(f.20544.0.8) & f.20544.0.8 == 6) |
                                     (!is.na(f.20544.0.9) & f.20544.0.9 == 6) |
                                     (!is.na(f.20544.0.10) & f.20544.0.10 == 6) |
                                     (!is.na(f.20544.0.11) & f.20544.0.11 == 6) |
                                     (!is.na(f.20544.0.12) & f.20544.0.12 == 6) |
                                     (!is.na(f.20544.0.13) & f.20544.0.13 == 6) |
                                     (!is.na(f.20544.0.14) & f.20544.0.14 == 6) |
                                     (!is.na(f.20544.0.15) & f.20544.0.15 == 6) |
                                     (!is.na(f.20544.0.16) & f.20544.0.16 == 6), 1, 0)))

MHQ$SROCD<-with(MHQ, ifelse(is.na(f.20499.0.0), NA,
                     ifelse((!is.na(f.20544.0.1) & f.20544.0.1 == 7) |
                            (!is.na(f.20544.0.2) & f.20544.0.2 == 7) |
                            (!is.na(f.20544.0.3) & f.20544.0.3 == 7) |
                            (!is.na(f.20544.0.4) & f.20544.0.4 == 7) |
                            (!is.na(f.20544.0.5) & f.20544.0.5 == 7) |
                            (!is.na(f.20544.0.6) & f.20544.0.6 == 7) |
                            (!is.na(f.20544.0.7) & f.20544.0.7 == 7) |
                            (!is.na(f.20544.0.8) & f.20544.0.8 == 7) |
                            (!is.na(f.20544.0.9) & f.20544.0.9 == 7) |
                            (!is.na(f.20544.0.10) & f.20544.0.10 == 7) |
                            (!is.na(f.20544.0.11) & f.20544.0.11 == 7) |
                            (!is.na(f.20544.0.12) & f.20544.0.12 == 7) |
                            (!is.na(f.20544.0.13) & f.20544.0.13 == 7) |
                            (!is.na(f.20544.0.14) & f.20544.0.14 == 7) |
                            (!is.na(f.20544.0.15) & f.20544.0.15 == 7) |
                            (!is.na(f.20544.0.16) & f.20544.0.16 == 7), 1, 0)))


MHQ$SRAgoraphobia<-with(MHQ, ifelse(is.na(f.20499.0.0), NA,
                             ifelse((!is.na(f.20544.0.1) & f.20544.0.1 == 17) |
                                    (!is.na(f.20544.0.2) & f.20544.0.2 == 17) |
                                    (!is.na(f.20544.0.3) & f.20544.0.3 == 17) |
                                    (!is.na(f.20544.0.4) & f.20544.0.4 == 17) |
                                    (!is.na(f.20544.0.5) & f.20544.0.5 == 17) |
                                    (!is.na(f.20544.0.6) & f.20544.0.6 == 17) |
                                    (!is.na(f.20544.0.7) & f.20544.0.7 == 17) |
                                    (!is.na(f.20544.0.8) & f.20544.0.8 == 17) |
                                    (!is.na(f.20544.0.9) & f.20544.0.9 == 17) |
                                    (!is.na(f.20544.0.10) & f.20544.0.10 == 17) |
                                    (!is.na(f.20544.0.11) & f.20544.0.11 == 17) |
                                    (!is.na(f.20544.0.12) & f.20544.0.12 == 17) |
                                    (!is.na(f.20544.0.13) & f.20544.0.13 == 17) |
                                    (!is.na(f.20544.0.14) & f.20544.0.14 == 17) |
                                    (!is.na(f.20544.0.15) & f.20544.0.15 == 17) |
                                    (!is.na(f.20544.0.16) & f.20544.0.16 == 17), 1, 0)))

MHQ$SRSocPhobia<-with(MHQ, ifelse(is.na(f.20499.0.0), NA,
                           ifelse((!is.na(f.20544.0.1) & f.20544.0.1 == 1) |
                                  (!is.na(f.20544.0.2) & f.20544.0.2 == 1) |
                                  (!is.na(f.20544.0.3) & f.20544.0.3 == 1) |
                                  (!is.na(f.20544.0.4) & f.20544.0.4 == 1) |
                                  (!is.na(f.20544.0.5) & f.20544.0.5 == 1) |
                                  (!is.na(f.20544.0.6) & f.20544.0.6 == 1) |
                                  (!is.na(f.20544.0.7) & f.20544.0.7 == 1) |
                                  (!is.na(f.20544.0.8) & f.20544.0.8 == 1) |
                                  (!is.na(f.20544.0.9) & f.20544.0.9 == 1) |
                                  (!is.na(f.20544.0.10) & f.20544.0.10 == 1) |
                                  (!is.na(f.20544.0.11) & f.20544.0.11 == 1) |
                                  (!is.na(f.20544.0.12) & f.20544.0.12 == 1) |
                                  (!is.na(f.20544.0.13) & f.20544.0.13 == 1) |
                                  (!is.na(f.20544.0.14) & f.20544.0.14 == 1) |
                                  (!is.na(f.20544.0.15) & f.20544.0.15 == 1) |
                                  (!is.na(f.20544.0.16) & f.20544.0.16 == 1), 1, 0)))

MHQ$SRGADandOthers<-with(MHQ, ifelse(is.na(f.20499.0.0), NA,
                              ifelse((!is.na(f.20544.0.1) & f.20544.0.1 == 15) |
                                     (!is.na(f.20544.0.2) & f.20544.0.2 == 15) |
                                     (!is.na(f.20544.0.3) & f.20544.0.3 == 15) |
                                     (!is.na(f.20544.0.4) & f.20544.0.4 == 15) |
                                     (!is.na(f.20544.0.5) & f.20544.0.5 == 15) |
                                     (!is.na(f.20544.0.6) & f.20544.0.6 == 15) |
                                     (!is.na(f.20544.0.7) & f.20544.0.7 == 15) |
                                     (!is.na(f.20544.0.8) & f.20544.0.8 == 15) |
                                     (!is.na(f.20544.0.9) & f.20544.0.9 == 15) |
                                     (!is.na(f.20544.0.10) & f.20544.0.10 == 15) |
                                     (!is.na(f.20544.0.11) & f.20544.0.11 == 15) |
                                     (!is.na(f.20544.0.12) & f.20544.0.12 == 15) |
                                     (!is.na(f.20544.0.13) & f.20544.0.13 == 15) |
                                     (!is.na(f.20544.0.14) & f.20544.0.14 == 15) |
                                     (!is.na(f.20544.0.15) & f.20544.0.15 == 15) |
                                     (!is.na(f.20544.0.16) & f.20544.0.16 == 15), 1, 0)))


MHQ$SRAnxietyAny<-with(MHQ, ifelse(is.na(f.20499.0.0), NA,
                            ifelse((!is.na(SRSocPhobia) & SRSocPhobia == 1) |
                                   (!is.na(SRGADandOthers) & SRGADandOthers == 1) |
                                   (!is.na(SRPanicAttacks) & SRPanicAttacks == 1) |
                                   (!is.na(SRAgoraphobia) & SRAgoraphobia == 1) |
                                   (!is.na(SROtherPhobia) & SROtherPhobia == 1) |
                                   (!is.na(SROCD) & SROCD == 1), 1, 0)))


controls <- MHQ[(MHQ$Depressed.Ever == 0 & 
		MHQ$SRPsychosisAny == 0 &
		MHQ$Anxiety.Any == 0 &
		MHQ$Wider.Bipolar.Definition == 0),]

controls <- controls[!is.na(controls[,1]),] # excluding missing


## extracting antidepressant prescriptions to exclude participants who reported treatment with antidepressants from controls:
antidepcodes<-c("1140879616","1140921600","1140879540","1140867878","1140916282","1140909806","1140867888","1141152732","1141180212","1140879634","1140867876","1140882236",
                "1141190158","1141200564","1140867726","1140879620","1140867818","1140879630","1140879628","1141151946","1140867948","1140867624","1140867756","1140867884",
                "1141151978","1141152736","1141201834","1140867690","1140867640","1140867920","1140867850","1140879544","1141200570","1140867934","1140867758","1140867914",
                "1140867820","1141151982","1140882244","1140879556","1140867852","1140867860","1140917460","1140867938","1140867856","1140867922","1140910820","1140882312",
                "1140867944","1140867784","1140867812","1140867668","1140867940")

DrugDepression<-apply(MHQ[,grep("f.20003.0", colnames(MHQ))], 1, function(row) antidepcodes %in% row)

DrugDepression_Combined<-numeric()

for(indiv in 1:dim(MHQ)[1]){
    DrugDepression_Combined[indiv]<-ifelse(sum(DrugDepression[(((indiv-1)*length(antidepcodes))+1):(indiv*length(antidepcodes))]) == 0, 0, 1)
}

MHQ$DrugDepression[DrugDepression_Combined==1]<-1

antidepressants <- MHQ[MHQ$DrugDepression == 1,]

controls <- controls[!controls$eid %in% antidepressants$eid,] # excluding those who reported treatment with antidepressants


### excluding also alcohol and substance-related disorders:
sub <- MHQ[(!is.na(MHQ$f.20401.0.0) & MHQ$f.20401.0.0 > 0) |
                (!is.na(MHQ$f.20406.0.0) & MHQ$f.20406.0.0 > 0),]

controls <- controls[!controls$eid %in% sub$eid,]


### excluding those with psychiatric diagnoses according to primary care records:

GP_codes1 <- fread("mood_BP_SU_codes.txt", h=T, data.table=F) # this table was extracted and annotated from the primary care table 'gp_clinical.txt', it includes diagnostic records of depressive disorders, psychotic, bipolar and substance use disorders, 
# see https://github.com/chiarafabbri/MDD_TRD_study/blob/master/scripts/extract_diagn_ADs_TRD_pheno.R for information on how this table was created

GP_codes2 <- fread("other_psych_diagnoses.txt",h=T) # other psychiatric diagnoses, extracted and annotated from the primary care table 'gp_clinical.txt', 
# see info at https://github.com/chiarafabbri/MDD_TRD_study/blob/master/scripts/extract_other_diagn_med.R 

# selection of diagnoses to be excluded:
GP_codes2 <- GP_codes2[GP_codes2$diagnostic_group == 'anxiety_disorders' | 
GP_codes2$diagnostic_group == 'anxiety_no_diagnosis' | 
GP_codes2$diagnostic_group == 'anxiety_symptoms' | 
GP_codes2$diagnostic_group == 'attempted_suicide_self_harm' | 
GP_codes2$diagnostic_group == 'depression_with_anxiety' | 
GP_codes2$diagnostic_group == 'depressive_symptoms' | 
GP_codes2$diagnostic_group == 'hypo_manic_symptoms' | 
GP_codes2$diagnostic_group == 'obsessive_compulsive_and_related_disorders' | 
GP_codes2$diagnostic_group == 'obsessive_compulsive_symptoms' | 
GP_codes2$diagnostic_group == 'psychogenic_psychosis' | 
GP_codes2$diagnostic_group == 'puerperium_disorders' | 
GP_codes2$diagnostic_group == 'self_harm_suicide' | 
GP_codes2$diagnostic_group == 'somatoform_anxiety_disorders' | 
GP_codes2$diagnostic_group == 'stress_related_disorders',]

controls <- controls[!controls$eid %in% GP_codes1$eid,]
controls <- controls[!controls$eid %in% GP_codes2$eid,]

## other measures of depression were also considered to select healthy controls and were included in the data.frame "dep_diagnoses"
	# Self-reported depression diagnosed by a professional (extracted from data field 20544)
	# Depression according to Smith et al definition (data field 20126)
	# Depression according to ICD-10 codes (data fields 41202 and 41204)
	# Seen GP or psychiatrist for nerves, anxiety, tension or depression (data fields 2100 and 2090)

dep_diagnoses <- fread("depression_phenotypes_ukb56514.txt", h=T, data.table=F) # see description above

controls <- controls[!controls$eid %in% dep_diagnoses$eid,]

# additionally, this set of controls was selected to include individuals with genome-wide genotypes passing quality control procedures indicated in the publication (doi:10.1038/s41386-021-01059-6)
# drop-outs at the time of the study were also exluded


## creating a dataset with cases of each depression subtype and healthy controls

# typical dep:
typical_dep <- MHQ[!is.na(MHQ$typical.past.yes) & MHQ$typical.past.yes == 1,]
typical_dep_controls <- data.frame(eid=c(typical_dep$eid, controls$eid),
			typical_dep_vs_controls=c(rep(1, length(typical_dep$eid)), rep(0, length(controls$eid))))

# atypical dep:
atypical_dep <- MHQ[!is.na(MHQ$atypical.past.yes) & MHQ$atypical.past.yes == 1,]
atypical_dep_controls <- data.frame(eid=c(atypical_dep$eid, controls$eid),
			atypical_dep_vs_controls=c(rep(1, length(atypical_dep$eid)), rep(0, length(controls$eid))))

# dep with weight gain:
weight_gain_dep <- MHQ[!is.na(MHQ$weight.gain) & MHQ$weight.gain == 1,]
weight_gain_dep_controls <- data.frame(eid=c(weight_gain_dep$eid, controls$eid),
                        weight_gain_dep_vs_controls=c(rep(1, length(weight_gain_dep$eid)), rep(0, length(controls$eid))))

# anxious dep:
anxious_dep <- anx_all[!is.na(anx_all$anxious_dep_all) & anx_all$anxious_dep_all == 1,]
anx_dep_controls <- data.frame(eid=c(anxious_dep$eid, controls$eid),
                        anxious_dep_vs_controls=c(rep(1, length(anxious_dep$eid)), rep(0, length(controls$eid))))

# peripartum dep:
peripartum_dep <- peripartum_all[!is.na(peripartum_all$peripartum_all) & peripartum_all$peripartum_all == 1,]
peripartum_dep_controls <- data.frame(eid=c(peripartum_dep$eid, controls$eid),
                        peripartum_dep_vs_controls=c(rep(1, length(peripartum_dep$eid)), rep(0, length(controls$eid))))

# psychotic dep:
psychotic_dep_controls <- data.frame(eid=c(psychotic_dep$eid, controls$eid),
			psychotic_dep_vs_controls = c(rep(1, length(psychotic_dep$eid)), rep(0, length(controls$eid))))			

# seasonal dep:
seasonal_dep_controls <- data.frame(eid=c(seasonal_dep$eid, controls$eid),
                        seasonal_dep_vs_controls = c(rep(1, length(seasonal_dep$eid)), rep(0, length(controls$eid))))

# endogenous dep:
endogenous_dep_controls <- data.frame(eid=c(endogenous_dep$eid, controls$eid),
                        endogenous_dep_vs_controls = c(rep(1, length(endogenous_dep$eid)), rep(0, length(controls$eid))))


# stress-related/reactive depression:
stress_rel_dep <- reactive_dep_all[!is.na(reactive_dep_all$reactive_dep_all) & reactive_dep_all$reactive_dep_all == 1,]
stress_rel_dep_controls <- data.frame(eid=c(stress_rel_dep$eid, controls$eid),
                        stress_rel_dep_vs_controls = c(rep(1, length(stress_rel_dep$eid)), rep(0, length(controls$eid))))
 

## merging in one data.frame:
all <- merge(typical_dep_controls, atypical_dep_controls, by='eid', all=T)
all <- merge(all, weight_gain_dep_controls, by='eid', all=T)
all <- merge(all, anx_dep_controls, by='eid', all=T)
all <- merge(all, peripartum_dep_controls, by='eid', all=T)
all <- merge(all, psychotic_dep_controls, by='eid', all=T)
all <- merge(all, seasonal_dep_controls, by='eid', all=T)
all <- merge(all, endogenous_dep_controls, by='eid', all=T)
all <- merge(all, stress_rel_dep_controls, by='eid', all=T)


write.table(all, "all_dep_subtypes_controls.csv", col.names=T, row.names=F, sep=',', quote=F) # dataset returned to UK Biobank
 



