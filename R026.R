######################################################################################
#################################### SETUP ###########################################
######################################################################################

	# change the language and date formatting to English if it is not already
		Sys.setenv(LANG = "EN")
		Sys.setlocale("LC_TIME", "English") # key, without this conversion to POSIXct does not work
		Sys.getlocale(category = "LC_ALL")
		
		setwd("F:/PolCa/Analysis/R/ProjectR026_control")
		getwd()
		# also see 
	
	# packages
		library(sqldf)
		library(stringr)
		library(lubridate)
		library(ggplot2)
		
	# import and inspect all the PCC data-frames
				
			# core
			
				# import and inspect
				ELEN = read.csv("PCC/ELEN.csv", header = TRUE, sep = ";")
				summary(ELEN)
				names(ELEN)
				
				ELDI = read.csv("PCC/ELDI.csv", header = TRUE, sep = ";")
				summary(ELDI)
				names(ELDI)
							
				# import and inspect
				ELLI = read.csv("PCC/ELLI.csv", header = TRUE, sep = ";")
				summary(ELLI)
				names(ELLI)
				
				# import and inspect
				FACT = read.csv("PCC/FACT.csv", header = TRUE, sep = ";")
				summary(FACT)
				names(FACT)
				
				# import and inspect
				MEME = read.csv("PCC/MEME.csv", header = TRUE, sep = ";")
				summary(MEME)
				names(MEME)
				
				# import and inspect
				PARE = read.csv("PCC/PARE.csv", header = TRUE, sep = ";")
				summary(PARE)
				names(PARE)
				
				# import and inspect
				PARL = read.csv("PCC/PARL.csv", header = TRUE, sep = ";")
				summary(PARL)
				names(PARL)
				
				# import and inspect
				PART = read.csv("PCC/PART.csv", header = TRUE, sep = ";")
				summary(PART)
				names(PART)
				
				# import and inspect
				POLI = read.csv("PCC/POLI.csv", header = TRUE, sep = ";")
				summary(POLI)
				names(POLI)
			
				# import and inspect
				RESE = read.csv("PCC/RESE.csv", header = TRUE, sep = ";")
				summary(RESE)
				names(RESE)
				
				# import and inspect
				QUOT = read.csv("PCC/QUOT.csv", header = TRUE, sep = ";")
				summary(QUOT)
				names(QUOT)
				
	
######################################################################################
#################################### DATA ############################################
######################################################################################

	# we start with getting ELEN level data-frames
		nrow(ELEN)
		ELEN$country <- substr(ELEN$elec_entry_id,1,2)
		table(ELEN$country)
		
		ELENBU <- sqldf("SELECT elec_entry_id, list_id, pers_id, listplace, country
						FROM ELEN
						")
	
	# then we merge a couple of POLI level things to inspect
		ELENBU <- sqldf("SELECT ELENBU.*, POLI.last_name, POLI.first_name, POLI.gender, POLI.birth_date
						FROM ELENBU LEFT JOIN POLI
						ON
						ELENBU.pers_id = POLI.pers_id
						")
		nrow(ELENBU)
		head(ELENBU)
		
	# merge in some ELLI characteristics
		ELENBU <- sqldf("SELECT ELENBU.*, ELLI.list_name, ELLI.parliament_id, ELLI.district_id, ELLI.list_length
						FROM ELENBU LEFT JOIN ELLI
						ON
						ELENBU.list_id = ELLI.list_id
						")
		nrow(ELENBU)
		head(ELENBU)
	
	# merge in some PARL data
		ELENBU <- sqldf("SELECT ELENBU.*, PARL.leg_period_start, PARL.leg_period_end
						FROM ELENBU LEFT JOIN PARL
						ON
						ELENBU.parliament_id = PARL.parliament_id
						")
		nrow(ELENBU)
		head(ELENBU)
	
	# and party membership
	
		# prepare the date formats
								
			ELENBU$leg_period_start_posoxctformat <- as.POSIXct(as.character(ELENBU$leg_period_start),format=c("%d%b%Y"))
			ELENBU$leg_period_start_monthlater <- ELENBU$leg_period_start_posoxctformat + months(1)
								
			ELENBU$leg_period_end_posoxctformat <- as.POSIXct(as.character(ELENBU$leg_period_end),format=c("%d%b%Y"))
			ELENBU$leg_period_end_monthearlier <- ELENBU$leg_period_end_posoxctformat - months(1)
			head(ELENBU)
	
		# below, we also need the party
			
			# get some date formats set
			ELENBU$leg_period_start_twoweekslater_posoxctformat <- ELENBU$leg_period_start_posoxctformat + weeks(2) # and a version two weeks later to make the query below slightly less sensitive
			MEME$memep_startdate_posoxctformat <- as.POSIXct(as.character(MEME$memep_startdate),format=c("%d%b%Y"))
			MEME$memep_enddate_posoxctformat <- as.POSIXct(as.character(MEME$memep_enddate),format=c("%d%b%Y"))
						
		# do the merge
			ELENBU <- sqldf("SELECT ELENBU.*, MEME.party_id
						     FROM ELENBU LEFT JOIN MEME 
							ON ELENBU.pers_id = MEME.pers_id AND ELENBU.leg_period_start_twoweekslater_posoxctformat BETWEEN MEME.memep_startdate_posoxctformat and MEME.memep_enddate_posoxctformat")
		nrow(ELENBU)
		head(ELENBU)
		

	############ gender guessing (if necessary) e.t.c. ##############
	
		ELENBU$gender[which(ELENBU$gender == "")] <- NA
		
		ifelse(ELENBU$gender == "")
	
		table(is.na(ELENBU$gender))
		table(is.na(ELENBU$gender),ELENBU$country)
		
		# now let's use the Dutch girl names list
		NL_girlnames = read.csv("NL_girlnames.csv", header = FALSE, sep = ";")
		colnames(NL_girlnames) <- c("name")
		head(NL_girlnames)
		tail(NL_girlnames)
		nrow(NL_girlnames)
		
		NL_boynames = read.csv("NL_boynames.csv", header = FALSE, sep = ";")
		colnames(NL_boynames) <- c("name")
		head(NL_boynames)
		tail(NL_boynames)
		nrow(NL_boynames)

		# example check
		TEST <- ELENBU[which(ELENBU$country == "NL"),]
		head(TEST)
		TEST$first_name[1] %in% NL_boynames$name
		TEST$first_name[1] %in% NL_girlnames$name

		resvec <- vector()
		for(i in 1:nrow(ELENBU))
		{
			if(ELENBU$country[i] == "NL" & is.na(ELENBU$gender[i])) # only do this when you are in NL and do not know the gender
			{
				if(ELENBU$first_name[i] %in% NL_boynames$name & !(ELENBU$first_name[i] %in% NL_girlnames$name)) # if this name occurs in the boyname list and not in girlname list, then set gender to m
				{
				resvec[i] <- "m"
				} else {
					if(ELENBU$first_name[i] %in% NL_girlnames$name & !(ELENBU$first_name[i] %in% NL_boynames$name)) # if this name occurs in the girlname list and not in boyname list, then set gender to f
					{
							resvec[i] <- "f"
					} else {
							resvec[i] <- NA
					}
				}
			} else { # if you are not in NL or do know the gender, just use that one
			resvec[i] <- ELENBU$gender[i]
			}
		}
		table(resvec)
		
		ELENBU$genderguesses <- resvec
		
		table(ELENBU$gender)
		table(ELENBU$genderguesses)
		table(ELENBU$gender,ELENBU$genderguesses)
		
		table(is.na(ELENBU$gender))
		table(is.na(ELENBU$genderguesses))
		
		table(is.na(ELENBU$genderguesses),ELENBU$country)
	
	
	#### find out which of these people entered parliament_id ####
	
		# get an 'in parliament'
		
			nrow(PARE)
			PARERED <- PARE[which(PARE$member_ofthisparliament_atsomepoint == "yes"),]
			nrow(PARERED)
			
			# make a fictional parliament id
			ELENBU$fictional_parl_episode_id<- paste(ELENBU$pers_id,ELENBU$parliament_id,sep="__")
			head(ELENBU)
			
			ELENBU$in_parliament <- ifelse(ELENBU$fictional_parl_episode_id %in% PARERED$parl_episode_id,"yes","no") 
			table(ELENBU$in_parliament) # is roughly one third
			
		# and reduce this to the people that where in parliament straight after the election  (taken from R019!)
					
					# tranform the dates
					RESE$res_entry_start_posoxctformat <- as.POSIXct(as.character(RESE$res_entry_start),format=c("%d%b%Y"))
					RESE$res_entry_end_posoxctformat <- as.POSIXct(as.character(RESE$res_entry_end),format=c("%d%b%Y"))
					
					RESEMPENT <- sqldf("SELECT RESE.res_entry_id, RESE.pers_id, RESE.res_entry_start, RESE.res_entry_end, RESE.res_entry_start_posoxctformat, RESE.res_entry_end_posoxctformat, RESE.parliament_id
							FROM RESE
							WHERE RESE.political_function = 'NT_LE_T3_NA_01'")
					nrow(RESEMPENT) 
					
				# update PARL to have the startdate in useable format
						PARL$leg_period_start_posoxctformat <- as.POSIXct(as.character(PARL$leg_period_start),format=c("%d%b%Y"))
					
			# make a dataframe that contains all the people that entered within 'x' weeks of the start of when the faction started
			
					# set the range
						ELENBU$leg_period_start_weeklater_posoxctformat <- ELENBU$leg_period_start_posoxctformat + weeks(1)

					# moving these dates one day to fix overlap issues (on day of transition matching now)
						RESEMPENT$res_entry_start_posoxctformat =  RESEMPENT$res_entry_start_posoxctformat + days(1)
						RESEMPENT$res_entry_end_posoxctformat = RESEMPENT$res_entry_end_posoxctformat - days(1)
			
				# first, a query that checks for each PARE of each politician if according to this MP its RESE episodes that specific parliaments, this MP was in this parliament in the first week
			
					ELENBURED <- sqldf("SELECT ELENBU.*
									   FROM 
									   ELENBU, RESEMPENT
									   WHERE
									   (
										   ELENBU.pers_id = RESEMPENT.pers_id
										   AND
										   RESEMPENT.parliament_id LIKE '%'||ELENBU.parliament_id||'%'
										   AND
										   (
											   (
													RESEMPENT.res_entry_start_posoxctformat >= ELENBU.leg_period_start_posoxctformat
													AND
													RESEMPENT.res_entry_start_posoxctformat <= ELENBU.leg_period_start_weeklater_posoxctformat
												)
												OR
												(
													RESEMPENT.res_entry_end_posoxctformat >= ELENBU.leg_period_start_posoxctformat
													AND
													RESEMPENT.res_entry_end_posoxctformat <= ELENBU.leg_period_start_weeklater_posoxctformat
												)
												OR
												(
													RESEMPENT.res_entry_start_posoxctformat <= ELENBU.leg_period_start_posoxctformat
													AND
													RESEMPENT.res_entry_end_posoxctformat >= ELENBU.leg_period_start_weeklater_posoxctformat
												)
											)
										)
										")
					nrow(ELENBURED)
					table(ELENBURED$in_parliament) # looks good in general, I should follow up these 75 people, these are people that we know where in parliament but that are somehow not in PARE?
					
	##################################################################################################
	###################################### aggregation here ##########################################
	##################################################################################################
	
	##### aggregation on the ELLI level ######
			GCELLI <- as.data.frame.matrix(table(ELENBU$list_id,ELENBU$genderguesses))
			GCELLI$list_id <- rownames(GCELLI)
			head(GCELLI)
			GCELLI[30:50,]
			tail(GCELLI)
			
			GCELLI$ratio <- GCELLI$f / (GCELLI$f+GCELLI$m)
			hist(GCELLI$ratio)

		# merge into an ELLI level data-frame
			
			nrow(ELLI)
			ELLI$country <- substr(ELLI$list_id,1,2)
			table(ELLI$country)
		
			ELLIBU <- sqldf("SELECT list_id, list_name, parliament_id, list_length, country, party_id, district_id
						FROM ELLI
						")
			head(ELLIBU)
			
			ELLIBU <- sqldf("SELECT ELLIBU.*, GCELLI.f, GCELLI.m, GCELLI.ratio as 'ratio_on_list'
						FROM ELLIBU LEFT JOIN GCELLI
						ON
						ELLIBU.list_id = GCELLI.list_id
						")
		
			head(ELLIBU)
			ELLIBU[30:50,]
			tail(ELLIBU)
			
		# if the party id is not a national party, get the mother party id
		
			ELLIBU[4500:4520,]
			i = 4500
			resvec <- vector()
			for(i in 1:nrow(ELLIBU))
			{
				if(grepl("_NT",as.character(ELLIBU$party_id[i])))
				{
					resvec[i] <- as.character(ELLIBU$party_id[i])
				} else {
					
					mymotherpartyid <- as.character(PART$mother_party_id[which(as.character(PART$party_id) == as.character(ELLIBU$party_id[i]))])
					
					if(length(mymotherpartyid) > 0)
					{
						resvec[i] <- mymotherpartyid
					}else{
						resvec[i] <- NA
					}
				}
			}
			ELLIBU$nat_party_id <- resvec
			head(ELLIBU)
			table(ELLIBU$nat_party_id)
			
			ELLIBU$nat_party_id <- ifelse(ELLIBU$nat_party_id == "lookup",NA,ELLIBU$nat_party_id)
			table(ELLIBU$nat_party_id)
		
		# get a count of the number of people in each faction
			MemVec <- as.matrix(table(ELENBU$list_id))
			MEMCOUNT <- data.frame(rownames(MemVec),unlist(MemVec))
			colnames(MEMCOUNT) <- c("list_id","list_member_count")
			head(MEMCOUNT)
			
			ELLIBU <- sqldf("SELECT ELLIBU.*, MEMCOUNT.list_member_count
						FROM ELLIBU LEFT JOIN MEMCOUNT
						ON
						ELLIBU.list_id = MEMCOUNT.list_id
						")
		
			head(ELLIBU)
			ELLIBU[30:50,]
			tail(ELLIBU)
			
			ELLIBU$sumcheck <- (ELLIBU$f+ELLIBU$m) - ELLIBU$list_member_count
			table(ELLIBU$sumcheck)
			
		# lets select the 'complete cases'
			nrow(ELLIBU)
			ELLIBUCOMP <- ELLIBU
		#	ELLIBUCOMP <- ELLIBU[which(ELLIBU$sumcheck == 0),]
		ELLIBUCOMP <- ELLIBU[which(ELLIBU$sumcheck > -6),]
			nrow(ELLIBUCOMP)
			
			nrow(ELLIBU) / (nrow(ELLIBUCOMP)+nrow(ELLIBU)) # using about 60% of the currently available cases (which are all list for CH, but only some of the main parties for NL and only 2017 for DE?)
		
		boxplot(ELLIBUCOMP$ratio_on_list~ELLIBUCOMP$country)
		table(is.na(ELLIBUCOMP$ratio_on_list),ELLIBUCOMP$country)
		
			EDE <- ELLIBUCOMP[which(ELLIBUCOMP$country == "DE"),]
			nrow(EDE)
			boxplot(EDE$ratio_on_list~droplevels(EDE$parliament_id))
			
			ENL <- ELLIBUCOMP[which(ELLIBUCOMP$country == "NL"),]
			nrow(ENL)
			boxplot(ENL$ratio_on_list~droplevels(ENL$parliament_id)) # carefull, there are only very few lists where we actually have the full gender composition!
			
			ECH <- ELLIBUCOMP[which(ELLIBUCOMP$country == "CH"),]
			nrow(ECH)
			boxplot(ECH$ratio_on_list~droplevels(ECH$parliament_id))
	
	###### gender aggregations on the reduced data! #######
	
			GCPARE <- as.data.frame.matrix(table(ELENBURED$list_id,ELENBURED$genderguesses))
			GCPARE$list_id <- rownames(GCPARE)
			head(GCPARE)
			GCPARE[30:50,]
			tail(GCPARE)
			
			GCPARE$ratio <- GCPARE$f / (GCPARE$f+GCPARE$m)
			hist(GCPARE$ratio)
			
		# now merge this in into the ELLI data we made above
		
		ELLIBU <- sqldf("SELECT ELLIBU.*, GCPARE.f as 'f_elected', GCPARE.m as 'm_elected', GCPARE.ratio as 'ratio_elected'
						FROM ELLIBU LEFT JOIN GCPARE
						ON
						ELLIBU.list_id = GCPARE.list_id
						")
		
			head(ELLIBU)
			ELLIBU[30:50,]
			ELLIBU[4030:4050,]
			ELLIBU[9030:9050,]
			tail(ELLIBU)
			
			
		# get district magnitude in (for now just number of people that got elected from this district in this parliament)
		
			mydistrict <- ELLIBU$district_id[9993]
		
			i = 9031
			resvec <- vector()
			for(i in 1:nrow(ELLIBU))
			{
				mydistrict <- ELLIBU$district_id[i]	
				resvec[i] <- nrow(ELENBURED[which(ELENBURED$district_id == mydistrict),])
			}
			resvec
			
			ELLIBU$district_magnitude <- resvec
			tail(ELLIBU)
			head(ELLIBU)
			
		# get party size in (for now just number of people from this party that got elected in the parliament)

			i = 1
			resvec <- vector()
			for(i in 1:nrow(ELLIBU))
			{
				mypartyid <- ELLIBU$nat_party_id[i]
				myparliamentid <- ELLIBU$parliament_id[i]		
				resvec[i] <- nrow(ELENBURED[which(ELENBURED$nat_party_id == mypartyid & ELLIBU$parliament_id == myparliamentid),])
			}
			resvec
			
			ELLIBU$party_size <- resvec
			tail(ELLIBU)
			head(ELLIBU)

##################################################################################################
######################################## reduction here ##########################################
##################################################################################################

	# exclude those cases in which nobody got elected
	
	nrow(ELLIBU)
	ELLIBU <- ELLIBU[which(!is.na(ELLIBU$f_elected)),] # we loose about 2/3 of cases
	nrow(ELLIBU)
		
######################################################################################
############################### DESCRIPTIVE RESULTS ##################################
######################################################################################			

	# country differences
	
				ggplot(ELLIBU, aes(x=ratio_on_list, y=ratio_elected,color=country)) + 
				geom_point() + 
				geom_smooth() +
				scale_x_continuous(limits = c(0, 0.6)) +
				geom_abline()
		
				ggplot(subset(ELLIBU, country == "NL"), aes(x=ratio_on_list, y=ratio_elected,color=country)) + 
				geom_point() + 
				geom_smooth() +
				scale_x_continuous(limits = c(0, 0.6)) +
				geom_abline()
				
				ggplot(subset(ELLIBU, country == "CH"), aes(x=ratio_on_list, y=ratio_elected,color=country)) + 
				geom_point() + 
				geom_smooth() +
				scale_x_continuous(limits = c(0, 0.6)) +
				geom_abline()
				
				ggplot(subset(ELLIBU, country == "DE"), aes(x=ratio_on_list, y=ratio_elected,color=country)) + 
				geom_point() + 
				geom_smooth() +
				scale_x_continuous(limits = c(0, 0.6)) +
				geom_abline()
				
	# district size
	
		# creating categories (country specific?)
		
	
			# for NL 
				hist(ELLIBU$district_magnitude[which(ELLIBU$country == "NL")])  # (not very meaningful for later years?)
			
			# for DE
				hist(ELLIBU$district_magnitude[which(ELLIBU$country == "DE")])
				hist(ELLIBU$district_magnitude[which(ELLIBU$country == "DE" & ELLIBU$district_magnitude < 5)]) # so lots of value 2 here, which is incorrect (LD issue?)				hist(ELLIBU$district_magnitude[which(ELLIBU$country == "DE" & ELLIBU$district_magnitude < 5)]) # so lots of value 2 here, which is incorrect (LD issue?)
				
			
			# for CH
			hist(ELLIBU$district_magnitude[which(ELLIBU$country == "CH")]) # only country with some actual proper variantion, lets for now only do this for CH!
				ELLIBU$district_magnitude_cat <- ELLIBU$district_magnitude
				ELLIBU$district_magnitude_cat[ELLIBU$district_magnitude > 0 & ELLIBU$district_magnitude < 11] <- "00-10"
				ELLIBU$district_magnitude_cat[ELLIBU$district_magnitude > 10 & ELLIBU$district_magnitude < 21] <- "11-20"
				ELLIBU$district_magnitude_cat[ELLIBU$district_magnitude > 20 & ELLIBU$district_magnitude < 31] <- "21-30"
				ELLIBU$district_magnitude_cat[ELLIBU$district_magnitude > 30] <- "30+"
			
			table(ELLIBU$district_magnitude_cat)
	
				ggplot(ELLIBU, aes(x=ratio_on_list, y=ratio_elected,color=district_magnitude_cat)) + 
				geom_point() + 
				geom_smooth(method='lm',formula=y~x) +
				scale_x_continuous(limits = c(0, 0.49)) +
				geom_abline()
	
	# party size
	
		# creating categories
		
			# for NL 
			hist(ELLIBU$district_magnitude[which(ELLIBU$country == "NL")])  # (not very meaningful for later years?)
				
######################################################################################
###################################### MODELS ########################################
######################################################################################

	names(ELLIBU)
	table(ELLIBU$party_id)
	
	# if the party is not a primary (national) party already, get the mother party id of this party