######################################################################################
#################################### SETUP ###########################################
######################################################################################

	# change the language and date formatting to English if it is not already
		Sys.setenv(LANG = "EN")
		Sys.setlocale("LC_TIME", "English") # key, without this conversion to POSIXct does not work
		Sys.getlocale(category = "LC_ALL")
		
		setwd("C:/Users/turnerzw/Basel Powi Dropbox/R/R026_temp")
		setwd("F:/PolCa/Analysis/R/ProjectR026_control")
		getwd()
		# also see 
	
	# packages
		library(sqldf)
		library(stringr)
		library(lubridate)
		library(ggplot2)
		library(stargazer)
		library(dplyr)
		
		
	substrRight <- function(x, n)
	{
		substr(x, nchar(x)-n+1, nchar(x))
	}
		
	# welll this is fun!	
	
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
				
				# import and inspect # now renamed, because we just generate this dataframe below and just merge some stuff into it from here
				PAREWRONG = read.csv("PCC/PARE.csv", header = TRUE, sep = ";")
				summary(PAREWRONG)
				names(PAREWRONG)
				
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
				
					# tranform dates to a format I can work with
					QUOT$quot_ep_startdate_posoxctformat <- as.POSIXct(as.character(QUOT$quot_ep_startdate),format=c("%d%b%Y"))
					QUOT$quot_ep_enddate_posoxctformat <- as.POSIXct(as.character(QUOT$quot_ep_enddate),format=c("%d%b%Y"))
				
######################################################################################
#################################### DATA ############################################
######################################################################################
	
	# new approach to getting PARE data
	
		# generate PARE type data on basis of RESE		
		# get the dates properly formatted into this
			PARLBU <- PARL
			PARLBU$leg_period_start_posoxctformat <- as.POSIXct(as.character(PARLBU$leg_period_start),format=c("%d%b%Y"))
			PARLBU$leg_period_end_posoxctformat <- as.POSIXct(as.character(PARLBU$leg_period_end),format=c("%d%b%Y"))
			
		# get all of the German parliamentary RESE episodes sub-selected so we can merge these in on date
		
			# write in a column with a standerat indicator
			
				# do the reduction		
				RESERED <- RESE[which((RESE$country == "DE" | RESE$country == "CH"| RESE$country == "NL") & RESE$political_function == "NT_LE_T3_NA_01" & RESE$res_entry_type == "pol" & !(grepl("-SR_",RESE$parliament_id))),] # also exclude standerat entries here with a grepl, they have the same political function code
				head(RESERED)
				tail(RESERED)
				table(RESERED$country)
				table(droplevels(RESERED$parliament_id))
			
			# get internal r-date format here as well
			
				# deal with left and right censored dates
				RESERED$res_entry_start_cleaned <- gsub("[[rcen]]","",RESERED$res_entry_start)
				RESERED$res_entry_start_cleaned <- gsub("[[lcen]]","",RESERED$res_entry_start)
				RESERED$res_entry_end_cleaned <- gsub("[[rcen]]","",RESERED$res_entry_end)
				RESERED$res_entry_end_cleaned <- gsub("[[lcen]]","",RESERED$res_entry_end)
			
				RESERED$res_entry_start_posoxctformat <- as.POSIXct(as.character(RESERED$res_entry_start_cleaned),format=c("%d%b%Y"))
				RESERED$res_entry_end_posoxctformat <- as.POSIXct(as.character(RESERED$res_entry_end_cleaned),format=c("%d%b%Y"))
				summary(RESERED$res_entry_start_posoxctformat)
				summary(RESERED$res_entry_end_posoxctformat)
				
		# now, lets build up a PARE like data-frames
			FPAREBU <- sqldf("SELECT PARLBU.parliament_id, PARLBU.leg_period_start,PARLBU.leg_period_end_posoxctformat, RESERED.res_entry_raw, RESERED.pers_id, PARLBU.country_abb
						    FROM PARLBU LEFT JOIN RESERED
							ON
							(
									(RESERED.res_entry_start_posoxctformat <= PARLBU.leg_period_start_posoxctformat AND  RESERED.res_entry_end_posoxctformat >= PARLBU.leg_period_end_posoxctformat)
								OR 
									(RESERED.res_entry_start_posoxctformat >= PARLBU.leg_period_start_posoxctformat AND RESERED.res_entry_start_posoxctformat <= PARLBU.leg_period_end_posoxctformat)
								OR 
									(RESERED.res_entry_end_posoxctformat >= PARLBU.leg_period_start_posoxctformat AND RESERED.res_entry_end_posoxctformat <= PARLBU.leg_period_end_posoxctformat)
									
							) 
								AND
								(PARLBU.country_abb = RESERED.country)
							")
			head(FPAREBU)
			nrow(FPAREBU)
			table(FPAREBU$parliament_id) # alright, this looks a lot more reasonable
			head(FPAREBU)
		
			# create a parliamentary episode ID 			
			FPAREBU$fake_parl_episode_id <- paste(FPAREBU$pers_id,FPAREBU$parliament_id,sep="__")
			length(unique(FPAREBU$fake_parl_episode_id)) # does not match!, several people occur double
			DUB <- FPAREBU[which(duplicated(FPAREBU$fake_parl_episode_id)),] # these are the problematic cases
			nrow(DUB) # NOTE TO SELF: you can parse theses cases to Adrian to fix # 93 cases!
			UNI <- FPAREBU %>% distinct(fake_parl_episode_id, .keep_all = TRUE)
			nrow(UNI)
			head(UNI)
			
			# get info from PAREWRONG if it exists
			
			# first check that one for duplicates as well
			nrow(PAREWRONG)
			length(unique(PAREWRONG$parl_episode_id)) # so there are duplicates here as well that I should remove
			PAREWRONG[which(duplicated(PAREWRONG$parl_episode_id)),]
			
			PAREWRONGRED <- PAREWRONG %>% distinct(parl_episode_id, .keep_all = TRUE)
			nrow(PAREWRONGRED) # alright, looking promissing
			
			TEMP <- sqldf("SELECT UNI.fake_parl_episode_id, PAREWRONGRED.*
						   FROM UNI LEFT JOIN PAREWRONGRED
						   ON UNI.fake_parl_episode_id = PAREWRONGRED.parl_episode_id
						 ")
			nrow(TEMP) # alright number is looking good
			
			# everyboyd in this new PARE file was in parliament at same point, so lets indicate that
			table(TEMP$member_ofthisparliament_atsomepoint)
			TEMP$member_ofthisparliament_atsomepoint <- "yes"
			table(TEMP$member_ofthisparliament_atsomepoint)
			
			# so lets make this our PARE for this script
			PARE <- TEMP

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
		tail(ELENBU)
		
	# merge in some ELLI characteristics
		ELENBU <- sqldf("SELECT ELENBU.*, ELLI.list_name, ELLI.parliament_id, ELLI.district_id, ELLI.list_length, ELLI.party_id as 'party_id_from_elli'
						FROM ELENBU LEFT JOIN ELLI
						ON
						ELENBU.list_id = ELLI.list_id
						")
		nrow(ELENBU) 
		head(ELENBU)
		tail(ELENBU)
	
	# merge in some PARL data
		ELENBU <- sqldf("SELECT ELENBU.*, PARL.leg_period_start, PARL.leg_period_end
						FROM ELENBU LEFT JOIN PARL
						ON
						ELENBU.parliament_id = PARL.parliament_id
						")
		nrow(ELENBU)
		head(ELENBU)
	
	# and party membership from MEME
	
		## please be aware that this is something of which we know there are issues
		## something which is also interesting is that party is taken from MEME, but we also have a lot of people in here that where just candidates right? --> indeed, now added these above, this is just party id from MEME
	
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
						
		# do the merge of party ids from meme
			ELENBU <- sqldf("SELECT ELENBU.*, MEME.party_id as 'party_id_from_meme'
						     FROM ELENBU LEFT JOIN MEME 
							ON ELENBU.pers_id = MEME.pers_id AND ELENBU.leg_period_start_twoweekslater_posoxctformat BETWEEN MEME.memep_startdate_posoxctformat and MEME.memep_enddate_posoxctformat")
			nrow(ELENBU)
			head(ELENBU)
		
			table(ELENBU$party_id_from_meme)
			summary(ELENBU$party_id_from_meme)
			table(is.na(ELENBU$party_id_from_meme)) # right, so indeed the far majority here is missing!
			
		# now lets compare these party ids!
		
			#set empty values as missing
				
				# in elli
					table(ELENBU$party_id_from_elli)
					ELENBU$party_id_from_elli[which(ELENBU$party_id_from_elli == "")] <- NA
					table(ELENBU$party_id_from_elli)
				# in meme?
					table(ELENBU$party_id_from_meme) # all NA already
		
			ELENBU$samepartyid <- ifelse(ELENBU$party_id_from_elli==ELENBU$party_id_from_meme,TRUE,FALSE)
			table(ELENBU$samepartyid)
			head(ELENBU[which(ELENBU$samepartyid == FALSE),]) # lots of cases where this is not the case
			tail(ELENBU[which(ELENBU$samepartyid == FALSE),])

		# quite some seem to be due to the national equivalent issue, so lets get one of those
			ELENBU$party_id_from_elli_nat_equiv <- gsub("RE-[A-Z]{2}","NT",ELENBU$party_id_from_elli)
			table(ELENBU$party_id_from_elli_nat_equiv)
			
			ELENBU$party_id_from_meme_nat_equiv <- gsub("RE-[A-Z]{2}","NT",ELENBU$party_id_from_meme)
			table(ELENBU$party_id_from_meme_nat_equiv)
			
			ELENBU$samepartyid <- ifelse(ELENBU$party_id_from_elli_nat_equiv==ELENBU$party_id_from_meme_nat_equiv,TRUE,FALSE)
			
			table(ELENBU$samepartyid) # still 530 cases where this is not case
			ELENBUNOTSAME <- ELENBU[which(ELENBU$samepartyid == FALSE),]
			nrow(ELENBUNOTSAME)
			head(ELENBUNOTSAME) 
			tail(ELENBUNOTSAME)
			table(ELENBUNOTSAME$country)
			table(ELENBUNOTSAME$party_id_from_meme_nat_equiv)
			ELENBUNOTSAME[which(ELENBUNOTSAME$party_id_from_meme_nat_equiv == "NL_GL_NT"),]
		
		# combining these two party indicators, taking the election list one as the most reliable one
			
			ELENBU$party_id_from_elliandmeme <- ifelse(is.na(ELENBU$party_id_from_elli_nat_equiv),ELENBU$party_id_from_meme_nat_equiv,ELENBU$party_id_from_elli_nat_equiv)
			table(is.na(ELENBU$party_id_from_elli_nat_equiv))
			table(is.na(ELENBU$party_id_from_meme_nat_equiv))
			table(is.na(ELENBU$party_id_from_elliandmeme)) # ok, so this only ads very little extra information but that is fine :)
			
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
		
		table(is.na(ELENBU$genderguesses),ELENBU$country) # so, decent amounts of missingness per country still
		prop.table(table(is.na(ELENBU$genderguesses),ELENBU$country),2) # about 13% in CH and NL and 5% in DE
	
		table(is.na(ELENBU$genderguesses),ELENBU$list_id) 
		prop.table(table(is.na(ELENBU$genderguesses),ELENBU$list_id),2) 
	
	
	##################################################################################################
	########################## German distric seats get combined here ################################ <- this got moved up from down below
	##################################################################################################

	# ELENBU > elected MPS
		table(ELENBU[which(ELENBU$country == "DE"),]$list_id)
		
		# so, what we would like is to have an adjusted version of the list id, that aggregates the district types to their land
			
			# lets start with taking the region_abb from ELDI
			nrow(ELENBU)
			TEMP <- sqldf("SELECT ELENBU.*, ELDI.region_abb
			               FROM ELENBU LEFT JOIN ELDI
						   ON ELENBU.district_id = ELDI.district_id
						  ")
			nrow(TEMP)
			table(TEMP$region_abb)
			ELENBU <- TEMP

		# for the german district candidates, we replace the list_id with another version on basis of this district abbreviation
			table(grepl("district-",ELENBU$list_name)) # cases where we are looking at german single member districts
			
			i = 163049
		
		resvec <- vector()
		ELENBU$list_id <- as.character(ELENBU$list_id)
		for(i in 1:nrow(ELENBU))
		{

			# loop throught the replacement here
				if (grepl("district-",ELENBU$list_name[i])) # if this is a district, then replace the list_id
				{ 
					resvec[i] <-  gsub("__.+__district-.+",paste("__district-seats-",ELENBU$region_abb[i],"__",ELENBU$party_id_from_elli_nat_equiv[i],sep=""),ELENBU$list_id[i])
				} else { #otherwise keep the current one
					resvec[i] <- ELENBU$list_id[i]
				}
		}
		length(unique(resvec))
		length(unique(ELENBU$list_id))
		# lets check this number
		head(ELENBU)
		tail(ELENBU)
		table(grepl("district-",ELENBU$list_name))
		(length(unique(ELENBU$list_id)) - length(unique(resvec))) 
		
		# lets add the result to another aggregation variable
		ELENBU$list_id_aggr <- resvec
	
		# now, we also need to not just give them the same ID, but really aggregate them
		
			# inspection
			ELENBU[which(ELENBU$list_id_aggr == "DE_NT-BT_1998__district-seats-NW__DE_OEDP_NT"),] # this looks quite good, all on one list now... 
		
		# temp trial overwrite here!
		length(unique(ELENBU$list_id))
		ELENBU$list_id <- ELENBU$list_id_aggr
		length(unique(ELENBU$list_id))
		
	## this same thing needs to be done in the ELLI data! ## 
		head(ELLI)
		tail(ELLI)
		ELLI[10000:10010,]
		
		# also get the region abbreviation in here 
			nrow(ELLI)
			TEMP2 <- sqldf("SELECT ELLI.*, ELDI.region_abb
			               FROM ELLI LEFT JOIN ELDI
						   ON ELLI.district_id = ELDI.district_id
						  ")
			nrow(TEMP2) # one double district id somewhere?
			table(TEMP2$region_abb)
			ELLI <- TEMP2
		
		# 
		ELLI$party_id_nat_equiv <- gsub("RE-[A-Z]{2}","NT",ELLI$party_id)
		
		resvec2 <- vector()
		ELLI$list_id <- as.character(ELLI$list_id)
		for(i in 1:nrow(ELLI))
		{

			# loop throught the replacement here
				if (grepl("district-",ELLI$list_name[i])) # if this is a district, then replace the list_id
				{ 
					resvec2[i] <-  gsub("__.+__district-.+",paste("__district-seats-",ELLI$region_abb[i],"__",ELLI$party_id_nat_equiv[i],sep=""),ELLI$list_id[i])
				} else { #otherwise keep the current one
					resvec2[i] <- ELLI$list_id[i]
				}
		}
		length(unique(resvec2))
		length(unique(ELLI$list_id))
		length(unique(ELLI$list_id)) - length(unique(resvec2)) # exact same amount as above, which is very promissing
		ELLI$list_id_aggr <- resvec2
	
	# overwrite here!
		length(unique(ELLI$list_id))
		ELLI$list_id <- ELLI$list_id_aggr
		length(unique(ELLI$list_id))
		
	# now for ELLI, we also need to combine thse values? Because this is technically only one list now?!
		ELLI[36000:36010,]
		
		# only thing really needs to happen for that is to remove the district id and to then select all the unique rows?
			nrow(ELLI)
			ELLI$country <- substr(ELLI$list_id,1,2)
			table(ELLI$country)
		
			ELLIBU <- sqldf("SELECT list_id, list_name, parliament_id, list_length, country, party_id
						FROM ELLI
						")
			head(ELLIBU)

			nrow(ELLIBU)
			ELLIBU <- ELLIBU[!duplicated(ELLIBU),]
			nrow(ELLIBU)
	
	
	##################################################################################################
	################### find out which of these people entered parliament_id #########################
	##################################################################################################
		# get an 'in parliament'
		
			nrow(PARE)
			PARERED <- PARE[which(PARE$member_ofthisparliament_atsomepoint == "yes"),]
			nrow(PARERED)
			
			# make a fictional parliament id
			ELENBU$fictional_parl_episode_id<- paste(ELENBU$pers_id,ELENBU$parliament_id,sep="__")
			head(ELENBU)
			
			ELENBU$in_parliament <- ifelse(ELENBU$fictional_parl_episode_id %in% PARERED$parl_episode_id,"yes","no") 
			table(ELENBU$in_parliament) 
			table(ELENBU$in_parliament)[2] / (table(ELENBU$in_parliament)[2]+table(ELENBU$in_parliament)[1]) # is roughly one third
			
		# and reduce this to the people that where in parliament straight after the election  (taken from R019!) # what am I doing this again?!
					
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
					table(ELENBURED$in_parliament) # should be 100?%
	
	##################################################################################################
	###################################### aggregation here ##########################################
	##################################################################################################
		
	##### aggregation on the ELLI level ######
			GCELLI <- as.data.frame.matrix(table(ELENBU$list_id,ELENBU$genderguesses)) # so, note to future self: if there is missingness here it is simply ignored. only known cases are counted
			
			GCELLI$list_id <- rownames(GCELLI)		
			GCELLI$country <- substr(GCELLI$list_id,1,2)
			table(GCELLI$country)
			GCELLI$ratio <- GCELLI$f / (GCELLI$f+GCELLI$m)
			
			head(GCELLI[which(GCELLI$country == "DE"),])
			tail(GCELLI[which(GCELLI$country == "DE"),])
			# this looks good
					
			hist(GCELLI$ratio) # so this is the ratio of men/women on the election lists

		# merge into the ELLI level data-frame which above was given the same abbreviated list ids, this data-frame was reduced above
			
			ELLIBU <- sqldf("SELECT ELLIBU.*, GCELLI.f, GCELLI.m, GCELLI.ratio as 'ratio_on_list'
						FROM ELLIBU LEFT JOIN GCELLI
						ON
						ELLIBU.list_id = GCELLI.list_id
						")
		
			head(ELLIBU)
			ELLIBU[30:50,]
			tail(ELLIBU)
			table(ELLIBU$parliament_id) # this looks a lot better now
			length(unique(ELLIBU$list_id))
			nrow(ELLIBU) # close enough for now
			
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
			
			ELLIBU$sumcheck <- (ELLIBU$f+ELLIBU$m) - ELLIBU$list_member_count # right, so zero is good here! this means that the number of women and men together add up to the total number on the list
			table(ELLIBU$sumcheck)
			
		# lets select the 'complete' cases: where these is not to little knowledge on the number of men and women
			
			
			
			nrow(ELLIBU)
			ELLIBUCOMP <- ELLIBU
		#	ELLIBUCOMP <- ELLIBU[which(ELLIBU$sumcheck == 0),]
		
			ELLIBUCOMP <- ELLIBU[which(ELLIBU$sumcheck > -8),] # arbritary decision, how big do I allow the gap to be?
			nrow(ELLIBUCOMP)
			nrow(ELLIBU) - nrow(ELLIBUCOMP) # only lossing about 700 election lists here
			
			nrow(ELLIBU) / (nrow(ELLIBUCOMP)+nrow(ELLIBU)) # using about 50% of the currently available cases (which are all list for CH, but only some of the main parties for NL and only 2017 for DE?)
		
			ELLIBUCOMP$parliament_id <- as.factor(ELLIBUCOMP$parliament_id)
		
		boxplot(ELLIBUCOMP$ratio_on_list~ELLIBUCOMP$country)
		table(is.na(ELLIBUCOMP$ratio_on_list),ELLIBUCOMP$country)
			
			EDE <- ELLIBUCOMP[which(ELLIBUCOMP$country == "DE"),]
			nrow(EDE)
			boxplot(EDE$ratio_on_list~droplevels(EDE$parliament_id))
			
			ENL <- ELLIBUCOMP[which(ELLIBUCOMP$country == "NL"),]
			nrow(ENL)
			boxplot(ENL$ratio_on_list~droplevels(ENL$parliament_id)) 
			
			ECH <- ELLIBUCOMP[which(ELLIBUCOMP$country == "CH"),]
			nrow(ECH)
			boxplot(ECH$ratio_on_list~droplevels(ECH$parliament_id))
	
	##### if the party id is not a national party, get the mother party id ###
		
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
	
	###### gender aggregations on the reduced data! #######
	
			GCPARE <- as.data.frame.matrix(table(ELENBURED$list_id,ELENBURED$genderguesses)) # is this correct? We calculate the ratio of men/women for each list that lead to anybody being elected
			GCPARE$list_id <- rownames(GCPARE)
			head(GCPARE)
			GCPARE[30:50,]
			tail(GCPARE)
			
			GCPARE$ratio <- GCPARE$f / (GCPARE$f+GCPARE$m)
			hist(GCPARE$ratio)
			
		# now merge this in into the ELLI data we made above - looks like something is going wrong here?!
		
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
			
		# so there is an issue here, none of the single member districts are merged in?!
		ELLIBU$list_id %in% GCPARE$list_id
		ELLIBU$list_id
		GCPARE$list_id
			
	
	##################################################################################################
	################################# variable building here #########################################
	##################################################################################################

		### lets make the type one again, just a grepl on word district
			ELLIBU$type <- ifelse(grepl("district-seats-",ELLIBU$list_id),"district","list")

		### country with lists/district device
			names(ELLIBU)
			ELLIBU$countryld <- ELLIBU$country
			table(ELLIBU$type)
			table(ELLIBU$country,ELLIBU$type)
			ELLIBU$countryld[which(ELLIBU$countryld == "DE" & ELLIBU$type == "list")] <- "DE-L"
			ELLIBU$countryld[which(ELLIBU$countryld == "DE" & ELLIBU$type == "district")] <- "DE-D"
			table(ELLIBU$countryld)
		
		### following Philip his suggested distinction
			ELLIBU$keylisttypes <- ELLIBU$countryld
			ELLIBU$keylisttypes[which(ELLIBU$keylisttypes == "CH")] <- "party-list-secondary-districts"
			ELLIBU$keylisttypes[which(ELLIBU$keylisttypes == "DE-L")] <- "party-list-secondary-districts"
			ELLIBU$keylisttypes[which(ELLIBU$keylisttypes == "DE-D")] <- "single-member-districts"
			ELLIBU$keylisttypes[which(ELLIBU$keylisttypes == "NL")] <- "one-list"
			table(ELLIBU$keylisttypes)
			table(ELLIBU$keylisttypes,ELLIBU$countryld)


		### get district magnitude in (for now just number of people that got elected from this district in this parliament)
		
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
			table(ELLIBU$district_magnitude) # this one now broke, fix later
			
		### get party size in (for now just number of people from this party that got elected in the parliament)

			# also here, get the national party versions
			resvec <- vector()
			for(i in 1:nrow(ELENBURED))
			{
				if(grepl("_NT",as.character(ELENBURED$party_id[i])))
				{
					resvec[i] <- as.character(ELENBURED$party_id[i])
				} else {
					
					mymotherpartyid <- as.character(PART$mother_party_id[which(as.character(PART$party_id) == as.character(ELENBURED$party_id[i]))])
					
					if(length(mymotherpartyid) > 0)
					{
						resvec[i] <- mymotherpartyid
					}else{
						resvec[i] <- NA
					}
				}
			}
			ELENBURED$nat_party_id <- resvec
			head(ELENBURED)
			table(ELENBURED$nat_party_id)
		
			i = 2790
			resvec <- vector()
			for(i in 1:nrow(ELLIBU))
			{
				mypartyid <- ELLIBU$nat_party_id[i]
				myparliamentid <- ELLIBU$parliament_id[i]		
				resvec[i] <- length(unique(ELENBURED$pers_id[which(ELENBURED$nat_party_id == mypartyid & ELENBURED$parliament_id == myparliamentid)]))
			}
			resvec
			
			ELLIBU$party_size <- resvec
			hist(ELLIBU$party_size)
			tail(ELLIBU)
			head(ELLIBU)
			
			boxplot(ELLIBU$party_size~ELLIBU$country)
			
			head(ELLIBU[which(ELLIBU$party_size == 0 & ELLIBU$country == "CH"),])
			tail(ELLIBU[which(ELLIBU$party_size == 0 & ELLIBU$country == "CH"),])

		### get a variable that indicates of this 'list' is a list list or a district list
		
			# if you are in Germany, and your list is only one person long, you are a district - also the word 'district' occurs in your listname
			ELLIBU$type <- ifelse(grepl("_district-",ELLIBU$list_id),"district","list") # _ and - are needed because otherwise it also hits on some swiss names
			head(ELLIBU)
			table(ELLIBU$country,ELLIBU$type)
			# /\ this needs to be developed to follow Philip his specification
			
		### was there a quota

			### get the start-date of the parliamentary term in
		
				ELLIBU <- sqldf("SELECT ELLIBU.*, PARL.leg_period_start, PARL.leg_period_end
                   FROM ELLIBU LEFT JOIN PARL 
                   ON ELLIBU.parliament_id = PARL.parliament_id")
				head(ELLIBU)
		
				ELLIBU$leg_period_start_posoxctformat <- as.POSIXct(as.character(ELLIBU$leg_period_start),format=c("%d%b%Y"))
				ELLIBU$leg_period_end_posoxctformat <- as.POSIXct(as.character(ELLIBU$leg_period_end),format=c("%d%b%Y"))
		
			names(ELLIBU)
			table(ELLIBU$nat_party_id)
			ELLIBU <- sqldf("SELECT ELLIBU.*, QUOT.quota_bin, QUOT.quot_ep_startdate_posoxctformat, QUOT.quota_percentage, QUOT.quota_zipper
					   FROM ELLIBU LEFT JOIN QUOT 
					   ON ELLIBU.nat_party_id = QUOT.party_id AND ELLIBU.leg_period_start_posoxctformat BETWEEN QUOT.quot_ep_startdate_posoxctformat and QUOT.quot_ep_enddate_posoxctformat")
				
			tail(ELLIBU)
			table(ELLIBU$quota_bin)
			summary(ELLIBU$quota_bin)
			ELLIBU[which(is.na(ELLIBU$quota_bin & !ELLIBU$country == "CH")),]	# i've asked Elena about this
					
			# we call this one 'quota now'
			names(ELLIBU)[match(c("quota_bin"),names(ELLIBU))] <- "quota_now"
			ELLIBU$quota_now <- as.factor(ELLIBU$quota_now)
			head(ELLIBU)
			tail(ELLIBU)
			table(ELLIBU$quota_now)
	
	### for all the list seats, get a variable as well that indicates what the percentage of women was on the district seats in this list its region
	
		# for all the districts, get the region from ELDI
			nrow(ELLIBU)
			ELLIBU <- sqldf("SELECT ELLIBU.*, ELDI.region_abb
					       FROM ELLIBU LEFT JOIN ELDI 
					       ON ELLIBU.district_id = ELDI.district_id	
						   ")
			nrow(ELLIBU)
			head(ELLIBU)

		# get a list of all district candidates in this region
			
			myregion <- "SN"
			myparliament <- "DE_NT-BT_2009"
			mycountry <- "DE"
			mynatpartyid <- "DE_CDU_NT"
			
			ADCIR <- ELLIBU[which(ELLIBU$type == "district" & ELLIBU$parliament_id == myparliament & ELLIBU$country == mycountry & ELLIBU$region == myregion & ELLIBU$nat_party_id == mynatpartyid),]
			mean(ADCIR$ratio_on_list)
			
			# and in a function
			getmyregionitsdistrictpercentage <- function(myregion,myparliament,mycountry,mynatpartyid)
			{
				ADCIR <- ELLIBU[which(ELLIBU$type == "district" & ELLIBU$parliament_id == myparliament & ELLIBU$country == mycountry & ELLIBU$region == myregion & ELLIBU$nat_party_id == mynatpartyid),]
				mean(ADCIR$ratio_on_list)
			}
			# check
			getmyregionitsdistrictpercentage("SN","DE_NT-BT_2009","DE","DE_CDU_NT")
			
			# and run in a loop
			for (i in 1:nrow(ELLIBU))
			{
			pb <- txtProgressBar(min = 1, max = nrow(ELLIBU), style = 3)
			a <- ELLIBU$region[i]
			b <- ELLIBU$parliament_id[i]
			c <- ELLIBU$country[i]
			d <- ELLIBU$nat_party_id[i]
			
			ELLIBU$percentage_onlist_indistrict[i] <- getmyregionitsdistrictpercentage(a,b,c,d)
			setTxtProgressBar(pb, i)
			}
			close(pb)
			summary(ELLIBU$percentage_onlist_indistrict)
			nrow(ELLIBU)

	## creating the two crucial gap variables 
		
		# there are a total of two steps: ambition --(step 1)--> percentage on list --(step 2)--> percentage elected
			head(ELLIBU)
		# ambition realisation gap (overall gap) (quota percentage - percentage elected)
			
			# inspection
			table(ELLIBU$quota_now)
			table(ELLIBU$quota_percentage)
			sum(table(ELLIBU$quota_percentage)) # percentage specifications seem complete
			
			# adding a 0% category
			ELLIBU$quota_percentage_cleaned <- ifelse(ELLIBU$quota_now == 0,0,ELLIBU$quota_percentage)
			
			# more inspection
			table(ELLIBU$quota_percentage_cleaned)
			table(is.na(ELLIBU$quota_percentage_cleaned)) # so, we have we have quota info for a little more then half the election lists
			table(is.na(ELLIBU$quota_percentage_cleaned),ELLIBU$country) # complete in NL, nothing in Switserland
			
			table(is.na(ELLIBU$quota_percentage),ELLIBU$keylisttypes) # still, some of everything?
			table(is.na(ELLIBU$ratio_elected),ELLIBU$keylisttypes) # no ratio elected for any of the single member districts?!
			
			# calculating the gap
			ELLIBU$ambition_realisation_gap <- (ELLIBU$ratio_elected - (ELLIBU$quota_percentage/100)) # negative numbers indicate that the quota was not reached (less women elected then specified)

			# inspection of the gap
			table(is.na(ELLIBU$ambition_realisation_gap))
			hist(ELLIBU$ambition_realisation_gap) # some suggestion here that unit of analysis of the district, and counting this as a simular case then lets say a list is not really fair?
			
			boxplot(ELLIBU$ambition_realisation_gap~ELLIBU$country) # see point above, districts that have a women count as a one, how to do this? Maybe take the average percentages at the level of the secundary districts for DE or so?
			hist(ELLIBU$ambition_realisation_gap[which(ELLIBU$country =="DE")]) # lets add this to the overleaf file - some suggestion indeed that much more often in DE quota is not reached?
			hist(ELLIBU$ambition_realisation_gap[which(ELLIBU$country =="NL")]) # lets add this to the overleaf file
				
			# key descriptive relating to Philip suggestion
			boxplot(ELLIBU$ambition_realisation_gap~ELLIBU$keylisttypes)
			
			table(is.na(ELLIBU$ambition_realisation_gap))
			table(is.na(ELLIBU$ambition_realisation_gap),is.na(ELLIBU$keylisttypes))
			
		# ambition to selection gap (quota percentage - percentage selected)
			
			# calculating this gap
			table(is.na(ELLIBU$ratio_on_list))
			table(is.na(ELLIBU$quota_percentage)) # ok, so hardly any quotas
			table(is.na(ELLIBU$quota_percentage),is.na())
			ELLIBU$ambition_selection_gap <- (ELLIBU$ratio_on_list - (ELLIBU$quota_percentage/100))
			
			# inspection of the gap
			table(is.na(ELLIBU$ambition_selection_gap)) # so, just as a note so self, quite a bit more cases because from many lists never anybody got elected?
			hist(ELLIBU$ambition_selection_gap) # looks a lot like the ambition realisation gap? - so some suggestion that the selection step is the crucial one in the sample as a whole?
			boxplot(ELLIBU$ambition_selection_gap~ELLIBU$country) 
			hist(ELLIBU$ambition_selection_gap[which(ELLIBU$country =="DE")])
			hist(ELLIBU$ambition_selection_gap[which(ELLIBU$country =="NL")]) 
		
			# inspecting some of these 'extreme' german cases
			ELLIBU[which(ELLIBU$ambition_selection_gap > 0.7),] # seem to be SPD cases, with 25% as the ambition, and then the districts select a women (1)
			table(ELLIBU[which(ELLIBU$ambition_selection_gap < -0.4),]$nat_party_id)
			table(ELLIBU[which(ELLIBU$ambition_selection_gap < -0.4),]$keylisttypes) # so, indeed, is all of the single-member districts
			
		# selection to election gap (percentage selected - percentage elected)

			# calculating this gap
			ELLIBU$selection_election_gap <- ELLIBU$ratio_elected - ELLIBU$ratio_on_list # negative numbers indicate that less women where elected then selected
			
			# inspection
			table(is.na(ELLIBU$selection_election_gap)) # very large number of NA here is the result of many election lists just not leading to anybody being elected
			table(is.na(ELLIBU$ratio_elected)) # by far most cases are lost here
			table(is.na(ELLIBU$ratio_on_list))
			
			hist(ELLIBU$selection_election_gap) # not a lot is happening here?
			boxplot(ELLIBU$selection_election_gap~ELLIBU$country) 
			hist(ELLIBU$selection_election_gap[which(ELLIBU$country =="CH")])
			hist(ELLIBU$selection_election_gap[which(ELLIBU$country =="DE")])
			hist(ELLIBU$selection_election_gap[which(ELLIBU$country =="NL")]) 
			
			
######################################## reduction here ##########################################
##################################################################################################

	# focus on list candidates only - temp!
#		table(ELLIBU$type)
#		nrow(ELLIBU)
#		ELLIBU <- ELLIBU[which(ELLIBU$type == "list"),]
#		nrow(ELLIBU)

	# reduce to those cases after 1982
#		ELLIBU$year <- as.numeric(as.character(substrRight(ELLIBU$parliament_id,4)))
#		table(ELLIBU$year)
		
#		nrow(ELLIBU)
#		ELLIBU <- ELLIBU[which(ELLIBU$year > 1981),]
#		nrow(ELLIBU)
			
	# exclude those cases in which no women got elected at all
#		nrow(ELLIBU)
#		ELLIBU <- ELLIBU[which(!is.na(ELLIBU$f_elected)),] # we loose about 2/3 of cases
#		nrow(ELLIBU)

######################################################################################
############################### DESCRIPTIVE RESULTS ##################################
######################################################################################			
		
	# some general descriptives for the first version of the paper
	
		table(ELLIBU$parliament_id)
		length(table(ELLIBU$parliament_id))
	
		nrow(ELLIBU)
		
		table(ELLIBU$country)
		table(ELLIBU$type)
		
		table(ELLIBU$nat_party_id)
		length(table(ELLIBU$nat_party_id))
		length(table(ELLIBU$nat_party_id[which(ELLIBU$country == "NL")]))
		length(table(ELLIBU$nat_party_id[which(ELLIBU$country == "DE")]))
		length(table(ELLIBU$nat_party_id[which(ELLIBU$country == "CH")]))
		
		table(ELLIBU$nat_party_id[which(ELLIBU$country == "CH")])
		
		
		
	# country differences
	
				ggplot(ELLIBU, aes(x=ratio_on_list, y=ratio_elected,color=countryld)) + 
				geom_point() + 
				geom_smooth(method='lm') +
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
			
			# all
				ggplot(subset(ELLIBU, country == "DE"), aes(x=ratio_on_list, y=ratio_elected,color=country)) + 
				geom_point() + 
				geom_smooth() +
				scale_x_continuous(limits = c(0, 0.6)) +
				geom_abline()
			
				# only the lists
				ggplot(subset(ELLIBU, countryld == "DE-L"), aes(x=ratio_on_list, y=ratio_elected,color=country)) + 
				geom_point() + 
				geom_smooth() +
				scale_x_continuous(limits = c(0, 0.6)) +
				geom_abline()
			
				# only the districts
				ggplot(subset(ELLIBU, countryld == "DE-D"), aes(x=ratio_on_list, y=ratio_elected,color=country)) + 
				geom_point() + 
				geom_smooth() +
				scale_x_continuous(limits = c(0, 0.6)) +
				geom_abline()
				
				head(ELLIBU[which(ELLIBU$countryld == "DE-D"),]) # yes, makes sense. Ratio elected is always exactly the same as ratio on list.	
				
	# district size
	
		# creating categories (country specific?)
		
	
			# for NL 
				hist(ELLIBU$district_magnitude[which(ELLIBU$country == "NL")])  # (not very meaningful for later years?)
			
			# for DE
				hist(ELLIBU$district_magnitude[which(ELLIBU$country == "DE")])
				hist(ELLIBU$district_magnitude[which(ELLIBU$country == "DE" & ELLIBU$district_magnitude < 5)]) # so lots of value 2 here, which is incorrect (LD issue?)				hist(ELLIBU$district_magnitude[which(ELLIBU$country == "DE" & ELLIBU$district_magnitude < 5)]) # so lots of value 2 here, which is incorrect (LD issue?)
				
			
			# for CH
			hist(ELLIBU$district_magnitude[which(ELLIBU$country == "CH")]) # only country with some actual proper variation, lets for now only do this for CH!
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
				hist(ELLIBU$party_size[which(ELLIBU$country == "NL")]) 
				ELLIBU$party_size_cat <- ELLIBU$party_size
				
				cut(ELLIBU$party_size[which(ELLIBU$country=="NL")], 3)
				
				ELLIBU$party_size_cat[ELLIBU$country == "NL" & ELLIBU$party_size >= 0 & ELLIBU$party_size < 16] <- "small"
				ELLIBU$party_size_cat[ELLIBU$country == "NL" &ELLIBU$party_size > 15 & ELLIBU$party_size < 33] <- "medium"
				ELLIBU$party_size_cat[ELLIBU$country == "NL" &ELLIBU$party_size > 32] <- "large"
				table(ELLIBU$party_size_cat)
				
				table(ELLIBU$party_size_cat[which(ELLIBU$country == "NL")],ELLIBU$party_id[which(ELLIBU$country == "NL")])
			
			# for DE 
				hist(ELLIBU$party_size[which(ELLIBU$country == "DE")]) 
				cut(ELLIBU$party_size[which(ELLIBU$country=="DE")], 3)
				ELLIBU$party_size_cat[ELLIBU$country == "DE" & ELLIBU$party_size >= 0 & ELLIBU$party_size < 110] <- "small"
				ELLIBU$party_size_cat[ELLIBU$country == "DE" &ELLIBU$party_size > 109 & ELLIBU$party_size < 156] <- "medium"
				ELLIBU$party_size_cat[ELLIBU$country == "DE" &ELLIBU$party_size > 155] <- "large"
				table(ELLIBU$party_size_cat)
			
			# for CH 
				hist(ELLIBU$party_size[which(ELLIBU$country == "CH")]) 
				cut(ELLIBU$party_size[which(ELLIBU$country=="CH")], 3)
				ELLIBU$party_size_cat[ELLIBU$country == "CH" & ELLIBU$party_size >= 0 & ELLIBU$party_size < 21] <- "small"
				ELLIBU$party_size_cat[ELLIBU$country == "CH" &ELLIBU$party_size > 20 & ELLIBU$party_size < 43] <- "medium"
				ELLIBU$party_size_cat[ELLIBU$country == "CH" &ELLIBU$party_size > 42] <- "large"
				table(ELLIBU$party_size_cat) 
				
				ggplot(ELLIBU, aes(x=ratio_on_list, y=ratio_elected,color=party_size_cat)) + 
				geom_point() + 
				geom_smooth(method='lm',formula=y~x) +
				scale_x_continuous(limits = c(0, 0.49)) +
				geom_abline()
				
	# time effects
	
		ELLIBU$election_year <- as.numeric(substrRight(as.character(ELLIBU$parliament_id),4))
		table(ELLIBU$election_year)
		ELLIBU$election_year_cent <-ELLIBU$election_year - round(mean(ELLIBU$election_year,na.rm=T),0) # not that the German 2017 sample is pulling this value up still at this stage
		table(ELLIBU$election_year_cent)
		
		stargazer(ELLIBU)
		
	### impact of gender quotas
	
		## constructing some combined factors
		
			# quota now and percentage
			
				ELLIBU$qnqp <- as.character(ELLIBU$quota_now)
				table(ELLIBU$quota_now,ELLIBU$quota_percentage)
				ELLIBU$qnqp[which(ELLIBU$quota_now == "1" & ELLIBU$quota_percentage == "25")] <- "1_25"
				ELLIBU$qnqp[which(ELLIBU$quota_now == "1" & ELLIBU$quota_percentage == "33")] <- "1_33"
				ELLIBU$qnqp[which(ELLIBU$quota_now == "1" & ELLIBU$quota_percentage == "40")] <- "1_40"
				ELLIBU$qnqp[which(ELLIBU$quota_now == "1" & ELLIBU$quota_percentage == "50")] <- "1_50"
				table(ELLIBU$qnqp)
			
			# + zipper
				table(ELLIBU$qnqp,ELLIBU$quota_zipper)
				ELLIBU$qnqpz <- ELLIBU$qnqp
				ELLIBU$qnqpz[which(ELLIBU$qnqp == "1_40")] <- "1_40_zip"
				ELLIBU$qnqpz[which(ELLIBU$qnqp == "1_50" & ELLIBU$quota_zipper == "1")] <- "1_50_zip"
				ELLIBU$qnqpz[which(ELLIBU$qnqp == "1_50" & ELLIBU$quota_zipper == "0")] <- "1_50_notzip"
				table(ELLIBU$qnqpz)
				
		## on the percentage
			
			# present
			ggplot(ELLIBU, aes(x=quota_now, y=ratio_elected)) + 
			geom_boxplot()

			# present + percentage
			ggplot(ELLIBU, aes(x=qnqp, y=ratio_elected)) + 
			geom_boxplot()
			
			# present + percentage + zipper
			ggplot(ELLIBU, aes(x=qnqpz, y=ratio_elected)) + 
			geom_boxplot()
			
			# present + percentage + zipper TEMP TEMP
			ggplot(ELLIBU, aes(x=qnqpz, y=ratio_on_list)) + 
			geom_boxplot()
			
			# quota times party size
			table(ELLIBU$party_size_cat)
			table(ELLIBU$quota_now)
			ELLIBU$quota_and_party_size <- ELLIBU$party_size_cat
			ELLIBU$quota_and_party_size[which(ELLIBU$party_size_cat == "small" & ELLIBU$quota_now == 0)] <- "small - no quota"
			ELLIBU$quota_and_party_size[which(ELLIBU$party_size_cat == "small" & ELLIBU$quota_now == 1)] <- "small - with quota"
			
			ELLIBU$quota_and_party_size[which(ELLIBU$party_size_cat == "medium" & ELLIBU$quota_now == 0)] <- "medium - no quota"
			ELLIBU$quota_and_party_size[which(ELLIBU$party_size_cat == "medium" & ELLIBU$quota_now == 1)] <- "medium - with quota"
			
			ELLIBU$quota_and_party_size[which(ELLIBU$party_size_cat == "large" & ELLIBU$quota_now == 0)] <- "large - no quota"
			ELLIBU$quota_and_party_size[which(ELLIBU$party_size_cat == "large" & ELLIBU$quota_now == 1)] <- "large - with quota"
			
			ELLIBU$quota_and_party_size[which(ELLIBU$quota_and_party_size == "small" | ELLIBU$quota_and_party_size == "medium" | ELLIBU$quota_and_party_size == "large")] <- NA
			
			table(ELLIBU$quota_and_party_size)
			
			TE <- ELLIBU[which(ELLIBU$quota_and_party_size == "medium - with quota"),]
			table(TE$parliament_id)
			table(TE$nat_party_id)
			
			# percentage - can also try ratio elected here
				ggplot(ELLIBU, aes(x=quota_and_party_size, y=ratio_elected, color=party_size_cat)) + 
				geom_boxplot()
		
				ggplot(ELLIBU, aes(x=quota_and_party_size, y=ratio_on_list, color=party_size_cat)) + 
				geom_boxplot()
		
			# relation
			
				table(ELLIBU$type)
				table(ELLIBU$country)
				table(ELLIBU$type,ELLIBU$country)
				
				ELLIBUA <- ELLIBU[which(ELLIBU$type=="list" & ELLIBU$country=="NL" & ELLIBU$ratio_on_list > 0),]
				nrow(ELLIBUA)
				
				# this should say!
				ggplot(ELLIBUA, aes(x=ratio_on_list, y=ratio_elected,color=quota_and_party_size)) + 
				geom_point() + 
				geom_smooth(method='lm') +
				scale_x_continuous(limits = c(0, 0.6)) +
				geom_abline()
	
	
	
		## on the relation
		
			# reduce the sample to not include 0,0 observations
		
				table(ELLIBU$ratio_on_list)
				nrow(ELLIBU)
				ELLIBUR <- ELLIBU[which(!(ELLIBU$ratio_on_list == 0)),]
				nrow(ELLIBUR)
		
			# present
				# only the districts
					ggplot(ELLIBUR, aes(x=ratio_on_list, y=ratio_elected,color=quota_now)) + 
					geom_point() + 
					geom_smooth(method='lm') +
					# geom_smooth() +
					scale_x_continuous(limits = c(0, 0.6)) +
					geom_abline()
				
			# + percentage
				
				ggplot(ELLIBUR, aes(x=ratio_on_list, y=ratio_elected,color=qnqp)) + 
					geom_point() + 
					geom_smooth(method='lm') +
					scale_x_continuous(limits = c(0, 0.6)) +
					geom_abline()
			
			# + strength / zipper
			
				ggplot(ELLIBUR, aes(x=ratio_on_list, y=ratio_elected,color=qnqpz)) + 
					geom_point() + 
					geom_smooth(method='lm') +
					scale_x_continuous(limits = c(0, 0.6)) +
					geom_abline()
			
		
	
		
				
######################################################################################
###################################### MODELS ########################################
######################################################################################

	# focus on the lists
		ELLIBU <- ELLIBUL
	
	# lets remove switserland as well for now
		table(ELLIBU$country)
		ELLIBU <- ELLIBU[which(!ELLIBU$country == "CH"),]
		nrow(ELLIBU)

	# group mean centering district size and party size
	
		# district magnitude
		NLm <- mean(ELLIBU$district_magnitude[ELLIBU$country =="NL"],na.rm=TRUE)
		NLsd <- sd(ELLIBU$district_magnitude[ELLIBU$country =="NL"],na.rm=TRUE)
		
		DEm <- mean(ELLIBU$district_magnitude[ELLIBU$country =="DE"],na.rm=TRUE)
		DEsd <- sd(ELLIBU$district_magnitude[ELLIBU$country =="DE"],na.rm=TRUE)
		
		CHm <- mean(ELLIBU$district_magnitude[ELLIBU$country =="CH"],na.rm=TRUE)
		CHsd <- sd(ELLIBU$district_magnitude[ELLIBU$country =="CH"],na.rm=TRUE)
		
		ELLIBU$district_mag_gmcent <- ifelse(ELLIBU$country=="NL",((ELLIBU$district_magnitude - NLm)/NLsd),NA)
		ELLIBU$district_mag_gmcent <- ifelse(ELLIBU$country=="DE",((ELLIBU$district_magnitude - DEm)/DEsd),ELLIBU$district_mag_gmcent)
		ELLIBU$district_mag_gmcent <- ifelse(ELLIBU$country=="CH",((ELLIBU$district_magnitude - CHm)/CHsd),ELLIBU$district_mag_gmcent)
		hist(ELLIBU$district_mag_gmcent)
		mean(ELLIBU$district_mag_gmcent,na.rm=T)
		sd(ELLIBU$ ,na.rm=T)
		
	# party size
		NLmps <- mean(ELLIBU$party_size[ELLIBU$country =="NL"],na.rm=TRUE)
		NLsdps <- sd(ELLIBU$party_size[ELLIBU$country =="NL"],na.rm=TRUE)
		
		DEmps <- mean(ELLIBU$party_size[ELLIBU$country =="DE"],na.rm=TRUE)
		DEsdps <- sd(ELLIBU$party_size[ELLIBU$country =="DE"],na.rm=TRUE)
		
		CHmps <- mean(ELLIBU$party_size[ELLIBU$country =="CH"],na.rm=TRUE)
		CHsdps <- sd(ELLIBU$party_size[ELLIBU$country =="CH"],na.rm=TRUE)
		
		ELLIBU$party_size_gmcent <- ifelse(ELLIBU$country=="NL",((ELLIBU$party_size - NLmps)/NLsdps),NA)
		ELLIBU$party_size_gmcent <- ifelse(ELLIBU$country=="DE",((ELLIBU$party_size - DEmps)/DEsdps),ELLIBU$party_size_gmcent)
		ELLIBU$party_size_gmcent <- ifelse(ELLIBU$country=="CH",((ELLIBU$party_size - CHmps)/CHsdps),ELLIBU$party_size_gmcent)
		hist(ELLIBU$party_size_gmcent)
		mean(ELLIBU$party_size_gmcent,na.rm=T)
		sd(ELLIBU$party_size_gmcent,na.rm=T)

		
	# ratio on list
		ELLIBU$ratio_on_list_cent <- scale(ELLIBU$ratio_on_list,center=TRUE,scale=FALSE)
		hist(ELLIBU$ratio_on_list_cent)
	
	names(ELLIBU)
	table(ELLIBU$party_id)
	
#	mempty <- lm(ratio_elected~1,
	mempty <- lm(ratio_on_list~1,
				 data=ELLIBU)
	summary(mempty)


#	m1 <- lm(ratio_elected~ratio_on_list_cent,
	m1 <- lm(ratio_on_list ~
				 data=ELLIBU)
	summary(m1)
	
#	m1a <- lm(ratio_elected~ratio_on_list_cent +
	m1a <- lm(ratio_on_list ~
				 election_year_cent
				 ,data=ELLIBU)
	summary(m1a)
	
#	m2 <- lm(ratio_elected~ratio_on_list_cent +
	m2 <- lm(ratio_on_list ~
				 election_year_cent +
				district_mag_gmcent
				,data=ELLIBU)
	summary(m2)
	
#	m2a <- lm(ratio_elected~ratio_on_list_cent +
	m2a <- lm(ratio_on_list ~
				election_year_cent +
				district_mag_gmcent #* ratio_on_list_cent 
				,data=ELLIBU)
	summary(m2a)
	
#	m3 <- lm(ratio_elected~ratio_on_list_cent +
	m3 <- lm(ratio_on_list ~
				election_year_cent +
				district_mag_gmcent + #* ratio_on_list_cent +
				party_size_gmcent
				,data=ELLIBU)
	summary(m3)
	
#	m3a <- lm(ratio_elected~ratio_on_list_cent +
	m3a <- lm(ratio_on_list ~
				election_year_cent +
				district_mag_gmcent + #* ratio_on_list_cent +
				party_size_gmcent  #* ratio_on_list_cent
				,data=ELLIBU)
	summary(m3a)
	
#	m4 <- lm(ratio_elected~ratio_on_list_cent +
	m4 <- lm(ratio_on_list ~
				election_year_cent +
				district_mag_gmcent + #* ratio_on_list_cent +
				party_size_gmcent + #* ratio_on_list_cent +
				country
				,data=ELLIBU)
	summary(m4)
	
#	m4a <- lm(ratio_elected~ratio_on_list_cent +
	m4a <- lm(ratio_on_list ~
				election_year_cent +
				district_mag_gmcent + #* ratio_on_list_cent +
				party_size_gmcent + #* ratio_on_list_cent +
				country  #* ratio_on_list_cent
				,data=ELLIBU)
	summary(m4a)
	
#	m5 <- lm(ratio_elected~ratio_on_list_cent +
	m5 <- lm(ratio_on_list ~
				election_year_cent +
				district_mag_gmcent + #* ratio_on_list_cent +
				party_size_gmcent + #* ratio_on_list_cent +
				country + #* ratio_on_list_cent +
				quota_now + #* ratio_on_list_cent +
				quota_now * party_size_gmcent
				,data=ELLIBU)
	summary(m5)
	
	
		# building up the stargazer output
			
			dirtynames <- names(coef(m5))
			
			specificnamecleaning <- function(dirtynamesloc)
			{
				cleanernames <- gsub("ratio_on_list_cent","ratio on list",dirtynamesloc,fixed=TRUE)
				cleanernames <- gsub("election_year_cent","election year",cleanernames,fixed=TRUE)
				cleanernames <- gsub("district_mag_gmcent","district magnitude gmcent",cleanernames,fixed=TRUE)
				cleanernames <- gsub("party_size_gmcent","party size gmcent",cleanernames,fixed=TRUE)
				cleanernames <- gsub("quota_now","quota now",cleanernames,fixed=TRUE)
				return(cleanernames)
				
			}
			specificnamecleaning(dirtynames)
			
			labelsinthisorder <- specificnamecleaning(names(coef(m5)))
	
	stargazer(mempty,m1a,m2a,m3,m4,m5,
			type="latex",
			covariate.labels = labelsinthisorder,
			intercept.bottom=F,
			omit.stat=c("f","ser"))




















	