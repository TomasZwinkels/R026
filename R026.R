######################################################################################
#################################### SETUP ########################################### test
######################################################################################

	###
	
		# !!at a later stage please search for '#fixlater' in the document, that are some data issues indentified that are currently left unfixed!!
	
	###


	# change the language and date formatting to English if it is not already
		Sys.setenv(LANG = "EN")
		Sys.setlocale("LC_TIME", "English") # key, without this conversion to POSIXct does not work
		Sys.getlocale(category = "LC_ALL")
		
		#setwd("C:/Users/turnerzw/Basel Powi Dropbox/R/R026_temp")
		setwd("F:/PolCa/Analysis/R/ProjectR026_control")
		setwd("C:/Users/turnerzw/Basel Powi Dropbox/Remote GW17PC05/PCP_Quotas_Paper/Analysis/ProjectR026_control")
		getwd()
	
	#	install.packages("sqldf")
	#	install.packages("stringr")
	#	install.packages("lubridate")
	#	install.packages("ggplot2")
	#	install.packages("stargazer")
	#	install.packages("dplyr")
	#	install.packages("reshape")
	#	install.packages("TraMineR")
	#	install.packages("lawstat")
	#	install.packages("beanplot")
	#	install.packages("stringr")
	#	install.packages("foreach")
	#	install.packages("doParallel")
	#	install.packages("car")
	#	install.packages("ggpubr")
	#	install.packages("sjPlot")
	#	install.packages("effects")
	
	# packages
		library(sqldf)
		library(stringr)
		library(lubridate)
		library(ggplot2)
		library(stargazer)
		library(dplyr)
		library(reshape)
		library(TraMineR)
		library(lawstat)
		library(beanplot)
		library(stringr)
		library(foreach)
		library(doParallel)
		library(lme4)
		library(car)
		library(ggpubr)
		library(sjPlot)
		library(effects)
		
		
	substrRight <- function(x, n)
	{
		substr(x, nchar(x)-n+1, nchar(x))
	}	
	
	# import and inspect all the PCC data-frames
				
			# core
			
				# import and inspect
				ELEN = read.csv("PCC/ELEN.csv", header = TRUE, sep = ";")
				summary(ELEN)
				names(ELEN)

				
				ELDI = read.csv("PCC/ELDI.csv", header = TRUE, sep = ";")
				summary(ELDI)
				names(ELDI)
				
				table(is.na(ELDI$country), is.na(ELDI$dist_magnitude))
				table(ELDI$country, is.na(ELDI$dist_magnitude))
					
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
				ELEC = read.csv("PCC/ELEC.csv", header = TRUE, sep = ";")
				summary(ELEC)
				names(ELEC)
				
				
				# import and inspect
				PART = read.csv("PCC/PART.csv", header = TRUE, sep = ";")
				summary(PART)
				names(PART)
				
				# import and inspect
				POLI = read.csv("PCC/POLI.csv", header = TRUE, sep = ";")
				summary(POLI)
				names(POLI)
				
					# this script does not deal well with duplicate pers_ids in POLI, so are there any?!
					length(POLI$pers_id)
					length(unique(POLI$pers_id))
					
					POLI[which(duplicated(POLI$pers_id)),]
					
			
				# import and inspect
				RESE = read.csv("PCC/RESE.csv", header = TRUE, sep = ";")
				summary(RESE)
				names(RESE)
				
				# import and inspect
				QUOT = read.csv("PCC/QUOT.csv", header = TRUE, sep = ";")
				summary(QUOT)
				names(QUOT)
				
					# tranform dates to a format I can work with
					
					# deal with missing start and end dates
					QUOT$quot_ep_startdate <- as.character(QUOT$quot_ep_startdate)
					QUOT$quot_ep_enddate <- as.character(QUOT$quot_ep_enddate)
					
					QUOT$quot_ep_startdate[which(is.na(QUOT$quot_ep_startdate))] <- "01jan1900"
					QUOT$quot_ep_enddate[which(is.na(QUOT$quot_ep_enddate))] <- "31dec2100"
					
					QUOT$quot_ep_startdate_posoxctformat <- as.POSIXct(as.character(QUOT$quot_ep_startdate),format=c("%d%b%Y"))
					QUOT$quot_ep_enddate_posoxctformat <- as.POSIXct(as.character(QUOT$quot_ep_enddate),format=c("%d%b%Y"))
				
				
######################################################################################
#################################### DATA BUILDING ###################################
######################################################################################
	
	##############################################
	# DATA 1: new approach to getting PARE data
	##############################################
	
		# generate PARE type data on basis of RESE		
		# get the dates properly formatted into this
			PARLBU <- PARL
			PARLBU$leg_period_start_posoxctformat <- as.POSIXct(as.character(PARLBU$leg_period_start),format=c("%d%b%Y"))
			PARLBU$leg_period_end_posoxctformat <- as.POSIXct(as.character(PARLBU$leg_period_end),format=c("%d%b%Y"))
			
			# excluding all of the regional parliaments
			head(PARLBU)
			nrow(PARLBU)
			PARLBU <- PARLBU[which(PARLBU$level == "NT"),]
			nrow(PARLBU)
			
			ELECBU <- ELEC[which(!duplicated(ELEC$parliament_id)),]
			ELECBU$country <- substr(ELECBU$elec_id,1,2)
			head(ELECBU[which(ELECBU$country == "NL"),])
			
			TEMPX <- sqldf("SELECT PARLBU.*, ELECBU.election_date
						   FROM PARLBU LEFT JOIN ELECBU
						   ON 
						   PARLBU.parliament_id = ELECBU.parliament_id
						   ")
			nrow(TEMPX)
			nrow(PARLBU)
			head(PARLBU)	
			PARLBU <- TEMPX			
			head(PARLBU)			   
			
		# below we also need the date for the election
			
		# get all of the German parliamentary RESE episodes sub-selected so we can merge these in on date
		
			# lets do some generic RESE cleaning, for now and for later
			
				# deal with left and right censored dates
					RESE$res_entry_start_cleaned <- gsub("[[rcen]]","",RESE$res_entry_start,fixed=TRUE)
					RESE$res_entry_start_cleaned <- gsub("[[lcen]]","",RESE$res_entry_start_cleaned,fixed=TRUE)
					RESE$res_entry_end_cleaned <- gsub("[[rcen]]","",RESE$res_entry_end,fixed=TRUE)
					RESE$res_entry_end_cleaned <- gsub("[[lcen]]","",RESE$res_entry_end_cleaned,fixed=TRUE)
					
				# deal with dates that are only years (select 1th of June)			
					RESE$res_entry_start_cleaned <- ifelse(nchar(RESE$res_entry_start_cleaned) == 4,paste("01jun",RESE$res_entry_start_cleaned,sep=""),RESE$res_entry_start_cleaned)
					RESE$res_entry_end_cleaned <- ifelse(nchar(RESE$res_entry_end_cleaned) == 4,paste("01jun",RESE$res_entry_end_cleaned,sep=""),RESE$res_entry_end_cleaned)
				
					RESE$res_entry_start_posoxctformat <- as.POSIXct(as.character(RESE$res_entry_start_cleaned),format=c("%d%b%Y"))
					RESE$res_entry_end_posoxctformat <- as.POSIXct(as.character(RESE$res_entry_end_cleaned),format=c("%d%b%Y"))
					summary(RESE$res_entry_start_posoxctformat)
					summary(RESE$res_entry_end_posoxctformat)
		
					# write in a column with a standerat indicator
			
				# do the reduction		
				RESERED <- RESE[which((RESE$country == "DE" | RESE$country == "CH"| RESE$country == "NL") & (RESE$political_function == "NT_LE_T3_NA_01" | RESE$political_function == "NT_LE-LH_T3_NA_01") & (!(grepl("-SR_",RESE$parliament_id)) | is.na(RESE$parliament_id) )),] # also exclude standerat entries here with a grepl, they have the same political function code, if rows do not have a parliament ID, do not exclude them on basis of this
				
				# run this line if you want to exclude CH completly 
				RESERED <- RESE[which((RESE$country == "DE" | RESE$country == "NL") & (RESE$political_function == "NT_LE_T3_NA_01" | RESE$political_function == "NT_LE-LH_T3_NA_01") & (!(grepl("-SR_",RESE$parliament_id)) | is.na(RESE$parliament_id) )),]
				
				head(RESERED)
				tail(RESERED)
				table(RESERED$country)
				table(is.na(RESERED$pers_id)) # no missings here
			
			# get internal r-date format here as well
			
				
				
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
			table(FPAREBU$parliament_id) # alright, this looks a lot more reasonable' although a lot of emtpy / small for earlier swiss years.
			head(FPAREBU)
		
			FPAREBU2 <-	sqldf("SELECT PARLBU.parliament_id, PARLBU.leg_period_start_posoxctformat, RESERED.pers_id,RESERED.res_entry_end_posoxctformat, RESERED.country
							FROM PARLBU LEFT JOIN RESERED
							ON
								PARLBU.country_abb = RESERED.country
								AND
								PARLBU.leg_period_start_posoxctformat >= RESERED.res_entry_start_posoxctformat
								AND
								PARLBU.leg_period_start_posoxctformat <= RESERED.res_entry_end_posoxctformat
						  ")
		
			head(FPAREBU2)
			nrow(FPAREBU2)
			table(FPAREBU2$parliament_id) # alright, this looks a lot more reasonable' although a lot of emtpy / small for earlier swiss years.
			head(FPAREBU2)
		
		# add a fake parliamentary episode ID
			nrow(FPAREBU)
			FPAREBU$fake_parl_episode_id <- paste(FPAREBU$pers_id,FPAREBU$parliament_id,sep="__")
			FPAREBU2$fake_parl_episode_id <- paste(FPAREBU2$pers_id,FPAREBU2$parliament_id,sep="__")
			length(unique(FPAREBU$fake_parl_episode_id)) # does not match!, several people occur double
			DUB <- FPAREBU[which(duplicated(FPAREBU$fake_parl_episode_id)),] # these are the problematic cases, not however, that because we remove these duplicates below this is not really an issue for this analysis. So will mark this as if 'fixed for now'
			nrow(DUB) 
			
			# for now, 
			nrow(FPAREBU)
			FPAREBU <- FPAREBU %>% distinct(fake_parl_episode_id, .keep_all = TRUE)
			FPAREBU2 <- FPAREBU2 %>% distinct(fake_parl_episode_id, .keep_all = TRUE)
			nrow(FPAREBU)
		
		# remove parliaments for which no matches exist
			FPAREBU <- FPAREBU[which(!is.na(FPAREBU$pers_id)),]
			nrow(FPAREBU)
			
			table(FPAREBU$parliament_id[which(FPAREBU$parliament_id %in% unique(POPABU$parliament_id))]) # there was a note about late entrants here, please note that this has now been fixed!
			
			table(FPAREBU2$parliament_id[which(FPAREBU2$parliament_id %in% unique(POPABU$parliament_id))]) # yes, not it is exactly the same! -- so, the decision is: focus on only right after the election or not?!
		
		
		# finally, lets merge in data from PAREWRONG if there is any for the fake parliamentary episode
			
			# first check that one for duplicates as well
				nrow(PAREWRONG)
				length(unique(PAREWRONG$parl_episode_id)) # so there are duplicates here as well that I should remove
				PAREWRONG[which(duplicated(PAREWRONG$parl_episode_id)),]
			
			# so also reduce this one before the merge to avoid duplicate matches
				PAREWRONGRED <- PAREWRONG %>% distinct(parl_episode_id, .keep_all = TRUE)
				nrow(PAREWRONGRED) # alright, looking promissing
				head(PAREWRONGRED)
			
			# now, lets merge in what we have there
				TEMP <- sqldf("SELECT FPAREBU.fake_parl_episode_id, PAREWRONGRED.*
						   FROM FPAREBU LEFT JOIN PAREWRONGRED
						   ON FPAREBU.fake_parl_episode_id = PAREWRONGRED.parl_episode_id
						 ")
				nrow(TEMP) # alright number is looking good, so lets write this result to FPAREBU
				tail(TEMP)
			
				table(is.na(FPAREBU$pers_id))
			#	FPAREBU <- TEMP
				table(is.na(FPAREBU$pers_id)) # why?! > looks like this is because there are RESE entries for earlier years e.t.c in FPAREBU that are not in PARE! - lets see what happens when we just leave merge out?!
				ISSUE <- FPAREBU[which(!FPAREBU$fake_parl_episode_id %in% PAREWRONGRED$parl_episode_id),]
				nrow(ISSUE)
				head(ISSUE)
				
			# everybody in this new FPAREBU file was in parliament at same point, so lets indicate that
				table(FPAREBU$member_ofthisparliament_atsomepoint)
				FPAREBU$member_ofthisparliament_atsomepoint <- "yes"
				table(FPAREBU$member_ofthisparliament_atsomepoint)

		head(FPAREBU)
		table(FPAREBU$parliament_id[which(FPAREBU$parliament_id %in% unique(POPABU$parliament_id))]) # so, these are not exacty the same... Do I understand why?

	##############################################
	# DATA 3: the buildup of the ELEN data-frame
	##############################################
			
		nrow(ELEN)
		ELEN$country <- substr(ELEN$elec_entry_id,1,2)
		table(ELEN$country)
		
		# do a country reduction, to make sure that we are not trowing energy and fixing and calculating things that are not important for later.
			ELENBU <- ELEN[which(ELEN$country == "DE" | ELEN$country == "NL"),]
			table(ELENBU$country)
		
		# first selection of variables of interest
			ELENBU <- sqldf("SELECT elec_entry_id, list_id, pers_id, listplace, country
							FROM ELENBU
							")
			nrow(ELENBU)
		
		# then we merge a couple of POLI level things to inspect
			ELENBU <- sqldf("SELECT ELENBU.*, POLI.last_name, POLI.first_name, POLI.gender, POLI.birth_date
							FROM ELENBU LEFT JOIN POLI
							ON
							ELENBU.pers_id = POLI.pers_id
							")
			nrow(ELENBU)
			table(ELENBU$country)
			head(ELENBU)
			tail(ELENBU)
			
		#	ELLI[which(duplicated(ELLI$list_id)),]
			
		# merge in some ELLI characteristics
			ELENBU <- sqldf("SELECT ELENBU.*, ELLI.list_name, ELLI.parliament_id, ELLI.district_id, ELLI.list_length, ELLI.party_id as 'party_id_from_elli'
							FROM ELENBU LEFT JOIN ELLI
							ON
							ELENBU.list_id = ELLI.list_id
							")
			nrow(ELENBU) # here there used to be an increase in cases but I fixed this in the data recently.
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
			tail(ELENBU)
		
		# and party membership from MEME
		
			## please be aware that this is something of which we know there are issues, although Adrian has by now fixed a lot of these!
			## something which is also interesting is that party is taken from MEME, but we also have a lot of people in here that where just candidates right? 
				## --> indeed, now the party id from ELLI above as well, this is now 'just' party id from MEME
		
			# prepare the date formats in both ELENBU and MEME
									
				# start and end of the legistlative period, including a month later and two weeks later
					ELENBU$leg_period_start_posoxctformat <- as.POSIXct(as.character(ELENBU$leg_period_start),format=c("%d%b%Y"))
					ELENBU$leg_period_start_monthlater <- ELENBU$leg_period_start_posoxctformat + months(1)
					ELENBU$leg_period_end_posoxctformat <- as.POSIXct(as.character(ELENBU$leg_period_end),format=c("%d%b%Y"))
					ELENBU$leg_period_end_monthearlier <- ELENBU$leg_period_end_posoxctformat - months(1)
					ELENBU$leg_period_start_twoweekslater_posoxctformat <- ELENBU$leg_period_start_posoxctformat + weeks(2) # and a version two weeks later to make the query below slightly less sensitive
					head(ELENBU)
				
				# start and end dates of the membership eppisodes
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
			
				# first also set empty values as missing
					
					# in elli
						table(ELENBU$party_id_from_elli)
						ELENBU$party_id_from_elli[which(ELENBU$party_id_from_elli == "")] <- NA
						table(ELENBU$party_id_from_elli)
					
					# in meme?
						table(ELENBU$party_id_from_meme) # all NA already
			
				ELENBU$samepartyid <- ifelse(ELENBU$party_id_from_elli==ELENBU$party_id_from_meme,TRUE,FALSE)
				table(ELENBU$samepartyid) # lots of cases where the party id is not the same
				head(ELENBU[which(ELENBU$samepartyid == FALSE),]) 
				tail(ELENBU[which(ELENBU$samepartyid == FALSE),])

			# quite some seem to be due to the national equivalent issue, so lets get a variable with national party equivalents
				ELENBU$party_id_from_elli_nat_equiv <- gsub("RE-[A-Z]{2}","NT",ELENBU$party_id_from_elli)
				table(ELENBU$party_id_from_elli_nat_equiv)
				
				ELENBU$party_id_from_meme_nat_equiv <- gsub("RE-[A-Z]{2}","NT",ELENBU$party_id_from_meme)
				table(ELENBU$party_id_from_meme_nat_equiv)
				
				ELENBU$samepartyid <- ifelse(ELENBU$party_id_from_elli_nat_equiv==ELENBU$party_id_from_meme_nat_equiv,TRUE,FALSE)
				
				# inspection of these results
				
					table(ELENBU$samepartyid) # 10 cases left #fixlater! - maybe!
					ELENBUNOTSAME <- ELENBU[which(ELENBU$samepartyid == FALSE),]
					ELENBUNOTSAME

			
			# combining these two party indicators, taking the election list one as the most reliable one
				
				ELENBU$party_id_from_elliandmeme <- ifelse(is.na(ELENBU$party_id_from_elli_nat_equiv),ELENBU$party_id_from_meme_nat_equiv,ELENBU$party_id_from_elli_nat_equiv)
				table(is.na(ELENBU$party_id_from_elli_nat_equiv))
				table(is.na(ELENBU$party_id_from_meme_nat_equiv))
				table(is.na(ELENBU$party_id_from_elliandmeme)) # ok, so this only ads very little extra information (30 cases or so) but that is fine :)
			
		####################
		# DATA 3.1: we analyse the percentages of german district seats on the level over their landes, hence we need to merge these together - 
		####################
			
			# we do this by simply overwriting the list ids with a generalised one
		
				# ELENBU > elected MPS
				table(ELENBU[which(ELENBU$country == "DE"),]$list_id)
			
			# so, what we would like is to have an adjusted version of the list id, that aggregates the district types to their bundesland
				
				# lets start with taking the region_abb from ELDI
				nrow(ELENBU)
				TEMP <- sqldf("SELECT ELENBU.*, ELDI.region_abb
							   FROM ELENBU LEFT JOIN ELDI
							   ON ELENBU.district_id = ELDI.district_id
							  ")
				nrow(TEMP)
				table(TEMP$region_abb)
				ELENBU <- TEMP

			# then for the german district candidates, we replace the list_id with another version on basis of this district abbreviation
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
			
			# lets add the result to another aggregation variable and OVERWRITE the general list id with this one
				ELENBU$list_id_aggr <- resvec
				ELENBU$list_id_old <- ELENBU$list_id
				ELENBU$list_id <- ELENBU$list_id_aggr
				head(ELENBU)
				head(ELENBU[which(!ELENBU$list_id_old == ELENBU$list_id_aggr),]) # this shows some cases where there are differences between the old and the new list id
		
	##############################################
	# DATA 4: the buildup of the ELLI data-frame
	##############################################
	
		####################
		# DATA 4.1: we analyse the percentages of german district seats on the level over their landes, also in ELLI this combination needs to be made
		####################
		
			# adding country reduction here as well
				ELLI$country <- substr(ELLI$list_id,0,2)
				table(ELLI$country)
				nrow(ELLI)
				ELLI <- ELLI[which(ELLI$country == "DE" | ELLI$country == "NL"),]
				nrow(ELLI)
	
			# also here we do this by simply overwriting the list ids with a generalised one
	
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
				
				# get a national party equivalent into ELLI as well, we need this below to generate the ID
					ELLI$party_id_nat_equiv <- gsub("RE-[A-Z]{2}","NT",ELLI$party_id)
					
					resvec2 <- vector()
					ELLI$list_id <- as.character(ELLI$list_id)
					for(i in 1:nrow(ELLI))
					{

						# loop throught the replacement here
							if (grepl("district-",ELLI$list_name[i])) # if this is a district, then replace the list_id
							{ 
								resvec2[i] <-  gsub("__.+__district-.+",paste("__district-seats-",ELLI$region_abb[i],"__",ELLI$party_id_nat_equiv[i],sep=""),ELLI$list_id[i])
							} else { #otherwise keep the current oneF
								resvec2[i] <- ELLI$list_id[i]
							}
					}	

			# OVERWRITE here!
				ELLI$list_id_aggr <- resvec2
				ELLI$list_id_old <- ELLI$list_id
				ELLI$list_id <- ELLI$list_id_aggr
			
			# inspect some relevant cases
				head(ELLI[which(!ELLI$list_id_old == ELLI$list_id_aggr),])
				
			# now for ELLI, we also need to combine (reduce?!) thse values? Because this is technically only ONE list now << Q: what do I mean with 'these values' here?!
				
				# only thing really needs to happen for that is to remove the district id and to then select all the unique rows?
				# lets think of a better way to do this, that keeps the district id in?! (Q: why would I want to keep the district_id?)
				
					ELLI$country <- substr(ELLI$list_id,1,2)
					table(is.na(ELLI$country))
					table(ELLI$country)
				
					ELLIBU <- sqldf("SELECT list_id, list_name, parliament_id, list_length, country, party_id, party_id_nat_equiv, district_id
								FROM ELLI
								")
					
					# inspect an example case
					ELLIBU[which(ELLIBU$list_id == "DE_NT-BT_2017__district-seats-NW__DE_Pi_NT"),]
					
					# this is where the magic happens (we just select the first occurence...)
					nrow(ELLIBU)
					ELLIBU <- ELLIBU[!duplicated(ELLIBU$list_id),] # seems rather simular (was 9714 before).. probably some other duplicates! .. fixes the issue for below already?
					nrow(ELLIBU)
					ELLIBU[which(ELLIBU$list_id == "DE_NT-BT_2017__district-seats-NW__DE_Pi_NT"),]
					
					table(is.na(ELLIBU$country))
					table(is.na(ELLIBU$list_id))
					table(ELLI$country)
					
			# also for ELLIBU, get rid of all lists with 'Netherlands' as the district as these are simply aggregated lists here with the intention to collect the number of votes per MP
				nrow(ELLIBU)
				ELLIBU  <- ELLIBU[which(!grepl("Netherlands", ELLIBU$district_id, ignore.case=TRUE)),]
				nrow(ELLIBU)
			
	##############################################
	# DATA 5: gender guessing on basis on namelist when nessary
	##############################################
	
		ELENBU$gender[which(ELENBU$gender == "")] <- NA
		
		ifelse(ELENBU$gender == "")
	
		table(is.na(ELENBU$gender))
		table(is.na(ELENBU$gender),ELENBU$country)
		
		# now let's use the Dutch girl names list #fixlater > no namelists for Germany and Switserland still at this stage
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
			
			DE_girlnames = read.csv("DE_girlnames.csv", header = FALSE, sep = ";")
			colnames(DE_girlnames) <- c("name")
			head(DE_girlnames)
			tail(DE_girlnames)
			nrow(DE_girlnames)
		
			DE_boynames = read.csv("DE_boynames.csv", header = FALSE, sep = ";")
			colnames(DE_boynames) <- c("name")
			head(DE_boynames)
			tail(DE_boynames)
			nrow(DE_boynames)
			
		# combine these
			girlnames <- rbind(NL_girlnames,DE_girlnames)
			boynames <- rbind(NL_boynames,DE_boynames)
			nrow(boynames)

		# example check
			TEST <- ELENBU[which(ELENBU$country == "NL"),]
			head(TEST)
			TEST$first_name[1] %in% boynames$name
			TEST$first_name[1] %in% girlnames$name

		resvec <- vector()
		for(i in 1:nrow(ELENBU))
		{
			if(is.na(ELENBU$gender[i])) # only do this when you do not know the gender
			{
				if(ELENBU$first_name[i] %in% boynames$name & !(ELENBU$first_name[i] %in% girlnames$name)) # if this name occurs in the boyname list and not in girlname list, then set gender to m
				{
				resvec[i] <- "m"
				} else {
					if(ELENBU$first_name[i] %in% girlnames$name & !(ELENBU$first_name[i] %in% boynames$name)) # if this name occurs in the girlname list and not in boyname list, then set gender to f
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
		
		# writing the result to ELENBU
			ELENBU$genderwithguesses <- resvec
		
		# inspection of the result
			table(ELENBU$gender)
			table(ELENBU$genderwithguesses)
			table(ELENBU$gender,ELENBU$genderwithguesses)
			
			table(is.na(ELENBU$gender))
			table(is.na(ELENBU$genderwithguesses))
			
			table(is.na(ELENBU$genderwithguesses),ELENBU$country) # so, decent amounts of missingness per country still
			prop.table(table(is.na(ELENBU$genderwithguesses),ELENBU$country),2) # about 13% in CH and NL and 5% in DE
			
			DECAS <- ELENBU[which(is.na(ELENBU$genderwithguesses)),]
			table(DECAS$parliament_id)
			
			DECAS[which(DECAS$parliament_id == "DE_NT-BT_2013"),]$first_name[0:40]
		
			table(is.na(ELENBU$genderwithguesses),ELENBU$list_id) 
			prop.table(table(is.na(ELENBU$genderwithguesses),ELENBU$list_id),2) 
			
			
			# lets export a list of all of these cases
			gendertodo <- unique(ELENBU[which(is.na(ELENBU$genderwithguesses)),]$pers_id)
			length(gendertodo)
			GENDERTODO <- as.data.frame(cbind(pers_id=gendertodo, country=substr(gendertodo,0,2)))
			head(GENDERTODO)
			
			prop.table(table(is.na(ELENBU$genderwithguesses),ELENBU$country),2)
			table(GENDERTODO$country)
			
	
	##############################################
	# DATA 6: find out which of these people entered parliament straight at the start
	##############################################
				
			# make a fictional parliament id
			table(is.na(ELENBU$pers_id))
			table(is.na(ELENBU$parliament_id)) # 30 parliament ids missing!
			ELENBU[which(is.na(ELENBU$parliament_id)),]
			
			ELENBU$fictional_parl_episode_id <- paste(ELENBU$pers_id,ELENBU$parliament_id,sep="__")
			head(ELENBU)
			
			ELENBU$in_parliament <- ifelse(ELENBU$fictional_parl_episode_id %in% FPAREBU$parl_episode_id,"yes","no") 
			table(ELENBU$in_parliament) 
			table(ELENBU$in_parliament)[2] / (table(ELENBU$in_parliament)[2]+table(ELENBU$in_parliament)[1]) # is roughly one third of the people on the election list that made it into parliament, that seems like a lot?
			
		# and reduce this to the people that where in parliament straight after the election  (taken from R019!) # Q: why am I doing this again?!
					
					# note that the cleanup and tranformation of the dates was done above already and also a selection to only eppisodes in parliament
					RESEMPENT <- RESERED
					
				# update PARL to have the startdate in useable format
						PARL$leg_period_start_posoxctformat <- as.POSIXct(as.character(PARL$leg_period_start),format=c("%d%b%Y"))
					
			# make a dataframe that contains all the people that entered within 'x' weeks of the start of when the faction started
			
					# set the range
						ELENBU$leg_period_start_weeklater_posoxctformat <- ELENBU$leg_period_start_posoxctformat + weeks(1)

					# moving these dates one day to fix overlap issues (on day of transition matching now) - commenting this out because should not be nessary anymore
					##	RESEMPENT$res_entry_start_posoxctformat =  RESEMPENT$res_entry_start_posoxctformat + days(1)
					##	RESEMPENT$res_entry_end_posoxctformat = RESEMPENT$res_entry_end_posoxctformat - days(1)
			
				# first, a query that checks for each constructed PARE of each politician if according to this MP its RESE episodes that specific parliaments, this MP was in this parliament in the first week
			
					# this used to contain a restriction where we also filtered on the parliament_id in RESE, but I do not think we should do that anymore so I removed that
					ELENBURED <- sqldf("SELECT ELENBU.*
									   FROM 
									   ELENBU, RESEMPENT
									   WHERE
									   (
										   ELENBU.pers_id = RESEMPENT.pers_id
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
				# seem to be not the case because above fictional_parl_episode_id are not created for everybody because some ELEN entries do not match with ELLI, I have asked Adrian to check this #fixlater / check when the data has been updated
				
				# who are these cases?
				ELENBURED[which(ELENBURED$in_parliament == "no"),]
		
		
				# is there missingness still on the fictional_parl_episode_id
				table(is.na((ELENBURED$fictional_parl_episode_id)))
	
	
				ELENBURED[which(ELENBURED$in_parliament == "no"),] #fixlater > so if there are still cases here even with the updated data..
	
	##############################################
	# DATA 7: find out what ELEN positions are actually 'electable' because we are reducing our data to these positions
	##############################################
	
			## ELEN integrity work
			ELEN[which(ELEN$list_id == "NL_NT-TK_1982__NL_NT-TK_1982__Nijmegen__Partij-van-de-Arbeid"),]
			ELENBU[which(ELENBU$list_id == "NL_NT-TK_1982__NL_NT-TK_1982__Nijmegen__Partij-van-de-Arbeid"),] # ok, here he is double?!
	
			# my suggested approach to this is to find your 'double ganger' in the last two elections, to see how (s)he did
			
			# find everybody there double gangers, you are a double ganger when
				# you ran for the same party
				# in the same district / on the same list /\ i.e. the same list id, but just with a different year
				
				# on the same list position (please note that for German districts listplace is always NA now in the data, as there is only 1!)
				
				# e.g. DE_Achilles_Matthias_1991 ran on list DE_NT-BT_2017__DE_NT-BT_2017__Aachen-I__district-Pi
				# his doubleganger from 2012 was? 
				TEMPE <- ELEN[which(ELEN$list_id == "DE_NT-BT_2013__DE_NT-BT_2013__Aachen-I__district-Pi"),]
				TEMPE
				
				ELLI[which(ELLI$list_id == "DE_NT-BT_2013__DE_NT-BT_2013__Aachen-I__district-Pi"),]
				
				TEMPE <- sqldf("SELECT TEMPE.*, ELLI.parliament_id 
								FROM TEMPE LEFT JOIN ELLI
								ON
								TEMPE.list_id = ELLI.list_id_old
								")
				
				# we can now also check if he got elected or not (if this returns a row, he did)
				FPAREBU[which(FPAREBU$pers_id == TEMPE$pers_id & FPAREBU$parliament_id == TEMPE$parliament_id),] # no hits, so no luck this year, how about the year before?
				
				TEMPE <- ELEN[which(ELEN$list_id == "DE_NT-BT_2009__DE_NT-BT_2009__Aachen-I__district-Pi"),] # right, so here we see that re-districting is screwing up our measure
				
				TEMPE <- sqldf("SELECT TEMPE.*, ELLI.parliament_id 
								FROM TEMPE LEFT JOIN ELLI
								ON
								TEMPE.list_id = ELLI.list_id_old
								")
				
				# we can now also check if he got elected or not (if this returns a row, he did)
				FPAREBU[which(FPAREBU$pers_id == TEMPE$pers_id & FPAREBU$parliament_id == TEMPE$parliament_id),]
				
			### /\ bit above is just here for illustrative purposes and can be removed later/
			
			# next step is to wrap this whole check in a (set of) functions
				# input: a list id
				# output: if your doubleganger had a seat in the last election
				
				# alright, so some prep work, first
				
					# I need to know the type already
					
						# for ELEN
						
							# for which first the trick from above needs to be applied
							ELEN$type <- ifelse(grepl("district-",ELEN$list_id),"district","list")
							ELENBU$type <- ifelse(grepl("district-",ELENBU$list_id),"district","list")
					
					# german district seats get list position NA, but lets make this '1' where it needs to be

						head(ELEN[which(ELEN$country == "DE", ELEN$type == "district"),])
						ELEN$listplace <- ifelse(ELEN$country == "DE" & ELEN$type == "district",1,ELEN$listplace)
						table(ELEN[which(ELEN$country == "DE", ELEN$type == "district"),]$listplace)
						
						head(ELENBU[which(ELENBU$country == "DE", ELENBU$type == "district"),])
						ELENBU$listplace <- ifelse(ELENBU$country == "DE" & ELENBU$type == "district",1,ELENBU$listplace)
						table(ELENBU[which(ELENBU$country == "DE", ELENBU$type == "district"),]$listplace)
						
					# function to match previous parliament
						
						# some variable values that can be used for debugging
                        local_list_id = "NL_NT-TK_2012__Groningen__12sep2012__Christen-Democratisch-Appel" #fixlater, check after data update if this goes alright with in the new version of the data the district id only being in their once!
                        local_list_position = 45
                        howfarback = 2
                        local_natparty_id = "NL_CDA_NT"
						
						local_list_id = ELENBU$list_id_old[22373]
						local_list_position = ELENBU$listplace[22373]
						howfarback =  1
						local_natparty_id =  ELENBU$party_id_from_elli_nat_equiv[22373]
						
						wasdoublegangertminxsuccesfull(local_list_id,local_list_position,howfarback,local_natparty_id)
						
						wasdoublegangertminxsuccesfull <- function(local_list_id,local_list_position,howfarback,local_natparty_id)
						{						
							country <- substr(local_list_id,0,2)
						
							# get the list id for my double ganger
						
								# first, get parliament id
								myparid <- substr(str_extract(local_list_id,".+?__"),0,nchar(str_extract(local_list_id,".+?__"))-2)
								myelectiondate <- str_extract(local_list_id,"[0-9]{2}[a-z]{3}[0-9]{4}")
								
								# find the previous parliament
								earlierparliament <- NA # reset
								earlierelectiondate <- NA # reset
								dateofearlierelection <- 
								if(howfarback == 1)
								{
									earlierparliament <- as.character(PARLBU[which(PARLBU$parliament_id == myparid),]$previous_parliament)
									earlierelectiondate <- as.character(PARLBU[which(PARLBU$parliament_id == earlierparliament),]$election_date)
								} else {
									if(howfarback == 2)
									{
										earlierparliament <- as.character(PARLBU[which(PARLBU$parliament_id == myparid),]$previous_parliament)
										earlierparliament <- as.character(PARLBU[which(PARLBU$parliament_id == earlierparliament),]$previous_parliament)
										
										earlierelectiondate <- as.character(PARLBU[which(PARLBU$parliament_id == earlierparliament),]$election_date)
										
									} else {
										if(howfarback == 3)
										{
										earlierparliament <- as.character(PARLBU[which(PARLBU$parliament_id == myparid),]$previous_parliament)
										earlierparliament <- as.character(PARLBU[which(PARLBU$parliament_id == earlierparliament),]$previous_parliament)
										earlierparliament <- as.character(PARLBU[which(PARLBU$parliament_id == earlierparliament),]$previous_parliament)
										
										earlierelectiondate <- as.character(PARLBU[which(PARLBU$parliament_id == earlierparliament),]$election_date)
										}
										
									}
									
								}
								
								# now generate the list id for my double ganger
									
									# first we replace the parliament
									doublegangerfakelistid <- gsub(myparid,earlierparliament,local_list_id) # this is where this goes wrong! We need to update the date as well with the new list id formats!
								
									# then we replace the date of the election
									doublegangerfakelistid <- gsub(myelectiondate,earlierelectiondate,doublegangerfakelistid)
									
								doublegangerparty <- local_natparty_id
								
							# and the district id
									doublegangerdistrictid <- str_extract(local_list_id, ".*?__.*?__") # first find the district id part of the complete list id < this need to be adjusted after the new formatting of the list ids
									doublegangerdistrictid <- gsub(myparid,earlierparliament,doublegangerdistrictid)# then replace the parliament_id part
									
									doublegangerdistrictid <- gsub("^__","",doublegangerdistrictid)	# then get rid of the __ and __ at the start and end of the remaining string
									doublegangerdistrictid <- gsub("__$","",doublegangerdistrictid)	# then get rid of the __ and __ at the start and end of the remaining string
									
									
							# now get her pers_id (Q: why are there two votes here?! again?! ) -- seems that vote two is for the districts!
                                
                                # lets relax the 'list position' assumption
                                
                                    # if their is a list_id match (more needed?) but no list_place match, set the local list positions to be the heightest number (lowest position)
                                    if(any(ELENBU$list_id_old == doublegangerfakelistid) & !any(ELENBU$list_id_old == doublegangerfakelistid & ELENBU$listplace == local_list_position))
                                    {
                                        local_list_position_updated <- max(as.numeric(ELENBU[which(ELENBU$list_id_old == doublegangerfakelistid),]$listplace))
                                    } else {
                                        local_list_position_updated <- local_list_position
                                    }
								
								#reset
								doublegangerpersidvoteone <- ""
								doublegangerpersidvotetwo <- ""
								
								doublegangerpersidvoteone <- ELENBU[which(ELENBU$list_id_old == doublegangerfakelistid & ELENBU$listplace == local_list_position_updated),]$pers_id
								doublegangerpersidvotetwo <- ELENBU[which(
																		  ELENBU$listplace == local_list_position_updated & 
																		  ELENBU$party_id_from_elli_nat_equiv == doublegangerparty &
																		  ELENBU$district_id == doublegangerdistrictid # Q: check here, are all of the district IDs still in ELENBU at this stage?!
																    ),]$pers_id 
								
								# if there are multiple double-gangers, for now please break the script.							
								if(length(unique(doublegangerpersidvoteone)) > 1 | length(unique(doublegangerpersidvotetwo)) > 1) # this is 'unique' because in the case of Dutch [8-12] number of vote indications e.t.c. for example we know we will get more then one hit. Which is not a problem for the script as long as this is the same person.
								{
									warning(paste("function was doing run - local_list_id:",local_list_id,
											   " - local_list_position_updated:",local_list_position_updated,
											   " - howfarback:",howfarback,
											   " - local_natparty_id:",local_natparty_id,
											   ". However, two or more double gangers where detected, so execution has been stopped. Please inspect!, the value for doublegangerpersidvoteone is:",doublegangerpersidvoteone,
											   " and the value for doublegangerpersidvotetwo is: ",doublegangerpersidvotetwo,
											   sep=""))
								}
								
								# if there are no double gangers tell me the reason! -- 
								if(length(doublegangerpersidvoteone) == 0 & length(doublegangerpersidvotetwo) == 0){
									
									mismatchreason <- "" # reset
									# is the 'old' list id the same? 
									if(nrow(ELENBU[which(ELENBU$list_id_old == doublegangerfakelistid
													     ),]) == 0)
									{
										mismatchreason <- "list_id: no match"
									} else {
										mismatchreason <- "list_id: match"
									}
									
									# are their any district matches 
									if(nrow(ELENBU[which(ELENBU$district_id == doublegangerdistrictid
													     ),]) == 0)
									{
										mismatchreason <- paste(mismatchreason," -- district: not found",sep="")
									} else {
										mismatchreason <- paste(mismatchreason," -- district: found",sep="")
									}
									
									# are their any party matches in this district?
									if(nrow(ELENBU[which(ELENBU$party_id_from_elli_nat_equiv == doublegangerparty),]) == 0)
									{
										mismatchreason <- paste(mismatchreason, " -- party: not found", sep="")
									} else {
										mismatchreason <- paste(mismatchreason, " -- party: found", sep="")
									}
									
									# does the party district combination exist?
									if(nrow(ELENBU[which(ELENBU$district_id == doublegangerdistrictid & 
														 ELENBU$party_id_from_elli_nat_equiv == doublegangerparty),]
														 ) == 0)
									{
										mismatchreason <- paste(mismatchreason, " -- party/district combo: not found", sep="")
									} else {
										mismatchreason <- paste(mismatchreason, " -- party/district combo: found", sep="")
									}
									
									
									# are their any list position matches for this group? (if not, better would be to pick the lowest list position that is a match?! -- this happens in recent years in the Netherlands where list are of varying length!)
									if(nrow(ELENBU[which(ELENBU$district_id == doublegangerdistrictid & 
														 ELENBU$party_id_from_elli_nat_equiv == doublegangerparty & 
														 ELENBU$listplace == local_list_position),]
														 ) == 0)
									{
										mismatchreason <- paste(mismatchreason, " -- list position: not found for this party and district", sep="")
									} else {
										mismatchreason <- paste(mismatchreason, " -- list position: found found for this party and district", sep="")
									}
	
								}
								
								# if either one of the two is empty, go for the one with a value
								if(length(doublegangerpersidvoteone) == 0 | length(doublegangerpersidvotetwo) == 0){
						
									if(length(doublegangerpersidvoteone) == 0){ # if one vote is empty, return the other one
									
										doublegangerpersid <- doublegangerpersidvotetwo
									} else {
										doublegangerpersid <- doublegangerpersidvoteone
									}
									
								} else { # if both have values, check if they are the same, if not trow an error, otherwise pick any
									if(doublegangerpersidvoteone == doublegangerpersidvotetwo)
									{
										doublegangerpersid <- doublegangerpersidvoteone
									} else {
										stop(paste(doublegangerpersidvoteone, " is not the same vote as ", doublegangerpersidvotetwo, "please check this entry and run the script again!",sep=""))
									}
								
								}
								
							# now, finally, check if this person got a seat in parliament for this election
						
							if(!length(doublegangerpersid) == 0) # only do this when a double ganger was actually found! Otherwise we have no way to tell so we would like the script to return the reason that no match was found!
							{
								if(nrow(FPAREBU[which(FPAREBU$pers_id == doublegangerpersid & FPAREBU$parliament_id == earlierparliament),]) > 0)
								{
									return(TRUE)
								} else {
									return(FALSE)
								}
							} else {
							return(mismatchreason)
							}
						}
						# why are there FPAREBU$pers_id entries that are NA?!
						
						
						# testing: proof is in the pudding!
						
						# for some of the very last entries shown here
						tail(ELENBU)
						start_time <- Sys.time()
						ELENBU[163000,]
						wasdoublegangertminxsuccesfull(ELENBU$list_id_old[163000],ELENBU$listplace[163000],1,ELENBU$party_id_from_elli_nat_equiv[163000]) # check manually and I think this is correct
						end_time <- Sys.time()
						end_time - start_time
						
						# same as below with the OM, also here I want to only run this bit of very time consuming code when it really is nessary
						
							# function to get the latest run
								
							getlatestrun <- function(type)
							{
								# fitst lets figure out what the newest time-stamp is
								filenames <- list.files(paste("INDA/",type,sep=""))
								
								# split
								filenamesmat <- as.data.frame(t(as.data.frame(strsplit(filenames,"_"))))
								colnames(filenamesmat) <- c("mydate","mytime","filetype") 
								filenamesmat$mydate <- as.numeric(as.character(filenamesmat$mydate))
								
								# this only among those that already have the highest date
								filenamesmatred <- filenamesmat[which(filenamesmat$mydate == max(filenamesmat$mydate)),]
								
								filenamesmatred$mytime <- as.numeric(as.character(filenamesmatred$mytime))
								
								latestrun <- paste(max(filenamesmatred$mydate),"_",sprintf("%04d",max(filenamesmatred$mytime)),sep="")
							
								return(latestrun)
							}
						
						# the default is to not run
						runDGagain <- FALSE
						
						latestrun <- getlatestrun("DG")
						
						# check if the version match
						if(!readLines("PCC/dataversion.txt") == readLines(paste("INDA/DG/",latestrun,"_dataversion.txt",sep=""))) # check if the version used in the latest run is the same as the currently used data version in the r-script
						{
							runDGagain = askYesNo("!Warning! The last stored data from the loop that does the Doppelganger detection is not the same as the current data version used for the R-script, do you want to run the DG loop again?! - this takes about 2 hours ")
						}
						
						# we can ofcourse also manually say we want do (not) run it again!
						
						nrow(ELENBU)
						# this bit can be ran to subsample ELENBU for debugging purposes
						# ELENBU <- sample_n(ELENBU,2000)
						nrow(ELENBU)
						
						# runDGagain = FALSE
						# runDGagain = TRUE
						
						if(runDGagain)
						{
								# and in a loop, for each ELEN position
								successfulldoubleganger_tminus1_vec <- vector(mode="character",length=nrow(ELENBU))
								successfulldoubleganger_tminus2_vec <- vector(mode="character",length=nrow(ELENBU))
								successfulldoubleganger_tminus3_vec <- vector(mode="character",length=nrow(ELENBU))
								pb <- txtProgressBar(min = 1, max = nrow(ELENBU), style = 3)
								for(i in 1:nrow(ELENBU))
								{
									
									successfulldoubleganger_tminus1_vec[i] <- wasdoublegangertminxsuccesfull(ELENBU$list_id_old[i],ELENBU$listplace[i],1,ELENBU$party_id_from_elli_nat_equiv[i])
									successfulldoubleganger_tminus2_vec[i] <- wasdoublegangertminxsuccesfull(ELENBU$list_id_old[i],ELENBU$listplace[i],2,ELENBU$party_id_from_elli_nat_equiv[i])
									successfulldoubleganger_tminus3_vec[i] <- wasdoublegangertminxsuccesfull(ELENBU$list_id_old[i],ELENBU$listplace[i],3,ELENBU$party_id_from_elli_nat_equiv[i])
									setTxtProgressBar(pb, i)
								}
								close(pb)
								
								tail(successfulldoubleganger_tminus1_vec)
								head(successfulldoubleganger_tminus2_vec)
								head(successfulldoubleganger_tminus3_vec)
								
								
								table(successfulldoubleganger_tminus1_vec)
								table(successfulldoubleganger_tminus2_vec)
								table(successfulldoubleganger_tminus3_vec)
								
								resvecelect <- vector(mode="logical",length=nrow(ELENBU))
								pb <- txtProgressBar(min = 1, max = nrow(ELENBU), style = 3)
								for(i in 1:nrow(ELENBU))
								{
									# because we included characters as well we need to turn this into a logical again
									successfulldoubleganger_tminus1 <- ifelse(successfulldoubleganger_tminus1_vec[i] == "TRUE",TRUE,FALSE) 
									successfulldoubleganger_tminus2 <- ifelse(successfulldoubleganger_tminus2_vec[i] == "TRUE",TRUE,FALSE) 
									successfulldoubleganger_tminus3 <- ifelse(successfulldoubleganger_tminus3_vec[i] == "TRUE",TRUE,FALSE) 
									
									resvecelect[i] <- successfulldoubleganger_tminus1 | successfulldoubleganger_tminus2 | successfulldoubleganger_tminus3 # if in one of the previous three elections then it counts as electable
									setTxtProgressBar(pb, i)
								}
								close(pb)
								
								write.csv(resvecelect,file=paste(getwd(),"/INDA/DG/",format(now(), "%Y%m%d_%H%M_"),"resvecelect.csv",sep=""))
								file.copy(paste(getwd(),"/PCC/dataversion.txt",sep=""),paste(getwd(),"/INDA/DG/",format(now(), "%Y%m%d_%H%M_"),"dataversion.txt",sep=""),overwrite=TRUE)
						} else # what to do when we are NOT running the loop again
						{
							# then load the latest of these files
								latestrun <- getlatestrun("DG")
								
								resvecelecttemp <- read.csv(paste("INDA/DG/",latestrun,"_resvecelect.csv",sep=""))
								resvecelect <- resvecelecttemp[,2]
						}
						
						table(successfulldoubleganger_tminus1_vec)
						sum(table(successfulldoubleganger_tminus1_vec))
						table(successfulldoubleganger_tminus1_vec) / sum(table(successfulldoubleganger_tminus1_vec))
						
						ELENBU$firstissue <- successfulldoubleganger_tminus1_vec
						ISSUES <- ELENBU[which(ELENBU$firstissue == "list_id: match -- district: found -- party: found -- party/district combo: found -- list position: not found for this party and district"),]
						head(ISSUES[which(ISSUES$country == "DE"),])
						table(ISSUES$country)
						table(ISSUES$country,ISSUES$firstissue)
						
						ISSUES2 <- ELENBU[which(ELENBU$firstissue == "list_id: no match -- district: found -- party: found -- party/district combo: not found -- list position: not found for this party and district"),]
						head(ISSUES2)
						table(ISSUES2$country)
						table(ISSUES2$country,ISSUES2$firstissue)
						
						ISSUES3 <- ELENBU[which(ELENBU$firstissue == "list_id: no match -- district: not found -- party: found -- party/district combo: not found -- list position: not found for this party and district"),]
						head(ISSUES3)
						table(ISSUES3$country)
						table(ISSUES3$country,ISSUES3$firstissue)
						
						table(successfulldoubleganger_tminus2_vec)
						table(successfulldoubleganger_tminus3_vec)
						
						ELENBU$firstissue <- successfulldoubleganger_tminus1_vec
						ELENBU$secondissue <- successfulldoubleganger_tminus2_vec
						ELENBU$thirdissue <- successfulldoubleganger_tminus3_vec
						
						
						ELENBU$electable <- ifelse(resvecelect,"electable","not electable")
						table(ELENBU$electable)
						table(is.na(ELENBU$electable))
						
						head(ELENBU)
						library("writexl")
						write_xlsx(ELENBU,"./ELENBU_20200804-1724.xlsx")
						
						tail(ELENBU)
						head(ELENBU[which(ELENBU$electable == "electable"),])
						ELENBU[38061:38100,]
			
			# do the actual reduction, so that the aggregates below are based on only the 'electable' part of the election lists
				
				ELENBU[which(ELENBU$list_id == "DE_NT-BT_2017__Baden-Wuerttemberg__Christlich-Demokratische-Union-Deutschlands-in-Niedersachsen"),]
				head(ELENBU)
				tail(ELENBU)
				
				ELENBUTOT <- ELENBU
				
				nrow(ELENBU)
				ELENBUTEMP <- ELENBU[which(ELENBU$electable == "electable"),] # switched to all positions when working on second analysis? (not sure anymore about what flavour we finally decided on here : conclusion from looking at manuscript now > I have been asked to focus on electable positions indeed, so this is correct.
				
				nrow(ELENBUTEMP)
				
				table(ELENBUTEMP$parliament_id,ELENBUTEMP$party_id_from_elli_nat_equiv)
				
				ELENBU <- ELENBUTEMP # Here you can switch of the reduction to only electable! --- this might be an issue I think, we use ELENBU below on multiple occasions as if it is not reduced.
				nrow(ELENBU)
				
			# analyse the remaining size of the issue, mainly by filtering on the relevant parties
			
				focuspartiesbelowvec <- c("DE_B90|Gru_NT","DE_CDU_NT","DE_CSU_NT","DE_Li|PDS_NT","DE_SPD_NT","NL_CDA_NT","NL_GL_NT","NL_PvdA_NT")
				
				table(ELENBUTOT$firstissue)
				ELENBUTOT$nojudgementpossible <- !((ELENBUTOT$firstissue == "TRUE" | ELENBUTOT$firstissue == "FALSE") | 
												 (ELENBUTOT$secondissue == "TRUE" | ELENBUTOT$secondissue == "FALSE") | 
												 (ELENBUTOT$thirdissue == "TRUE" | ELENBUTOT$thirdissue == "FALSE"))
				ELENBUTOT[0:20,]
				table(ELENBUTOT$nojudgementpossible)
				
				ELENBUISSUESLEFT <- ELENBUTOT[which((ELENBUTOT$party_id_from_elliandmeme %in% focuspartiesbelowvec) &
												     ELENBUTOT$nojudgementpossible & 
													 !(ELENBUTOT$parliament_id == "NL_NT-TK_1981") & 
													 !(ELENBUTOT$parliament_id == "NL_NT-TK_1982") & 
													 ! grepl("Netherlands", ELENBUTOT$district_id, ignore.case=TRUE)   # rest is temp!
												#	 !(ELENBUTOT$district_id == "NL_NT-TK_1986__Lelystad") & 
												#	 !(ELENBUTOT$district_id == "NL_NT-TK_2012__Bonaire") &
												#	 ! grepl("06sep1989__GroenLinks", ELENBUTOT$elec_entry_id, ignore.case=TRUE) &
												#	 ! grepl("05oct1980__Buendnis-90-Die-Gruenen", ELENBUTOT$elec_entry_id, ignore.case=TRUE) &
												#	 ! grepl("05oct1980__district-B90|Gru", ELENBUTOT$elec_entry_id, ignore.case=TRUE) &
												#	 ! grepl("Berlin__02dec1990", ELENBUTOT$elec_entry_id, ignore.case=TRUE) & 
												#	 ! grepl("02dec1990__Die-Linke-Partei-des", ELENBUTOT$elec_entry_id, ignore.case=TRUE)
													 ),]
				nrow(ELENBUISSUESLEFT)
				nrow(ELENBUTOT)
				nrow(ELENBUISSUESLEFT) / nrow(ELENBUTOT)
				
				# issue left distribution accross countries
				prop.table(table(ELENBUISSUESLEFT$country))
				
				# kinds of issues left
				table(ELENBUISSUESLEFT$firstissue,ELENBUISSUESLEFT$country)
				prop.table(table(ELENBUISSUESLEFT$firstissue,ELENBUISSUESLEFT$country),2)

				# inspect the most prominent issues left
				I1 <- ELENBUISSUESLEFT[which(ELENBUISSUESLEFT$firstissue == "list_id: no match -- district: found -- party: found -- party/district combo: not found -- list position: not found for this party and district"),]
				nrow(I1)
				head(I1)
				tail(I1)
				# quite some 1981 in the Netherlands, but we do not use that year!, so lets filter it out above!
				
					# outline of issues
					
						# NL > nice, for sure, the issues below are the only issues left now! Doppelganger could be found for all(!) other cases
							# new electoral district: Lelystad in 1986 - 60 cases
							nrow(ELENBUISSUESLEFT[which(ELENBUISSUESLEFT$district_id == "NL_NT-TK_1986__Lelystad"),])
							
							# new electoral district: Bonaire in 2012 - 178 cases
							nrow(ELENBUISSUESLEFT[which(ELENBUISSUESLEFT$district_id == "NL_NT-TK_2012__Bonaire"),])
							
							# new party : groenlinks in 1989
							nrow(ELENBUISSUESLEFT[which(grepl("06sep1989__GroenLinks", ELENBUISSUESLEFT$elec_entry_id, ignore.case=TRUE)),]) # 570 cases
				
						# DE
							
							# a lot of these are issues with new districts, I think this is the list
							unique(I1$district_id)
							
							# new party: Buendnis-90-Die-Gruenen in 1980
							nrow(ELENBUISSUESLEFT[which(grepl("05oct1980__Buendnis-90-Die-Gruenen", ELENBUISSUESLEFT$elec_entry_id, ignore.case=TRUE)),]) # 166 cases
							nrow(ELENBUISSUESLEFT[which(grepl("05oct1980__district-B90|Gru", ELENBUISSUESLEFT$elec_entry_id, ignore.case=TRUE)),]) # 646 cases
							
							
							# new party: die Linke in 1990
							nrow(ELENBUISSUESLEFT[which(grepl("02dec1990__Die-Linke-Partei-des", ELENBUISSUESLEFT$elec_entry_id, ignore.case=TRUE)),]) # 143 cases
							
							
							# 1990 Berlin issue
							nrow(ELENBUISSUESLEFT[which(grepl("Berlin__02dec1990", ELENBUISSUESLEFT$elec_entry_id, ignore.case=TRUE)),]) # 40 cases
				
							# 419 cases where German parties did not run any candidates in this district in the previous 3 elections
							unique(I1$list_id)
				
				
			# some inspections, just to make sure that things are making sense here
			
				# list big party from NL
					ELENBUTOT[which(ELENBUTOT$party_id_from_elliandmeme == "NL_CDA_NT" & ELENBUTOT$district_id == "NL_NT-TK_2003__Nijmegen"),]
					ELENBUTOT[44645,]
					ELENBUTOT[44645,]$listplace
					ELENBUTOT[44646,]	
				
				# list big part from DE
				
				# list small party from NL
					TD <- ELENBUTOT[which(ELENBUTOT$party_id_from_elliandmeme == "NL_GL_NT" & ELENBUTOT$district_id == "NL_NT-TK_2003__Tilburg"),]
					TD[order(TD$listplace),]
					ELENBUTOT[44645,]
					ELENBUTOT[44645,]$listplace
					ELENBUTOT[44646,]				
		

			# 

		#################################### DATA AGGREGATION starts here ###################################
	
	##############################################
	# DATA 8: get percentage of women on the list, with gender taken from ELENBU, which contains the gender guesses as well > please note that we do NOT use ELENBURED here, because we want everybody, not just those elected! - at start or later does not matter!
	##############################################
	
	##### aggregation on the ELLI level ######
	
			length(unique(ELENBU$list_id))
	
			GCELLI <- as.data.frame.matrix(table(ELENBU$list_id,ELENBU$genderwithguesses)) 
			head(GCELLI)
			nrow(GCELLI)
			
			# does this give us a value for every single list?
			nrow(GCELLI) == length(unique(ELENBU$list_id)) # yes it does!
			
			GCELLI$list_id <- rownames(GCELLI)		
			GCELLI$country <- substr(GCELLI$list_id,1,2)
			table(GCELLI$country)
			table(is.na(GCELLI$country))
			GCELLI$ratio <- GCELLI$f / (GCELLI$f+GCELLI$m)
			summary(GCELLI$ratio)
			
			head(GCELLI[which(GCELLI$country == "DE"),])
			tail(GCELLI[which(GCELLI$country == "DE"),])
			# this looks good
					
			hist(GCELLI$ratio) # so this is the ratio of men/women on the election lists

		# merge into the ELLI level data-frame which above was given the same abbreviated list ids, this data-frame was reduced above to get a ratio on list variable which we need later!
			
			head(ELLIBU)
			table(ELLIBU$list_id %in% GCELLI$list_id) # alright, so a lot of list_ids do not occur!
			
			
			ELLIBU <- sqldf("SELECT ELLIBU.*, GCELLI.f, GCELLI.m, GCELLI.ratio as 'ratio_on_list'
						FROM ELLIBU LEFT JOIN GCELLI
						ON
						ELLIBU.list_id = GCELLI.list_id
						")
			summary(ELLIBU$ratio_on_list) # so indeed the majoruty is missing.. maybe genderwithguesses does not have what it needs?
		
			head(ELLIBU)
			tail(ELLIBU)
			ELLIBU[30:50,]
			tail(ELLIBU)
			table(ELLIBU$parliament_id) # this looks a lot better now
			length(unique(ELLIBU$list_id))
			nrow(ELLIBU) # bang on
			
		# get a count of the number of people in each faction
			MemVec <- as.matrix(table(ELENBU$list_id)) # also this list id has been replaced with the aggregate one
			MEMCOUNT <- data.frame(rownames(MemVec),unlist(MemVec))
			colnames(MEMCOUNT) <- c("list_id","list_member_count")
			head(MEMCOUNT)

			nrow(ELLIBU)
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
			
			# how big is this issue among the actual parties we are interested in?
			table(ELLIBU$party_id_nat_equiv)
			table(is.na(ELLIBU$party_id_nat_equiv))
			
			table(ELLIBU$sumcheck[which(ELLIBU$party_id_nat_equiv %in% focuspartiesbelowvec)])
			ELLIBU[which(ELLIBU$sumcheck < -10),] # a lot of NL 2012... does that make sense? : yes it does, a lot entries with only innitials.. this is pulling NL CDA down very probably! - I do think we should maybe still invest time into this!
			
			# lets make a todolist here! - pers_ids for which I need a full first name
			
				# Complete Name Search Needed
				CNSN <- ELENBU[which(is.na(ELENBU$genderwithguesses)),]
				nrow(CNSN)
				head(CNSN)
				length(unique(CNSN$pers_id)) # only 370 cases!
				persidstocheck <- unique(CNSN$pers_id)

			# to make things easier, lets enricht things a but here!
			
				# first, some obvious additions to the keyword list are in place - need to add these to the gender keyword lists!
				 # male: Chris, Jan, Herman, Otto, Frank, Martin, Michael, Peter, Paul, Simon, Teun, Frank, Johannes, Reinhard, Ludwig, Christian, Dirk, Olaf, Daniel, Jens, Otto
				 # female: Andrea, Anka, Anna, Heike, Melanie, Caroline, Julia, Maria, Elske, Ulrike, Anke, Evelyn
				
				# then there are the remainind Dutch 'single innitial' cases
				persidstocheck[which(grepl("_[A-Z]_",persidstocheck))]
				length(persidstocheck[which(grepl("_[A-Z]_",persidstocheck))])
				
				CNSN2 <- as.data.frame(persidstocheck[which(grepl("_[A-Z]_",persidstocheck))])
				colnames(CNSN2) <- "pers_id"
				CNSN3 <- sqldf("SELECT CNSN2.pers_id, min(ELEN.list_id)
						FROM CNSN2 LEFT JOIN ELEN
						ON
						CNSN2.pers_id = ELEN.pers_id
						GROUP BY CNSN2.pers_id
						")
				nrow(CNSN2)
				nrow(CNSN3)
				
				# export
				write_xlsx(CNSN3,"./PoliticiansForWhichWeWouldLikeTheFullName-20200806-1521.xlsx")
				
		# lets select the 'complete' cases: where these is sufficient knowledge on the number of men and women
			
			nrow(ELLIBU)
			ELLIBUCOMP <- ELLIBU
			
		#	ELLIBUCOMP <- ELLIBU[which(ELLIBU$sumcheck == 0),]
		
			CD <- ELLIBU[which(!ELLIBU$sumcheck > -8),]
			table(CD$parliament_id) # right, so way to many from the Dutch 2012 parliament indeed!
			CD[which(CD$parliament_id == "NL_NT-TK_2012"),] # so inspection reveals, very many cases with only innitials, this is where this goes wrong, because no gender guesses are possible in this case.. 
		
		# so... lets just remove this restriction for now
		##	ELLIBUCOMP <- ELLIBU[which(ELLIBU$sumcheck > -8),] # arbritary decision, how big do I allow the gap to be?, right so the last year for NL is off a lot?!
			nrow(ELLIBUCOMP)
			nrow(ELLIBU) - nrow(ELLIBUCOMP) # only lossing about 700 election lists here
			
			nrow(ELLIBU) / (nrow(ELLIBUCOMP)+nrow(ELLIBU)) # using about 50% of the currently available cases (which are all list for CH, but only some of the main parties for NL and only 2017 for DE?)
			
			table(ELLIBUCOMP$parliament_id)
			ELLIBUCOMP$parliament_id <- as.factor(as.character(ELLIBUCOMP$parliament_id))
			table(ELLIBUCOMP$parliament_id)
		
		boxplot(ELLIBUCOMP$ratio_on_list~ELLIBUCOMP$country,ylab="Ratio on list")
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
	
	
	##################################################################################################
	######### creation of bunch of variables before reduction to analytical sample ###################
	##################################################################################################
	
	##############################################
	# DATA 9: building up additional ELLI variables
	##############################################
	
		# 
		##### if the party id is not a national party, get the mother party id ###
		
			ELLIBU[0:20,]
			head(ELLIBU[which(ELLIBU$party_id ==""),])
			i = 6
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
			
			# some further fixing up and cleanup of the national party ids?

			table(ELLIBU$nat_party_id) # inspection suggests that the empty values left here are due to regional party ids that do not occur in PART?
			table(is.na(ELLIBU$nat_party_id)) # current 468 missings here.. --- I should inspect again how big this issue is once the quota info is in?!
			table(ELLIBU$party_id_nat_equiv) # currently no more empty values here... 
			
			#fixlater : what is going on here: why are there two, and which one should I use? / do I use? 
				# party_id_nat_equiv is simply a string replace, see line 467 gsub("RE-[A-Z]{2}","NT",ELLI$party_id)
				# nat_party_id is based on the mother party id..
				# one question here is why we do not combine these... maybe no need because at least for the parties that have a quota we do have nat_party_ids? #fix later maybe -- not a very big isssue I think!
			
			table(is.na(ELLIBU$party_id_nat_equiv)) # values for all here, for whatever its worth

		## need the pargov ids as well
		
		TEMPX <- sqldf("SELECT ELLIBU.*, PART.party_parlgov_id
						FROM ELLIBU LEFT JOIN PART
						ON ELLIBU.party_id_nat_equiv = PART.party_id
						")
		head(TEMPX)
		table(TEMPX$party_parlgov_id)
		table(is.na(TEMPX$party_parlgov_id))
		nrow(ELLIBU)
		nrow(TEMPX)
		ELLIBU <- TEMPX
	
	##############################################
	# DATA 10: ELLIBU gets ratio of women elected on the list level, please note that we calculate the ratio of men/women for each list that lead to anybody being elected - we do use reduced list here, because we only want those people that where there at the start!
	##############################################
	
	
			# so we loose some lists here, for example 'DE_NT-BT_2009__district-seats-HB' -- why?!
			table(is.na(ELENBURED$list_id),is.na(ELENBURED$genderwithguesses)) # it is not missingness
			ELENBURED[which(grepl("DE_NT-BT_2009__district-seats-HB",ELENBURED$list_id,fixed=TRUE)),] # only 5 cases as well.. I guess if nobody got elected from the list, we don't get a ratio (makes a lot of sense). I think we are OK with that.
			
			GCPARE <- as.data.frame.matrix(table(ELENBURED$list_id,ELENBURED$genderwithguesses)) 
			GCPARE$list_id <- rownames(GCPARE)
			head(GCPARE)
			GCPARE[30:50,]
			tail(GCPARE)
			
			GCPARE$ratio <- GCPARE$f / (GCPARE$f+GCPARE$m)
			hist(GCPARE$ratio)
			
			
			
		# now merge this in into the ELLI data we made above - looks like something is going wrong here?!
		
		nrow(ELLIBU)
		ELLIBU <- sqldf("SELECT ELLIBU.*, GCPARE.f as 'f_elected', GCPARE.m as 'm_elected', GCPARE.ratio as 'ratio_elected'
						FROM ELLIBU LEFT JOIN GCPARE
						ON
						ELLIBU.list_id = GCPARE.list_id
						")
		nrow(ELLIBU)
			head(ELLIBU)
			ELLIBU[30:50,]
			ELLIBU[4030:4050,]
			ELLIBU[9030:9050,] # these are the district seats, looks good now. Lots of NA is because lots of parties never got anybody elected.
			tail(ELLIBU)
			
		# lets make a variable that indicates if anybody from the list was elected at all - can be used for exclusion below
		ELLIBU$anycandidateselected <- ifelse(ELLIBU$list_id %in% GCPARE$list_id,"yes","nobody")
		table(ELLIBU$anycandidateselected)

	##############################################
	# DATA 11: ELLIBU gets a couple of other additional variables
	##############################################

	##### get a was there a quota variable so it can be used to reduce to the analytical sample #####

			### get the start-date of the parliamentary term in
		
				ELLIBU <- sqldf("SELECT ELLIBU.*, PARL.leg_period_start, PARL.leg_period_end
                   FROM ELLIBU LEFT JOIN PARL 
                   ON ELLIBU.parliament_id = PARL.parliament_id")
				head(ELLIBU)
		
				# fix left and right censoring things
				ELLIBU$leg_period_start <- gsub("[[rcen]]","",ELLIBU$leg_period_start,fixed=TRUE)
				ELLIBU$leg_period_start <- gsub("[[lcen]]","",ELLIBU$leg_period_start,fixed=TRUE)
				ELLIBU$leg_period_end <- gsub("[[rcen]]","",ELLIBU$leg_period_end,fixed=TRUE)
				ELLIBU$leg_period_end <- gsub("[[lcen]]","",ELLIBU$leg_period_end,fixed=TRUE)
					
				ELLIBU$leg_period_start_posoxctformat <- as.POSIXct(as.character(ELLIBU$leg_period_start),format=c("%d%b%Y"))
				ELLIBU$leg_period_end_posoxctformat <- as.POSIXct(as.character(ELLIBU$leg_period_end),format=c("%d%b%Y"))
				summary(ELLIBU$leg_period_start_posoxctformat)
				summary(ELLIBU$leg_period_end_posoxctformat)
		
			names(ELLIBU)
			table(ELLIBU$nat_party_id) # please note the one from the mother party is used here, so I can alert Elena to that. #fix at some point: if we add Swiss quota data - on the level of the regional parties - this will need to be fixed!
			
			# dealing with NA in 'QUOT' was done above
			
			ELLIBU <- sqldf("SELECT ELLIBU.*, QUOT.quota_bin, QUOT.quot_ep_startdate_posoxctformat, QUOT.quota_percentage, QUOT.quota_zipper, QUOT.quota_soft
					   FROM ELLIBU LEFT JOIN QUOT 
					   ON 
					   
					   ((ELLIBU.nat_party_id = QUOT.party_id) OR (ELLIBU.party_id = QUOT.party_id))
					   
					   AND 
					   
						(ELLIBU.leg_period_start_posoxctformat BETWEEN QUOT.quot_ep_startdate_posoxctformat and QUOT.quot_ep_enddate_posoxctformat)
						
						")
				
			tail(ELLIBU)
			table(ELLIBU$quota_bin)
			table(ELLIBU$quota_bin,ELLIBU$country) # looking good now
			summary(ELLIBU$quota_bin)
					
			# we call this one 'quota now'
			names(ELLIBU)[match(c("quota_bin"),names(ELLIBU))] <- "quota_now"
			ELLIBU$quota_now <- as.factor(ELLIBU$quota_now)
			head(ELLIBU)
			tail(ELLIBU)
			table(ELLIBU$quota_now)
			table(is.na(ELLIBU$quota_now),ELLIBU$country) # 0 for NL here simply means we have quota infor for all Dutch parties.


	##### get the number of people that was elected from this district in (please note that in the current version of the script we take the district_magnitude straight from ELLI!).
		
			# step 1: this issue has been solved above, kept district ids in so that they can be used here!

			ELLIBU[9030:9050,] # good to use for checks because bunch of district ids here that should get magnitude 1
			
			i = 9030
			mydistrict <- "DE_NT-BT_2017__Erfurt-Weimar-Weimarer-Land-II"
			resvec <- vector()
			for(i in 1:nrow(ELLIBU))
			{
				mydistrict <- ELLIBU$district_id[i]	
				resvec[i] <- nrow(ELENBURED[which(ELENBURED$district_id == mydistrict),]) 
			}
			resvec
			
			ELLIBU$nr_elected_from_district <- resvec
			tail(ELLIBU)
			head(ELLIBU)
			table(ELLIBU$nr_elected_from_district) # in earlier versions of the script this used to be called district magnitude, now I am replacing this with proper values from ELDI
			hist(ELLIBU$nr_elected_from_district)
				
	##### get party size in (for now just number of people from this party that got elected in the parliament) << BROKEN now, fix later! # fix later

			# also here, get the national party versions for ELENBURED, lets use party_id_from_elli_nat_equiv for this?! - if coded is needed again later you can take it from above

			table(ELENBURED$party_id_from_elli_nat_equiv)
		
			resvec <- vector()
			for(i in 1:nrow(ELLIBU))
			{
				mypartyid <- ELLIBU$nat_party_id[i]
				myparliamentid <- ELLIBU$parliament_id[i]	
				# so what I do below is the following: I check the number of unique individuals 
				resvec[i] <- length(unique(ELENBURED$pers_id[which(ELENBURED$party_id_from_elli_nat_equiv == mypartyid & ELENBURED$parliament_id == myparliamentid)]))
			}
			resvec
			
			ELLIBU$party_size <- resvec
			hist(ELLIBU$party_size)
			tail(ELLIBU)
			head(ELLIBU)
			
			boxplot(ELLIBU$party_size~ELLIBU$country)
			
			head(ELLIBU[which(ELLIBU$party_size == 0 & ELLIBU$country == "CH"),])
			tail(ELLIBU[which(ELLIBU$party_size == 0 & ELLIBU$country == "CH"),])

######################################################################################
#################################### RED A: REDUCTION TO ANALYTICAL SAMPLE ##################
######################################################################################

		# reduction of the election list data, ratio elected has already been merged in, so this is the only reduction needed?
		
		# get rid of parliaments that we don't want to analyse -- time-frame
			table(ELLIBU$parliament_id)
			ELLIBU$intimeframe <- ifelse(ELLIBU$leg_period_start_posoxctformat > 1981-01-01,"yes","before 1981")
			head(ELLIBU)
			tail(ELLIBU)
			table(ELLIBU$intimeframe)
			
			nrow(ELLIBU)
			ELLIBU <- ELLIBU[which(!ELLIBU$intimeframe == "before 1981"),] # this is where the reduction is done
			nrow(ELLIBU)
			table(ELLIBU$party_id_nat_equiv)
			table(ELLIBU$country)
			
		# get rid of all election lists from which nobody ever got eleced
			table(ELLIBU$anycandidateselected)
			
			nrow(ELLIBU)
			ELLIBU <- ELLIBU[which(!ELLIBU$anycandidateselected == "nobody"),]
			nrow(ELLIBU)		
			table(ELLIBU$country)
		
		# get rid of all observations for which there is no gender quota -- please note that this is indirectly also a filter on the election lists we have the (correct) national party ids for!
			table(ELLIBU$quota_now)
			
			nrow(ELLIBU)
			ELLIBU <- ELLIBU[which(ELLIBU$quota_now == 1),]
			nrow(ELLIBU) # 1270 election lists left
			
	###########################
	# RED A1: dropping the district lists from small german parties.
	###########################
	
		# NEW - get rid of German district candidates from SMALL parties (because they don’t stand any chance to win, there is nothing at stake.
			# .. which is why the logic for selection is very likely very different from the other district candidates, and election does not exist).
			
				# need this here already now
				ELENBURED$type <- ifelse(grepl("district-seats-",ELENBURED$list_id),"district","list")
				ELLIBU$type <- ifelse(grepl("district-seats-",ELLIBU$list_id),"district","list")
			
				# what is a small German party?
					ELENBUREDDETEMP <- ELENBURED[which(ELENBURED$country=="DE"),]
					ELLIBUDETEMP <- ELLIBU[which(ELLIBU$country=="DE"),]
				
					# what parties 'never' won district seats?
					table(ELENBUREDDETEMP$type,ELENBUREDDETEMP$party_id_from_elliandmeme) # this is actually hard to get at, because of double mandates...
					hist(ELLIBUDETEMP$party_size)
					
					meannarm <- function(input)
					{
						return(mean(input,na.rm=TRUE))
					}
					
					aggregate(ELLIBUDETEMP$party_size, by=list(ELLIBUDETEMP$party_id_nat_equiv), meannarm)
			
				nrow(ELLIBU)
				
				## this is where the reduction happens, two options (the second of which is currently used)
				
					# option 1: do this really on size of a number, which is time-specific.
						table(ELLIBU$party_size,ELLIBU$type) # so, still plenty of cases in here with list seats won by small parties.
						
						#	ELLIBU <- ELLIBU[which(!(ELLIBU$party_size < 75 & ELLIBU$country == "DE" & ELLIBU$type == "district")),] # this is the line that does the reduction
							nrow(ELLIBU)
						
						table(ELLIBU$party_id_nat_equiv) # so, CU is also dropped here already... why?! > answer is that the election list data for CU is not yet in the data. Simply not done yet #fixlater
						table(ELLIBU$party_id_nat_equiv,ELLIBU$type) # looking alright to me
					
					# option 2: do this on basis of party id's < option currently active!
						boxplot(as.numeric(ELLIBU$party_size)~ELLIBU$party_id_nat_equiv)
						
							# alright so the 'small' parties in DE are: 
							partiestoexcludesmalldistricsfromvec <- c("DE_B90|Gru_NT","DE_Li|PDS_NT")
							table(ELLIBU$party_id_nat_equiv,ELLIBU$type)
							ELLIBU <- ELLIBU[which(!((ELLIBU$party_id_nat_equiv %in% partiestoexcludesmalldistricsfromvec) & ELLIBU$type == "district")),] # this is the line that does the reduction
							nrow(ELLIBU) # number seem to match
	
						# Philip suggested to also include CSU lists, but the CSU does not have a gender quota specified so is not in the data in the first place
						
				# couple of inspections of key variables after reduction
					head(ELLIBU)
					tail(ELLIBU)
					ELLIBU[500:510,]
					table(ELLIBU$party_id_nat_equiv)
					
					table(is.na(ELLIBU$ratio_on_list)) # not complete! #fixlater one missing
					ELLIBU[which(is.na(ELLIBU$ratio_on_list)),]
					
					table(is.na(ELLIBU$ratio_elected)) # complete
					table(is.na(ELLIBU$quota_percentage)) # complete
					
					
				# and the 'table 1' for the publication
				
					table(ELLIBU$parliament_id,ELLIBU$country)
					
	###########################
	# RED A2: is OM etc to get Dutch election lists that are 100% simular so only the ones that actually vary are included 
	###########################
		
		## step 1: make a function that generates an - ';' separated - list position ordered - array of all pers_ids occurring on a certain list (takes a list id as input)
		
			head(ELENBUTOT)
			
			# testing what goes wrong we DE Bundestag
			local_list_id = "DE_NT-BT_2017__Baden-Wuerttemberg__Christlich-Demokratische-Union-Deutschlands-in-Niedersachsen" # so, does not occur in ELENBU apparently?!
				
			getpersidarrayforlistid <- function(local_list_id)
			{
			# select 
				ELENBUME <- ELENBUTOT[which(ELENBUTOT$list_id == local_list_id),]
				return(paste(ELENBUME$pers_id,collapse=";"))
			}
	
			getpersidarrayforlistid(ELLIBU$list_id[4000]) # inspections seems promissing
			tail(ELLIBU)
			
			# old incorrect version! (remove comments first if you want to run it)
	#		getpersidarrayforlistid <- function(local_list_id)
	#		{
	#		# select 
	#			ELENBUME <- ELENBU[which(ELENBU$list_id == local_list_id),]
	#			return(paste(ELENBUME$pers_id,collapse=";"))
	#		}1
			
		## step 2:

			# split the arrays to use into a vector again

				getindeldistfortwoarrays <- function(local_pers_id_array,other_pers_id_array)
				{
				# looked into doing this myself, but tricky, lets just used traminer for this
				
					# first lets get a current party dictionary
					local_dictionary <- unique(c(local_pers_id_array,other_pers_id_array))
					
					if(!length(local_dictionary) == 1)
					{
						# create data format as input for the seqdef function
						
							# create
							LOCALSEQDAT <- as.data.frame(rbind(local_pers_id_array,other_pers_id_array))
							longestarraylength <- max(length(local_pers_id_array),length(other_pers_id_array))
							colnames(LOCALSEQDAT) <- seq(from=1,to=longestarraylength,by=1)
							
							# set NA values to avoid repitition
							if (!length(local_pers_id_array) == longestarraylength)
							{
								LOCALSEQDAT[1,][(length(local_pers_id_array)+1):longestarraylength] <- NA
							}
							if (!length(other_pers_id_array) == longestarraylength)
							{
								LOCALSEQDAT[2,][(length(other_pers_id_array)+1):longestarraylength] <- NA
							}
						
						submat <- matrix(1L, nrow = length(local_dictionary) , ncol = length(local_dictionary))
						diag(submat) <- 0
						
						LocalSeqObj <- suppressWarnings(suppressMessages(seqdef(LOCALSEQDAT,states=local_dictionary)))
						resultingindeldist <- suppressMessages(seqdist(LocalSeqObj,method="OM", indel=1,sm=submat))[2,1]
					} else {
						resultingindeldist = 0
					}
					
					return(resultingindeldist)
				}
			
			# testing
			aa <- strsplit(getpersidarrayforlistid(ELLIBU$list_id[200]),";")[[1]]
			bb <- sort(strsplit(getpersidarrayforlistid(ELLIBU$list_id[900]),";")[[1]])
			
			getindeldistfortwoarrays(aa,bb)
		
		## step 3: 
			# 1. make a script that..
				# creates a vector of all relevant election lists to compare
				# loops through the pers_id arrays of all relevant lists
				# calculates and returns an average % difference for 'list' with the relevant comparison lists.
				# calculates and returns percentage of lists that is 95% simular
		
		# wrapping this whole thing in an overall loop through ELLIBU
		
		
				# create a selection of all relevant election lists to compare - relevant lists are all lists from the same party in the same election

					# so this takes - a lot - of time to run, so it only does when you explicity tell it to and suggests you to when the dataversion has changed
					
					# by default we are not running this!
						runOMagain <- FALSE
				
					# manual overwrite is possible (should be of by default!)
					manualoverwrite <- ""
					#  manualoverwrite <- "20190519_0845"
				
					# check version numbers though!
						
						# is there maybe a newer version then the latest one saved?
					
					latestrun <- getlatestrun("OM")
						
					if(!readLines("PCC/dataversion.txt") == readLines(paste("INDA/OM/",latestrun,"_dataversion.txt",sep=""))) # check if the version used in the latest run is the same as the currently used data version in the r-script
					{
						runOMagain = askYesNo("!Warning! The last stored data from the loop that does the OM on the elections lists is not the same as the current data version used for the R-script, do you want to run the loop again?! - this takes about 1.5 hours ")
					}
					
					# setting this manualy is ofcourse also possible
					# runOMagain <- FALSE
					# runOMagain <- TRUE
					
					if(runOMagain)
						{
							# in a loop
							meanpersdifferentresvec <- vector()
							percentage95simularresvec <- vector()

							pb <- txtProgressBar(min = 1, max = nrow(ELLIBU), style = 3)
							j=1000
							for(j in 1:nrow(ELLIBU))
							{
								mylistid <- ELLIBU$list_id[j]
								mypartyid <- ELLIBU$party_id[which(ELLIBU$list_id == mylistid)][1]
								myparliamentid <- ELLIBU$parliament_id[which(ELLIBU$list_id == mylistid)][1]
								
								RELLISTS <- ELLIBU[which(ELLIBU$party_id == mypartyid & ELLIBU$parliament_id == myparliamentid),]
							
							# only continue when we have values here, otherwise set result to NA # with option to only run this for NL
							if(nchar(mylistid) > 0 & nchar(mypartyid) > 0 & nchar(myparliamentid) > 0)
								{
								# get the average % difference and percentage of lists that is more then 95% simular
								
									# should I maybe exclude myself from the analysis here?
										RELLISTS <- RELLISTS[which(!RELLISTS$list_id == mylistid),]
										
										# only continue when there are any other lists to compare myself with, otherwise set results to no difference
										if(nrow(RELLISTS)>0)
										{
											mypersids <- getpersidarrayforlistid(mylistid)
											localresvec <- vector()
											number95simular <- 0 # reset
											for(i in 1:nrow(RELLISTS))
											{
												# for one list entry
												otherspersids <- getpersidarrayforlistid(RELLISTS[i,]$list_id)
												
												# only continue when there are any people with these list ids in ELEN, otherwise: NA
												if(!(otherspersids == "" | mypersids == ""))
												{
													# get the distance between them
													disthere <- getindeldistfortwoarrays(strsplit(mypersids,";")[[1]],strsplit(otherspersids,";")[[1]])
													
													# calculate the relative percentage that needs to be replaced and return
													localresvec[i] <- (disthere/length(strsplit(mypersids,";")[[1]]))
													
													# if disthere larger then 95% then update counter
													if(disthere <= 0.05)
													{
														number95simular <- number95simular + 1
													}
												} else {
													localresvec[i] <- NA
												}
											}
											averagepersdifferent <- mean(localresvec,na.rm=TRUE)
											percentagesimular <- number95simular / length(localresvec)
										
											meanpersdifferentresvec[j] <- averagepersdifferent
											percentage95simularresvec[j] <- percentagesimular
										} else {
											meanpersdifferentresvec[j] <- 0
											percentage95simularresvec[j] <- 1
										}
									} else {
										meanpersdifferentresvec[j] <- NA
										percentage95simularresvec[j] <- NA
									}
									setTxtProgressBar(pb, j)
							}
							close(pb)
							meanpersdifferentresvec
							percentage95simularresvec
							
							write.csv(meanpersdifferentresvec,file=paste(getwd(),"/INDA/OM/",format(now(), "%Y%m%d_%H%M_"),"meanpersdifferentresvec.csv",sep=""))
							write.csv(percentage95simularresvec,file=paste(getwd(),"/INDA/OM/",format(now(), "%Y%m%d_%H%M_"),"percentage95simularresvec.csv",sep=""))
							file.copy(paste(getwd(),"/PCC/dataversion.txt",sep=""),paste(getwd(),"/INDA/OM/",format(now(), "%Y%m%d_%H%M_"),"dataversion.txt",sep=""),overwrite=TRUE)
							
						} else # what to do when we are NOT running the analysis again
						{
							# then load the latest of these files
							if(!nchar(manualoverwrite) > 0)
							{
								latestrun <- getlatestrun("OM")
							} else {
								latestrun <- manualoverwrite
								}

							
								meanpersdifferentresvectemp <- read.csv(paste("INDA/OM/",latestrun,"_meanpersdifferentresvec.csv",sep=""))
								meanpersdifferentresvec <- meanpersdifferentresvectemp[,2]
								
								percentage95simularresvectemp <- read.csv(paste("INDA/OM/",latestrun,"_percentage95simularresvec.csv",sep=""))
								percentage95simularresvec <- percentage95simularresvectemp[,2]
						}

		ELLIBU$meanpersdifferent <- meanpersdifferentresvec
		ELLIBU$percentage95simular <- percentage95simularresvec
		
		# check, run manual if you like: increase in cases (from 653 in the paper to 726 now) happens here?
		# meanpersdifferentresvec <- read.csv("INDA/OM/20200818_2142_meanpersdifferentresvec.csv",sep="")[,2]
		# percentage95simularresvec <- read.csv("INDA/OM/20200818_2142_percentage95simularresvec.csv",sep="")[,2]
		
		ELLIBU$meanpersdifferent <- meanpersdifferentresvec
		ELLIBU$percentage95simular <- percentage95simularresvec
		
		nrow(ELLIBU)
		
		ELLIBUNL <- ELLIBU[which(ELLIBU$country == "NL" & !ELLIBU$parliament_id == "NL_NT-TK_1981"),]
		boxplot(ELLIBUNL$meanpersdifferent~ELLIBUNL$parliament_id)
		hist(ELLIBUNL$meanpersdifferent)
		table(ELLIBUNL$party_id)
		table(ELLIBUNL$party_id,ELLIBUNL$parliament_id)
		
		# development over time
		boxplot(ELLIBUNL$meanpersdifferent~ELLIBUNL$parliament_id,ylab="average percentage of unique politicians accross districts")
		
		ggplot(ELLIBUNL, aes(x=parliament_id, y=meanpersdifferent)) + 
		  geom_boxplot() + ylab("average percentage of unique politicians accross districts")		

		
		# development over time per party
		ggplot(ELLIBUNL, aes(x=parliament_id, y=meanpersdifferent,color=party_id)) + 
		  geom_boxplot() + ylab("average percentage of unique politicians accross districts") + 
		  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

		head(ELLIBUNL)
		
		
		#  requested graph: similarity of the list on the x axis and each gap on the y axis
			# please note that this is done below because here we do not yet have the crucial gap size calculations. 
			# see around line 2458 for ambition-selection gap
			# see around line 3443 for the selection-election gap
		
		# lets add a box plot broken down per party here as well.
		
		aggregate(ELLIBUNL$meanpersdifferent, by=list(ELLIBUNL$parliament_id), meannarm)
		
		ELLIBUNL[which(ELLIBUNL$parliament_id == "NL_NT-TK_1989"),]
		
		table(ELLIBU$percentage95simular)
		
		# all of this now results in further sample restriction, which is to select unique election lists only
		
		persidarrayvec <- vector()
		for(i in 1:nrow(ELLIBU))
		{
			persidarrayvec[i] <- getpersidarrayforlistid(ELLIBU$list_id[i])
		}
		ELLIBU$persidarray <- persidarrayvec
		tail(ELLIBU$persidarray)
		
		table(ELLIBU$parliament_id,ELLIBU$party_id_nat_equiv)
		table(ELLIBU$parliament_id,ELLIBU$party_id)
		
		# inspect duplicated cases, just by the persidarray
		duplicated(ELLIBU$persidarray)
		
		ELLIBU[which(ELLIBU$parliament_id == "DE_NT-BT_2017" & ELLIBU$party_id_nat_equiv == "DE_CDU_NT"),]
		
		# e.g.
		ELLIBU[2,]
		ELLIBU$persidarray[2]
		# find all cases
		ELLIBU[which(ELLIBU$persidarray == ELLIBU$persidarray[2]),]
		TEMP4[which(TEMP4$persidarray == ELLIBU$persidarray[2]),] # so it seems the first one is kept -- intersting! pers_id_arrays are empty!
		
		nrow(ELLIBU)
		TEMP4 <- ELLIBU %>% distinct(persidarray, .keep_all = TRUE) ## this where the 2017 CDU Bundestag members are dropped!! -- so what do we do here, when arrays are the same?!
		nrow(TEMP4) # so indeed, quite some overlap here between a lot of lists so it is good we exclude these?
		length(unique(ELLIBU$persidarray))
		ELLIBU <- TEMP4
		
		table(ELLIBU$parliament_id,ELLIBU$party_id_nat_equiv)
		table(ELLIBU$parliament_id,ELLIBU$party_id)
		
	##################################################################################################
	################################# even more variable building here #########################################
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
			
		### we would like to also have an image of party level electoral fluctuations,
		
			# script below requires
			
				# the parlgov id of a party
				summary(droplevels(ELLIBU$party_parlgov_id)) # implemented this above now
				
				# an election year variable
				ELLIBU$year <- as.numeric(substrRight(ELLIBU$parliament_id,4))
				table(ELLIBU$year)
				summary(ELLIBU$year)
				table(is.na(ELLIBU$year))
				
			# this data contains the voteshares for each election
				PG_ELEC <- read.csv("parlgov_electiondata_20190815.csv", header = TRUE, sep = ";")
				names(PG_ELEC)
				names(PG_ELEC)[names(PG_ELEC)=="party_id"] <- "party_parlgov_id"
				
				# and only use the national elections
				PG_ELEC <- PG_ELEC[which(PG_ELEC$election_type == "parliament"),]
				nrow(PG_ELEC)
				
			# lets get share for one case
			
				head(ELLIBU)
				myyear <-  ELLIBU$year[1]
				myparlgovid <- ELLIBU$party_parlgov_id[1]
				PG_ELEC$vote_share[which(PG_ELEC$party_parlgov_id == myparlgovid & PG_ELEC$election_year == myyear)]
			
			# only nati
			
			# merge in with query
			TEMPY <- sqldf("SELECT ELLIBU.*, PG_ELEC.vote_share
							FROM ELLIBU LEFT JOIN PG_ELEC
							ON
								ELLIBU.party_parlgov_id = PG_ELEC.party_parlgov_id
								AND
								ELLIBU.year = PG_ELEC.election_year
						   ")
			head(TEMPY)
			nrow(TEMPY)
			nrow(ELLIBU)
			
			TEMPY[which(is.na(TEMPY$vote_share)),]
			
			unique(ELLIBU[which(ELLIBU$parliament_id == "DE_NT-BT_2013"),]$nat_party_id)
			unique(ELLIBU[which(ELLIBU$parliament_id == "DE_NT-BT_2017"),]$nat_party_id) # why is CDU missing in 2017?!
			
			ELLIBU <- TEMPY 
			
			# so lets so him much vote share the national party last compared to the previous election

				# first, lets get the previous parliament in and get the year that comes with it
				TEMPXX <- sqldf("SELECT ELLIBU.*, PARL.previous_parliament
								FROM ELLIBU LEFT JOIN PARL
								ON ELLIBU.parliament_id = PARL.parliament_id
								")

				head(TEMPXX)
				nrow(TEMPXX)
				nrow(ELLIBU)
				ELLIBU <- TEMPXX 

				ELLIBU$previous_election_year <- as.numeric(substrRight(as.character(ELLIBU$previous_parliament),4))
			
			# then, do the exact same thing, but with the number from the previous election

			TEMPYY <- sqldf("SELECT ELLIBU.*, PG_ELEC.vote_share as 'vote_share_previous'
							FROM ELLIBU LEFT JOIN PG_ELEC
							ON
								ELLIBU.party_parlgov_id = PG_ELEC.party_parlgov_id
								AND
								ELLIBU.previous_election_year = PG_ELEC.election_year
						   ")
			head(TEMPYY)
			nrow(TEMPYY)
			nrow(ELLIBU)
			ELLIBU <- TEMPYY 
		
			# now, calculate the 'drop'  (note for later: if you look up you see this is based on the voteshare according to parlgov.
			
			ELLIBU$vote_share_change <- ELLIBU$vote_share - ELLIBU$vote_share_previous
			hist(ELLIBU$vote_share_change)
			hist(ELLIBUTEMP$vote_share_change)
			
			# lets inspecting missing cases quickly
			table(is.na(ELLIBU$vote_share_change))
			ELLIBU[which(is.na(ELLIBU$vote_share_change)),]
			
			
	## Selection and election control variables ###!

			## selection control
				ELLIBU$selection_control <- NA
				
				# low, german district seats
				ELLIBU$selection_control[which(ELLIBU$country == "DE" & ELLIBU$type == "district")] <- "low selection control"
				ELLIBU$selection_control[which(ELLIBU$country == "NL" &  ELLIBU$meanpersdifferent >= 0.5)] <- "low selection control"  # used to be "medium selection control"
				
				# medium, german regional list seats + Dutch lists with regional variation
				ELLIBU$selection_control[which(ELLIBU$country == "DE" & ELLIBU$type == "list")] <- "medium selection control"

				# high, Dutch centralised national lists + all swiss lists
				ELLIBU$selection_control[which(ELLIBU$country == "NL" &  ELLIBU$meanpersdifferent < 0.5)] <- "high selection control" 
				ELLIBU$selection_control[which(ELLIBU$country == "CH")] <- "high selection control" 
				
				# checking the distribution so I can pick a good cutoff for NL
				hist(ELLIBU$meanpersdifferent[which(ELLIBU$country == "NL")],breaks=20)
				hist(ELLIBU$meanpersdifferent[which(ELLIBU$country == "DE")]) # lets think about why the big values here!
				hist(ELLIBU$meanpersdifferent[which(ELLIBU$country == "CH")],breaks=20)
				head(ELLIBU)
				
				table(ELLIBU$selection_control)
				table(ELLIBU$selection_control,ELLIBU$country)
				table(is.na(ELLIBU$selection_control)) 
				
				ELLIBUSCNA <- ELLIBU[which(is.na(ELLIBU$selection_control)),]
				ELLIBUSCNA$type
				ELLIBUSCNA$meanpersdifferent # this is culprit - to get this out I need to inspect where the warnings are comming from when the OM script is ran
				ELLIBUSCNA$country
				
				# temporary workaround! If you have NA we take the first selection control variable from a same party in a same election that does have a value
				
					resvec <- vector()
					for(i in 1:nrow(ELLIBU))
					{
						if(is.na(ELLIBU$selection_control[i]))
						{# only run if we do not already have a selection control variable
							mypartyid <- ELLIBU$party_id_nat_equiv[i]
							myelection <- ELLIBU$parliament_id[i]
							mylistid <- ELLIBU$list_id[i]
							
							resvec[i] <- as.character(ELLIBU[which(ELLIBU$party_id_nat_equiv == mypartyid & ELLIBU$parliament_id == myelection & !ELLIBU$list_id == mylistid & !is.na(ELLIBU$selection_control)),]$selection_control[1])
						} else {
							resvec[i] <- NA
						}
					}
					resvec
					table(resvec) # ok, that hardly solves anything
				
			## selection control details
				ELLIBU$selection_control_detailed <- NA
				
				# low, german district seats
				ELLIBU$selection_control_detailed[which(ELLIBU$country == "DE" & ELLIBU$type == "district")] <- "low - DE district (aggregated at land level)"
				
				# medium, german regional list seats + Dutch lists with regional variation
				ELLIBU$selection_control_detailed[which(ELLIBU$country == "DE" & ELLIBU$type == "list")] <- "medium - DE list"
				ELLIBU$selection_control_detailed[which(ELLIBU$country == "NL" &  ELLIBU$meanpersdifferent >= 0.2)] <- "medium - NL with different regional lists" 

				# high, Dutch centralised national lists + all swiss lists
				ELLIBU$selection_control_detailed[which(ELLIBU$country == "NL" &  ELLIBU$meanpersdifferent < 0.2)] <- "high - NL with gen. homogenous regional lists" 
				ELLIBU$selection_control_detailed[which(ELLIBU$country == "CH")] <- "high - all swiss lists" 
				
				table(ELLIBU$selection_control_detailed)
				table(ELLIBU$selection_control_detailed,ELLIBU$country)
				table(is.na(ELLIBU$selection_control_detailed)) 
				ELLIBU$selection_control_detailed_fac <- factor(ELLIBU$selection_control_detailed,levels=c("low - DE district (aggregated at land level)","medium - DE list","medium - NL with different regional lists","high - NL with gen. homogenous regional lists","high - all swiss lists"))
				table(ELLIBU$selection_control_detailed_fac)
				
			
			## electoral uncertainty
				ELLIBU$election_uncertainty <- NA
				
				ELLIBU$party_size_cat <- NA
				ELLIBU$party_size_cat_de[which(ELLIBU$party_size <= 75)] <- "small party"
				ELLIBU$party_size_cat_de[which(ELLIBU$party_size > 75)] <- "big party"
				table(ELLIBU$party_size_cat_de)
				table(ELLIBU$party_size_cat_de,ELLIBU$country)
				table(ELLIBU[which(ELLIBU$country == "DE"),]$party_size_cat_de,ELLIBU[which(ELLIBU$country == "DE"),]$nat_party_id) # why die Linke more then 75 seats? --
			
				# low uncertainty - german regional lists for small parties + all Dutch election lists
				ELLIBU$election_uncertainty[which(ELLIBU$country == "DE" & ELLIBU$type == "list" & ELLIBU$party_size_cat_de == "small party")] <- "low electoral uncertainty"
				ELLIBU$election_uncertainty[which(ELLIBU$country == "NL")] <- "low electoral uncertainty"
				ELLIBU$election_uncertainty[which(ELLIBU$country == "DE" & ELLIBU$type == "district" & ELLIBU$party_size_cat_de == "small party")] <- "low electoral uncertainty" # do Elena and Philip agree with this classification?
				
				# checking the cutoff
				hist(ELLIBU$party_size[which(ELLIBU$country == "DE")])
				table(ELLIBU$party_size)
				
				# high uncertainty - german regional list big parties + German district seats for big parties + all swiss election lists
				ELLIBU$election_uncertainty[which(ELLIBU$country == "DE" & ELLIBU$type == "list" & ELLIBU$party_size_cat_de == "big party")] <- "high electoral uncertainty"
				ELLIBU$election_uncertainty[which(ELLIBU$country == "DE" & ELLIBU$type == "district" & ELLIBU$party_size_cat_de == "big party")] <- "high electoral uncertainty"
				ELLIBU$election_uncertainty[which(ELLIBU$country == "CH")] <- "high electoral uncertainty"
				
				table(ELLIBU$election_uncertainty)
				table(ELLIBU$election_uncertainty,ELLIBU$country)
				table(is.na(ELLIBU$election_uncertainty))
				
			## electoral uncertainty with details
				
				ELLIBU$election_uncertainty_detailed <- NA
			
				# low uncertainty - german regional lists for small parties + all Dutch election lists
				
				table(ELLIBU$country,ELLIBU$type,ELLIBU$party_size_cat_de) # right, so small parties where removed all together! that is not what we would like right?
				
				ELLIBU$election_uncertainty_detailed[which(ELLIBU$country == "DE" & ELLIBU$type == "list" & ELLIBU$party_size_cat_de == "small party")] <- "low unc. - DE list small parties"
				ELLIBU$election_uncertainty_detailed[which(ELLIBU$country == "NL")] <- "low unc. - NL all lists"
				ELLIBU$election_uncertainty_detailed[which(ELLIBU$country == "DE" & ELLIBU$type == "district" & ELLIBU$party_size_cat_de == "small party")] <- "low unc. - DE district small parties" # this one should indeed have been removed
				table(ELLIBU$election_uncertainty_detailed)
				
				# checking the cutoff
				hist(ELLIBU$party_size[which(ELLIBU$country == "DE")])
				table(ELLIBU$party_size)
				
				# high uncertainty - german regional list big parties + German district seats for big parties + all swiss election lists
				ELLIBU$election_uncertainty_detailed[which(ELLIBU$country == "DE" & ELLIBU$type == "list" & ELLIBU$party_size_cat_de == "big party")] <- "high unc. DE lists big parties"
				ELLIBU$election_uncertainty_detailed[which(ELLIBU$country == "DE" & ELLIBU$type == "district" & ELLIBU$party_size_cat_de == "big party")] <- "high unc. DE district big par. (aggr. at land lvl)]"
				ELLIBU$election_uncertainty_detailed[which(ELLIBU$country == "CH")] <- "high unc. all CH"
				
				table(ELLIBU$election_uncertainty_detailed)
				table(ELLIBU$election_uncertainty_detailed,ELLIBU$country)
				table(is.na(ELLIBU$election_uncertainty_detailed))
				
				ELLIBU$election_uncertainty_detailed_fac <- factor(ELLIBU$election_uncertainty_detailed)
				table(ELLIBU$election_uncertainty_detailed_fac)
				
		## so a new attempt for an electoral uncertainty variable
		
			
			
		### now also, using the information from above, move some Dutch cases away from the single-list variable, because there is actually quite some diversity!
			table(ELLIBU$keylisttypes,ELLIBU$countryld)
			table(ELLIBU$percentage95simular)
			
			ELLIBU[which(ELLIBU$percentage95simular > 0 & ELLIBU$percentage95simular < 1),]
			
			
			ELLIBU$keylisttypes <- ifelse(ELLIBU$country == "NL" & ELLIBU$percentage95simular > 0.20,"party-list-secondary-districts",ELLIBU$keylisttypes)
			table(ELLIBU$keylisttypes,ELLIBU$countryld) # 23 election lists from NL got moved to the other category
					summary(ELLIBU$ratio_on_list) 
	### for all the list seats, get a variable as well that indicates what the percentage of women was on the district seats in this list its region << BROKEN now, fix later or DROP
	
		# for all the districts, get the region and district magnitude from ELDI
			nrow(ELLIBU)
			ELLIBU <- sqldf("SELECT ELLIBU.*, ELDI.region_abb, ELDI.dist_magnitude
					       FROM ELLIBU LEFT JOIN ELDI 
					       ON ELLIBU.district_id = ELDI.district_id	
						   ")
			nrow(ELLIBU)
			head(ELLIBU)
			
			table(ELLIBU$country)
			table(ELLIBU$country,is.na(ELLIBU$dist_magnitude))
			
			

		# get a list of all district candidates in this region to get an image of the percentage of MPS of this list that also have district seats.
			
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

	##### creating the three crucial gap variables 
		
		# there are a total of two steps: ambition --(step 1)--> percentage on list --(step 2)--> percentage elected
		
			### ambition realisation gap (overall gap) (quota percentage - percentage elected)
				
				# inspection
				table(ELLIBU$quota_now)
				table(ELLIBU$quota_percentage)
				sum(table(ELLIBU$quota_percentage)) # percentage specifications seem complete
				
				# adding a 0% category
				ELLIBU$quota_percentage_cleaned <- ifelse(ELLIBU$quota_now == 0,0,ELLIBU$quota_percentage)
				table(is.na(ELLIBU$quota_percentage),ELLIBU$keylisttypes) # still, some of everything?
				table(is.na(ELLIBU$ratio_elected),ELLIBU$keylisttypes) # no ratio elected for any of the single member districts?!
				table(ELLIBU$keylisttypes,ELLIBU$country)
				
				# calculating the gap
				ELLIBU$ambition_realisation_gap <- (ELLIBU$ratio_elected - (ELLIBU$quota_percentage/100)) # negative numbers indicate that the quota was not reached (less women elected then specified)
				
				# using the absolute version as Philip suggested
		#		ELLIBU$ambition_realisation_gap <- abs(ELLIBU$ambition_realisation_gap)

				# inspection of the gap
				table(is.na(ELLIBU$ambition_realisation_gap))
				hist(ELLIBU$ambition_realisation_gap) # some suggestion here that unit of analysis of the district, and counting this as a simular case then lets say a list is not really fair?
				
				boxplot(ELLIBU$ambition_realisation_gap~ELLIBU$country) # see point above, districts that have a women count as a one, how to do this? Maybe take the average percentages at the level of the secundary districts for DE or so?
				hist(ELLIBU$ambition_realisation_gap[which(ELLIBU$country =="DE")]) # lets add this to the overleaf file - some suggestion indeed that much more often in DE quota is not reached?
				hist(ELLIBU$ambition_realisation_gap[which(ELLIBU$country =="NL")]) # lets add this to the overleaf file
					
				# key descriptive relating to Philip suggestion
				
				# reordering things for the graphic
				ELLIBU$selection_control <- factor(ELLIBU$selection_control, levels=c("low selection control","medium selection control","high selection control"))
				table(ELLIBU$selection_control)
				table(ELLIBU$selection_control,ELLIBU$selection_control_detailed_fac)
				table(is.na(ELLIBU$selection_control))
				
				table(ELLIBU$selection_control_detailed_fac)
				ELLIBU$selection_control_detailed_fac <- factor(ELLIBU$selection_control_detailed_fac, levels=c("low - DE district (aggregated at land level)","medium - DE list","medium - NL with different regional lists","high - NL with gen. homogenous regional lists"))
				
				boxplot(ELLIBU$ambition_realisation_gap~ELLIBU$selection_control, main="abs(% women elected into parliament - % from quota)")
				boxplot(ELLIBU$ambition_realisation_gap~ELLIBU$selection_control_detailed_fac, main="abs(% women elected into parliament - % from quota)")
				
				beanplot(ELLIBU$ambition_realisation_gap~ELLIBU$selection_control, main="abs(% women elected into parliament - % from quota)",maxstripline=0.1, col = c("#CAB2D6", "#33A02C", "#B2DF8A"))
				beanplot(ELLIBU$ambition_realisation_gap~ELLIBU$selection_control_detailed_fac, main="abs(% women elected into parliament - % from quota)",maxstripline=0.1, col = c("#CAB2D6", "#33A02C", "#B2DF8A"))
				
				# variance per group
				table(ELLIBU$selection_control)
				var(ELLIBU[which(ELLIBU$selection_control == "high selection control"),]$ambition_realisation_gap)
				var(ELLIBU[which(ELLIBU$selection_control == "low selection control"),]$ambition_realisation_gap)
				var(ELLIBU[which(ELLIBU$selection_control == "medium selection control"),]$ambition_realisation_gap)
				levene.test(ELLIBU$ambition_realisation_gap,ELLIBU$selection_control) # no significant difference in variance between the groups
				table(ELLIBU$selection_control)
				
					# break down to hard and soft quotas
						ELLIBU$typeandquota <- paste(ELLIBU$keylisttypes,ELLIBU$quota_soft,sep="")
						table(ELLIBU$typeandquota)
						boxplot(ELLIBU$ambition_realisation_gap~ELLIBU$typeandquota, main="% women elected into parliament - % from quota")
				
				
					# and a table of means per group
					meannarm <- function(input)
					{
						return(mean(input,na.rm=TRUE))
					}
					
					aggregate(ELLIBU$ambition_realisation_gap, by=list(ELLIBU$keylisttypes), meannarm)
				
			### ambition to selection gap (quota percentage - percentage selected)
				
				# calculating this gap
				table(is.na(ELLIBU$ratio_on_list))
				table(is.na(ELLIBU$quota_percentage)) # ok, so hardly any quotas
				table(is.na(ELLIBU$quota_percentage),is.na())
				
				ELLIBU$ambition_selection_gap <- (ELLIBU$ratio_on_list - (ELLIBU$quota_percentage/100)) * 100
				
				ELLIBUINSPE <- ELLIBU[c("ambition_selection_gap","ratio_on_list","quota_percentage")]
				head(ELLIBUINSPE)
				
				# and the absolute version as suggested
#				ELLIBU$ambition_selection_gap <- abs(ELLIBU$ambition_selection_gap) # decision is real numbers indeed
				
				# inspection of the gap
				table(is.na(ELLIBU$ambition_selection_gap)) # so, just as a note so self, quite a bit more cases because from many lists never anybody got elected?
				hist(ELLIBU$ambition_selection_gap) # looks a lot like the ambition realisation gap? - so some suggestion that the selection step is the crucial one in the sample as a whole?
				boxplot(ELLIBU$ambition_selection_gap~ELLIBU$country,main="abs(% of women selected onto the list - % from quota)") 
				hist(ELLIBU$ambition_selection_gap[which(ELLIBU$country =="DE")])
				hist(ELLIBU$ambition_selection_gap[which(ELLIBU$country =="NL")]) 
				
				
				ELLIBU$selection_control_fac <- factor(ELLIBU$selection_control, levels=c("high selection control","medium selection control","low selection control"))
				
				# two manual fixes for some of the scarce Dutch data-points
				table(is.na(ELLIBU$selection_control_fac))
				ELLIBU[which(is.na(ELLIBU$selection_control_fac)),]
				ELLIBU$selection_control_fac[which(ELLIBU$list_id == "NL_NT-TK_2012__Bonaire__12sep2012__Christen-Democratisch-Appel")]	<- "high selection control"
				ELLIBU$selection_control_fac[which(ELLIBU$list_id == "NL_NT-TK_1982__sHertogenbosch__08sep1982__Partij-van-de-Arbeid")]	<- "low selection control"
				
				table(ELLIBU$selection_control_fac)
				boxplot(ELLIBU$ambition_selection_gap~ELLIBU$selection_control_fac, main="% of women selected onto the list - % from quota")
				beanplot(ELLIBU$ambition_selection_gap~ELLIBU$selection_control_fac, main="abs(% of women selected onto the list - % from quota)",maxstripline=0.1, col = c("#CAB2D6", "#33A02C", "#B2DF8A"))
				beanplot(ELLIBU$ambition_selection_gap~ELLIBU$selection_control_detailed_fac, main="abs(% of women selected onto the list - % from quota)",maxstripline=0.1, col = c("#CAB2D6", "#33A02C", "#B2DF8A"))
				
			# boxplot with dots, like in the paper, and maybe color per party?
				
				is.na(ELLIBU$selection_control_fac)
				ELLIBU[which(is.na(ELLIBU$selection_control_fac)),]
				
				library(viridis)
				
				ELLIBU$party_id_nat_equiv_short <- gsub("_NT","",ELLIBU$party_id_nat_equiv)
				ELLIBU$party_id_nat_equiv_short <- gsub("[A-Z]{2}_","",ELLIBU$party_id_nat_equiv_short)
				ELLIBU$party_id_nat_equiv_short <- paste0(ELLIBU$party_id_nat_equiv_short," (",ELLIBU$country,")")
				
				table(ELLIBU$party_id_nat_equiv_short )
				table(ELLIBU$party_id_nat_equiv_short,ELLIBU$selection_control_fac)
				
				myquantile <- function(x)
				{
					quantile(x,0.75,na.rm=TRUE)
				}
				myquantile(ELLIBU$ambition_selection_gap)
				
				group_average_for_labels <- aggregate(ambition_selection_gap ~ selection_control_fac + party_id_nat_equiv_short, myquantile, data=ELLIBU)
				control_average <- aggregate(ambition_selection_gap ~ selection_control_fac, mean, data=ELLIBU)
				colnames(group_average_for_labels) <- c("selection_control_fac","party_id_nat_equiv_short","ambition_selection_gap_75q")
				
				geom.text.size = 6
				theme.size = (14/5) * geom.text.size
				
				ggplot(data=ELLIBU, aes(x=selection_control_fac, y=ambition_selection_gap,fill=party_id_nat_equiv_short)) + 
				geom_boxplot(position=position_dodge(width=.9),outlier.shape = 2) +
				stat_summary(fun.y = mean, geom = "errorbar",aes(ymax = ..y.., ymin = ..y.., group = factor(selection_control_fac)),width = 0.5,size=2.5, linetype = "solid") +
				stat_summary(aes(ymax = ..y.., ymin = ..y.., group = factor(selection_control_fac),label=round(..y..,2)), fun.y=mean, geom="label", size=8) +
			#	scale_color_brewer(palette = "Dark2") +
				labs(title = "List level selection ambition gap * selection control", x = "Selection control", y = "(Selection - ambition) gap electable positions", color = "Party\n") +
				geom_text(aes(x=0.6,y=49.5,label="overshooting"),angle=0,size=geom.text.size) + 
				geom_text(aes(x=0.6,y=-48.5,label="undershooting"),angle=0,size=geom.text.size) +
				geom_hline(yintercept=0,linetype="dashed") + 
				geom_text(data=group_average_for_labels, aes(label=party_id_nat_equiv_short, y = 35), position=position_dodge(width=0.9),size=geom.text.size-1,color="black",angle=90) +
				theme_pubr(base_size = theme.size) +
				ylim(c(-50,50)) +
				guides(fill=FALSE) +
			#	coord_flip() +
				scale_fill_manual(values=rep("#999999",7)) +
				geom_vline(xintercept=1.5,linetype="solid") +
				geom_vline(xintercept=2.5,linetype="solid")
		
	##  requested graphs: similarity of the list on the x axis and each gap on the y axis
		
			# only ambition_selection_gap here, see below for election selection gap
				ELLIBUNL2 <- ELLIBU[which(ELLIBU$country == "NL" & !ELLIBU$parliament_id == "NL_NT-TK_1981"),]
				table(ELLIBU$country)
				nrow(ELLIBUNL2)
		
				ELLIBUNL2$parliament_id <- as.factor(ELLIBUNL2$parliament_id)
		
				ggplot(ELLIBUNL2, aes(x=meanpersdifferent, y=ambition_selection_gap,color=party_id,shape=parliament_id)) + 
				  geom_point() + 
				  ylab("ambition selection gap") + 
				  xlab("average percentage of unique politicians accross districts") + 
				  scale_shape_manual(values=1:nlevels(ELLIBUNL2$parliament_id)) + 
				  geom_text(aes(x=0.6,y=12.5,label="overshooting"),angle=0,color="black") + 
				  geom_text(aes(x=0.6,y=-48.5,label="undershooting"),angle=0,color="black") 
		
			# why is this gap the same for all the GL lists? ? makes sense, only bottom of list varies? so gap the same in all elections?
				ELLIBUNLGL <- ELLIBUNL2[which(ELLIBUNL2$party_id == "NL_GL_NT"),]
				table(ELLIBU$NL_GL_NT)
				nrow(ELLIBUNLGL)
				
				table(ELLIBUNLGL$parliament_id) # so, please do note we are using the reduced sample here! I think that is indeed the best to do, but just saying :)
				
				ggplot(ELLIBUNLGL, aes(x=meanpersdifferent, y=ambition_selection_gap,color=parliament_id)) + 
				  geom_point() + ylab("ambition selection gap") + xlab("average percentage of unique politicians accross districts") # indeed confirms this, all the same elections!
		
			# why the low numbers for the CDA in the Netherlands?
			
				head(ELLIBU)
				
				#e.g.
				ELENBU[which(ELENBU$list_id == "NL_NT-TK_2012__Groningen__12sep2012__Christen-Democratisch-Appel"),]
			
				# elena ask: which PvdA lists are undershooting
				 NLPvdA <- ELLIBU[which(ELLIBU$party_id_nat_equiv == "NL_PvdA_NT"),]
			
			
				
			# reorder for the models
				ELLIBU$selection_control_fac <- factor(ELLIBU$selection_control_fac, levels=c("low selection control","medium selection control","high selection control"))
				
				#geom_dotplot(binaxis='y', stackdir='center', dotsize=0.05)
				
				# in the high category, how large are these groups? -- maybe time controls are needed? People care less in recent years?
					table(ELLIBU$selection_control_fac,ELLIBU$party_id_nat_equiv_short) # so we can see here that is is basically the PvdA that drives the whole effect? -- supprising as they have a zipper quota?!!
					
		
					
					head(ELLIBU)
					table(ELLIBU$parliament_id, ELLIBU$party_id_nat_equiv_short)
					table(ELLIBU$parliament_id, ELLIBU$party_id_nat_equiv_short,ELLIBU$selection_control_fac)
				
				
				# variance per group
				var(ELLIBU[which(ELLIBU$selection_control == "high selection control"),]$ambition_selection_gap)
				var(ELLIBU[which(ELLIBU$selection_control == "medium selection control"),]$ambition_selection_gap)
				var(ELLIBU[which(ELLIBU$selection_control == "low selection control"),]$ambition_selection_gap)
				table(ELLIBU$selection_control)
				
				levene.test(ELLIBU$ambition_selection_gap,ELLIBU$selection_control)
				
					# break down to hard and soft quotas
						boxplot(ELLIBU$ambition_selection_gap~ELLIBU$typeandquota, main="% of women selected onto the list - % from quota")
						
				## and in a regression model
				
					options(lmerControl=list(check.nobs.vs.rankZ = "warning",
						 check.nobs.vs.nlev = "warning",
						 check.nobs.vs.nRE = "warning",
						 check.nlev.gtreq.5 = "warning",
						 check.nlev.gtr.1 = "warning"))
				
					table(ELLIBU$selection_control_fac,ELLIBU$country)
				
					# I think we should use the model with party fixed effects
					
						# preparing this with CDU as the reference category
							table(ELLIBU$party_id_nat_equiv_short)
			
							ELLIBU$party_id_nat_equiv_short <- factor(ELLIBU$party_id_nat_equiv_short, levels=c("CDU (DE)","SPD (DE)","B90|Gru (DE)","Li|PDS (DE)","CDA (NL)","GL (NL)","PvdA (NL)"))
							table(ELLIBU$party_id_nat_equiv_short)
				
					me <- lmer(ambition_selection_gap~1+
							#	party_id_nat_equiv_short + # added this here on 19/10/2021
								(1 | year_cent) +
								(1 | country),
								data=ELLIBU)#data=ELLIBU[which(ELLIBU$ambition_selection_gap <= 0),])#
					summary(me)
				
					
					# integrating Philip' suggestion to have district magnitude as model 1
					
						# let's get a country standardised version of district magnitude(moved from below)
					
							# using the new district magnitude measure!
							ELLIBU$district_magnitude <- ELLIBU$dist_magnitude 
					
							hist(ELLIBU$district_magnitude)
							hist(ELLIBU$district_magnitude[which(ELLIBU$country == "DE")])
							hist(ELLIBU$district_magnitude[which(ELLIBU$country == "NL")])
							
							dedismagavg <- mean(ELLIBU$district_magnitude[which(ELLIBU$country == "DE")])
							nldismagavg <- mean(ELLIBU$district_magnitude[which(ELLIBU$country == "NL")])
							dedismagsd <-  sd(ELLIBU$district_magnitude[which(ELLIBU$country == "DE")])
							nldismagsd <-  sd(ELLIBU$district_magnitude[which(ELLIBU$country == "NL")])
							
							
							ELLIBU$district_magnitude_country_stan <- ifelse(ELLIBU$country == "DE", (ELLIBU$district_magnitude-dedismagavg)/dedismagsd, (ELLIBU$district_magnitude-nldismagavg)/nldismagsd)
							# check
							mean(ELLIBU[which(ELLIBU$country == "DE"),]$district_magnitude_country_stan)
							sd(ELLIBU[which(ELLIBU$country == "DE"),]$district_magnitude_country_stan) # looking good!
					
					m0 <- lmer(ambition_selection_gap~
										district_magnitude + # is country mean centered and country standard deviation scaled
										(1 | country),
										,data=ELLIBU)
							summary(m0)
					
					ELLIBU$type <- factor(ELLIBU$type, levels = c("list","district"))
					
					m05 <- lmer(ambition_selection_gap~
										district_magnitude + # is country mean centered and country standard deviation scaled
										type +
										(1 | country),
										,data=ELLIBU)
							summary(m05)
					
					
					m1 <- lmer(ambition_selection_gap~
								party_id_nat_equiv_short + # added this here on 19/10/2021
							#	district_magnitude + # is country mean centered and country standard deviation scaled
							#	type +
								selection_control_fac+
								(1 | country),
								,data=ELLIBU)
					summary(m1)
					
					table(ELLIBU$party_size_cat_de)
					table(ELLIBU$country)


					ELLIBU$country <- droplevels(as.factor(ELLIBU$country))
		
		# creating a 'linked list' variable
			table(ELLIBU$party_id_nat_equiv)
			ELLIBU$linkedlist <- ifelse(((ELLIBU$party_id_nat_equiv == "DE_CDU_NT" | ELLIBU$party_id_nat_equiv== "DE_SPD_NT") & ELLIBU$type == "list"),"linked","not-linked")
			ELLIBU$linkedlist <- factor(ELLIBU$linkedlist,c("not-linked","linked"))
			table(ELLIBU$linkedlist)
			table(ELLIBU$linkedlist,ELLIBU$nat_party_id)
							
		# lets center the quota percentage!
			hist(ELLIBU$quota_percentage)
			ELLIBU$quota_percentage_cent <- ELLIBU$quota_percentage - mean(ELLIBU$quota_percentage)
					
					m2 <- lmer(	ambition_selection_gap~
							#	type +
								selection_control_fac +
								district_magnitude + # is country mean centered and country standard deviation scaled
							#	party_size_cat +
								quota_percentage_cent +
								(1 | country),
								data=ELLIBU)
					summary(m2)
					
					stargazer(m1,m2,type="text",intercept.bottom=FALSE)
					stargazer(m1,m2,intercept.bottom=FALSE)
			
					### so, continuing the buildup of the model particualry a control for a general time trend seems rather important
					
					ELLIBU_DE <- ELLIBU[which(ELLIBU$country == "DE"),]
					ELLIBU_NL <- ELLIBU[which(ELLIBU$country == "NL"),]
					
					boxplot(ELLIBU_DE$ambition_selection_gap~ELLIBU_DE$parliament_id)
					
						# inspection of some cases
							ELLIBU[which(ELLIBU$parliament_id == "DE_NT-BT_1983"),] # only very few observations... 
							ELLIBU[which(ELLIBU$parliament_id == "DE_NT-BT_1987"),] # only very few observations...

							# checking if the numbers in general match
							ELLIBU_DE_1987 <- ELLIBU[which(ELLIBU$parliament_id == "DE_NT-BT_1987"),]
							sum(ELLIBU_DE_1987$f_elected) + sum(ELLIBU_DE_1987$m_elected) # 40, not 42, why? well: because two candidates selected via district seats probably?
							
					boxplot(ELLIBU_NL$ambition_selection_gap~ELLIBU_NL$parliament_id) # much more eradic pattern in the Netherlands
					
						# inspection of subcases
						table(ELLIBU_NL$parliament_id)
						ELLIBU[which(ELLIBU$parliament_id == "NL_NT-TK_2006"),]
					
					head(ELLIBU)
					
					# this suggest we should use different trend in the different countries, we see a small increase in DE and a small decrease in NL? In general it must be said that - especially in the earlier years - there are really not that many observations anymore
					
						# building up a time control
							ELLIBU$year <- as.numeric(substrRight(ELLIBU$parliament_id,4))
							hist(ELLIBU$year)
							median(ELLIBU$year)
							ELLIBU$year_cent <- ELLIBU$year - median(ELLIBU$year)
							hist(ELLIBU$year_cent)
							
						# inspect this per country
							
							ELLIBU$year_fac <- as.factor(ELLIBU$year)
							ggplot(data=ELLIBU,aes(y=ambition_selection_gap,x=year,color=country)) +
							geom_point() +
							geom_smooth()

						# building up a proper way of controling for party size
							hist(ELLIBU$party_size)
							boxplot(ELLIBU$party_size~ELLIBU$country)
						
						
							hist(ELLIBU$party_size[which(ELLIBU$country == "DE")])
						
							# standardise within country
							
							desizeavg <- mean(ELLIBU$party_size[which(ELLIBU$country == "DE")])
							nlsizeavg <- mean(ELLIBU$party_size[which(ELLIBU$country == "NL")])
							desizesd <-  sd(ELLIBU$party_size[which(ELLIBU$country == "DE")])
							nlsizesd <-  sd(ELLIBU$party_size[which(ELLIBU$country == "NL")])
							
							ELLIBU$party_size_country_stan <- ifelse(ELLIBU$country == "DE", (ELLIBU$party_size-desizeavg)/desizesd, (ELLIBU$party_size-nlsizeavg)/nlsizesd)
							# check
							mean(ELLIBU[which(ELLIBU$country == "DE"),]$party_size_country_stan)
							sd(ELLIBU[which(ELLIBU$country == "DE"),]$party_size_country_stan) # looking good!
							
							# alternatively, we can make this seat share
							
							table(FPAREBU$parliament_id)
							
				
					# this is the model with a variable slope for a time-trend per country
					m3 <- lmer(	ambition_selection_gap~
								district_magnitude + # is country mean centered and country standard deviation scaled
						#		type +
								selection_control_fac +
								party_size_country_stan + # is country mean centered and country standard deviation scaled
								(year_cent | country),
								data=ELLIBU)
					summary(m3)
					
					# lets recode quota soft
					ELLIBU$quota_soft_fact <- factor(ifelse(ELLIBU$quota_soft == 1,"soft","hard"),levels=c("soft","hard"))
					table(ELLIBU$quota_soft_fact)
					
					m4 <- lmer(	ambition_selection_gap~
								district_magnitude +
						#		type +
								selection_control_fac +
								party_size_country_stan + # is country mean centered and country standard deviation scaled
								quota_percentage_cent +
								quota_soft_fact +
							#	country +
							#	year_cent +
							#	(year_cent | country) +
								(1 | year_cent) +
								(1 | country),
								data=ELLIBU)
					summary(m4)
				
					stargazer(me,m0,m1,m3,m4,type="text",intercept.bottom=FALSE)
					
					m5 <- lmer(	ambition_selection_gap~
								district_magnitude +
						#		type +
								selection_control_fac +
								party_size_country_stan + # is country mean centered and country standard deviation scaled
								quota_percentage_cent +
								quota_soft_fact +
								country *
								year_cent +
							#	(year_cent | country) +
								(1 | year_cent) +
								(1 | country),
								data=ELLIBU)
					summary(m5)
		
		# how about Germany only checking?!


			# district magnitude, make it country centered
			hist(ELLIBU$district_magnitude)
		
			dedismagavg <- mean(ELLIBU$district_magnitude[which(ELLIBU$country == "DE")])
			nldismagavg <- mean(ELLIBU$district_magnitude[which(ELLIBU$country == "NL")])
			dedismagsd <-  sd(ELLIBU$district_magnitude[which(ELLIBU$country == "DE")])
			nldismagsd <-  sd(ELLIBU$district_magnitude[which(ELLIBU$country == "NL")])
							
			ELLIBU$district_magnitude_country_cent <- ifelse(ELLIBU$country == "DE", (ELLIBU$district_magnitude-dedismagavg), 0)
			
			ggplot(data=ELLIBU, aes(y=district_magnitude_country_cent, fill=country)) +
			geom_boxplot()
			
			table(is.na(ELLIBU$district_magnitude_country_cent))

		# new model buildup! - general controls first, then time controls, only then the selection control stuff

			m1 <- lmer(	ambition_selection_gap~
								selection_control_fac + # new
						#		party_id_nat_equiv_short + # added this here on 19/10/2021
						#		district_magnitude_country_cent +
								(1 | year_cent) +
								(1 | country),
								data=ELLIBU)#data=ELLIBU[which(ELLIBU$ambition_selection_gap <= 0),])#
					summary(m1)
					stargazer(me,m1,type="text")

		# some reordering of the quota variables still?
		table(ELLIBU$quota_percentage)
		table(ELLIBU$quota_percentage_cent)
		ELLIBU$quota_percentage_lessthen50 <-  (50 - ELLIBU$quota_percentage)
		table(ELLIBU$quota_percentage_lessthen50)
		
		table(ELLIBU$quota_soft_fact)
		ELLIBU$quota_soft_fact <- factor(ELLIBU$quota_soft_fact,levels=c("hard","soft"))
		
		table(ELLIBU$quota_zipper)

	
			
			m1a <- lmer(	ambition_selection_gap~
								selection_control_fac + # new
								party_id_nat_equiv_short + # added this here on 19/10/2021
							#	district_magnitude_country_cent +
			#					type +
							#	quota_percentage_lessthen50 +
							#	quota_soft_fact +
							#	quota_zipper +
								# party_size_country_stan +
								(1 | year_cent) +
								(1 | country),
								data=ELLIBU)#data=ELLIBU[which(ELLIBU$ambition_selection_gap <= 0),])#
					summary(m1a)
					stargazer(m1a,type="text")
					
			m2 <- lmer(	ambition_selection_gap~
								party_id_nat_equiv_short + # added this here on 19/10/2021
								district_magnitude_country_cent +
			#					type +
								quota_percentage_lessthen50 +
								quota_soft_fact +
								quota_zipper
							#	party_size_country_stan +
							#	I(year_cent^2) +
								country * year_cent +
								(1 | year_cent) +
								(1 | country),
								data=ELLIBU)#data=ELLIBU[which(ELLIBU$ambition_selection_gap <= 0),])#
					summary(m2)
					stargazer(me,m1,m2,type="text",intercept.bottom=FALSE)
			
			# time NL!, same model as above but without issue of droped levels in the next step?
			ELLIBU$timeNL <- ifelse(ELLIBU$country == "DE",0,ELLIBU$year_cent)
			table(ELLIBU$timeNL,ELLIBU$country)
			
			# using party size versus vote_share?
			
				ELLIBU$vote_share
				ELLIBU$party_size_country_stan
				cor(ELLIBU$vote_share,ELLIBU$party_size_country_stan) # ok, so really basically the same thing
				table(is.na(ELLIBU$vote_share))
				
				ggplot(data=ELLIBU, aes(y=vote_share, fill=country)) +
				geom_boxplot()
				aggregate(vote_share~country, mean, data=ELLIBU)
				
				ELLIBU$vote_share_cent <- scale(ELLIBU$vote_share,center=TRUE,scale=FALSE)
				hist(ELLIBU$vote_share)
				hist(ELLIBU$vote_share_cent)
				ELLIBU$vote_share_cent <- ELLIBU$vote_share_cent/10
			
				
			
			m3 <- lmer(	ambition_selection_gap~
								selection_control_fac + # new
								party_id_nat_equiv_short + # added this here on 19/10/2021
								district_magnitude_country_cent +
			#					type +
								quota_percentage_lessthen50 +
								quota_soft_fact +
							#	quota_zipper +
								vote_share_cent +
							#	year_cent + #was timeNL + # used to replace the interaction
							#	country +
							#	I(timeNL^2) +
								(1 | year_cent)+
								(1 | country),
							#	(1 | party_id_nat_equiv_short),
								data=ELLIBU)#data=ELLIBU[which(ELLIBU$ambition_selection_gap <= 0),])#
					summary(m3)
					stargazer(m3,type="text",intercept.bottom=FALSE)
			# key check here: model 'as good'?!
			anova(m3,m2) # yes it is!
			
			
			m4 <- lmer(	ambition_selection_gap~
								selection_control_fac +
								party_id_nat_equiv_short + # added this here on 19/10/2021
								district_magnitude_country_cent +
			#					type +
								quota_percentage_lessthen50 +
								quota_soft_fact +
							#	quota_zipper +
								vote_share_cent +
							#	party_size_country_stan +
								year_cent + #was timeNL + # used to replace the interaction
							#	I(year_cent^2) +
								
								(1 | year_cent) +
								(1 | country),
								data=ELLIBU)#data=ELLIBU[which(ELLIBU$ambition_selection_gap <= 0),])#
					summary(m4)
					stargazer(m1,m3,m4,type="text",intercept.bottom=FALSE) 
					
			m4a <- lmer(	ambition_selection_gap~
								selection_control_fac +
								party_id_nat_equiv_short + # added this here on 19/10/2021
								district_magnitude_country_cent +
			#					type +
							#	quota_percentage_lessthen50 +
								quota_soft_fact +
							#	quota_zipper +
								vote_share_cent +
							#	party_size_country_stan +
								year_cent + #was timeNL + # used to replace the interaction
							#	I(year_cent^2) +
								
								(1 | year_cent) +
								(1 | country),
								data=ELLIBU)#data=ELLIBU[which(ELLIBU$ambition_selection_gap <= 0),])#
					summary(m4a)
					stargazer(m1,m3,m4,m4a,type="text",intercept.bottom=FALSE) 
		
			m5 <- lmer(	ambition_selection_gap~
								selection_control_fac +
								party_id_nat_equiv_short + # added this here on 19/10/2021
								district_magnitude_country_cent +
								quota_percentage_lessthen50 +
								quota_soft_fact +
								party_size_country_stan +
								timeNL + 
								type +
								(1 | year_cent) +
								(1 | country),
								data=ELLIBU)#data=ELLIBU[which(ELLIBU$ambition_selection_gap <= 0),])#
					summary(m5)
					stargazer(me,m1,m3,m5,type="text",intercept.bottom=FALSE)
					
			# some additional attemps to see what is going on whith the high control condition...  how about party dummies?
			
			m6 <- lmer(	ambition_selection_gap~
								selection_control_fac +
								party_id_nat_equiv_short + # added this here on 19/10/2021
								district_magnitude_country_cent +
								quota_percentage_lessthen50 +
								quota_soft_fact +
							#	quota_zipper +
								party_size_country_stan +
							#	timeNL + 
							#	party_id_nat_equiv_short + # should be consider the inclusion of a party fixed effect?!
							 	parliament_id, # parliament fixed effects do seem to 'solve?!' issue... big estimate for high control now... model not reliable, but intersing massive negative etiamtes for ealy years in NL.. timeNL does not yet capture the time-trend properly?!
							# (1 | parliament_id) +
							#	(1 | country),
							#	(1 | party_id_nat_equiv_short), # this model suggests that a random effect for party is quite a good idea?!
								data=ELLIBU)#data=ELLIBU[which(ELLIBU$ambition_selection_gap <= 0),])#
					summary(m6)
					stargazer(me,m1,m3,m4,m6,type="text",intercept.bottom=FALSE)				
	
	
	### older 'final' stargazer model
	
				
					stargazer(
								caption = "Regression model predicting selection - ambition gap with selection control",
								me,
								m1,
								m3,
								m4,
							#	m5,
								type="text",
								intercept.bottom=FALSE,
								no.space=FALSE,
								column.labels=(c("Empty","Dist.mag. ","Control","Quota")),
								star.char = c(".", "*", "**", "***"),
								star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
							#	keep.stat=c("ll"),
							#	omit.stat=c("aic","bic"),
								font.size = "small",
								label = "SelecElecRegTab",
								dep.var.labels = c("(Ratio on list - ambition)")
							)
					
					
	
	
		
	##### working towards a nice regression output
	
	
	m2 <- m1
	m1 <- me
	
	m4 <- m4
	m3 <- m1a
	
	
	
	
	
	summary(m1)
	summary(m2)
	summary(m3)
	summary(m4)

# name replacements

	specificnamecleaning <- function(dirtynamesloc)	
			{
				cleanernames <- gsub("(Intercept)","Constant",dirtynamesloc,fixed=TRUE)
				cleanernames <- gsub("district_magnitude_country_cent","district magnitude",cleanernames,fixed=TRUE)
				cleanernames <- gsub("quota_percentage_lessthen50","quota percentage deviation",cleanernames,fixed=TRUE)
				cleanernames <- gsub("quota_soft_factsoft","soft quota",cleanernames,fixed=TRUE)
				cleanernames <- gsub("quota_zipper","zipper quota",cleanernames,fixed=TRUE)
				cleanernames <- gsub("vote_share_cent","vote share",cleanernames,fixed=TRUE)
				cleanernames <- gsub("year_cent","election year",cleanernames,fixed=TRUE)
				cleanernames <- gsub("selection_control_facmedium selection control","medium selection control",cleanernames,fixed=TRUE)
				cleanernames <- gsub("selection_control_fachigh selection control","high selection control",cleanernames,fixed=TRUE)
				cleanernames <- gsub("party_id_nat_equiv_shortSPD (DE)","SPD(DE)",cleanernames,fixed=TRUE)
				cleanernames <- gsub("party_id_nat_equiv_shortB90|Gru (DE)","B90|Gru(DE)",cleanernames,fixed=TRUE)
				cleanernames <- gsub("party_id_nat_equiv_shortLi|PDS (DE)","PDS(DE)",cleanernames,fixed=TRUE)
				cleanernames <- gsub("party_id_nat_equiv_shortCDA (NL)","CDA(NL)",cleanernames,fixed=TRUE)
				cleanernames <- gsub("party_id_nat_equiv_shortGL (NL)","GL(NL)",cleanernames,fixed=TRUE)
				cleanernames <- gsub("party_id_nat_equiv_shortPvdA (NL)","PvdA(NL)",cleanernames,fixed=TRUE)
				
				return(cleanernames)
			}
			
			dirtynames <- names(fixef(m4))
			
			specificnamecleaning(dirtynames)


# use bootstrapping to get a standard error for the variance estimates.
		runconfints <- TRUE
		
	# list level
				listlvar <- format(round(c(
							as.data.frame(VarCorr(m1))$vcov[3],
							as.data.frame(VarCorr(m2))$vcov[3],
							as.data.frame(VarCorr(m3))$vcov[3],
							as.data.frame(VarCorr(m4))$vcov[3]
											),digits=3),nsmall=3)
		
		if (runconfints)
		{
				simulations <- 100
				am1 <- confint(m1,method="boot",nsim=simulations)
				am2 <- confint(m2,method="boot",nsim=simulations)
				am3 <- confint(m3,method="boot",nsim=simulations)
				am4 <- confint(m4,method="boot",nsim=simulations)

				listlvarse <- format(round(c(
					((am1[3,2] - am1[3,1]) / 1.98),
					((am2[3,2] - am2[3,1]) / 1.98),
					((am3[3,2] - am3[3,1]) / 1.98),
					((am4[3,2] - am4[3,1]) / 1.98)
					),digits=3),nsmall=3)
		} else {
			listlvarse <- rep("NE",4)
		}								
	
	# country level
				countryvar <- format(round(c(
							as.data.frame(VarCorr(m1))$vcov[2],
							as.data.frame(VarCorr(m2))$vcov[2],
							as.data.frame(VarCorr(m3))$vcov[2],
							as.data.frame(VarCorr(m4))$vcov[2]
											),digits=3),nsmall=3)
		if (runconfints)
		{					
				countryvarse <- format(round(c(
					((am1[2,2] - am1[2,1]) / 1.98),
					((am2[2,2] - am2[2,1]) / 1.98),
					((am3[2,2] - am3[2,1]) / 1.98),
					((am4[2,2] - am4[2,1]) / 1.98)
					),digits=3),nsmall=3)
		} else {
			countryvarse <- rep("NE",4)
		}	
		
		
	# election year level
		
				elecyearvar <- format(round(c(
							as.data.frame(VarCorr(m1))$vcov[1],
							as.data.frame(VarCorr(m2))$vcov[1],
							as.data.frame(VarCorr(m3))$vcov[1],
							as.data.frame(VarCorr(m4))$vcov[1]
											),digits=3),nsmall=3)
		
		
		if (runconfints)
		{					
				elecyearvarse <- format(round(c(
					((am1[1,2] - am1[1,1]) / 1.98),
					((am2[1,2] - am2[1,1]) / 1.98),
					((am3[1,2] - am3[1,1]) / 1.98),
					((am4[1,2] - am4[1,1]) / 1.98)
					),digits=3),nsmall=3)
		} else {
			elecyearvarse <- rep("NE",4)
		}

	nobsc <-  c(nobs(m1),
				nobs(m2),
				nobs(m3),
				nobs(m4))
						
	nrofcountries <- c(		nrow(ranef(m1)$country),
							nrow(ranef(m2)$country),
							nrow(ranef(m3)$country),
							nrow(ranef(m4)$country))
							
	nrofelectionyears <- c(	nrow(ranef(m1)$year_cent),
							nrow(ranef(m2)$year_cent),
							nrow(ranef(m3)$year_cent),
							nrow(ranef(m4)$year_cent))

			GiveBrackets <- function(vector1)
				{
					resultvec <- vector()
					for(i in 1:length(vector1))
					{
						resultvec[i] <- paste("(",vector1[i],")",sep="")
					}
					return(resultvec)
				}


	varlabels <- specificnamecleaning(names(fixef(m4)))

	stargazer(
		m1,
		m2,
		m3,
		m4,
		type="text",
		intercept.bottom=FALSE,
		no.space=FALSE,
		column.labels=(c("Empty","Control only","Party fix.ef.","Quota + context")),
		star.char = c(".", "*", "**", "***"),
		star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
		keep.stat=c("ll"),
		omit.stat=c("aic","bic"),
		font.size = "small",
		label = "RegTab",
		caption = "Regression model predicting selection - ambition gap with selection control",
		dep.var.labels = c("(Ratio on list - ambition)"),
		covariate.labels = varlabels,
			add.lines = list(	
							c("Random effects"),
							c("--------------------------"),
							c("NR of (semi) lists",nobsc),
							c("list-level var",listlvar),
							c("",GiveBrackets(listlvarse)),
							c("NR of countries",nrofcountries),
							c("country-level var",countryvar),
							c("",GiveBrackets(countryvarse)),
							c("NR of election years",nrofelectionyears),
							c("election-level var",elecyearvar),
							c("",GiveBrackets(elecyearvarse))
							)
		  )	
		
		# some interpretation things
		aggregate(data=ELLIBU, vote_share_cent~quota_percentage_lessthen50, mean) # it is typically the bigger parties that have quotas that are not 50%!
		table(ELLIBU$quota_percentage_lessthen50,ELLIBU$party_id_nat_equiv_short)
		
		table(ELLIBU$selection_control_fac,ELLIBU$party_id_nat_equiv_short)
		summary(ELLIBU$quota_percentage_lessthen50)
		
		table(ELLIBU$year,ELLIBU$year_cent)
		
		# some basic descriptives for the final sample that is being used
		nrow(ELLIBU)
		table(is.na(ELLIBU$ambition_selection_gap))
		
		
		
		
		# so there is this suggestion to select only undershooting cases now?
			summary(ELLIBU$ambition_selection_gap)
			hist(ELLIBU$ambition_selection_gap)
			
			plot(m4)
			
			par(mfrow=c(1,2))
			hist(ELLIBU[which(ELLIBU$ambition_selection_gap <= 0),]$ambition_selection_gap,xlim=c(-50,50),breaks=20)
			#hist(ELLIBU$ambition_selection_gap,xlim=c(-50,50),breaks=20)
			hist(predict(m4),xlim=c(-50,50),breaks=10)
			par(mfrow=c(1,1))
		
		# and delta method
		
		dmm <- m4
		
		fixef(dmm)
		seq(from=1,to=length(fixef(dmm)),by=1)
		cbind(fixef(dmm),seq(from=1,to=length(fixef(dmm)),by=1))
		
		# low
		deltaMethod(dmm,"x1", parameterNames= paste("x", 1:length(fixef(dmm)), sep=""))

		# med
		deltaMethod(dmm,"x1+x2", parameterNames= paste("x", 1:length(fixef(dmm)), sep=""))
		
		# high
		deltaMethod(dmm,"x1+x3", parameterNames= paste("x", 1:length(fixef(dmm)), sep=""))
		
		# in a marginal effect plot
		set_theme(base = theme_minimal())
		plot_model(m4,
					type = "emm",
					terms="selection_control_fac",
					title ="Estimated marginal effects: predicted ambition-selection gap given level of control"
					) + 
				#	ylim(-25,20) +
					scale_x_continuous(name="selection control",breaks=c(1,2,3),labels=c("low","medium","high")) +
					scale_y_continuous(name="ambition selection gap") +
					geom_hline(yintercept=0, linetype="dashed", color = "darkgreen",size=1.1) +
					geom_text(aes(x=1.6,y=-22,label="less women on electable list position than specified ambition"),angle=0,size=3) +
					geom_text(aes(x=1.6,y=12,label="more women on electable list position than specified ambition"),angle=0,size=3) 
					
					# I think this is what is being used here: https://rdrr.io/cran/emmeans/man/emmeans.html # I would like to understand this fully! 
	
		# do I indeed have unbalanced cases?
		table(ELLIBU$selection_control_fac) # yes for sure! Right, so it does make sense. Lets just use this one!
		
					stargazer(me,m0,m1,m3,m4,intercept.bottom=FALSE)
					
			# to answer the question if small parties are still excluded?
			names(ELLIBU)
			summary(ELLIBU$party_size)
			hist(ELLIBU$party_size)
			
			table(ELLIBU$party_id_nat_equiv_short, ELLIBU$type)
					
					
			# strategy to get a ranking of 'cynism'? (how big the ambition / selection gap is for different parties?)
			
				head(ELLIBU)
				
				nat_party_id

				mod.rank.c <- lmer(ambition_selection_gap~
								nat_party_id +
								district_magnitude_country_stan +
								selection_control_fac +
								party_size_country_stan + # is country mean centered and country standard deviation scaled
								quota_percentage_cent +
							#	quota_soft + # plays a big role in the difference, probably not control for this?!
								# (year_cent | country),
								(1 | country) + (1|nat_party_id),
								data=ELLIBU)
					summary(mod.rank.c)
				
				# left/right from parlgov
				ELLIBU$lr <- rep(NA,times=nrow(ELLIBU))
				ELLIBU$lr <- ifelse(ELLIBU$nat_party_id == "DE_B90|Gru_NT",2.9,ELLIBU$lr)
				ELLIBU$lr <- ifelse(ELLIBU$nat_party_id == "DE_CDU_NT",6.3,ELLIBU$lr)
				ELLIBU$lr <- ifelse(ELLIBU$nat_party_id == "DE_Li|PDS_NT",1.2,ELLIBU$lr)
				ELLIBU$lr <- ifelse(ELLIBU$nat_party_id == "DE_SPD_NT",3.6,ELLIBU$lr)
				ELLIBU$lr <- ifelse(ELLIBU$nat_party_id == "NL_CDA_NT",5.9,ELLIBU$lr)
				ELLIBU$lr <- ifelse(ELLIBU$nat_party_id == "NL_GL_NT",2.0,ELLIBU$lr)
				ELLIBU$lr <- ifelse(ELLIBU$nat_party_id == "NL_PvdA_NT",3.6,ELLIBU$lr)
					
				mod.rank.ideo <- lmer(ambition_selection_gap~
								nat_party_id +
								district_magnitude_country_stan +
								selection_control_fac +
								party_size_country_stan + # is country mean centered and country standard deviation scaled
								quota_percentage_cent +
							#	quota_soft + # plays a big role in the difference, probably not control for this?!
								# (year_cent | country),
								(1 | country) + (1|nat_party_id),
								data=ELLIBU)
					summary(mod.rank.ideo)

				mod.rank.nc <- lmer(ambition_selection_gap~
								nat_party_id +
							#	district_magnitude_country_stan +
							#	selection_control_fac +
							#	party_size_country_stan + # is country mean centered and country standard deviation scaled
							#	quota_percentage_cent +
							#	quota_soft +
								(year_cent | country),
								data=ELLIBU)
					summary(mod.rank.nc)
					stargazer(m4,mod.rank.c,mod.rank.nc,type="text",intercept.bottom=FALSE)
					table(ELLIBU$nat_party_id)

			# estimated gap size for the different parties
				
				mod.rank <- mod.rank.nc
				mod.rank <- mod.rank.c
				
				x1 <- deltaMethod(mod.rank,"x1", parameterNames= paste("x", 1:length(fixef(mod.rank)), sep=""))	
				x2 <- deltaMethod(mod.rank,"x1+x2", parameterNames= paste("x", 1:length(fixef(mod.rank)), sep=""))	
				x3 <- deltaMethod(mod.rank,"x1+x3", parameterNames= paste("x", 1:length(fixef(mod.rank)), sep=""))	
				x4 <- deltaMethod(mod.rank,"x1+x4", parameterNames= paste("x", 1:length(fixef(mod.rank)), sep=""))	
				x5 <- deltaMethod(mod.rank,"x1+x5", parameterNames= paste("x", 1:length(fixef(mod.rank)), sep=""))	
				x6 <- deltaMethod(mod.rank,"x1+x6", parameterNames= paste("x", 1:length(fixef(mod.rank)), sep=""))	
				x7 <- deltaMethod(mod.rank,"x1+x7", parameterNames= paste("x", 1:length(fixef(mod.rank)), sep=""))	
			
				A <- rbind(x1,x2,x3,x4,x5,x6,x7)
				rownames(A) <- names(table(ELLIBU$nat_party_id))
				A
				A[order(-A$Estimate),]
				
				table(ELLIBU$quota_soft,ELLIBU$nat_party_id)
			
			
			### selection to election gap (percentage selected - percentage elected)

				# calculating this gap
				mean(ELLIBU$ratio_elected)
				mean(ELLIBU$ratio_on_list,na.rm=TRUE)
				summary(ELLIBU$ratio_on_list)

				hist(ELLIBU$ratio_elected)
				hist(ELLIBU$ratio_on_list)
				
				nrow(ELLIBU)

############################
# \/  \/  \/  \/  \/  \/  \/  
############################
	
				ELLIBU$selection_election_gap <- ELLIBU$ratio_elected - ELLIBU$ratio_on_list # negative numbers indicate that less women where elected then selected
				ELLIBU$selection_election_gap <- ELLIBU$selection_election_gap * 100
				
				mean(ELLIBU$selection_election_gap)
				
				DATDE <- ELLIBU[which(ELLIBU$country == "DE"),]
				boxplot(DATDE$ratio_elected~DATDE$year)
				DATDE$year_fac <- as.factor(DATDE$year)
				
					ggplot(DATDE, aes(x=year_fac, y=ratio_elected)) + 
					 geom_boxplot()  +
					 geom_smooth(method = "lm", se=TRUE, aes(group=1))
				
					ggplot(DATDE, aes(x=year_fac, y=ratio_on_list)) + 
					 geom_boxplot()  +
					 geom_smooth(method = "lm", se=TRUE, aes(group=1))
				
				# other country differences - so yes: relatively speaking a bigger gap in NL in general. - but not 12 points?!
					ggplot(data=ELLIBU, aes(y=selection_election_gap,fill=country)) +
					geom_boxplot()
					
					ggplot(data=ELLIBU, aes(selection_election_gap)) +
					geom_histogram() +  
					facet_grid(country ~ .)
				
				####### so' what is up with these cases where parties 'overshoot'

					head(ELLIBU[,c("selection_election_gap","ratio_elected","ratio_on_list")])

					# is it country?
					ggplot(data=ELLIBU, aes(y=selection_election_gap,fill=country)) +
					geom_boxplot()
					
					# is it list type?
					ggplot(data=ELLIBU, aes(y=selection_election_gap,fill=type)) +
					geom_boxplot()
					
					# both?
					ELLIBU$country_type <- paste0(ELLIBU$country,ELLIBU$type)
					table(ELLIBU$country_type)
					
						ggplot(data=ELLIBU, aes(y=selection_election_gap,fill=country_type)) +
						geom_boxplot()
					
					# for specific parties?
					ggplot(data=ELLIBU, aes(y=selection_election_gap,fill=party_id_nat_equiv)) +
					geom_boxplot()
					
					

				
#################				
#################			
				# and the absolute version as suggested
				ELLIBU$selection_election_gap <- abs(ELLIBU$selection_election_gap) # here we select absolute values or not!
				ELLIBU$selection_election_gap_abs <- abs(ELLIBU$selection_election_gap) 
				
				# and the more radical just not looking at selecting to many women
				nrow(ELLIBU)
			#	ELLIBU <- ELLIBU[which(ELLIBU$selection_election_gap <= 0),]
				nrow(ELLIBU)
				
#################
#################				
				# inspection
				table(is.na(ELLIBU$selection_election_gap)) # very large number of NA here is the result of many election lists just not leading to anybody being elected
				table(is.na(ELLIBU$ratio_elected)) # by far most cases are lost here
				table(is.na(ELLIBU$ratio_on_list))
				
				hist(ELLIBU$selection_election_gap) # not a lot is happening here?
				boxplot(ELLIBU$selection_election_gap~ELLIBU$country,main="abs(% women elected  into parliament - % women selected onto list)") 
				hist(ELLIBU$selection_election_gap[which(ELLIBU$country =="CH")])
				hist(ELLIBU$selection_election_gap[which(ELLIBU$country =="DE")])
				hist(ELLIBU$selection_election_gap[which(ELLIBU$country =="NL")]) 
				
				boxplot(ELLIBU$selection_election_gap~ELLIBU$election_uncertainty, main="% women elected  into parliament - % women selected onto list")
				boxplot(ELLIBU$selection_election_gap~ELLIBU$election_uncertainty_detailed_fac, main="% women elected  into parliament - % women selected onto list")
				
				beanplot(ELLIBU$selection_election_gap~ELLIBU$election_uncertainty, main="abs(% women elected  into parliament - % women selected onto list)",maxstripline=0.1, col = c("#CAB2D6", "#33A02C", "#B2DF8A"))
				beanplot(ELLIBU$selection_election_gap~ELLIBU$election_uncertainty_detailed_fac, main="abs(% women elected  into parliament - % women selected onto list)",maxstripline=0.1, col = c("#CAB2D6", "#33A02C", "#B2DF8A"))
				
				# variance per group
				table(ELLIBU$election_uncertainty)
				var(ELLIBU[which(ELLIBU$election_uncertainty == "low electoral uncertainty"),]$selection_election_gap,na.rm=T)
				var(ELLIBU[which(ELLIBU$election_uncertainty == "high electoral uncertainty"),]$selection_election_gap)
				levene.test(ELLIBU$selection_election_gap,ELLIBU$election_uncertainty)
				table(ELLIBU$election_uncertainty)
				
					# break down to hard and soft quotas
						boxplot(ELLIBU$selection_election_gap~ELLIBU$typeandquota, main="% women elected  into parliament - % women selected onto list")
						beanplot(ELLIBU$selection_election_gap~ELLIBU$keylisttypes, main="abs(% women elected  into parliament - % women selected onto list)",maxstripline=0.1, col = c("#CAB2D6", "#33A02C", "#B2DF8A"))
				
				# lets inspect some of these positive cases, for example in the Netherlands
				SAMPLETOCHECK <- ELLIBU[which(ELLIBU$country == "DE" & (ELLIBU$ratio_elected > ELLIBU$ratio_on_list)),] # 629 cases
				SAMPLETOCHECK <- ELLIBU[which(ELLIBU$country == "DE" & (ELLIBU$ratio_elected < ELLIBU$ratio_on_list)),] # 448 cases
				nrow(SAMPLETOCHECK)
				
				table(ELLIBU$party_id)
				table(SAMPLETOCHECK$party_id) # happens a lot especially for the greens?
				
				
				# and a simple regresion model
				
					ELLIBU$party_size_cat_de <- factor(ELLIBU$party_size_cat_de, levels=c("big party","small party"))
					table(ELLIBU$party_size_cat_de,is.na(ELLIBU$selection_election_gap))
					table(ELLIBU$party_size_cat_de,ELLIBU$quota_percentage)
					table(ELLIBU$party_size_cat_de,ELLIBU$election_uncertainty)
					
					table(ELLIBU$district_magnitude)
					table(ELLIBU$district_magnitude,ELLIBU$parliament_id)
					
					nrow(ELLIBU)
				##	ELLIBUTEMP <- ELLIBU[which(ELLIBU$type == "district"),]
					ELLIBUTEMP <- ELLIBU
					nrow(ELLIBUTEMP)
				
					mb1 <- lm(selection_election_gap~election_uncertainty
								,data=ELLIBUTEMP)
					summary(mb1)
					
					mb2 <- lm(	selection_election_gap~
							#	election_uncertainty +
							#	party_size_cat_de + # cannot be included anymore because perfectly co-lineair with election_uncertainty 
								quota_percentage
								,data=ELLIBUTEMP)
					summary(mb2)
					
					lm(	selection_election_gap~
								party_size_cat_de
								,data=ELLIBUTEMP)
					
					stargazer(mb1,mb2,type="text",intercept.bottom=FALSE)
					stargazer(mb1,mb2,intercept.bottom=FALSE)
				
				head(ELLIBUTEMP)
				
				# district magnitude effect descriptive
					plot(ELLIBUTEMP$district_magnitude,ELLIBUTEMP$selection_election_gap)
				
					plot(ELLIBUTEMP$district_magnitude_country_stan,ELLIBUTEMP$selection_election_gap)
					
					ggplot(ELLIBUTEMP, aes(x=district_magnitude_country_stan, y=selection_election_gap,color=party_id_nat_equiv)) +
					geom_point() + 
					geom_smooth(method = lm, se = FALSE) +
					geom_jitter()
					
					ggplot(ELLIBUTEMP, aes(x=district_magnitude_country_stan, y=selection_election_gap,color=party_id_nat_equiv)) +
					geom_point() + 
					geom_smooth(method = lm, se = FALSE) +
					geom_jitter()
					
					ggplot(ELLIBUTEMP, aes(x=district_magnitude_country_stan)) + geom_histogram()
					
					ggplot(data=subset(ELLIBUTEMP,ELLIBUTEMP$district_magnitude_country_stan < 2), aes(x=district_magnitude_country_stan, y=selection_election_gap,color=party_id_nat_equiv)) +
					geom_point() + 
					geom_smooth(method = lm, se = FALSE) +
					geom_jitter()
					
					ggplot(data=ELLIBUTEMP, aes(x=district_magnitude_country_stan, y=selection_election_gap_abs,color=party_id_nat_equiv)) +
					geom_point() + 
					geom_smooth(method = lm, se = FALSE) +
					geom_jitter()
					
					ggplot(data=ELLIBUTEMP, aes(x=district_magnitude, y=selection_election_gap_abs,color=party_id_nat_equiv)) +
					geom_point() + 
					geom_smooth(method = lm, se = FALSE) +
					geom_jitter()
					
					
					My_Theme = theme(
									  axis.title.x = element_text(size = 16),
									  axis.text.x = element_text(size = 14),
									  axis.title.y = element_text(size = 16),
									  axis.text.y = element_text(size = 14))
					
					
					# graphic to include!
					# \/ this one probably works the best?
					
					ELLIBUTEMP$country_and_type <- paste0(ELLIBUTEMP$country,":",ELLIBUTEMP$type)
					countrytypagg <- as.data.frame(aggregate(data=ELLIBUTEMP, selection_election_gap~country_and_type,mean))
					countrytypagg$x <- c(1,mean(ELLIBUTEMP$district_magnitude[which(ELLIBUTEMP$country_and_type == "DE:list")]),150)
					countrytypagg
					
					library("scales")
					reverselog_trans <- function(base = exp(1)) {
						trans <- function(x) -log(x, base)
						inv <- function(x) base^(-x)
						trans_new(paste0("reverselog-", format(base)), trans, inv, 
								  log_breaks(base = base), 
								  domain = c(1e-100, Inf))
					}
					
					ggplot(data=ELLIBUTEMP, aes(x=district_magnitude, y=selection_election_gap,shape=country_and_type,color=country_and_type)) +
					geom_point(size=3) + 
					scale_color_grey() +
					geom_smooth(method = "loess",color="black") +
					geom_label(data=countrytypagg,aes(x=x,y=5,label=round(selection_election_gap,2)),size=7,shape=3,color="black") +
					labs(title = "election selection gap VS district magnitude, observed values", 
					x = "District magnitude", 
					y = "(Election-selection) gap whole list") +
					theme_pubr(base_size=20) +
					scale_x_continuous(trans='log2') +
				#	scale_y_continuous(trans=reverselog_trans(10)) +
					ylim(c(-75,75)) +
					geom_text(aes(x=2.5,y=50,label="more women than expected"),angle=0,size=geom.text.size,color="black") + 
					geom_text(aes(x=2.5,y=-50,label="less women than expected"),angle=0,size=geom.text.size,color="black") 
					
					# some data inspection: a gap of -100, how?! -- answer is: we only look at lists from which somebody got elected, is a big reductio, CDU only ran one candidate on this 'semi-list' environment that was successfull, 
					ELLIBUTEMP[which(ELLIBUTEMP$selection_election_gap == -100),]
					ELLIBUTEMP[which(grepl("DE_NT-BT_2009__district-seats-HB",ELLIBUTEMP$list_id,fixed=TRUE)),] # are the other cases dropped because they are considered not electable?
					ELLIBUCOMP[which(grepl("DE_NT-BT_2009__district-seats-HB",ELLIBUCOMP$list_id,fixed=TRUE)),] # it actually seems the CDU only ran one candidate in the 'semi-list' only for Bremen-II-Bremerhaven 

					head(GCPARE)
					D1 <- as.data.frame (GCPARE)
					D1$list_id <- rownames(D1)
					head(D1)
					D1[which(grepl("DE_NT-BT_2009__district-seats-HB",D1$list_id,fixed=TRUE)),] # so four indeed... so this the step where we loose the gender counts ...

	##  requested graphs: similarity of the list on the x axis and each gap on the y axis
		
			# only selection_election_gap here, see upwards for election selection gap
				ELLIBUNL3 <- ELLIBU[which(ELLIBU$country == "NL" & !ELLIBU$parliament_id == "NL_NT-TK_1981"),]
				table(ELLIBU$country)
				nrow(ELLIBUNL3)
		
				ELLIBUNL3$parliament_id <- as.factor(ELLIBUNL3$parliament_id)
		
				ggplot(ELLIBUNL3, aes(x=meanpersdifferent, y=selection_election_gap,color=party_id,shape=parliament_id)) + 
				  geom_point() + 
				  ylab("(Election-selection) gap whole list") + 
				  xlab("average percentage of unique politicians accross districts") + 
				  scale_shape_manual(values=1:nlevels(ELLIBUNL2$parliament_id)) +
				  geom_text(aes(x=0.5,y=30,label="more women than expected"),angle=0,color="black") + 
				  geom_text(aes(x=0.5,y=-30,label="less women than expected"),angle=0,color="black") 
		
					# why is the f and m count missing for for example 'DE_NT-BT_2009__Bremen-II-Bremerhaven' and  'DE_Moellenstaedt_Oliver_1978' - does this have to do with the redistricting e.t.c. again?
					
					
					ggplot(data=ELLIBUTEMP, aes(x=district_magnitude, y=selection_election_gap,color=country)) +
					geom_point() + 
					geom_smooth(method = loess, se = FALSE) +
					labs(title = "election selection gap VS district magnitude, observed values", 
					x = "District magnitude", 
					y = "(Election-selection) gap whole list") +
					My_Theme
				
					
					# other graphic to include
					ggplot(data=ELLIBUTEMP, aes(x=vote_share_increase, y=selection_election_gap,color=linkedlist)) +
					geom_point() + 
					geom_smooth(method = loess, se = FALSE) +
					labs(title = "Selection election gap VS vote share increases, observed values", 
					x = "Vote share increase", 
					y = "Selection-election gap whole list") +
					My_Theme
				
							
					
					ggplot(ELLIBUTEMP, aes(x=district_magnitude)) + geom_histogram()
				

				
				hist(ELLIBUTEMP$selection_election_gap)
				
				
				
				# and the multi-level regression model
				
					ELLIBU$country <- droplevels(as.factor(ELLIBU$country))
				
					# graphic to include!
					ggplot(data=ELLIBUTEMP, aes(x=district_magnitude, y=selection_election_gap)) +
					geom_point() + 
					geom_smooth(method = loess, se = FALSE) +
					geom_jitter()
					
					ELLIBUTEMP$vote_share_change_ten <- ELLIBUTEMP$vote_share_change / 10
					hist(ELLIBUTEMP$vote_share_change)
					hist(ELLIBUTEMP$vote_share_change_ten)
					
					ELLIBUTEMP$vote_share_change
					head(ELLIBUTEMP)
					
					
					
					ELLIBUTEMP$vote_share_increase <- ifelse(ELLIBUTEMP$vote_share_change_ten > 0, ELLIBUTEMP$vote_share_change_ten, 0)
					ELLIBUTEMP$vote_share_lost <- ifelse(ELLIBUTEMP$vote_share_change_ten < 0, ELLIBUTEMP$vote_share_change_ten, 0)#
					
					hist(ELLIBUTEMP$vote_share_increase)
					hist(ELLIBUTEMP$vote_share_lost)
					
					# we probably need to drop the vote share increase larger then 1 cases, outliers
					nrow(ELLIBUTEMP)
				#	ELLIBUTEMP <- ELLIBUTEMP[which(ELLIBUTEMP$vote_share_increase < 1),]
					nrow(ELLIBUTEMP)
					
					# other graphic to include
					ggplot(data=ELLIBUTEMP, aes(x=vote_share_increase, y=selection_election_gap,color=linkedlist)) +
					geom_point() + 
					geom_smooth(method = loess, se = FALSE) +
					geom_jitter()			

					# district_magnitude, reset
					
						# total number of list places is always the district_magnitude take from max list lenght in secundary disctricts -- number of list > in Philip his data it was coded how they entered; 'member_type'.
							# -- north-rhein-westfalia should have 70: 
							# -- for quasi lists: 
								# option 1: set all too 1 -- does not make sense >  we do stick to the label > we need to make this dillema transparent. 'the selectors are faced with this uncertainty' 
								# option 2: set to 70
								# option 3: just say 'district magnitude does not play a role' - maybe this means we need two models?
							# quasi list for small parties as a measure of symbolic representation.


					# lets add an interaction between vote_share_increase and vote_share_lost 

					# NL, maybe we should run after 1992 -- see if findings get stronger?

					# some other graphics for Philip
					table(ELLIBUTEMP$linkedlist,droplevels(as.factor(ELLIBUTEMP$nat_party_id)))
					
					table(ELLIBUTEMP$nat_party_id)
					ELLIBUTEMP$nat_party_id <- factor(ELLIBUTEMP$nat_party_id,levels=c("DE_CDU_NT","DE_B90|Gru_NT","DE_SPD_NT","DE_Li|PDS_NT","NL_CDA_NT","NL_GL_NT","NL_PvdA_NT"))
					table(ELLIBUTEMP$nat_party_id)
					
				    mee <- lmer(selection_election_gap~1+ # selection_election_gap
							#	nat_party_id + # added on 2021/10/19
								(1 | year_cent) +
								(1 | country),
								data=ELLIBUTEMP)#data=ELLIBUTEMP[which(ELLIBUTEMP$selection_election_gap <= 0),])
					summary(mee)
				
					# lets model this differently,
					
						# we want type
						# we want country
						# and then only for German lists the district magitude matters
						
						table(ELLIBU$type,ELLIBU$country)
						table(ELLIBUTEMP$type,ELLIBUTEMP$country)
						
						# district magnitude, make it country centered
						hist(ELLIBUTEMP$district_magnitude)
					
						dedismagavg <- mean(ELLIBUTEMP$district_magnitude[which(ELLIBUTEMP$country == "DE")])
						dedismagsd <-  sd(ELLIBUTEMP$district_magnitude[which(ELLIBUTEMP$country == "DE")])
						
						delistdismagavg <- mean(ELLIBUTEMP$district_magnitude[which(ELLIBUTEMP$country == "DE" & ELLIBUTEMP$type == "list")])
						delistdismagsd <-  sd(ELLIBUTEMP$district_magnitude[which(ELLIBUTEMP$country == "DE" & ELLIBUTEMP$type == "list")])
						
						dedistrictdismagavg <- mean(ELLIBUTEMP$district_magnitude[which(ELLIBUTEMP$country == "DE" & ELLIBUTEMP$type == "district")])
						dedistrictdismagsd <-  sd(ELLIBUTEMP$district_magnitude[which(ELLIBUTEMP$country == "DE" & ELLIBUTEMP$type == "district")])
						
						nldismagavg <- mean(ELLIBUTEMP$district_magnitude[which(ELLIBUTEMP$country == "NL")])
						nldismagsd <-  sd(ELLIBUTEMP$district_magnitude[which(ELLIBUTEMP$country == "NL")])
						
						# (please note that this indeed both country and type centered, because when type = district all values are also zero.
						ELLIBUTEMP$district_magnitude_country_and_type_cent <- ifelse(
																					 ELLIBUTEMP$country == "DE" & ELLIBUTEMP$type == "list", 
																					 (ELLIBUTEMP$district_magnitude-delistdismagavg), 
																					 0) # this is actually all centered for the lists!
						
						# and do the same for german districts
						
						ggplot(data=ELLIBUTEMP, aes(y=district_magnitude_country_and_type_cent, fill=country_and_type)) +
						geom_boxplot()
			
			table(is.na(ELLIBU$district_magnitude_country_cent))
				
					# adding just a model with the one one key variable of interest here
					mkey <- lmer(selection_election_gap~1+ # selection_election_gap
								district_magnitude_country_and_type_cent +
								(1 | year_cent) +
								(1 | country),
								data=ELLIBUTEMP)
					summary(mkey)
					stargazer(mkey,type="text")
				
					# to remind us again of the type we are doing
					summary(ELLIBUTEMP$selection_election_gap)

					ma <- lmer(selection_election_gap~
								district_magnitude_country_and_type_cent +
								nat_party_id + # added on 2021/10/19
					#			type +
								country +
								(1 | year_cent) +
								(1 | country),
								data=ELLIBUTEMP)#data=ELLIBUTEMP[which(ELLIBUTEMP$selection_election_gap <= 0),])
					summary(ma)
					stargazer(mee,ma,type="text",intercept.bottom=FALSE)
				
					ELLIBUTEMP$type <- factor(ELLIBUTEMP$type, levels = c("list","district")) 
				#	ELLIBUTEMP$type <- ifelse(ELLIBUTEMP$type == "list", "list", "quasi-list")
				
					mb <- lmer(selection_election_gap~
								district_magnitude_country_and_type_cent + # district_magnitude_country_stan
								nat_party_id + # added on 2021/10/19
							#	type +
								(1 | year_cent) +
								(1 | country),
								data=ELLIBUTEMP)#data=ELLIBUTEMP[which(ELLIBUTEMP$selection_election_gap <= 0),])
					summary(mb)
					
					hist(ELLIBUTEMP$party_size_country_stan)
					hist(ELLIBUTEMP$vote_share)
					
					ELLIBUTEMP$vote_share_cent <- ELLIBUTEMP$vote_share - mean(ELLIBUTEMP$vote_share)
					hist(ELLIBUTEMP$vote_share_cent)
					
					cor(ELLIBUTEMP$party_size_country_stan,ELLIBUTEMP$vote_share) # so indeed pretty much exactly the same thing.
					
					
					mc <- lmer(selection_election_gap~
								district_magnitude_country_and_type_cent +
								nat_party_id + # added on 2021/10/19
								type +
								country +
								vote_share_cent +# party_size_country_stan +
								(1 | year_cent) +
								(1 | country),
								data=ELLIBUTEMP)#data=ELLIBUTEMP[which(ELLIBUTEMP$selection_election_gap <= 0),])
					summary(mc)

					md <- lmer(selection_election_gap~
								district_magnitude_country_and_type_cent +
								nat_party_id + # added on 2021/10/19
								type +
								country +
								vote_share_cent +
								vote_share_change_ten +
						#		vote_share_lost +
								year_cent +
						#		I(year_cent^2)+
						#		nat_party_id +
								(1 | year_cent) +
								(1 | country),
								data=ELLIBUTEMP)#data=ELLIBUTEMP[which(ELLIBUTEMP$selection_election_gap <= 0),])
					summary(md)

					table(ELLIBUTEMP$nat_party_id,ELLIBUTEMP$linkedlist)


					me <- lmer(selection_election_gap~
								district_magnitude_country_and_type_cent +
								nat_party_id + # added on 2021/10/19
								type +
								country +
								vote_share_cent + #party_size_country_stan +
								vote_share_change_ten +
						#		vote_share_lost +
								year_cent+
						#		I(year_cent^2)+
							#	nat_party_id +
								vote_share_change_ten*linkedlist +
						#		vote_share_lost*linkedlist +
								(1 | year_cent) +
								(1 | country),
								data=ELLIBUTEMP)#data=ELLIBUTEMP[which(ELLIBUTEMP$selection_election_gap <= 0),])#data=ELLIBUTEMP)#data=ELLIBUTEMP[which(ELLIBUTEMP$selection_election_gap <= 0 & ELLIBUTEMP$selection_election_gap > -30),])
					summary(me)
					stargazer(mee,ma,md,me,type="text",intercept.bottom=FALSE)
					
					table(ELLIBUTEMP$country_and_type)
					ELLIBUTEMP$country_and_type <- factor(ELLIBUTEMP$country_and_type, levels=c("DE:list","DE:district","NL:list"))
					
					me2 <- lmer(selection_election_gap~
								district_magnitude*country_and_type +
								nat_party_id + # added on 2021/10/19
						#		type +
						#		country +
								vote_share_cent + #party_size_country_stan +
								vote_share_change_ten +
						#		vote_share_lost +
								year_cent+
						#		I(year_cent^2)+
							#	nat_party_id +
								vote_share_change_ten*linkedlist +
						#		vote_share_lost*linkedlist +
								(1 | year_cent) +
								(1 | country),
								data=ELLIBUTEMP)#data=ELLIBUTEMP[which(ELLIBUTEMP$selection_election_gap <= 0),])
					summary(mee)
					stargazer(mee,ma,md,me2,type="text",intercept.bottom=FALSE)
					# variance estimates e.t.c? < later!
					
					stargazer(mee,ma,md,me,type="text",intercept.bottom=FALSE)
					
					# mecs <- me
					anova(mecs,me)
					
					stargazer(mee,ma,mb,md,me,intercept.bottom=FALSE)
					
					
					
		# the big regression model in which everything in life comes together?
		
					stargazer(
								caption = "Regression model predicting selection election gap with district magnitude, linked lists and controls",
								mee,
								ma,
								md,
								me,
								type="text",
								intercept.bottom=FALSE,
								no.space=FALSE,
								column.labels=(c("Empty","Dist.mag. ","Controls","Linked lists")),
								star.char = c(".", "*", "**", "***"),
								star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
							#	keep.stat=c("ll"),
							#	omit.stat=c("aic","bic"),
								font.size = "small",
								label = "SelecElecRegTab",
								dep.var.labels = c("abs(ratio elected - ratio on list)")
							)
					
		# properly layouted version
			
	m1 <- mee
	m2 <- mkey
	m3 <- ma
	m4 <- md
	
	summary(m1)
	summary(m2)
	summary(m3)
	summary(m4)
	
# to make sure that we are reminded of what version we are doing
	hist(ELLIBUTEMP$selection_election_gap)
	summary(ELLIBUTEMP$selection_election_gap)

# name replacements

	table(ELLIBUTEMP$nat_party_id)

	specificnamecleaning <- function(dirtynamesloc)	
			{
				cleanernames <- gsub("(Intercept)","Constant",dirtynamesloc,fixed=TRUE)
				cleanernames <- gsub("district_magnitude_country_and_type_cent","district magnitude",cleanernames,fixed=TRUE)
				cleanernames <- gsub("typedistrict","type:district (quasi-list)",cleanernames,fixed=TRUE)
				cleanernames <- gsub("vote_share_cent","vote share",cleanernames,fixed=TRUE)
				cleanernames <- gsub("vote_share_change_ten","vote share change",cleanernames,fixed=TRUE)
				cleanernames <- gsub("year_cent","election year",cleanernames,fixed=TRUE)
				cleanernames <- gsub("election_year","election year",cleanernames,fixed=TRUE)
				cleanernames <- gsub("linkedlistlinked","linked list",cleanernames,fixed=TRUE)
				cleanernames <- gsub("nat_party_idDE_CDU_NT","CDU(DE)",cleanernames,fixed=TRUE)	
				cleanernames <- gsub("nat_party_idDE_SPD_NT","SPD(DE)",cleanernames,fixed=TRUE)	
				cleanernames <- gsub("nat_party_idDE_B90|Gru_NT","B90|GRu(DE)",cleanernames,fixed=TRUE)	
				cleanernames <- gsub("nat_party_idDE_Li|PDS_NT","Li|PDS(DE)",cleanernames,fixed=TRUE)	
				cleanernames <- gsub("nat_party_idNL_CDA_NT","CDA(NL)",cleanernames,fixed=TRUE)	
				cleanernames <- gsub("nat_party_idNL_GL_NT","GL(NL)",cleanernames,fixed=TRUE)	
				cleanernames <- gsub("nat_party_idNL_PvdA_NT","PvdA(NL)",cleanernames,fixed=TRUE)	
				return(cleanernames)
			}
			
			dirtynames <- names(fixef(m4))
			
			specificnamecleaning(dirtynames)

# use bootstrapping to get a standard error for the variance estimates.
		runconfints <- TRUE
		
	# list level
				listlvar <- format(round(c(
							as.data.frame(VarCorr(m1))$vcov[3],
							as.data.frame(VarCorr(m2))$vcov[3],
							as.data.frame(VarCorr(m3))$vcov[3],
							as.data.frame(VarCorr(m4))$vcov[3]
											),digits=3),nsmall=3)
		
		if (runconfints)
		{
				simulations <- 100
				am1 <- confint(m1,method="boot",nsim=simulations)
				am2 <- confint(m2,method="boot",nsim=simulations)
				am3 <- confint(m3,method="boot",nsim=simulations)
				am4 <- confint(m4,method="boot",nsim=simulations)

				listlvarse <- format(round(c(
					((am1[3,2] - am1[3,1]) / 1.98),
					((am2[3,2] - am2[3,1]) / 1.98),
					((am3[3,2] - am3[3,1]) / 1.98),
					((am4[3,2] - am4[3,1]) / 1.98)
					),digits=3),nsmall=3)
		} else {
			listlvarse <- rep("NE",4)
		}								
	
	# country level
				countryvar <- format(round(c(
							as.data.frame(VarCorr(m1))$vcov[2],
							as.data.frame(VarCorr(m2))$vcov[2],
							as.data.frame(VarCorr(m3))$vcov[2],
							as.data.frame(VarCorr(m4))$vcov[2]
											),digits=3),nsmall=3)
		if (runconfints)
		{					
				countryvarse <- format(round(c(
					((am1[2,2] - am1[2,1]) / 1.98),
					((am2[2,2] - am2[2,1]) / 1.98),
					((am3[2,2] - am3[2,1]) / 1.98),
					((am4[2,2] - am4[2,1]) / 1.98)
					),digits=3),nsmall=3)
		} else {
			countryvarse <- rep("NE",4)
		}	
		
		
	# election year level
		
				elecyearvar <- format(round(c(
							as.data.frame(VarCorr(m1))$vcov[1],
							as.data.frame(VarCorr(m2))$vcov[1],
							as.data.frame(VarCorr(m3))$vcov[1],
							as.data.frame(VarCorr(m4))$vcov[1]
											),digits=3),nsmall=3)
		
		
		if (runconfints)
		{					
				elecyearvarse <- format(round(c(
					((am1[1,2] - am1[1,1]) / 1.98),
					((am2[1,2] - am2[1,1]) / 1.98),
					((am3[1,2] - am3[1,1]) / 1.98),
					((am4[1,2] - am4[1,1]) / 1.98)
					),digits=3),nsmall=3)
		} else {
			elecyearvarse <- rep("NE",4)
		}

	nobsc <-  c(nobs(m1),
				nobs(m2),
				nobs(m3),
				nobs(m4))
						
	nrofcountries <- c(		nrow(ranef(m1)$country),
							nrow(ranef(m2)$country),
							nrow(ranef(m3)$country),
							nrow(ranef(m4)$country))
							
	nrofelectionyears <- c(	nrow(ranef(m1)$year_cent),
							nrow(ranef(m2)$year_cent),
							nrow(ranef(m3)$year_cent),
							nrow(ranef(m4)$year_cent))

			GiveBrackets <- function(vector1)
				{
					resultvec <- vector()
					for(i in 1:length(vector1))
					{
						resultvec[i] <- paste("(",vector1[i],")",sep="")
					}
					return(resultvec)
				}


	varlabels <- specificnamecleaning(names(fixef(m4)))

	# to make sure that we are reminded of what version we are doing
	hist(ELLIBUTEMP$selection_election_gap)
	summary(ELLIBUTEMP$selection_election_gap)

	stargazer(
		m1,
		m2,
		m3,
		m4,
		type="text",
		intercept.bottom=FALSE,
		no.space=FALSE,
		column.labels=(c("Empty","DiMa only","Party fix.ef.","Context")),
		star.char = c(".", "*", "**", "***"),
		star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
		keep.stat=c("ll"),
		omit.stat=c("aic","bic"),
		font.size = "small",
		label = "RegTab",
		caption = "Regression model predicting selection election gap with district magnitude, linked lists and controls",
		dep.var.labels = c("abs(ratio elected - ratio on list)"),
		covariate.labels = varlabels,
			add.lines = list(	
							c("Random effects"),
							c("--------------------------"),
							c("NR of (semi) lists",nobsc),
							c("list-level var",listlvar),
							c("",GiveBrackets(listlvarse)),
							c("NR of countries",nrofcountries),
							c("country-level var",countryvar),
							c("",GiveBrackets(countryvarse)),
							c("NR of election years",nrofelectionyears),
							c("election-level var",elecyearvar),
							c("",GiveBrackets(elecyearvarse))
							)
		  )			
					
	
	# some interpreation things
		
		summary(ELLIBUTEMP$district_magnitude_country_and_type_cent)
		table(ELLIBUTEMP$district_magnitude_country_and_type_cent,ELLIBUTEMP$country) 
		table(ELLIBUTEMP$district_magnitude_country_and_type_cent,ELLIBUTEMP$type) 
		
		hist(ELLIBUTEMP[which(ELLIBUTEMP$type=="list" & ELLIBUTEMP$country=="DE"),]$district_magnitude)
		hist(ELLIBUTEMP[which(ELLIBUTEMP$type=="list" & ELLIBUTEMP$country=="DE"),]$district_magnitude_country_and_type_cent)
		
		summary(ELLIBUTEMP[which(ELLIBUTEMP$type=="list" & ELLIBUTEMP$country=="DE"),]$district_magnitude)
		summary(ELLIBUTEMP[which(ELLIBUTEMP$type=="list" & ELLIBUTEMP$country=="DE"),]$district_magnitude_country_and_type_cent)
		
		table(ELLIBUTEMP$district_magnitude_country_and_type_cent,ELLIBUTEMP$district_magnitude)
		
		# so, do I agree that this is a small effect?
		summary(ELLIBUTEMP$district_magnitude_country_and_type_cent) # it ranges from roughly -20 to 55, so that is 75 steps.
		75*-0.091 # = 6.8%, that is indeed a relatively small effect, but given the small effect size in general: not to bad.
	
		table(ELLIBUTEMP$nat_party_id,ELLIBUTEMP$linkedlist)
		
	# in a marginal effect plot
		set_theme(base = theme_minimal())
		plot_model(m4,
					type = "emm",
					terms="district_magnitude_country_and_type_cent",
					title ="Estimated marginal effects: predicted election-selection gap given district magnitude"
					) + 
				#	ylim(-25,20) +
					scale_x_continuous(name="district magnitude (centered values)",limits=c(-20,55),breaks=c(-20,0,20,40,55),label=c("2 (-20)","22* (0)","42 (20)","62 (40)","77 (55)")) +
					scale_y_continuous(name="abs(selection-election) gap") +
					geom_hline(yintercept=0, linetype="dashed", color = "darkgreen",size=1.1)
		
		delistdismagavg
		
	
	# the requested data export from Elena
		# 
		table(is.na(ELLIBU$selection_election_gap)) # please note that with the now extended selection that are more cases for which the selection_election gap is missing!
		table(is.na(ELLIBU$ambition_selection_gap)) # same number here? > yes > same issue probably (see below).
		
		nrow(ELLIBU)
		ELLIBU[which(is.na(ELLIBU$selection_election_gap)),]
		head(ELLIBU[which(is.na(ELLIBU$selection_election_gap)),]) # issue seems to be the 'ratio_on_list', which is NA --- I think I narrowed this issue down again .. 
																   # ..I think these are new districs, so they get kicked out from the selection when the 'electable' decisions is made.
		
		ELLIBUEXP <- ELLIBU[which(!is.na(ELLIBU$selection_election_gap)),]
		nrow(ELLIBUEXP)
		
		library("writexl")
		write_xlsx(ELLIBUEXP,"./ELLIBU_Export_Control_Paper.xlsx")
		
					
					# probaly some effect visualisations would be good here
					
					
					ELLIBUDE <- ELLIBU[which(ELLIBU$country == "DE"),]
					ELLIBUNL <- ELLIBU[which(ELLIBU$country == "NL"),]
					boxplot(ELLIBUDE$selection_election_gap~ELLIBUDE$year)
					boxplot(ELLIBUNL$selection_election_gap~ELLIBUNL$year)
		
		# descriptives for the paper
		
			table(ELLIBUEXP$country, ELLIBUEXP$year)



		
		# and some diagnostics here
			
			summary(ELLIBU$selection_election_gap)
			hist(ELLIBU$selection_election_gap)
			
			plot(m4)
			
			par(mfrow=c(1,2))
			hist(ELLIBU[which(ELLIBU$selection_election_gap <= 0),]$selection_election_gap,xlim=c(-50,50),breaks=40)
			#hist(ELLIBU$selection_election_gap,xlim=c(-50,50),breaks=20)
			hist(predict(m4),xlim=c(-50,50),breaks=5)
			par(mfrow=c(1,1))


############ some info that was requested as input for some of our additional decisions

	table(ELLIBUTEMP$district_magnitude,ELLIBUTEMP$type)
	
	
	

	




######################################################################################
################### suggestion from Philip to 'predict' soft quotas ##################
######################################################################################		

names(ELLIBU)

	# if there is no relation the relative percentage of quota soft parties should not differ accross to columns
	
	table(ELLIBU$quota_soft,ELLIBU$selection_control_fac)
	prop.table(table(ELLIBU$quota_soft,ELLIBU$selection_control_fac),2)
	
	
	
	
	# and for district magnitude
	
		ELLIBU$district_magnitude_bins <- as.character(ELLIBU$district_magnitude)
		
		ELLIBU$district_magnitude_bins[which(ELLIBU$district_magnitude_bins == "1")] <- "dm_1"
		
		ELLIBU$district_magnitude_bins[which(ELLIBU$district_magnitude > 1 & ELLIBU$district_magnitude <= 20)] <- "dm_2-10"
		ELLIBU$district_magnitude_bins[which(ELLIBU$district_magnitude > 10 & ELLIBU$district_magnitude <= 20)] <- "dm_11-20"
		ELLIBU$district_magnitude_bins[which(ELLIBU$district_magnitude > 20 & ELLIBU$district_magnitude <= 30)] <- "dm_21-30"
		ELLIBU$district_magnitude_bins[which(ELLIBU$district_magnitude > 30 & ELLIBU$district_magnitude <= 50)] <- "dm_31-50"
		ELLIBU$district_magnitude_bins[which(ELLIBU$district_magnitude > 50 & ELLIBU$district_magnitude <= 80)] <- "dm_51-80"
		ELLIBU$district_magnitude_bins[which(ELLIBU$district_magnitude > 80 & ELLIBU$district_magnitude < 150)] <- "dm_81-149"
		
		ELLIBU$district_magnitude_bins[which(ELLIBU$district_magnitude == 150)] <- "dm_150"
		
		ELLIBU$district_magnitude_bins <- factor(ELLIBU$district_magnitude_bins,levels=c("dm_1","dm_2-10","dm_11-20","dm_21-30","dm_31-50","dm_51-80","dm_81-149","dm_150"))
		
		table(ELLIBU$district_magnitude_bin)
		
	table(ELLIBU$quota_soft,ELLIBU$district_magnitude_bin)
	prop.table(table(ELLIBU$quota_soft,ELLIBU$district_magnitude_bin),2)
	
	table(ELLIBU$quota_soft,ELLIBU$country)

	
	# and both in simple model
	
	mod_qe <- glm(quota_soft ~ 1, data = ELLIBU, family = binomial)
	
	mod_q1 <- glm(quota_soft ~ selection_control_fac, data = ELLIBU, family = binomial)
	
	mod_q2 <- glm(quota_soft ~ selection_control_fac + district_magnitude_bins, data = ELLIBU, family = binomial)

	
	stargazer(mod_qe,mod_q1,mod_q2,type="text",intercept.bottom=FALSE)











######################################################################################
############################ OLD DESCRIPTIVE RESULTS #################################
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




















	