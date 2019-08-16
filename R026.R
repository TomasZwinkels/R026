######################################################################################
#################################### SETUP ###########################################
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
		setwd("C:/Users/turnerzw/Basel Powi Dropbox/Tomas Zwinkels/F-drive-fork/R026")
		getwd()
	
		# install.packages("foreach")
		# install.packages("foreach")
		# install.packages("doParallel")
		#install.packages("lme4")
	
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
			
		# get all of the German parliamentary RESE episodes sub-selected so we can merge these in on date
		
			# lets do some generic RESE cleaning, for now and for later
			
				# deal with left and right censored dates
					RESE$res_entry_start_cleaned <- gsub("[[rcen]]","",RESE$res_entry_start,fixed=TRUE)
					RESE$res_entry_start_cleaned <- gsub("[[lcen]]","",RESE$res_entry_start_cleaned,fixed=TRUE)
					RESE$res_entry_end_cleaned <- gsub("[[rcen]]","",RESE$res_entry_end,fixed=TRUE)
					RESE$res_entry_end_cleaned <- gsub("[[lcen]]","",RESE$res_entry_end_cleaned,fixed=TRUE)
					
				# deal with dates that are only years (select 1th of June)			
					RESE$res_entry_start_cleaned <- ifelse(nchar(RESE$res_entry_start_cleaned) == 4,paste("01jun",RESE$res_entry_start_cleaned,sep=""),RESE$res_entry_start_cleaned)
					RESE$res_entry_end_cleaned <- ifelse(nchar(RESE$res_entry_end_cleaned) == 4,paste("01jun",RESE$res_entry_start_cleaned,sep=""),RESE$res_entry_end_cleaned)
				
					RESE$res_entry_start_posoxctformat <- as.POSIXct(as.character(RESE$res_entry_start_cleaned),format=c("%d%b%Y"))
					RESE$res_entry_end_posoxctformat <- as.POSIXct(as.character(RESE$res_entry_end_cleaned),format=c("%d%b%Y"))
					summary(RESE$res_entry_start_posoxctformat)
					summary(RESE$res_entry_end_posoxctformat)
		
					# write in a column with a standerat indicator
			
				# do the reduction		
				RESERED <- RESE[which((RESE$country == "DE" | RESE$country == "CH"| RESE$country == "NL") & RESE$political_function == "NT_LE_T3_NA_01" & (!(grepl("-SR_",RESE$parliament_id)) | is.na(RESE$parliament_id) )),] # also exclude standerat entries here with a grepl, they have the same political function code, if rows do not have a parliament ID, do not exclude them on basis of this
				
				# run this line if you want to exclude CH completly 
				RESERED <- RESE[which((RESE$country == "DE" | RESE$country == "NL") & RESE$political_function == "NT_LE_T3_NA_01" & (!(grepl("-SR_",RESE$parliament_id)) | is.na(RESE$parliament_id) )),]
				
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
		
		# add a fake parliamentary episode ID
			nrow(FPAREBU)
			FPAREBU$fake_parl_episode_id <- paste(FPAREBU$pers_id,FPAREBU$parliament_id,sep="__")
			length(unique(FPAREBU$fake_parl_episode_id)) # does not match!, several people occur double
			DUB <- FPAREBU[which(duplicated(FPAREBU$fake_parl_episode_id)),] # these are the problematic cases ## #fixlater!
			nrow(DUB) # NOTE TO SELF: you can parse theses cases to Adrian to fix # 88 cases! # some inspection myself suggest that these are re-entries into the same parliament.
			
			# for now, 
			nrow(FPAREBU)
			FPAREBU <- FPAREBU %>% distinct(fake_parl_episode_id, .keep_all = TRUE)
			nrow(FPAREBU)
		
		# remove parliaments for which no matches exist
			FPAREBU <- FPAREBU[which(!is.na(FPAREBU$pers_id)),]
			nrow(FPAREBU)
		
		# finally, lets merge in data from PAREWRONG if there is any for the fake parliamentary episode
			
			# first check that one for duplicates as well
				nrow(PAREWRONG)
				length(unique(PAREWRONG$parl_episode_id)) # so there are duplicates here as well that I should remove
				PAREWRONG[which(duplicated(PAREWRONG$parl_episode_id)),]
			
			# so also reduce this one before the merge to avoid duplicate matches
				PAREWRONGRED <- PAREWRONG %>% distinct(parl_episode_id, .keep_all = TRUE)
				nrow(PAREWRONGRED) # alright, looking promissing
			
			# now, lets merge in what we have there
				TEMP <- sqldf("SELECT FPAREBU.fake_parl_episode_id, PAREWRONGRED.*
						   FROM FPAREBU LEFT JOIN PAREWRONGRED
						   ON FPAREBU.fake_parl_episode_id = PAREWRONGRED.parl_episode_id
						 ")
				nrow(TEMP) # alright number is looking good, so lets write this result to FPAREBU
				tail(TEMP)
			
				FPAREBU <- TEMP
			
			# everybody in this new FPAREBU file was in parliament at same point, so lets indicate that
				table(FPAREBU$member_ofthisparliament_atsomepoint)
				FPAREBU$member_ofthisparliament_atsomepoint <- "yes"
				table(FPAREBU$member_ofthisparliament_atsomepoint)

	##############################################
	# DATA 3: the buildup of the ELEN data-frame
	##############################################
			
		nrow(ELEN)
		ELEN$country <- substr(ELEN$elec_entry_id,1,2)
		table(ELEN$country)
		
		# first selection of variables of interest
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
			ELENBU <- sqldf("SELECT ELENBU.*, ELLI.list_name, ELLI.parliament_id, ELLI.district_id, ELLI.list_length, ELLI.district_id, ELLI.party_id as 'party_id_from_elli'
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
		
			## please be aware that this is something of which we know there are issues, although Adrian has by now fixed a lot of these!
			## something which is also interesting is that party is taken from MEME, but we also have a lot of people in here that where just candidates right? 
				## --> indeed, now added these above, this is just party id from MEME
		
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
				
					table(ELENBU$samepartyid) # still 382 cases where this is not case #fixlater! - probably really are manual matches that just need to be set proper
					ELENBUNOTSAME <- ELENBU[which(ELENBU$samepartyid == FALSE),]
					nrow(ELENBUNOTSAME)
					head(ELENBUNOTSAME) 
					tail(ELENBUNOTSAME)
					table(ELENBUNOTSAME$country)
					table(ELENBUNOTSAME$party_id_from_meme_nat_equiv)
					
					table(ELENBUNOTSAME$party_id_from_meme_nat_equiv)
	
					ELENBUNOTSAME[which(ELENBUNOTSAME$party_id_from_meme_nat_equiv == "NL_GL_NT"),]
			
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
		
	##############################################
	# DATA 4: the buildup of the ELLI data-frame
	##############################################
	
		####################
		# DATA 4.1: we analyse the percentages of german district seats on the level over their landes, also in ELLI this combination needs to be made
		####################
		
			# we do this by simply overwriting the list ids with a generalised one
	
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
							} else { #otherwise keep the current one
								resvec2[i] <- ELLI$list_id[i]
							}
					}	

			# OVERWRITE here!
				ELLI$list_id_aggr <- resvec2
				ELLI$list_id_old <- ELLI$list_id
				ELLI$list_id <- ELLI$list_id_aggr
				
			# now for ELLI, we also need to combine thse values? Because this is technically only ONE list now
				
				# only thing really needs to happen for that is to remove the district id and to then select all the unique rows?
				# lets think of a better way to do this, that keeps the district id in?!
				
					ELLI$country <- substr(ELLI$list_id,1,2)
					table(is.na(ELLI$country))
				
					ELLIBU <- sqldf("SELECT list_id, list_name, parliament_id, list_length, country, party_id, party_id_nat_equiv, district_id
								FROM ELLI
								")
					
					# this is where the magic happens
					nrow(ELLIBU)
					ELLIBU <- ELLIBU[!duplicated(ELLIBU$list_id),] # seems rather simular (was 9714 before).. probably some other duplicates! .. fixes the issue for below already?
					nrow(ELLIBU)
					table(is.na(ELLIBU$country))
			
	##############################################
	# DATA 5: gender guessing on basis on namelist when nessary
	##############################################
	
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
		
		# writing the result to ELENBU
			ELENBU$genderguesses <- resvec
		
		# inspection of the result
			table(ELENBU$gender)
			table(ELENBU$genderguesses)
			table(ELENBU$gender,ELENBU$genderguesses)
			
			table(is.na(ELENBU$gender))
			table(is.na(ELENBU$genderguesses))
			
			table(is.na(ELENBU$genderguesses),ELENBU$country) # so, decent amounts of missingness per country still
			prop.table(table(is.na(ELENBU$genderguesses),ELENBU$country),2) # about 13% in CH and NL and 5% in DE
		
			table(is.na(ELENBU$genderguesses),ELENBU$list_id) 
			prop.table(table(is.na(ELENBU$genderguesses),ELENBU$list_id),2) 
	
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
			
		# and reduce this to the people that where in parliament straight after the election  (taken from R019!) # why am I doing this again?!
					
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
				# seem to be not the case because above fictional_parl_episode_id are not created for everybody because some ELEN entries do not match with ELLI, I have asked Adrian to check this #fixlater
	
				ELENBURED[which(ELENBURED$in_parliament == "no"),]
	
	##############################################
	# DATA 7: find out what ELEN positions are actually 'electable' because we are reducing our data to these positions
	##############################################
	
			# my suggested approach to this is to find your 'double ganger' in the last two elections, to see how (s)he did
			
			# find everybody there double gangers, you are a double ganger when
				# you ran for the same party
				# in the same district / on the same list /\ i.e. the same list id, but just with a different year
				
				# on the same list position (please note that for German disctricst listplace is always NA now in the data, as there is only 1!)
				
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
								# the reason this goes wrong is because above I have updated the list ids, which I should have given another name?!
				
				# we can now also check if he got elected or not (if this returns a row, he did)
				FPAREBU[which(FPAREBU$pers_id == TEMPE$pers_id & FPAREBU$parliament_id == TEMPE$parliament_id),] # no hits, so no luck this year, how about the year before?
				
				TEMPE <- ELEN[which(ELEN$list_id == "DE_NT-BT_2009__DE_NT-BT_2009__Aachen-I__district-Pi"),] # right, so here we see that re-districting is screwing up our measure
				
				TEMPE <- sqldf("SELECT TEMPE.*, ELLI.parliament_id 
								FROM TEMPE LEFT JOIN ELLI
								ON
								TEMPE.list_id = ELLI.list_id_old
								")
								# the reason this goes wrong is because above I have updated the list ids, which I should have given another name?!
				
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

					
					# german district seats get list position NA, but lets make this '1' where it needs to be

						head(ELEN[which(ELEN$country == "DE", ELEN$type == "district"),])
						ELEN$listplace <- ifelse(ELEN$country == "DE" & ELEN$type == "district",1,ELEN$listplace)
						table(ELEN[which(ELEN$country == "DE", ELEN$type == "district"),]$listplace)
						
					# function to match previous parliament
						
						# debugging, german BT CDU 2017
						local_list_id = "DE_NT-BT_2017__DE_NT-BT_2017__Baden-Wuerttemberg__Christlich-Demokratische-Union-Deutschlands-in-Niedersachsen"
						local_list_position = 1
						howfarback = 1
						local_natparty_id = "DE_CDU_NT"
						
						
						# get persid 
						
						wasdoublegangertminxsuccesfull <- function(local_list_id,local_list_position,howfarback,local_natparty_id)
						{
						
							# get the list id for my double ganger
						
								# first, get parliament id
								myparid <- substr(str_extract(local_list_id,".+?__"),0,nchar(str_extract(local_list_id,".+?__"))-2)
							
								# find the previous parliament
								earlierparliament <- NA # reset
								if(howfarback == 1)
								{
									earlierparliament <- as.character(PARL[which(PARL$parliament_id == myparid),]$previous_parliament)
								} 
								else 
								{
									if(howfarback == 2)
									{
										earlierparliament <- as.character(PARL[which(PARL$parliament_id == myparid),]$previous_parliament)
										earlierparliament <- as.character(PARL[which(PARL$parliament_id == earlierparliament),]$previous_parliament)
									} 
									else 
									{
										if(howfarback == 3)
										{
										earlierparliament <- as.character(PARL[which(PARL$parliament_id == myparid),]$previous_parliament)
										earlierparliament <- as.character(PARL[which(PARL$parliament_id == earlierparliament),]$previous_parliament)
										earlierparliament <- as.character(PARL[which(PARL$parliament_id == earlierparliament),]$previous_parliament)
										}
										
									}
									
								}
								
								# now generate the list id for my double ganger
								doublegangerfakelistid <- gsub(myparid,earlierparliament,local_list_id)
								doublegangerparty <- local_natparty_id
								
							# and the district id
									doublegangerdistrictid <- str_extract(local_list_id, "__.+__") # first find the district id part of the complete list id
									doublegangerdistrictid <- gsub(myparid,earlierparliament,doublegangerdistrictid)# then replace the parliament_id part
									doublegangerdistrictid <- gsub("^__","",doublegangerdistrictid)	# then get rid of the __ and __ at the start and end of the remaining string
									doublegangerdistrictid <- gsub("__$","",doublegangerdistrictid)	# then get rid of the __ and __ at the start and end of the remaining string
									
							# now get her pers_id
								
								#reset
								doublegangerpersidvoteone <- ""
								doublegangerpersidvotetwo <- ""
								
								doublegangerpersidvoteone <- ELENBU[which(ELENBU$list_id_old == doublegangerfakelistid & ELENBU$listplace == local_list_position),]$pers_id[1] # if there are multiple double-gangers, only use the first one?
								doublegangerpersidvotetwo <- ELENBU[which(
																		  ELENBU$listplace == local_list_position & 
																		  ELENBU$party_id_from_elli_nat_equiv == doublegangerparty &
																		  ELENBU$district_id == doublegangerdistrictid
																    ),]$pers_id 
								
								# if there are multiple double-gangers, for now please break the script.							
								if(length(doublegangerpersidvotetwo) > 1 | doublegangerpersidvotetwo) > 1)
								{
									stop("function was doing run - local_list_id:",local_list_id," - local_list_position:",local_list_position," - howfarback:",howfarback," - local_natparty_id",local_natparty_id,". However, two or more double gangers where detected, so execution has been stopped. Please inspect!, the value for doublegangerpersidvoteone is:",doublegangerpersidvoteone," and the value for doublegangerpersidvotetwo is: ",doublegangerpersidvotetwo,sep=""))
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
						
							if(!length(doublegangerpersid) == 0) # only do this when a double ganger was actually found! Otherwise we have no way to tell so we would like the script to return NA
							{
								if(nrow(FPAREBU[which(FPAREBU$pers_id == doublegangerpersid & FPAREBU$parliament_id == earlierparliament),]) > 0)
								{
									return(TRUE)
								} else {
									return(FALSE)
								}
							} else {
							return(NA)
							}
						}
						
						# testing, proof is in the pudding!
						
						# for some of the very last entries shown here
						tail(ELENBU)
						wasdoublegangertminxsuccesfull(ELENBU$list_id_old[163045],ELENBU$listplace[163045],1,ELENBU$party_id_from_elli_nat_equiv[163045]) # check manually and I think this is correct
			
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
							runDGagain = askYesNo("!Warning! The last stored data from the loop that does the OM on the elections lists is not the same as the current data version used for the R-script, do you want to run the loop again?! - this takes about 1.5 hours ")
						}
						
						# we can ofcourse also manually say we want do (not) run it again!
						
						# runDGagain = FALSE
						# runDGagain = TRUE
						
						if(runDGagain)
						{
								# and in a loop, for each ELEN position
								resvecelect <- vector()
								pb <- txtProgressBar(min = 1, max = nrow(ELENBU), style = 3)
								for(i in 1:nrow(ELENBU))
								{
									# this line does not deal well with having NA on one of these values, so lets make it more resilient to that
									
									successfulldoubleganger_tminus1 <- wasdoublegangertminxsuccesfull(ELENBU$list_id_old[i],ELENBU$listplace[i],1,ELENBU$party_id_from_elli_nat_equiv[i])
									successfulldoubleganger_tminus2 <- wasdoublegangertminxsuccesfull(ELENBU$list_id_old[i],ELENBU$listplace[i],2,ELENBU$party_id_from_elli_nat_equiv[i])
									successfulldoubleganger_tminus3 <- wasdoublegangertminxsuccesfull(ELENBU$list_id_old[i],ELENBU$listplace[i],3,ELENBU$party_id_from_elli_nat_equiv[i])
									
									successfulldoubleganger_tminus1 <- ifelse(is.na(successfulldoubleganger_tminus1),FALSE,TRUE)
									successfulldoubleganger_tminus2 <- ifelse(is.na(successfulldoubleganger_tminus2),FALSE,TRUE)
									successfulldoubleganger_tminus3 <- ifelse(is.na(successfulldoubleganger_tminus3),FALSE,TRUE)
									
									resvecelect[i] <- successfulldoubleganger_tminus1 | successfulldoubleganger_tminus2 | successfulldoubleganger_tminus3 # if in one of the previous three elections then it counts as electable
									setTxtProgressBar(pb, i)
								}
								close(pb)
								
								write.csv(resvecelect,file=paste("INDA/DG/",format(now(), "%Y%m%d_%H%M_"),"resvecelect.csv",sep=""))
								file.copy("PCC/dataversion.txt",paste("INDA/DG/",format(now(), "%Y%m%d_%H%M_"),"dataversion.txt",sep=""),overwrite=TRUE)
						} else # what to do when we are NOT running the loop again
						{
							# then load the latest of these files
								latestrun <- getlatestrun("DG")
								
								resvecelecttemp <- read.csv(paste("INDA/DG/",latestrun,"_resvecelect.csv",sep=""))
								resvecelect <- resvecelecttemp[,2]
						}
						
						ELENBU$electable <- ifelse(resvecelect,"electable","not electable")
						table(ELENBU$electable)
						table(is.na(ELENBU$electable))
						
						tail(ELENBU)
						head(ELENBU[which(ELENBU$electable == "electable"),])
						ELENBU[38061:38100,]
			
			# do the actual reduction, so that the aggregates below are based on only the 'electable' part of the election lists
				# before this electable step, maybe then they are still in?!
				
				ELENBU[which(ELENBU$list_id == "DE_NT-BT_2017__DE_NT-BT_2017__Baden-Wuerttemberg__Christlich-Demokratische-Union-Deutschlands-in-Niedersachsen"),]
				head(ELENBU)
				tail(ELENBU)
				
				nrow(ELENBU)
				ELENBUTEMP <- ELENBU[which(ELENBU$electable == "electable"),]
				nrow(ELENBUTEMP)
				
				table(ELENBUTEMP$parliament_id,ELENBUTEMP$party_id_from_elli_nat_equiv)
				
				ELENBU <- ELENBUTEMP # Here you can switch of the reduction to only electable!
				nrow(ELENBU)
		

		#################################### DATA AGGREGATION starts here ###################################
	
	##############################################
	# DATA 8: get percentage of women on the list, with gender taken from ELENBU
	##############################################
	
	##### aggregation on the ELLI level ######
			GCELLI <- as.data.frame.matrix(table(ELENBU$list_id,ELENBU$genderguesses)) 
				# so, note to future self: if there is missingness here it is simply ignored. only known cases are counted
				# ohter note, this list id has now above been replaced with the aggregate list id
			
			GCELLI$list_id <- rownames(GCELLI)		
			GCELLI$country <- substr(GCELLI$list_id,1,2)
			table(GCELLI$country)
			table(is.na(GCELLI$country))
			GCELLI$ratio <- GCELLI$f / (GCELLI$f+GCELLI$m)
			
			head(GCELLI[which(GCELLI$country == "DE"),])
			tail(GCELLI[which(GCELLI$country == "DE"),])
			# this looks good
					
			hist(GCELLI$ratio) # so this is the ratio of men/women on the election lists

		# merge into the ELLI level data-frame which above was given the same abbreviated list ids, this data-frame was reduced above to get a ratio on list variable which we need later!
			
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
			
		# lets select the 'complete' cases: where these is sufficient knowledge on the number of men and women
			
			nrow(ELLIBU)
			ELLIBUCOMP <- ELLIBU
			
		#	ELLIBUCOMP <- ELLIBU[which(ELLIBU$sumcheck == 0),]
		
			CD <- ELLIBU[which(!ELLIBU$sumcheck > -8),]
			table(CD$parliament_id) # right, so way to many from the Dutch 2012 parliament indeed!
			CD[which(CD$parliament_id == "NL_NT-TK_2012"),] # so inspection reveals, very many cases with only innitials, this is where this goes wrong, because no gender guesses possible like this.. 
		
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
			table(is.na(ELLIBU$nat_party_id)) # still 2500 missings here..
			table(ELLIBU$party_id_nat_equiv) # still about 1838 empty
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
	# DATA 10: ELLIBU gets number elected from PARE
	##############################################
	
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
			ELLIBU[9030:9050,] # these are the district seats, looks good now. Lots of NA is because lots of parties never got anybody elected.
			tail(ELLIBU)
			
		# lets make a variable that indicates if anybody from the list was elected at all - can be used for exclusion below
		ELLIBU$anycandidateselected <- ifelse(ELLIBU$list_id %in% GCPARE$list_id,"yes","nobody")
		table(ELLIBU$anycandidateselected)

	##############################################
	# DATA 11: ELLIBU gets number elected from PARE
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
			table(ELLIBU$nat_party_id) # please note the one from the mother party is used here, so I can alert Elena to that.
			
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
			table(is.na(ELLIBU$quota_now),ELLIBU$country)


	##### get district magnitude in (for now just number of people that got elected from this district in this parliament - needs to be done before the reduction to the anlytical sample
		
			# step 1: this issue has be solved above, kept district ids in so that they can be used here!

			ELLIBU[9030:9050,] # good to use for checks because bunch of district ids here that should get magnitude 1
			
			i = 9030
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
			table(ELLIBU$district_magnitude) 
			hist(ELLIBU$district_magnitude)
			## DO LATER > inspect the very large cases here?!
			ELLIBU[which(ELLIBU$district_magnitude > 135),] # right, so this is actually correct, Dutch recent year cases. Indeed suggestions here 'towards one big district'
			
			##### ASK! Elena and Philip about where the '2' are comming from here?! use the i = 9030 case from above!
			
	##### get party size in (for now just number of people from this party that got elected in the parliament) << BROKEN now, fix later!

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
			ELLIBU <- ELLIBU[which(!ELLIBU$intimeframe == "before 1981"),]
			nrow(ELLIBU)
			table(ELLIBU$party_id_nat_equiv)
			
		# get rid of all election lists from which nobody ever got eleced
			table(ELLIBU$anycandidateselected)
			
			nrow(ELLIBU)
			ELLIBU <- ELLIBU[which(!ELLIBU$anycandidateselected == "nobody"),]
			nrow(ELLIBU)		
		
		# get rid of all observations for which there is no gender quota
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
				
				## this is where the reduction happens, two options
				
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
					
					table(is.na(ELLIBU$ratio_on_list)) # complete
					table(is.na(ELLIBU$ratio_elected)) # complete
					table(is.na(ELLIBU$quota_percentage)) # complete

	###########################
	# RED A2: is OM etc to get Dutch election lists that are 100% simular so only the ones that actually vary are included 
	###########################
		
		## step 1: make a function that generates an - ';' separated - list position ordered - array of all pers_ids occurring on a certain list (takes a list id as input)
		
			head(ELENBU)
			
			# testing what goes wrong we DE Bundestag
			local_list_id = "DE_NT-BT_2017__DE_NT-BT_2017__Baden-Wuerttemberg__Christlich-Demokratische-Union-Deutschlands-in-Niedersachsen" # so, does not occur in ELENBU apparently?!
				
			getpersidarrayforlistid <- function(local_list_id)
			{
			# select 
				ELENBUME <- ELENBU[which(ELENBU$list_id == local_list_id),]
				return(paste(ELENBUME$pers_id,collapse=";"))
			}
	
			getpersidarrayforlistid(ELLIBU$list_id[4000]) # inspections seems promissing
			tail(ELLIBU)
			
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
							j=5248
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
							
							write.csv(meanpersdifferentresvec,file=paste("INDA/OM/",format(now(), "%Y%m%d_%H%M_"),"meanpersdifferentresvec.csv",sep=""))
							write.csv(percentage95simularresvec,file=paste("INDA/OM/",format(now(), "%Y%m%d_%H%M_"),"percentage95simularresvec.csv",sep=""))
							file.copy("PCC/dataversion.txt",paste("INDA/OM/",format(now(), "%Y%m%d_%H%M_"),"dataversion.txt",sep=""),overwrite=TRUE)
							
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
		nrow(ELLIBU)
		
		ELLIBUNL <- ELLIBU[which(ELLIBU$country == "NL"),]
		boxplot(ELLIBUNL$meanpersdifferent~ELLIBUNL$parliament_id)
		hist(ELLIBUNL$meanpersdifferent)
		table(ELLIBUNL$party_id)
		table(ELLIBUNL$party_id,ELLIBUNL$parliament_id)
		boxplot(ELLIBUNL$meanpersdifferent~ELLIBUNL$parliament_id)
		
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
		nrow(TEMP4) # unfortunately loosing a lot of cases here... 
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
		
			# now, calculate the 'drop'
			
			ELLIBU$vote_share_change <- ELLIBU$vote_share - ELLIBU$vote_share_previous
			hist(ELLIBU$vote_share_change)
			
			# lets inspecting missing cases quickly
			table(is.na(ELLIBU$vote_share_change))
			ELLIBU[which(is.na(ELLIBU$vote_share_change)),]
			
			
	## NEW! selection and election control variables ###!

			## selection control
				ELLIBU$selection_control <- NA
				
				# low, german district seats
				ELLIBU$selection_control[which(ELLIBU$country == "DE" & ELLIBU$type == "district")] <- "low selection control"
				
				# medium, german regional list seats + Dutch lists with regional variation
				ELLIBU$selection_control[which(ELLIBU$country == "DE" & ELLIBU$type == "list")] <- "medium selection control"
				ELLIBU$selection_control[which(ELLIBU$country == "NL" &  ELLIBU$meanpersdifferent >= 0.2)] <- "medium selection control" 

				# high, Dutch centralised national lists + all swiss lists
				ELLIBU$selection_control[which(ELLIBU$country == "NL" &  ELLIBU$meanpersdifferent < 0.2)] <- "high selection control" 
				ELLIBU$selection_control[which(ELLIBU$country == "CH")] <- "high selection control" 
				
				# checking the distribution so I can pick a good pickoff for NL
				hist(ELLIBU$meanpersdifferent[which(ELLIBU$country == "NL")],breaks=20)
				hist(ELLIBU$meanpersdifferent[which(ELLIBU$country == "DE")]) # lets think about why the big values here!
				hist(ELLIBU$meanpersdifferent[which(ELLIBU$country == "CH")],breaks=20)
				head(ELLIBU)
				
				table(ELLIBU$selection_control)
				table(ELLIBU$selection_control,ELLIBU$country)
				table(is.na(ELLIBU$selection_control)) 
				
				district_magnitude_country_stan
			
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
				
	### for all the list seats, get a variable as well that indicates what the percentage of women was on the district seats in this list its region << BROKEN now, fix later or DROP
	
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
				ELLIBU$ambition_realisation_gap <- abs(ELLIBU$ambition_realisation_gap)

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
				
				ELLIBU$ambition_selection_gap <- (ELLIBU$ratio_on_list - (ELLIBU$quota_percentage/100))
				
				# and the absolute version as suggested
#				ELLIBU$ambition_selection_gap <- abs(ELLIBU$ambition_selection_gap)
				
				# inspection of the gap
				table(is.na(ELLIBU$ambition_selection_gap)) # so, just as a note so self, quite a bit more cases because from many lists never anybody got elected?
				hist(ELLIBU$ambition_selection_gap) # looks a lot like the ambition realisation gap? - so some suggestion that the selection step is the crucial one in the sample as a whole?
				boxplot(ELLIBU$ambition_selection_gap~ELLIBU$country,main="abs(% of women selected onto the list - % from quota)") 
				hist(ELLIBU$ambition_selection_gap[which(ELLIBU$country =="DE")])
				hist(ELLIBU$ambition_selection_gap[which(ELLIBU$country =="NL")]) 
				
				
				ELLIBU$selection_control_fac <- factor(ELLIBU$selection_control, levels=c("low selection control","medium selection control","high selection control"))
				table(ELLIBU$selection_control_fac)
				boxplot(ELLIBU$ambition_selection_gap~ELLIBU$selection_control_fac, main="% of women selected onto the list - % from quota")
				beanplot(ELLIBU$ambition_selection_gap~ELLIBU$selection_control_fac, main="abs(% of women selected onto the list - % from quota)",maxstripline=0.1, col = c("#CAB2D6", "#33A02C", "#B2DF8A"))
				beanplot(ELLIBU$ambition_selection_gap~ELLIBU$selection_control_detailed_fac, main="abs(% of women selected onto the list - % from quota)",maxstripline=0.1, col = c("#CAB2D6", "#33A02C", "#B2DF8A"))
				
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
				
					me <- lmer(ambition_selection_gap~1+
								(1 | country),
								,data=ELLIBU)
					summary(me)
				
					
					# integrating Philip' suggestion to have district magnitude as model 1
					
						# let's get a country standardised version of district magnitude(moved from below)
					
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
										district_magnitude_country_stan + # is country mean centered and country standard deviation scaled
										(1 | country),
										,data=ELLIBU)
							summary(m0)
					
					
					m1 <- lmer(ambition_selection_gap~
								district_magnitude_country_stan + # is country mean centered and country standard deviation scaled
								selection_control_fac+
								(1 | country),
								,data=ELLIBU)
					summary(m1)
					
					table(ELLIBU$party_size_cat_de)
					table(ELLIBU$country)


					ELLIBU$country <- droplevels(as.factor(ELLIBU$country))
					
							
		# lets center the quota percentage!
			hist(ELLIBU$quota_percentage)
			ELLIBU$quota_percentage_cent <- ELLIBU$quota_percentage - mean(ELLIBU$quota_percentage)
					
					m2 <- lmer(	ambition_selection_gap~
								selection_control_fac +
								district_magnitude_country_stan + # is country mean centered and country standard deviation scaled
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
							
				
					# this is the model with a variable slope for a time-trend per country
					m3 <- lmer(	ambition_selection_gap~
								district_magnitude_country_stan + # is country mean centered and country standard deviation scaled
								selection_control_fac +
								party_size_country_stan + # is country mean centered and country standard deviation scaled
								(year_cent | country),
								data=ELLIBU)
					summary(m3)
					
					m4 <- lmer(	ambition_selection_gap~
								district_magnitude_country_stan +
								selection_control_fac +
								party_size_country_stan + # is country mean centered and country standard deviation scaled
								quota_percentage_cent +
								quota_soft +
								(year_cent | country),
								data=ELLIBU)
					summary(m4)
				
					stargazer(me,m0,m1,m3,m4,type="text",intercept.bottom=FALSE)
					
					stargazer(me,m0,m1,m3,m4,intercept.bottom=FALSE)
					
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
				ELLIBU$selection_election_gap <- ELLIBU$ratio_elected - ELLIBU$ratio_on_list # negative numbers indicate that less women where elected then selected
				
				# and the absolute version as suggested
				ELLIBU$selection_election_gap <- abs(ELLIBU$selection_election_gap)
				
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
					ELLIBUTEMP <- ELLIBU[which(ELLIBU$type == "district"),]
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
				
				# and the multi-level regression model
				    mee <- lmer(selection_election_gap~1+
								(1 | country),
								,data=ELLIBUTEMP)
					summary(mee)
				
					ma <- lmer(selection_election_gap~
							#	party_size_cat_de +
							#	election_uncertainty+
							#	election_uncertainty_detailed +
								(1 | country),
								,data=ELLIBUTEMP)
					summary(ma)
				
					mb <- lmer(selection_election_gap~
							#	party_size_cat_de +
							#	election_uncertainty +
							#	election_uncertainty_detailed +
								party_size_country_stan + # is country mean centered and country standard deviation scaled
								district_magnitude_country_stan + # is country mean centered and country standard deviation scaled
								(year_cent | country),
								data=ELLIBUTEMP)
					summary(mb)
					
					mc <- lmer(selection_election_gap~
							#	party_size_cat_de +
							#	election_uncertainty +
							#	election_uncertainty_detailed +
								party_size_country_stan + 
								district_magnitude_country_stan +
							#	quota_percentage +
							#	quota_soft +
								(year_cent | country),
								data=ELLIBUTEMP)
					summary(mc)

					ELLIBUTEMP$type <- factor(ELLIBUTEMP$type,levels = c("list","district"))

					md <- lmer(selection_election_gap~
							#	party_size_cat_de +
							#	election_uncertainty +
							#	election_uncertainty_detailed +
								party_size_country_stan + 
								district_magnitude_country_stan +
							#	quota_percentage +
							#	quota_soft +
								type +
								(year_cent | country),
								data=ELLIBUTEMP)
					summary(md)
					
					ELLIBUTEMP$vote_share_change_ten <- ELLIBUTEMP$vote_share_change / 10
					hist(ELLIBUTEMP$vote_share_change_ten)
					
					ELLIBUTEMP$vote_share_increase <- ifelse(ELLIBUTEMP$vote_share_change_ten > 0, ELLIBUTEMP$vote_share_change_ten, 0)
					ELLIBUTEMP$vote_share_lost <- ifelse(ELLIBUTEMP$vote_share_change_ten < 0, ELLIBUTEMP$vote_share_change_ten, 0)#
					
					hist(ELLIBUTEMP$vote_share_increase)
					hist(ELLIBUTEMP$vote_share_lost)
					
					me <- lmer(selection_election_gap~
							#	party_size_cat_de +
							#	election_uncertainty +
							#	election_uncertainty_detailed +
								party_size_country_stan + 
								district_magnitude_country_stan +
							#	quota_percentage +
							#	quota_soft +
								type +
					#			country +
								vote_share_increase +
								vote_share_lost +
								(year_cent | country),
								data=ELLIBUTEMP)
					summary(me)
					
					hist(ELLIBU$vote_share_change)
					
					stargazer(mee,ma,mb,md,me,type="text",intercept.bottom=FALSE)
					stargazer(mee,ma,mb,md,me,intercept.bottom=FALSE)
					
					ELLIBUDE <- ELLIBU[which(ELLIBU$country == "DE"),]
					ELLIBUNL <- ELLIBU[which(ELLIBU$country == "NL"),]
					boxplot(ELLIBUDE$selection_election_gap~ELLIBUDE$year)
					boxplot(ELLIBUNL$selection_election_gap~ELLIBUNL$year)

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




















	