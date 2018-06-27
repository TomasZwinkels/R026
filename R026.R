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
		
	##################################################################################################
	###################################### aggregation here ##########################################
	##################################################################################################
	
		# aggregation on the ELLI level
			GCELLI <- as.data.frame.matrix(table(ELENBU$list_id,ELENBU$genderguesses))
			GCELLI$list_id <- rownames(GCELLI)
			head(GCELLI)
			GCELLI[30:50,]
			tail(GCELLI)
			
			GCELLI$ratio <- GCELLI$f / (GCELLI$f+GCELLI$m)
			hist(GCELLI$ratio)

		# merge into an ELLI level data-frame
			ELLIBU <- sqldf("SELECT list_id, list_name, parliament_id, list_length
						FROM ELLI
						")
			head(ELLIBU)
			
			ELLIBU <- sqldf("SELECT ELLIBU.*, GCELLI.f, GCELLI.m, GCELLI.ratio
						FROM ELLIBU LEFT JOIN GCELLI
						ON
						ELLIBU.list_id = GCELLI.list_id
						")
		
			head(ELLIBU)
			ELLIBU[30:50,]
			tail(ELLIBU)
	
		