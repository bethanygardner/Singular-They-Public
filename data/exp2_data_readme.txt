03/26/2022

'data.frame':	11520 obs. of  18 variables:

Participant-level variables:

	$ Participant: Factor w/ 320 levels 
	Participant ID.
	
	$ SubjAge    : int  
	Participant Age.
	Min. 	1st Qu.  Median  Mean 	 3rd Qu. Max. 
	22.00   30.00    36.00   38.79   45.00   77.00 
 
	$ SubjEnglish: Factor w/ 4 levels 
	Participant English histor, choosing from:
	(304) Native (learned from birth)  
    (14)  Fully competent in speaking, listening, 
		  reading, and writing, but not native  
    (1)   Limited but adequate competence in speaking, 
		  reading, and writing  
    (0)   Restricted ability (e.g. only reading or 
		  speaking/listening)   
    (1)   Some familiarity (e.g. a year of instruction in 	
		  school)  
 
	$ SubjGender : Factor w/ 2 levels "female","male"
	Participant gender, entered in a free response box, with 
	spelling/capitalization standardized.
	(125) Female
	(194) Male
	(1)   Did not provide
 
 Between-participant conditions:
 
	$ Condition  : Factor w/ 4 levels 
	Original naming conventions for conditions.
	Both    = gendered language PSA + they bios
	PSA     = gendered langauge PSA + he/she bios
	Story   = unrelated PSA         + they bios
	Neither = unrelated PSA         + he/she bios
 
	$ List       : Factor w/ 12 levels 
	Lists counter-balancing name-pronoun mappings.
 
	$ PSA        : Factor w/ 2 levels "0","1"
	1 = gendered-language PSA
	0 = unrelated PSA
 
	$ Biographies: Factor w/ 2 levels "0","1"
	1 = they bios
	0 = he/she bios

Trial-level variables:

This file has one row per observation, with columns belonging
to different tasks left blank. The analysis script has a
section creating a dataframe with one row per character, to 
directly compare memory and production responses for the same
item.

Correct answers for current character:
	$ Name       : Factor w/ 12 levels 
	
	$ Job        : Factor w/ 12 levels 
	
	$ Pet        : Factor w/ 3 levels 
	
	$ Pronoun    : Factor w/ 3 levels 
	
Memory task (12 per participant):

	$ M_Type     : Factor w/ 3 levels 
	Question type in multiple-choice task 
	(job, pet, pronoun).
	
	$ M_Response : Factor w/ 18 levels 
	Response in multiple-choice task 
	(12 jobs + 3 pronouns + 3 pets = 18)
	
	$ M_Acc      : int  1 1 0 0 1 0 0 0 0 1
	Accuracy, calculated by comparing M_Response 
	with Job/Pet/Pronoun.
	
Production task (12 per participant):

	$ P_Text     : Factor w/ 2918 levels 
	Full response for sentence completion task.
	
	$ P_Response : Factor w/ 4 levels 
	Pronouns used in sentence completion task 
	(he/him, she/her, they/them, none). This is 
	calculated in a prior script that reads the 
	PCIbex output into a format R can deal with.
	
	$ P_Acc      : int  NA 0 1 
	Accuracy of pronouns used in sentence completion 
	task, calculated by comparing P_Response with Pronoun.