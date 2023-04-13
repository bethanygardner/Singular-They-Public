12/04/2020

'data.frame':	5202 obs. of  17 variables:

Participant-level variables:
    $ SubjID     : Factor w/ 102 levels "R_0qPfWjp8o4W3Z61",..: 84 84 84 84 84 84 84 84 84 84 ...
    Participant ID.

    $ SubjAge      : int  18 18 18 18 18 18 18 18 18 18 ...
    Participant age.
    Min.   1st Qu.  Median    Mean 3rd Qu.    Max. 
    18.00   18.00   19.00   19.49   20.00   27.00 

    $ SubjGender   : Factor w/ 3 levels "female","male",..: 1 1 1 1 1 1 1 1 1 1 ...
    Participant gender, entered in a free response box, with spelling/capitalization standardized.
    79 female, 22 male, 1 nonbinary

    $ SubjEnglish: Factor w/ 3 levels "Fully competent in speaking listening reading and writing but not native\n",..: 3 3 3 3 3 3 3 3 3 3 ...
    Participant English history, choosing from:
    (86) Native (learned from birth)  
    (15) Fully competent in speaking, listening, reading, and writing, but not native  
    (1)  Limited but adequate competence in speaking, reading, and writing  
    (0)  Restricted ability (e.g. only reading or speaking/listening)  
    (0)  Some familiarity (e.g. a year of instruction in school)  

    $ List         : int  2 2 2 2 2 2 2 2 2 2 ...
    List of stimuli, counterbalancing which names have they/them pronouns.

Which task this observation comes from. There is one row per observation, with columns
belonging to different tasks left blank. See analysis script for a dataframe structured 
with one row per item, to directly compare memory and production responses for the same
item.
    $ Task         : Factor w/ 3 levels "introduction",..: 2 2 2 3 2 2 2 3 2 2 ...
    
Correct answers (see stimuli spreadsheet):
    $ Name         : Factor w/ 12 levels "Amanda","Andrew",..: 3 3 3 3 8 8 8 8 4 4 ...
    
    $ Pronoun      : Factor w/ 3 levels "he/him","she/her",..: 1 1 1 1 1 1 1 1 3 3 ...
    
    $ Pet          : Factor w/ 3 levels "cat","dog","fish": 1 1 1 1 2 2 2 2 3 3 ...
    
    $ Job          : Factor w/ 12 levels "accountant","doctor",..: 12 12 12 12 4 4 4 4 9 9 ...

Memory task (12 per participant):
    $ M_Type       : Factor w/ 4 levels "","job","pet",..: 2 3 4 1 2 3 4 1 2 3 ...
    Question type in multiple-choice task (job, pet, pronoun).
    
    $ M_Response   : Factor w/ 19 levels "","accountant",..: 19 3 9 1 8 5 18 1 14 7 ...
    Response for multiple-choice questions
    
    $ M_Acc        : int  0 1 1 NA 1 1 0 NA 1 1 ...
    Accuracy for multiple-choice questions. If Pronoun/Pet/Job matches M_Response.

Production task (12 per participant): 
    $ P_Response : Factor w/ 793 levels "","amanda fixed her computer",..: 1 1 1 169 1 1 1 235 1 1 ...
    Full response for sentence completion task
    
    $ P_Pronoun    : Factor w/ 5 levels "","he/him","none",..: 1 1 1 2 1 1 1 2 1 1 ...
    Pronouns used in sentence completion task (he/him, she/her, they/them, none).
    
    $ P_Pronoun_Acc: int  NA NA NA 1 NA NA NA 1 NA NA ...
    Accuracy of pronouns used in sentence completion task. If Pronoun matches P_Pronoun.

Introduction task (3 per participant):
    $ I_Response : Factor w/ 299 levels ""," I would ask Jessica about food spots ",..: 1 1 1 1 1 1 1 1 1 1 ...
    Full text of response to introduction task, not coded yet.