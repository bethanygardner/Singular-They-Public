4/27/2021

'data.frame':	5151 obs. of  17 variables:

Participant-level variables:
    $ SubjID     : Factor w/ 101 levels "R_10uYBrhUOzbUAGV",..:
    Participant ID.

    $ SubjAge      : int  18 18 18 18 18 18 18 18 18 18 ...
    Participant age.

    $ SubjGender      : Factor w/ 14 levels "female","Female",..: 1 1 1 1 1 1 1 

    $ SubjGenderRecode: Factor w/ 2 levels "female","male": 1 1 1 1 1 1 1 1 1 1 
    Participant gender, entered in a free response box, with spelling/capitalization standardized.
    72 female, 29 male

    $ SubjEnglish: Factor w/ 3 levels "Fully competent in speaking listening reading and writing but not native\n",..: 3 3 3 3 3 3 3 3 3 3 ...
    Participant English history, choosing from:
    (82) Native (learned from birth)  
    (17) Fully competent in speaking, listening, reading, and writing, but not native  (2) 
    (2)  Limited but adequate competence in speaking, reading, and writing  (3) 
    (0)  Restricted ability (e.g. only reading or speaking/listening)  (4) 
    (0)  Some familiarity (e.g. a year of instruction in school)  (5) 

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
    Contrast coded later.
    
    $ Pet          : Factor w/ 3 levels "cat","dog","fish": 1 1 1 1 2 2 2 2 3 3 ...
    
    $ Job          : Factor w/ 12 levels "accountant","doctor",..: 12 12 12 12 4 4 4 4 9 9 ...

Memory task (12 per participant):
    $ M_Type       : Factor w/ 4 levels "","job","pet",..: 2 3 4 1 2 3 4 1 2 3 ...
    Question type in multiple-choice task (job, pet, pronoun, not memory trial).
    
    $ M_Response   : Factor w/ 19 levels "","accountant",..: 19 3 9 1 8 5 18 1 14 7 ...
    Response for multiple-choice questions
    
    $ M_Acc        : int  0 1 1 NA 1 1 0 NA 1 1 ...
    Accuracy for multiple-choice questions. If Pronoun/Pet/Job matches M_Response.

Production task (12 per participant): 
    $P_Response      : Factor w/ 755 levels "","After Amanda got home from working as an engineer they took a rest directly."
    Full response for sentence completion task
    
    $ P_Pronoun    : Factor w/ 5 levels "","he/him","none",..: 1 1 1 2 1 1 1 2 1 1 ...
    Pronouns used in sentence completion task (he/him, she/her, they/them, none).
    
    $ P_Pronoun_Acc: int  NA NA NA 1 NA NA NA 1 NA NA ...
    Accuracy of pronouns used in sentence completion task. If Pronoun matches P_Pronoun.

Introduction task (3 per participant):
    $ I_Response      : Factor w/ 297 levels "","Amanda goes by she/her. Amanda is a salesman and has a pet cat. 
    Full text of response to introduction task, not coded yet.