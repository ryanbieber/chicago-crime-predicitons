## put the parameters you want hard coded into the program in this one
source("functions.R")

Crimes <- c("Homicide 1 and 2","Criminal Sexual Assualt","Robbery","Aggravated Assualt","Aggravated Battery","Burglary","Larceny","Motor Vehicle Theft","Arson",
                 "Involuntary Manslaugherter", "Simple Assualt","Simple Battery","Forgery and Counterfeiting","Fraud","Embezzlement","Stolen Property","Vandalism","Weapons Violation","Prostitution","Criminal Sexual Abuse",
                 "Drug Abuse","Gambling","Offenses Against Family","Liquor License","Disorderly Conduct","Misc Non Index Offense")
FBI_Code <- c("01A","02","03","04A","04B","05","06","07","09","01B","08A","08B","10","11","12","13","14","15","16","17","18","19","20","22","24","26")

fbi_merge_code <- cbind.data.frame(Crimes, FBI_Code)
