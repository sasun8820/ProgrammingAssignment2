outcome = read.csv("outcome-of-care-measures.csv", colClasses = "character")
getwd()
head(outcome)
View(outcome)
dim(outcome)

outcome[, 11] = as.numeric(outcome[ , 11])
# on average, how much of the mortality rates from heart attack 
hist(outcome[, 11])
outcome$
sort()     
str(outcome[ ,11])

outcome[which(outcome$heartattack[1])]

#### 2. ####

best <- function(state, outcome) {
     ## Read outcome data
     
     outcomes <- read.csv("outcome-of-care-measures.csv", 
                          colClasses = "character",
                          header = TRUE)
     
     ## Get data we're interested in
     
     rates <- as.data.frame(cbind(outcomes[, 2],  # hospital 
                                  outcomes[, 7],   # state
                                  outcomes[, 11],  # heart attack 
                                  outcomes[, 17],  # heart failure 
                                  outcomes[, 23]), # pneumonia
                            stringsAsFactors = FALSE)
     
     ## Rename columns
     
     colnames(rates) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
     
     ## Check that state and outcome are valid
     
     if(!state %in% rates[,"state"]){
          stop('invalid state')
     }
     
     if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
          stop('invalid outcome')
     }
     
     
     ## Return hospital name in that state with lowest 30-day death rate
     
     ## Get only the hospitals in chosen state
     hRates <- rates[(rates[, "state"] == state), ]
     
     ## Convert outcome rate to numberic
     hRates[, outcome] <- as.numeric(hRates[, outcome])
     
     ## Remove NA values
     hRates <- hRates[!is.na(hRates[, outcome]), ]
     
     ## Order by outcome rate
     hRates <- hRates[order(hRates[, outcome]), ]
     
     ## Get names of hosptial with the lowest rate
     hNames <- hRates[hRates[, outcome] == min(hRates[,outcome]),1]
     
     ## Sort by hospital name if tie
     sort(hNames)[1]
}
best("TX", "heart attack")

#### 3. ####
rankhospital <- function(state, outcome, num = 'best') {
     
     ## Read outcome data
     
     outcomes <- read.csv("outcome-of-care-measures.csv", 
                          colClasses = "character",
                          header = TRUE)
     
     ## Get data we're interested in
     
     rates <- as.data.frame(cbind(outcomes[, 2],   # hospital
                                  outcomes[, 7],   # state
                                  outcomes[, 11],  # heart attack
                                  outcomes[, 17],  # heart failure
                                  outcomes[, 23]), # pneumonia
                            stringsAsFactors = FALSE)
     
     ## Rename columns
     
     colnames(rates) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
     
     ## Check that state and outcome are valid
     
     if(!state %in% rates[,"state"]){
          stop('invalid state')
     }
     
     if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
          stop('invalid outcome')
     }
     
     
     ## Return hospital name in that state with lowest 30-day death
     ## rate
     
     ## Get only the hospitals in chosen state
     hRates <- rates[(rates[, "state"] == state), ]
     
     ## Convert outcome rate to numberic, gets a warning
     hRates[, outcome] <- as.numeric(hRates[, outcome])
     
     ## Remove NA values
     hRates <- hRates[!is.na(hRates[, outcome]), ]
     
     ## convert num argument to valid rank
     
     if(num == "best") {
          num <- 1 
     }
     
     if (num == "worst") {
          num <- nrow(hRates) 
     }
     
     ## Order by outcome rate
     hRates <- hRates[order(hRates[, outcome], hRates[, "hospital"]), ]
     
     ## Get names of hospital 
     
     hRates[num,1]
     
}
rankhospital("TX", "heart failure", 4)

#### 4. ####
rankall <- function(outcome, num = "best") {
     ## Read outcome data
     outcome = read.csv("outcome-of-care-measures", colClasses = "character",
                        stringsAsFactors = FALSE)
     
     rates <- as.data.frame(cbind(outcome[, 2],   # hospital
                                  outcome[, 7],   # state
                                  outcome[, 11],  # heart attack
                                  outcome[, 17],  # heart failure
                                  outcome[, 23]), # pneumonia
                            stringsAsFactors = FALSE)
     
     ## Check that state and outcome are valid
     if (!state %in% rates[ , "state"]) {
          stop("invalid state")
     }
     
     if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
          stop('invalid outcome')
     }

     # Return hospital name in that state with lowest 30-day death rate
     
     hrank <- data.frame() # 이건 output이 data frame 이니까 
     
     for(state in sort(unique(rates[ , "state"]))) {  # outcome에 해당하는 모든 "state" 별 
          
          hRates <- rates[(rates[ , "state"] == state), ]
          
          hRates[ , outcome] <- as.numeric(hRates[ , outcome])
          
          hRates <- hRates[!is.na(hRates[ , outcome]), ]
          
          if (num == "best") {
               rnum <- 1
               
          } else if (num == "worse") {
               rnum <- nrow(hRates)
          }
          else {
               rnum = num
          }
          
          # Order by outcome rate & hospital name 
          hRates <- hRates[order(hRates[ , outcome], hRates[ , "hospital"]), ]
          
          hName <- hRates[rnum, 1]
          
          hRank <- rbind(hRank, data.frame(hospital = hName,
                                           state = state))
     }
     
     hRank
     
}

?rbind
str(rbind)

best("SC", "heart attack")



outcome = read.csv("outcome-of-care-measures.csv", colClasses = "character")
getwd()
head(outcome)
View(outcome)
dim(outcome)

dim(data)

best <- function(state, outcome) {
     ## Read outcome data
     outcome = read.csv("outcome-of-care-measures.csv", colClasses = "character")
     
     data <- as.data.frame(cbind(outcome[ , 2],
                                outcome[ , 7],
                                outcome[ , 11],
                                outcome[ , 17],
                                outcome[ , 23]), stringAsFactors = FALSE)
     
     colnames(data) = c("hospital", "state", "heart attack", "heart failure", "pneumonia")
     
     ## Check that state and outcome are valid
     if(!state %in% data[ , "state"]) {
          stop("invalid state")
     } 
     if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
          stop('invalid outcome')
     }
     
     ## Return hospital name in that state with lowest 30-day death rate
     hdata = data[(data[, "state"]==state), ]
     
     hdata[ , outcome] = as.numeric(hdata[ , outcome])
     
     hdata = hdata[!is.na(hdata[ , outcome]), ]
     
     hdata = hdata[order(hdata[ , outcome]), ]
     
     hname = hdata[hdata[ , outcome]== min(hdata[ , outcome]), 1]
     
     ## Get only the hospitals in chosen state
     
     sort(hname)[1]
     
}

best("MD", "pneumonia")


#3. 
rankhospital <- function(state, outcome, num = "best") {
     ## Read outcome data
     outcome = read.csv("outcome-of-care-measures.csv", colClasses = "character")
     
     data <- as.data.frame(cbind(outcome[ , 2],
                                 outcome[ , 7],
                                 outcome[ , 11],
                                 outcome[ , 17],
                                 outcome[ , 23]), stringAsFactors = FALSE)
     
     colnames(data) = c("hospital", "state", "heart attack", "heart failure", "pneumonia")
     
     ## Check that state and outcome are valid
     
     if(!state %in% data[ , "state"]) {
          stop("invalid state")
     } 
     if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
          stop('invalid outcome')
     }

     ## Return hospital name in that state with the given rank
     # get data "only from the chosen state" 
     hdata = data[(data[ , "state"] == state), ]
     
     
     # numeric으로 바꾸고, rm 제거 
     hdata[, outcome] = as.numeric(hdata[ , outcome])
     
     hdata = hdata[!is.na(hata[, outcome]), ]
     
     # num 을 검열해줄차례. 
     if(num == "best") {
          num = 1
     } else if (num == "worst") {
          num = nrow(hdata)
     } else {
          num = num 
     }
     
     # 이젠 outcome 검열해줄차례. input을 넣었을 때 어떻게 될 것이다. 라는 식. 
     # 일단 order로 검열된 후에, 숫자로 가야하는 순서 
     
     hdata = hdata[order(hdata[, outcome], hdata[, "hospital"]), ]
     
     hdata[num, 1]     

     ## 30-day death rate
}





