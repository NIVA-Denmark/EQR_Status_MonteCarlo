

# ----------------- CalcEQR -----------------
CalcEQR <- function(Value,Value00,Value06,Value10){
  # Value: observed value
  # value00: value corresponding to EQR=0.0
  # Value06: value corresponding to EQR=0.6 (Good/Mod boundary)
  # Value10: valeu corresponding to EQR=1.0
  
  EQR <- ifelse(Value>Value06,
                (0.6-0.6*(Value-Value06)/(Value00-Value06)),
                (1-0.4*(Value-Value10)/(Value06-Value10)))
  
  # truncate values to 0 <= EQR <= 1.0
  EQR <- max(0,min(1,EQR))
  return(EQR)
}

# ----------------- AggregateQE -----------------
AggregateQE <- function(df){
  #aggregate from indicator to quality element
  if(length(names(df)[names(df)=="SimID"])>0){
    # aggregate by QE and SimID
    df <- df %>%
      group_by(AssessmentUnit,QE,SimID)
  }else{
    # aggregate by QE
    df <- df %>%
      group_by(AssessmentUnit,QE)
  }
  df <- df %>%
    summarise(EQR=mean(EQR,na.rm=T),n=n()) %>%
    ungroup()
  return(df)
}

# ----------------- AggregateOverall -----------------
AggregateOverall <- function(df){
  #aggregate from QE to overall status
  # using one-out all-out (worst QE determines overall status)
  
  if(length(names(df)[names(df)=="SimID"])>0){
    # aggregate by AssessmentUnit and SimID
    # get minimum EQR value (worst QE)
    dfmin <- df %>%
      group_by(AssessmentUnit,SimID) %>%
      summarise(EQR=min(EQR,na.rm=T)) %>%
      ungroup()
    
    # join back to the original dataframe to get the QE giving the worst status
    dfmin <- dfmin %>%
      left_join(df,by=c("SimID","AssessmentUnit","EQR"))
    
    # if there are two "equal worst" QEs having the same lowest EQR score,
    # we will now have two rows. So, group these together
    
    dfmin <- dfmin %>%
      group_by(AssessmentUnit,EQR) %>%  
      summarise(WorstQE=paste0(QE,collapse=",")) %>%
      ungroup()
    
  }else{
    # aggregate by AssessmentUnit
    # get minimum EQR value (worst QE)
    dfmin <- df %>%
      group_by(AssessmentUnit) %>%  
      summarise(EQR=min(EQR,na.rm=T)) %>%
      ungroup()
    
    # join back to the original dataframe to get the QE giving the worst status
    dfmin <- dfmin %>%
      left_join(df,by=c("AssessmentUnit","EQR"))
    
    # if there are two "equal worst" QEs having the same lowest EQR score,
    # we will now have two rows. So, group these together
    
    dfmin <- dfmin %>%
      group_by(AssessmentUnit,EQR) %>%  
      summarise(WorstQE=paste0(QE,collapse=",")) %>%
      ungroup()
  }
  
  return(dfmin)
}

# ----------------- StatusClass -----------------
StatusClass<-function(EQR){
  if(EQR<0.2){
    Class<-"Bad"
  }else if(EQR<0.4){
    Class<-"Poor"
  }else if(EQR<0.6){
    Class<-"Mod"
  }else if(EQR<0.8){
    Class<-"Good"
  }else{
    Class<-"High"
  }
}