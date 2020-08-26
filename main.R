#########################
# Author: Thomas Gredig
# Date: August 15, 2020
#
# Converts Zoom Reports
# for attendance into
# files for import in
# Desire2Learn
#########################

library(dplyr)
library(tools)
source('config.R')

# function to clean names
# =======================
cleanNames <- function(nameList) {
  gsub('\\((.*)','',nameList) -> m1
  gsub('mobile','',m1) -> m1
  gsub('@(.*)','',m1) -> m1
  gsub('\\.',' ',m1) -> m1
  gsub('\\d+','',m1) -> m1
  tolower(gsub('\\s+$','',m1))
}

# load Zoom files
# ===============
file.list = dir(path.data, pattern=MY.PATTERN)
if (length(file.list)<1) {
  warning(paste('No files found in folder:',path.data,'. Modify config.R.'))
}


# compute time for each user
# ==========================
fname = file.path(path.data, file.list[1])
for(filename in file.list) {
  fname = file.path(path.data, filename)
  print(fname)
  d = read.csv(fname, skip=0, header=TRUE, stringsAsFactors = FALSE)
  if (names(d)[1]=='Meeting.ID') {
    d = read.csv(fname, skip=2, header=TRUE, stringsAsFactors = FALSE)
  }
  #d$Name..Original.Name.
  d$Name = factor(cleanNames(d$Name..Original.Name.))

  if (length(grep('Total.Duration..Minutes.',names(d)))<1) {
    d$Join.Time = as.POSIXct(d$Join.Time)
    d$Leave.Time = as.POSIXct(d$Leave.Time) + 2  # time to change to break-out rooms

    str(levels(d$Join.Time)[d$Join.Time[1]])
    # remove overlapping times (if logged in on two devices, for example)
    d %>%
      group_by(Name) %>%
      mutate(indx = c(0, cumsum(as.numeric(lead(Join.Time)) >
                                  cummax(as.numeric(Leave.Time)))[-n()])) %>%
      group_by(Name, indx) %>%
      summarise(start = first(Join.Time), end = last(Leave.Time)) -> d1
    # summarize the times and create the total times
    d1 %>%
      group_by(Name) %>%
      mutate(Total.Time.Minutes = end-start) %>%
      summarise(Total.Duration.Seconds = sum(Total.Time.Minutes)) -> d2
  } else {
    d %>%
      group_by(Name..Original.Name.) %>%
      mutate(Name = Name..Original.Name., Total.Duration.Seconds = Total.Duration..Minutes.*60)  -> d2
  }

  # data.frame: d2
  # Name, Total.Duration.Seconds
  # ============================

  # Load OrgIDs
  # ===========
  f1=dir(path.data, pattern='GradesExport')
  fname=file.path(path.data, f1)
  d.ID = data.frame()
  for(f in fname) {
    di = read.csv(f, stringsAsFactors = FALSE)
    d.ID = rbind(d.ID, data.frame(OrgDefinedId=di$OrgDefinedId, Last.Name=di$Last.Name, First.Name=di$First.Name))
  }
  d.ID$fullnames = tolower(paste(d.ID$First.Name, d.ID$Last.Name))

  # Brute Force go Through Classroster
  # ==================================
  d.ID$minutes = NA
  d$Found = FALSE
  for(j in 1:nrow(d.ID)) {
    d.ID$fullnames[j]
    which(agrepl(d.ID$fullnames[j], levels(d$Name))==TRUE) -> q1
    if (length(q1)==0) { q2 = NA } else {
      q2 = q1[1]
      d$Found[q2] = TRUE
      if (is.na(d.ID$minutes[q2])) {
        d.ID$minutes[q2] = d$Total.Duration..Minutes.[q2]
      } else { d.ID$minutes[q2] = d.ID$minutes[q2] +
        d$Total.Duration..Minutes.[q2]}
    }
  }
  fname.noext = gsub(paste0('.',file_ext(filename)),'',filename)
  write.csv(d.ID, file=file.path(path.data,paste0('roster-',fname.noext, '.csv')))
  write.csv(d[which(d$Found==FALSE),], file=file.path(path.data,paste0('missing-',fname.noext, '.csv')))
}




