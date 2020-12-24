#########################
# Author: Thomas Gredig
# Date: September 17, 2020
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
  gsub('^\\s+','',m1) -> m1
  tolower(gsub('\\s+$','',m1))
}


# Load OrgIDs and Students enrolled in Course
# ============================================
f1=dir(path.data, pattern='GradesExport')
fname=file.path(path.data, f1)
d.ID = data.frame()
for(f in fname) {
  di = read.csv(f, stringsAsFactors = FALSE)
  d.ID = rbind(d.ID, data.frame(OrgDefinedId=di$OrgDefinedId, Last.Name=di$Last.Name, First.Name=di$First.Name))
}
d.ID$fullnames = tolower(paste(d.ID$First.Name, d.ID$Last.Name))
d.ID$unique = paste0(substr(d.ID$fullnames,1,3),substr(d.ID$fullnames,nchar(d.ID$fullnames)-3,nchar(d.ID$fullnames)))
d.ID
head(d.ID)
tail(d.ID)
# load Zoom files
# ===============
file.list = dir(path.data, pattern=ZOOM.PATTERN)
if (length(file.list)<1) {
  warning(paste('No files found in folder:',path.data,'. Modify config.R.'))
}

print(paste("Found ", length(file.list)," Zoom files."))
#fname = file.path(path.data, file.list[1])
result = data.frame()
result.stats = data.frame()
filename = file.list[1]
for(filename in file.list) {
  fname = file.path(path.data, filename)
  print(fname)

  d = read.csv(fname, skip=0, header=TRUE, stringsAsFactors = FALSE)
  totalParticipants = d$Participants[1]
  if (names(d)[1]=='Meeting.ID') {
    meetingDate = substr(d$Start.Time[1],1,10)
    d = read.csv(fname, skip=2, header=TRUE, stringsAsFactors = FALSE)
  } else { warning("cannot load file.") }
  print(paste("Meeting: ",meetingDate))

  d$n1 = cleanNames(d$Name..Original.Name.)
  d$n2 = cleanNames(d$User.Email)

  d.ID$time = 0
  d.ID$cnt = 0
  gsub('\\s{2}',' ',d$n1) -> d$n1
  gsub('\\s{2}',' ',d$n2) -> d$n2
  for(i in 1:nrow(d)) {
    nm = d$n1[i]
    q = grep(nm, d.ID$fullnames)
    if (length(q)==1) {
      #print(paste("Found: ", nm, " -> ", d.ID$OrgDefinedId[q]))
      d.ID$time[q]=d.ID$time[q] + d$Total.Duration..Minutes.[i]
      d.ID$cnt[q]= d.ID$cnt[q]+1
    } else {
      nm = d$n2[i]
      q = grep(nm, d.ID$fullnames)
      if (length(q)==1) {
        #print(paste("Found: ", nm, " -> ", d.ID$OrgDefinedId[q]))
        d.ID$time[q]=d$Total.Duration..Minutes.[i]
        d.ID$cnt[q]= d.ID$cnt[q]+1
      } else {
        q = grep(paste0(substr(nm,1,3), substr(nm, nchar(nm)-3, nchar(nm))), d.ID$unique)
        if (length(q)==1) {
          #print(paste("Found: ", nm, " -> ", d.ID$OrgDefinedId[q]))
          d.ID$time[q]=d$Total.Duration..Minutes.[i]
          d.ID$cnt[q]= d.ID$cnt[q]+1
        } else {
          nm = d$n1[i]
          q = grep(paste0(substr(nm,1,3), substr(nm, nchar(nm)-3, nchar(nm))), d.ID$unique)
          if (length(q)==1) {
            #print(paste("Found: ", nm, " -> ", d.ID$OrgDefinedId[q]))
            d.ID$time[q]=d$Total.Duration..Minutes.[i]
            d.ID$cnt[q]= d.ID$cnt[q]+1
          } else {
            print(paste("Not found:",i, " ", d$n1[i], " ", d$n2[i]))
          }
        }
      }
    }
  }
  d.ID$date = meetingDate
  result = rbind(result, d.ID)
  rs = data.frame(meetingDate, totalParticipants)
  result.stats = rbind(result.stats, rs)
}

head(result)
result$fullnames<-NULL
result$unique <-NULL

# create output file
library(reshape2)
r = result[,c(1,6,4)]
head(r)
m = dcast(r, OrgDefinedId ~ date )
head(m)

zoomTime.min = apply(m[,-1], 1, sum)
zoomMissed.days = apply(m[,-1], 1, function(x)sum(x==0))

# Attendance in Percentage
apply(m[,-1], 2, function(x)sum(x>0))/(nrow(d.ID)/100)

m$zoomTime.min = zoomTime.min
m$zoomMissed.days= zoomMissed.days

# add name
d1 = data.frame(OrgDefinedId= d.ID$OrgDefinedId, Last.Name = d.ID$Last.Name, First.Name = d.ID$First.Name)
m = merge(m,d1)

plot(m$zoomTime.min)
head(m)
m$end = "#"
write.csv(file = file.path(path.data,'output.csv'), m, row.names = FALSE)

result.stats
