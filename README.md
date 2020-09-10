# Zoom-ClassAttendance
 class attendance in zoom for import in Desire2Learn

## Setup

- **Step 1:** configure the `myConfig.R` file with the data path
- **Step 2:** download the `Reports` from Zoom and move to the data path
- **Step 3:** download an empty gradebook from Desire2Learn and save into the data path
- **Step 4:** run `main.R`


## Process

The program will load all Zoom files, then clean the usernames (remove *mobile*, portions in brackets, and convert email addresses). Then, a table is created with the overlapped times (since some students may log in with multiple devices). The total class time is then computed. The Desire2Learn spreadsheet has a mapping of ID and name, the names are tried to match and a `warning` file is exported with names that were uncertain.

This time is compared with the time from the configuration and the percentage is saved in an output file.
