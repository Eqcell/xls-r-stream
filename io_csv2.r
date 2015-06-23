##
## Procedure to import data and formulas and extend forecast based on existing data. 
## 
## Call:
## io_csv.r SOURCE_FILENAME RESULT_FILENAME
## 
# ET: (Maybe better) Call: Rscript io_csv.r SOURCE_FILENAME RESULT_FILENAME 
#
## EP: must exit with error notice to log if something is missing 
##
## Input:
##     SOURCE_FILENAME - input data file (like data_input.txt) 
##
## Output:
##     RESULT_FILENAME = "data_output.txt"
##
##

args<-commandArgs(trailingOnly = T)

#SOURCE_FILENAME = args[1]
#SOURCE_FILENAME = "C:\\Users\\Евгений\\Desktop\\data_input.txt"

#RESULT_FILENAME = args[2]
#RESULT_FILENAME = "C:\\Users\\Евгений\\Desktop\\data_output.txt"
#todo(1): need some easier way to change between hardcoded and supplied arguements.
#      or an easy way to run the program with arguments in IDE 

# ET: todo(1), one of possible solutions:
# EP: +
inIDE <- TRUE  # FALSE / TRUE
if (inIDE) {   # to work in IDE
   SOURCE_FILENAME <- "data_input.txt"
   RESULT_FILENAME <- "data_output1.txt"
} else {       # to work in command line
   if (length(args) == 2) {
      SOURCE_FILENAME <- args[1]
      RESULT_FILENAME <- args[2]
      if (! file.exists(SOURCE_FILENAME)) {   # if not exist, stop
         print('SOURCE_FILENAME does not exist')
         print(xx)  # use unknown variable to stop the code
      }
   } else {    # if improper format, stop
      print('Call format:  Rscript io_csv.r SOURCE_FILENAME RESULT_FILENAME')
      print(xx)    
   }
}


# used for basic testing of interface: 
# file.copy(SOURCE_FILENAME, RESULT_FILENAME, overwrite = FALSE)

# read input data
raw_data <- read.csv(SOURCE_FILENAME, dec = ",", sep = "\t", header=FALSE, 
         strip.white = TRUE, check.names=FALSE , stringsAsFactors = FALSE) 

# max columns
N = ncol(raw_data)

# contains varnames, formulas, blank lines and comments
pivot = raw_data[,1]

# ET: Done. todo(2): add second condition: formula must contain [t]
# EP: +
indexFormulas = grepl("=", pivot) & grepl("[t]", pivot) 

formulas = pivot[indexFormulas]
varnames = make.names(pivot[!indexFormulas], unique = TRUE) 

# supress any data in folmulas columns
raw_data[indexFormulas,2:N] = NA 

# apply filter (some values in data area will not convert to numeric on import)
# todo(3): apply without loop.Alslo related to Problem 1 (below). 
# size = dim(raw_data)
# for (i in (1:size[1])){
#  for (j in (2:size[2]))
#  raw_data[i,j] = as.numeric(gsub(",", ".", raw_data[i,j]))
#  }


# ET: todo(3) (ET2: commented, see another solution below)
#subs <- function(x) {as.numeric(gsub(",", ".", x, fixed= TRUE))}
#raw_data[, 2:N] <- as.data.frame(lapply(raw_data[, 2:N], 
#   FUN = function(x) {sapply(x, FUN= subs)}) )

# ET: Actually this filter is not necessary. R (ver.3.1.1) makes correct conversion.
# This can be checked by checking data structure 'str(raw_data)'.
# This function indicates that all columns from 2 to 13 are numerical.
# EP: still quite a long formulation, and lapply over sapply is not a bit of a mess
#     
#     I know it works, but can there be a shorter formulation? 
#
#     Can we get more near to  
#     subs <- function(x) {as.numeric(gsub(",", ".", x, fixed= TRUE))}
#     raw_data[, 2:N] = subs(raw_data[, 2:N])
#         

# ET2: Agree with you, that the solution proposed is not transparent.
# Below is another solution. It is much more clear (I hope).
# Unfortunately, it can not be extended to apply to several columns,
# so for loop is necessary.

# ET2: Also agree with you that this filter might be necessary,
# e.g. when there is a string in some of columns [2:N]. In this
# case the column is read as a factor (character), so one need
# to change ',' to '.' . 

# ET2: todo(3)
subs <- function(x) {as.numeric(gsub(",", ".", x, fixed= TRUE))}
for (j in 2:N) {
   raw_data[, j] <- subs(raw_data[, j]) 
}

# ET2: To Problem 1. 
raw_data[, 2:N] <- lapply(raw_data[, 2:N], as.numeric)

# ET2: A note: These three functions ((1)'supress any data in 
# folmulas columns', (2) filter ',', (3) lapply(....as.numeric),
# Problem 1) are related to each other. The order of these functions
# should be (1), (2) and (3). Though step (3) can be unnecessary after steps (1) and (2).


# unpack variables
for (vn in varnames){
  i = which(pivot == vn)
  values_vector = as.numeric(as.vector(as.matrix(raw_data[i,-1]))) # need this long conversion to match type
  cat(values_vector)
  cat("\n")
  assign(vn, values_vector) 
  }

# calculations in loop
for (t in 1:length(is_forecast)){

   if (is_forecast[t] == 1) lapply(as.vector(formulas), function(x) eval(parse(text=x), parent.frame(3))) 

}
# todo(4): not sure about lapply(as.vector(formulas) - it just works. May suggest a different formulation. 
# ET: This part looks okay to me.
# EP: +

   ## Use of parent.frame(3)):
   ## It is about environment in which eval() works. If to set it to something other than 3, 
   ## then it will create its own variables, and won't use variables in current workspace.
   ## helpful example: http://courses.cs.washington.edu/courses/cse341/05au/lectures/scheme-eval-apply.html 
   

# pack variables back 
data_out = raw_data


for (vn in varnames){
  i = which(pivot == vn)
  replace_vector = get(vn)
  if (length(replace_vector) == N-1) data_out[i,2:N] = replace_vector
}

# write to file
write.table(data_out, file=RESULT_FILENAME, dec=",", sep="\t", na = "",  quote=FALSE,
            col.names = FALSE, row.names=FALSE, append = FALSE)


# ET: I presume that in an actual input file, data in columns, where 
# is_forecast = 1, will be undefined (absent). So, you calculate them,
# using the formulas, and make 'data_out'. 
# For the current input file (data_input.txt), data in input and output
# files are the same. In other words, data frames 'data_out' and 'raw_data'
# are the same (up to some numerical error). This can be checked, using
# the following commands:
# for (i in 2:13) {
#   s1 <- sum(abs(data_out[, i] - raw_data[, i]) > 1.e-10, na.rm = TRUE)
#   print(s1)
# }


# Problem 1. Fails to read and write properly a file with additional text in column 2
# Try data_input_with_additional_text.txt

# ET: In the file, words 'Additional text' are separated by Tab.
# Since Tab is a separation parameter ('\t') in read.csv, R reads 
# 'Additional text' in the 2nd column. Change Tab to Space(s) to fix the problem.

# EP: text file data_input_with_additional_text.txt can be generated by Excel,
# so moving a tab is not a solution. The question is that io_csv.r behaviour must not be 
# affected by a string in second column. It seems that the issue does not appear now,
# but maybe it depends on R version. In some runs I had a result that all of column in 'raw_data' was converted 
# to string if there was a single string in column in data_input.txt. Cannot reproduce the error now, but cautious.  

# ET2: See solution above (after the filter) to Problem 1.


# extra to-do (5), time permitting:
# I am writing a kind of log by doing a  cat(values_vector) abve
# I want to output a log to file io_csv.LOG by writing a string "<timestamp> + <did something> + \n" to it at some points in code
# Can you implement an example of that please?

# ET2: todo(5). This is a possible solution:
logstring <- function(out_string, logfile= '') {
# Appends 'out_string' as a string to file with name 'logfile'.
# Format: Date Time : <out_string>.  
   out_str <-  as.character(out_string)
   cat(as.character(Sys.time()), ': ', out_str, '\n', 
      file = logfile, sep = " ", append = TRUE)
}

# ET2: Usage of logstring().
logfile <- 'io_csv.LOG'
logstring(2*loan, logfile)
