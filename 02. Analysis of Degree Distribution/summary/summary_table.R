write_summary <- function(language, file) {
   degree_sequence = read_sequence_table(file)
   
   # Initialize a variable to store the the sum of
   # logarithm of degree factorials
   C <- 0
   
   # Loop through the elements of x
   for (i in 1:length(degree_sequence$V1)) {
     ki <- degree_sequence$V1[i]  # Get the value of k_i for the current element
     
     # Initialize a variable to store the inner sum
     inner_sum <- 0
     
     # Loop from j=2 to k_i
     for (j in 2:ki) {
       inner_sum <- inner_sum + log(j)
     }
     
     # Add the inner sum to the result
     C <- C + inner_sum
   }
   
   lin <- c(language,
            file,
            length(degree_sequence$V1),
            max(degree_sequence$V1),
            sum(degree_sequence$V1)/length(degree_sequence$V1),
            length(degree_sequence$V1)/sum(degree_sequence$V1), 
            sum(degree_sequence$V1),
            sum(log(degree_sequence$V1)),
            C
   )
   # cat(language,length(degree_sequence$V1),max(degree_sequence$V1),
   # sum(degree_sequence$V1)/length(degree_sequence$V1),
   # length(degree_sequence$V1)/sum(degree_sequence$V1),"\n")
   return(lin)
}

read_sequence_table <- function (file) {
  degree_sequence <- read.table(file, header = FALSE)
}

# Generate Table 1  
source = read.table("summary/list_in.txt", 
         header = TRUE, # this is to indicate the first line of the file 
         # contains the names of the columns instead of the real data
         as.is = c("language","file") # this is need to have the cells treated 
         # as real strings and not as categorical data.
        )
df <- data.frame()
for (x in 1:nrow(source)) {
    df <- rbind(df, write_summary(source$language[x], source$file[x]))
}
colnames(df) <- c("Language", "File", "N", "MaximumDegree", "M/N", "N/M", 
                  "SumDegrees", "SumDegreesLogarithm", "C")
rm(x, source)
return(df)
