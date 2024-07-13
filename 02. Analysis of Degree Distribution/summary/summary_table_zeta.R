write_summary <- function(language, file) {
   degree_sequence = read_sequence_table(file)
   
   # Extract the number after the underscore
   gamma <- as.numeric(sub(".*_(\\d+)", "\\1", language))
   
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
            C, gamma
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
source = read.table("summary/list_zeta.txt", 
         header = TRUE, # this is to indicate the first line of the file 
         # contains the names of the columns instead of the real data
         as.is = c("language","file") # this is need to have the cells treated 
         # as real strings and not as categorical data.
        )
art_zeta <- data.frame()
for (x in 1:nrow(source)) {
  art_zeta <- rbind(art_zeta, write_summary(source$language[x], source$file[x]))
}
colnames(art_zeta) <- c("Language", "File", "N", "MaximumDegree", "M/N", "N/M", 
                  "SumDegrees", "SumDegreesLogarithm", "C", "gamma")
rm(x, source)
return(art_zeta)
