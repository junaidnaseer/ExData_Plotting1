get_data <- function() {
  
  if (!file.exists("./electric_power_consumption.zip")) {
    download.file(
      url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", 
      destfile = "./electric_power_consumption.zip")
  }
  if (!file.exists("household_power_consumption.txt")) {
    unzip("./electric_power_consumption.zip")
  }
  
  return(TRUE)
}

size_of_data <- function() {
  # we have 2,075,259 rows and 9 columns of data
  #
  # Using the 'file' command in linux we can see that the file is encoded in 
  # the ASCII format. In ASCII format, one character has 7 bytes.
  #
  # we can read just first two lines to count how many characters are there in 
  # one row
  hpc <- read.table("./household_power_consumption.txt", 
                    header = FALSE, 
                    sep = ";", 
                    dec = ".", 
                    nrows = 2)
  # we can now count the number of characters in a typical row like so,
  characters_in_a_row <- sum(nchar(deparse(hpc[2,])))
  # which gives us the result '745'
  # So we have approximately 750 characters per row, and we have 2 million rows
  # approx. and each character is 7 bytes large. Then total required memory 
  # should be about,
  # 2 million x 750 x 7 = 10895109750 bits
  # and 10,895,109,750 bits/8 = 1,3GB
  
  # Now we can verify the output from the following function, which gives use the 
  # size of the full object.
  format(object.size(hpc), standard = "SI", units = "auto")
}

read_data <- function() {
  hpc <- read.table("./household_power_consumption.txt", 
                    header = TRUE, 
                    sep = ";", 
                    dec = ".")
  
  dates_of_concern <- c("1/2/2007", "2/2/2007")
  subset_data_from_two_dates = hpc[hpc$Date == dates_of_concern[1] |
                                     hpc$Date == dates_of_concern[2],]
  
  return(subset_data_from_two_dates)
}

prepare_data <- function() {
  if (get_data() != TRUE) {
    print("failed to load data")
    return(FALSE)
  }
  return(read_data())
}

required_data_set <- prepare_data()
png(filename = "./plot1.png", width = 480, height = 480)
hist(as.numeric(required_data_set$Global_active_power)/500, 
     col = "red", 
     xlab = "Global Active Power (kilowatts)", 
     main = "Global Active Power")
dev.off()
