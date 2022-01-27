# A = 5
# 
# library(acsii)
# library(asciiSetupReader)
# 
# asciiSetupReader::read_ascii_setup("2020_NIBRS_NATIONAL_MASTER_FILE_ENC.txt")
# 
# read.table("2020_NIBRS_NATIONAL_MASTER_FILE_ENC.txt",
#            keep_columns = c("ORI",
#                             "BH007", "BH019", "BH023", "BH027",
#                             "BH031", "BH035", "V4007", "V4008",
#                             "V4009", "V4010", "V4011", "V4012",
#                             "V4013", "V4014", "V4015", "V4016",
#                             "V4018", "V4019"))
# data.table::fread("2020_NIBRS_NATIONAL_MASTER_FILE_ENC.txt")
# 
# library(readr)
# data = read_fwf("2020_NIBRS_NATIONAL_MASTER_FILE_ENC.txt")
