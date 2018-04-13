
# This script creates the sqlite database file

library(DBI)
library(readr)
library(dplyr)
library(odbc)

mssql_server_name <- "localhost\\SQLEXPRESS" # Replace with actual DB location

con <- dbConnect(odbc(), 
                 Driver = "SQL Server", 
                 Server = mssql_server_name, 
                 Database = "master",              
                 Trusted_Connection = "True")



dbSendQuery(con, "CREATE DATABASE immunogenicity")
dbDisconnect(con)

con <- dbConnect(odbc(), 
                 Driver = "SQL Server", 
                 Server = mssql_server_name, 
                 Database = "immunogenicity", 
                 Trusted_Connection = "True")

dbSendQuery(con, "CREATE SCHEMA study_01;")

confirmatory <- read_csv("Data/Sample_Immunogenicity_Data/Sample_ADA_Data_05062017_Confirmatory.csv")
dbWriteTable(con, SQL("study_01.confirmatory"), confirmatory)

titer <- read_csv("Data/Sample_Immunogenicity_Data/Sample_ADA_Data_05062017_Titer.csv")
dbWriteTable(con, SQL("study_01.titer"), titer)

screening <- read_csv("Data/Sample_Immunogenicity_Data/Sample_ADA_Data_05062017_Screening.csv")
dbWriteTable(con, SQL("study_01.screening"), screening)

dbDisconnect(con)

rm(confirmatory, titer, screening, con, db_location, mssql_server_name)

# Run if the database needs to be deleted
#dbSendQuery(con, "DROP DATABASE immunogenicity")
