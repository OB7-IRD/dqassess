## ----loading_library, eval = FALSE, include = TRUE-----------------------
#  # Devtools is a necessary package
#  # If is not installed, run the following line
#  install.packages("devtools")
#  # Load the package from the Git
#  devtools::install_github("https://github.com/OB7-IRD/dqassess.git",  build_opts = c("--no-resave-data", "--no-manual"))
#  # Load the library
#  library(dqassess)
#  # You can access to the package documentation with the following line
#  ?dqassess
#  # If you want the documentation of a specific package function use the same syntax, for example for the function build_template_format_db
#  ?build_template_format_db

## ----checking_data, eval = FALSE, include = TRUE-------------------------
#  result_checking <- checking_data(obj,
#                                   format_db,
#                                   ignore_case_in_codelist,
#                                   report,
#                                   report_dir,
#                                   text_file_sep,
#                                   text_file_dec,
#                                   file_name_slot)

## ----checking_data_example1, eval = FALSE, include = TRUE----------------
#  result_checking1 <- checking_data(obj = "path_test_fictive_data1.xlsx",
#                                    format_db = "path_recolape_definition_data_format.xlsx",
#                                    report_dir = "path_output_directory")

## ----checking_data_example1_results, echo=FALSE--------------------------
cat(paste("(INFO) Empty enumeration table (code list) \"codelist_vessel\" in the format definition file."), 
    "(INFO) Empty enumeration table (code list) \"codelist_vessel\" in the format definition file.",
    "(INFO) Empty enumeration table (code list) \"codelist_vessel\" in the format definition file.",
    "Correct import of the format file definition",
    "Correct import of data",
    "Slot effort found",
    "Checking in progress, be patient or take a coffee",
    "Slot landing found",
    "Checking in progress, be patient or take a coffee",
    "Slot sampling not found",
    sep = "\n")

## ----checking_data_example2, eval = FALSE, include = TRUE----------------
#  result_checking2 <- checking_data(obj = "path_test_fictive_data2.csv",
#                                    format_db = "path_recolape_definition_data_format.xlsx",
#                                    report_dir = "path_output_directory",,
#                                    file_name_slot = "sampling")

## ----checking_data_example2_results, echo=FALSE--------------------------
cat(paste("(INFO) Empty enumeration table (code list) \"codelist_vessel\" in the format definition file."), 
    "(INFO) Empty enumeration table (code list) \"codelist_vessel\" in the format definition file.",
    "(INFO) Empty enumeration table (code list) \"codelist_vessel\" in the format definition file.",
    "Correct import of the format file definition",
    "Correct import of data",
    "Slot effort not found",
    "Process for the next slot available",
    "Slot landing not found",
    "Process for the next slot available",
    "Slot sampling found",
    "Checking in progress, be patient or take a coffee",
    sep = "\n")

