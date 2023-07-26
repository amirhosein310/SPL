################################################################################ 
#################### Statistical Programming Languages 2023 ####################
###################             Take-home Exam              ####################
################################################################################

#-------------------------------------------------------------------------------
# Surname: Bartari
# Name: Amirhossein
# Student ID (Matrikelnummer): 629940
#-------------------------------------------------------------------------------

### Exercise 4 -----------------------------------------------------------------



library(stringr)
######################################
octal_to_decimal <- function(input) {
  # Check if the input has a fractional part enclosed in parentheses
  if (str_detect(input, "\\([^\\)]+\\)")) {
    # Extract the fractional part inside parentheses
    fractional_part <- str_extract(input, "\\([^\\)]+\\)")
    fractional_part <- str_replace_all(fractional_part, "[\\(\\)]", "")  # Remove parentheses
    fractional_part <- as.numeric(fractional_part)  # Convert fractional part to numeric
    # Remove the fractional part from the input
    input <- str_replace(input, "\\([^\\)]+\\)", "")
  } else {
    fractional_part <- NULL  # No fractional part found
  }
  
  # Convert the remaining input to a numeric variable
  integerPart <- as.numeric(input)
  
  # Check if the number is negative
  isNegative <- FALSE
  if (!is.na(integerPart) && !is.nan(integerPart) && !is.na(input)) {
    isNegative <- integerPart < 0
  }
  
  # Convert the integer part from base 8 to base 10
  integerPartBase10 <- if (!is.na(integerPart) && !is.nan(integerPart) && !is.na(input)) baseToInt(as.character(abs(integerPart)), base = 8) else NA
  
  # Convert the fractional part from base 8 to base 10
  fractionalPartBase10 <- 0
  if (!is.null(fractional_part)) {
    fractionalDigits <- nchar(as.character(fractional_part))
    
    for (i in 1:fractionalDigits) {
      digit <- substr(as.character(fractional_part), i, i)
      fractionalPartBase10 <- fractionalPartBase10 + as.numeric(digit) * 8^(-i)
    }
  }
  
  # Calculate the final number in base 10
  numberBase10 <- if (!is.na(integerPartBase10) && !is.nan(integerPartBase10) && !is.na(input)) integerPartBase10 + fractionalPartBase10 else NA
  
  # Apply negative sign if necessary
  if (isNegative && !is.na(numberBase10) && !is.nan(numberBase10)) {
    numberBase10 <- -numberBase10
  }
  
  return(numberBase10)
}



octal_to_decimal("-451(735)")



## Second Part (decimal to Octal function)

decimal_to_octal <- function(decimal_num) {
  is_negative <- FALSE
  
  if (decimal_num < 0) {
    is_negative <- TRUE
    decimal_num <- abs(decimal_num)
  }
  
  integer_part <- floor(decimal_num)
  fractional_part <- decimal_num - integer_part
  
  integer_base8 <- integerToBase8(integer_part)
  fractional_base8 <- fractionalToBase8(fractional_part)
  
  if (fractional_base8 != "") {
    base8_num <- paste0(integer_base8, "(", fractional_base8, ")")
  } else {
    base8_num <- as.character(integer_base8)
  }
  
  if (is_negative) {
    base8_num <- paste0("-", base8_num)
  }
  
  return(base8_num)
}

integerToBase8 <- function(decimal_num) {
  quotient <- decimal_num
  base8_num <- ""
  
  if (decimal_num == 0) {
    base8_num <- "0"
  } else {
    while (quotient > 0) {
      remainder <- quotient %% 8
      base8_num <- paste0(remainder, base8_num)
      quotient <- quotient %/% 8
    }
  }
  
  return(base8_num)
}

fractionalToBase8 <- function(decimal_num) {
  precision <- 8  # Adjust the precision as needed
  base8_num <- ""
  
  for (i in 1:precision) {
    decimal_num <- decimal_num * 8
    integer_part <- floor(decimal_num)
    base8_num <- paste0(base8_num, integer_part)
    decimal_num <- decimal_num - integer_part
  }
  
  return(base8_num)
}

decimal_to_octal(octal_to_decimal("4(0010)"))







# last part - calculator
calculate_base8_math <- function(input_expression) {
  # Splitting the input expression into numbers and operations
  numbers <- strsplit(input_expression, "[\\+\\-\\*/]")[[1]]
  operations <- unlist(str_extract_all(input_expression, "[\\+\\-\\*/]"))
  
  # Replace minus sign with "subtract" in the operation list (does not work properly still)
  operations[operations == "-"] <- "subtract"
  
  # Converting numbers from base 8 to decimal
  decimal_numbers <- sapply(numbers, octal_to_decimal)
  
  # Creating a formula expression string
  formula_expression <- ""
  for (i in seq_along(decimal_numbers)) {
    if (i > 1) {
      # Check if the operation is subtraction
      if (operations[i - 1] == "subtract") {
        formula_expression <- paste0(formula_expression, " - ", decimal_numbers[i])
      } else {
        formula_expression <- paste0(formula_expression, " ", operations[i - 1], " ", decimal_numbers[i])
      }
    } else {
      formula_expression <- paste0(formula_expression, decimal_numbers[i])
    }
  }
  
  # Replace the word "subtract" with "-" in the formula_expression
  formula_expression <- gsub("subtract", "-", formula_expression)
  
  # Evaluating the formula expression
  result <- eval(parse(text = formula_expression))
  
  # Converting the result back to base 8
  base8_result <- decimal_to_octal(result)
  
  # Printing the result in base 8
  cat("Result in base 8:", base8_result, "\n")
}

calculate_base8_math("42(4) + 35(4)")
