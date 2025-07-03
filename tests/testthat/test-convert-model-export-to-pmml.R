expect_xml_equal <- function(test_dir, db_name, custom_function_files = NULL) {
    local_mocked_bindings(
        packageVersion = function(package_name) {
            return('0.1.0')
        },
        .package = 'utils'
    )

  actual_pmml <- convert_model_export_to_pmml(
    test_dir,
    paste(test_dir, "model-export.csv", sep = ""),
    database_name = db_name,
    custom_function_files = custom_function_files
  )
  
  expected_pmml_file_path <-
    paste(test_dir, "expected-pmml.xml", sep = "")
  expected_pmml <-
    XML::xmlTreeParse(expected_pmml_file_path)
  actual_pmml <- XML::asXMLNode(actual_pmml)
  
  actual_pmml_string <- XML::toString.XMLNode(actual_pmml)

  

  suppressWarnings(expected_pmml_string <-
                     XML::toString.XMLNode(expected_pmml[[1]]$children$PMML))

    expect_equal(actual_pmml_string, expected_pmml_string)
}

test_that("Correctly converts the variables and variable details files into PMML",
{
    
  test_dir <- "../../assets/specs/model-csv-to-pmml/test-files/1/"
  custom_function_files <- c(
    file.path(normalizePath(test_dir), "custom-function-file.R")
  )
  
  expect_xml_equal(test_dir, "cchs2001_p", custom_function_files)
  
})

test_that("Correctly converts a dummy step file into PMML", {
    

  test_dir <- "../../assets/specs/model-csv-to-pmml/test-files/2/"
  
  expect_xml_equal(test_dir, "cchs2001_p")
})

test_that("Correctly converts a centering step file into PMML", {
    

  test_dir <- "../../assets/specs/model-csv-to-pmml/test-files/3/"
  
  expect_xml_equal(test_dir, "cchs2001_p")
})

test_that("Correctly converts a RCS step file into PMML", {
    

  test_dir <- "../../assets/specs/model-csv-to-pmml/test-files/4/"
  
  expect_xml_equal(test_dir, "cchs2001_p")
})

test_that("Correctly converts an interactions step file into PMML", {
    

  test_dir <- "../../assets/specs/model-csv-to-pmml/test-files/5/"
  
  expect_xml_equal(test_dir, "cchs2001_p")
})

test_that("Correctly converts a fine and gray step file into PMML", {
    

  test_dir <- "../../assets/specs/model-csv-to-pmml/test-files/6/"
  custom_function_files <- c(
    file.path(normalizePath(test_dir), "custom-functions.R")
  )
  
  expect_xml_equal(test_dir, "cchs2001_p", custom_function_files)
})

test_that("Converting simple models", {
    

  test_dir <- "../../assets/specs/model-csv-to-pmml/test-files/8/"
  custom_function_files <- c()
  
  expect_xml_equal(test_dir, "database_one", custom_function_files)
})

test_that("It correectly converts tables", {
    

  test_dir <- "../../assets/specs/model-csv-to-pmml/test-files/9/"
  custom_function_files <- c()
  
  expect_xml_equal(test_dir, "database_one", custom_function_files)
})

describe("Interpolating values in the centerValue column", {

  it("7.1: It correctly interpolates values for the centerValue column in the centering step file", {
    test_dir <- "../../assets/specs/model-csv-to-pmml/test-files/7/7.1/"
    custom_function_files <- c()
    
    expect_xml_equal(test_dir, "database_one", custom_function_files)  
  })
  
  it("7.2: It stops with an error if the interpolated value is not a nunber", {
    test_dir <- "../../assets/specs/model-csv-to-pmml/test-files/7/7.2/"
    custom_function_files <- c()
    
    expect_error(
      convert_model_export_to_pmml(
        test_dir,
        paste(test_dir, "model-export.csv", sep = ""),
        database_name = "database_one",
        custom_function_files = c()
      ),
      "Error interpolating reference[reference$columnOne == 1, ]$value. Value a should be a number but is character and could not be coerced to a number",
      fixed = TRUE
    )
  })
  
  it("7.3: It stops with an error if no row was found in the reference file for the expression", {
    test_dir <- "../../assets/specs/model-csv-to-pmml/test-files/7/7.3/"
    custom_function_files <- c()
    
    expect_error(
      convert_model_export_to_pmml(
        test_dir,
        paste(test_dir, "model-export.csv", sep = ""),
        database_name = "database_one",
        custom_function_files = c()
      ),
      "Error interpolating reference[reference$columnOne == 1, ]$value. No row found in reference file for expression.",
      fixed = TRUE
    )
  })
})

test_that("When parsing a cox or fine and grey model, it should stop if the
          time variable is missing from the variable details sheet", {
  model_export_file <- data.frame(
    fileType = c('variables', 'variable-details', 'model-steps'),
    filePath = c('variables.csv', 'variable-details.csv', 'model-steps.csv')
  )
  variables_file <- data.frame(variable = c('sex'), databaseStart = c('cchs'))
  variable_details_file <- data.frame(variable = c(''))
  model_steps_file <- data.frame(
    step = c('fine-and-gray', 'fine-and-gray'),
    fileType = c('beta-coefficients', 'baseline-hazards'),
    filePath = c('./beta-coefficients.csv', 'baseline-hazards.csv')
  )
  beta_coefficients_file <- data.frame(
    variable = c(''), coefficient = c(''), type = c(''))
  baseline_hazards_file <- data.frame(time = c(''))

  test_dir <- tempdir()
  write.csv(model_export_file, file.path(test_dir, '/model-export.csv'))
  write.csv(variables_file, file.path(test_dir, '/variables.csv'))
  write.csv(variable_details_file, file.path(test_dir, '/variable-details.csv'))
  write.csv(model_steps_file, file.path(test_dir, '/model-steps.csv'))
  write.csv(beta_coefficients_file, file.path(test_dir, '/beta-coefficients.csv'))
  write.csv(baseline_hazards_file, file.path(test_dir, '/baseline-hazards.csv'))

  expect_error(
    convert_model_export_to_pmml(
      test_dir,
      file.path(test_dir, '/model-export.csv'),
      database_name = 'db_one',
      custom_function_files = c()
    ),
    'Missing time variable'
  )
})

test_that("When converting a variable, if there's a start variable with the same
          name it should throw an error", {
  model_exports_file <- data.frame(
    fileType = c('variables', 'variable-details', 'model-steps'),
    filePath = c('./variables.csv', 'variable-details.csv', 'model-steps.csv')
  )
  variables_file <- data.frame(
    variable = c('age'),
    databaseStart = c('cchs'),
    variableStart = c('cchs::age')
  )
  variable_details_file <- data.frame(
    variable = c('age'),
    databaseStart = c('cchs'),
    variableStart = c('cchs::age')
  )
  model_steps_file <- data.frame(step = c("dummy"), fileType = c("N/A"), filePath = c("./dummy.csv"))
  dummy_file <- data.frame(origVarible = c(""), catValue = c(""), dummyVarible = c(""))

  test_dir <- tempdir()
  write.csv(model_exports_file, file.path(test_dir, '/model-export.csv'))
  write.csv(variables_file, file.path(test_dir, '/variables.csv'))
  write.csv(variable_details_file, file.path(test_dir, '/variable-details.csv')) 
  write.csv(model_steps_file, file.path(test_dir, '/model-steps.csv'), row.names = FALSE)
  write.csv(dummy_file, file.path(test_dir, './dummy.csv'))

  expect_error(
    convert_model_export_to_pmml(
      test_dir,
      file.path(test_dir, '/model-export.csv'),
      database_name = 'cchs'
    ),
    .get_same_var_and_start_var_err_msg(
      "age",
      1
    )
  )
})

test_that("When an entry for a model steps file is missing from the model
          exports file, it should throw an error", {
  model_exports_file <- data.frame(
    fileType = c('variables', 'variable-details'),
    filePath = c('./variables.csv', './variable-details.csv')
  )
  variables_file <- data.frame(
    variable = c('age'),
    databaseStart = c('cchs'),
    variableStart = c('cchs::age')
  )
  variable_details_file <- data.frame(
    variable = c('age'),
    databaseStart = c('cchs'),
    variableStart = c('cchs::age')
  )

  test_dir <- tempdir()
  model_export_path <- file.path(test_dir, '/model-export.csv')
  write.csv(model_exports_file, model_export_path)
  write.csv(variables_file, file.path(test_dir, '/variables.csv'))
  write.csv(variable_details_file, file.path(test_dir, '/variable-details.csv'))

  expect_error(
    convert_model_export_to_pmml(
      test_dir,
      model_export_path,
      database_name = 'cchs'
    ),
    paste("The model exports file at path", model_export_path,
          "is missing an entry for a model steps file. The following file types",
          "were found in the model exports file variables, variable-details"),
    fixed = TRUE
  )
})

test_that("When the model steps file cannot be found at the provided path,
          it should throw an error", {
  model_exports_file <- data.frame(
    fileType = c('variables', 'variable-details', 'model-steps'),
    filePath = c(
      './variables.csv', './variable-details.csv', 'non-existent.csv'
    )
  )
  variables_file <- data.frame(
    variable = c('age'),
    databaseStart = c('cchs'),
    variableStart = c('cchs::age')
  )
  variable_details_file <- data.frame(
    variable = c('age'),
    databaseStart = c('cchs'),
    variableStart = c('cchs::age')
  )

  test_dir <- tempdir()
  model_export_path <- file.path(test_dir, '/model-export.csv')
  write.csv(model_exports_file, model_export_path)
  write.csv(variables_file, file.path(test_dir, '/variables.csv'))
  write.csv(variable_details_file, file.path(test_dir, '/variable-details.csv'))

  model_steps_path <- file.path(test_dir, 'non-existent.csv')
  expect_error(
    convert_model_export_to_pmml(
      test_dir,
      model_export_path,
      database_name = 'cchs'
    ),
    paste("The model steps file was not found at path", model_steps_path)
  )
})
