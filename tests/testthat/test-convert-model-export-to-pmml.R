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
  variables_file <- data.frame(
    variable = c('sex'),
    label = c('sex'),
    labelLong = c('sex'),
    section = c('Demographics'),
    subject = c('Demographics'),
    variableType = c('Categorical'),
    units = c('N/A'),
    databaseStart = c('cchs'),
    variableStart = c('cchs::SEX'),
    description = c('')
  )
  variable_details_file <- data.frame(
    variable = c('sex'),
    dummyVariable = c('N/A'),
    typeStart = c('cat'),
    typeEnd = c('cat'),
    databaseStart = c('cchs'),
    variableStart = c('cchs::SEX'),
    recStart = c('male'),
    recEnd = c('1'),
    numValidCat = c('2'),
    catLabel = c('male'),
    catLabelLong = c('male'),
    units = c('N/A'),
    catStartLabel = c('male'),
    variableStartShortLabel = c('sex'),
    variableStartLabel = c('sex')
  )
  model_steps_file <- data.frame(
    step = c('fine-and-gray', 'fine-and-gray'),
    fileType = c('beta-coefficients', 'baseline-hazards'),
    filePath = c('./beta-coefficients.csv', 'baseline-hazards.csv'),
    notes = c('', '')
  )
  beta_coefficients_file <- data.frame(
    variable = c('sex'),
    coefficient = c('1'),
    type = c('cat')
  )
  baseline_hazards_file <- data.frame(
    time = c('1'),
    baselineHazard = c('1')
  )

  test_dir <- tempdir()
  write.csv(model_export_file, file.path(test_dir, '/model-export.csv'), row.names = FALSE)
  write.csv(variables_file, file.path(test_dir, '/variables.csv'), row.names = FALSE)
  write.csv(variable_details_file, file.path(test_dir, '/variable-details.csv'), row.names = FALSE)
  write.csv(model_steps_file, file.path(test_dir, '/model-steps.csv'), row.names = FALSE)
  write.csv(beta_coefficients_file, file.path(test_dir, '/beta-coefficients.csv'), row.names = FALSE)
  write.csv(baseline_hazards_file, file.path(test_dir, '/baseline-hazards.csv'), row.names = FALSE)

  expect_error(
    convert_model_export_to_pmml(
      test_dir,
      file.path(test_dir, '/model-export.csv'),
      database_name = 'cchs',
      custom_function_files = c()
    ),
    'Missing time variable in variable_details sheet for fine and gray model'
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
    label = c('age'),
    labelLong = c('age'),
    section = c('Demographics'),
    subject = c('Demographics'),
    variableType = c('Continuous'),
    units = c('years'),
    databaseStart = c('cchs'),
    variableStart = c('cchs::age'),
    description = c('')
  )
  variable_details_file <- data.frame(
    variable = c('age'),
    dummyVariable = c('N/A'),
    typeStart = c('cont'),
    typeEnd = c('cont'),
    databaseStart = c('cchs'),
    variableStart = c('cchs::age'),
    recEnd = c('copy'),
    numValidCat = c('N/A'),
    catLabel = c('N/A'),
    catLabelLong = c('N/A'),
    units = c('years'),
    recStart = c('else'),
    catStartLabel = c('N/A'),
    variableStartShortLabel = c('age'),
    variableStartLabel = c('age')
  )
  model_steps_file <- data.frame(
    step = c("dummy"),
    fileType = c("N/A"),
    filePath = c("./dummy.csv"),
    notes = c("")
  )
  dummy_file <- data.frame(
    origVariable = c(""),
    catValue = c(""),
    dummyVariable = c("")
  )

  test_dir <- tempdir()
  write.csv(model_exports_file, file.path(test_dir, '/model-export.csv'), row.names = FALSE)
  write.csv(variables_file, file.path(test_dir, '/variables.csv'), row.names = FALSE)
  write.csv(variable_details_file, file.path(test_dir, '/variable-details.csv'), row.names = FALSE)
  write.csv(model_steps_file, file.path(test_dir, '/model-steps.csv'), row.names = FALSE)
  write.csv(dummy_file, file.path(test_dir, './dummy.csv'), row.names = FALSE)

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
    label = c('age'),
    labelLong = c('age'),
    section = c('Demographics'),
    subject = c('Demographics'),
    variableType = c('Continuous'),
    units = c('years'),
    databaseStart = c('cchs'),
    variableStart = c('cchs::age'),
    description = c('')
  )
  variable_details_file <- data.frame(
    variable = c('age'),
    dummyVariable = c('N/A'),
    typeStart = c('cont'),
    typeEnd = c('cont'),
    databaseStart = c('cchs'),
    variableStart = c('cchs::age'),
    recEnd = c('copy'),
    numValidCat = c('N/A'),
    catLabel = c('N/A'),
    catLabelLong = c('N/A'),
    units = c('years'),
    recStart = c('else'),
    catStartLabel = c('N/A'),
    variableStartShortLabel = c('age'),
    variableStartLabel = c('age')
  )

  test_dir <- tempdir()
  model_export_path <- file.path(test_dir, '/model-export.csv')
  write.csv(model_exports_file, model_export_path, row.names = FALSE)
  write.csv(variables_file, file.path(test_dir, '/variables.csv'), row.names = FALSE)
  write.csv(variable_details_file, file.path(test_dir, '/variable-details.csv'), row.names = FALSE)

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
    label = c('age'),
    labelLong = c('age'),
    section = c('Demographics'),
    subject = c('Demographics'),
    variableType = c('Continuous'),
    units = c('years'),
    databaseStart = c('cchs'),
    variableStart = c('cchs::age'),
    description = c('')
  )
  variable_details_file <- data.frame(
    variable = c('age'),
    dummyVariable = c('N/A'),
    typeStart = c('cont'),
    typeEnd = c('cont'),
    databaseStart = c('cchs'),
    variableStart = c('cchs::age'),
    recEnd = c('copy'),
    numValidCat = c('N/A'),
    catLabel = c('N/A'),
    catLabelLong = c('N/A'),
    units = c('years'),
    recStart = c('else'),
    catStartLabel = c('N/A'),
    variableStartShortLabel = c('age'),
    variableStartLabel = c('age')
  )

  test_dir <- tempdir()
  model_export_path <- file.path(test_dir, '/model-export.csv')
  write.csv(model_exports_file, model_export_path, row.names = FALSE)
  write.csv(variables_file, file.path(test_dir, '/variables.csv'), row.names = FALSE)
  write.csv(variable_details_file, file.path(test_dir, '/variable-details.csv'), row.names = FALSE)

  model_steps_path <- file.path(test_dir, 'non-existent.csv')
  expect_error(
    convert_model_export_to_pmml(
      test_dir,
      model_export_path,
      database_name = 'cchs'
    ),
    paste("No model parameter file found at path", model_steps_path)
  )
})

test_that("When no variables are found for the specified database name,
          it should throw an error", {
  model_exports_file <- data.frame(
    fileType = c('variables', 'model-steps'),
    filePath = c('./variables.csv', './model-steps.csv')
  )
  variables_file <- data.frame(
    variable = c('age'),
    label = c('age'),
    labelLong = c('age'),
    section = c('Demographics'),
    subject = c('Demographics'),
    variableType = c('Continuous'),
    units = c('years'),
    databaseStart = c('cchs'),
    variableStart = c('cchs::age'),
    description = c('')
  )
  model_steps_file <- data.frame(
    step = c('simple-model'),
    fileType = c('N/A'),
    filePath = c('./simple-model.csv'),
    notes = c('')
  )
  simple_model_file <- data.frame(
    name = c('outputVariableName'),
    value = c('test')
  )

  test_dir <- tempdir()
  write.csv(model_exports_file, file.path(test_dir, '/model-export.csv'), row.names = FALSE)
  write.csv(variables_file, file.path(test_dir, '/variables.csv'), row.names = FALSE)
  write.csv(model_steps_file, file.path(test_dir, '/model-steps.csv'), row.names = FALSE)
  write.csv(simple_model_file, file.path(test_dir, '/simple-model.csv'), row.names = FALSE)

  expect_error(
    convert_model_export_to_pmml(
      test_dir,
      file.path(test_dir, '/model-export.csv'),
      database_name = 'nonexistent_db',
      custom_function_files = c()
    ),
    'No variables found for database nonexistent_db'
  )
})

test_that("Correctly converts copy value for categorical non-derived
          variables", {
  local_mocked_bindings(
    packageVersion = function(package_name) return('0.1.0'),
    .package = 'utils'
  )
  model_exports_file <- data.frame(
    fileType = c('variables', 'model-steps', 'variable-details'),
    filePath = c('./variables.csv', './model-steps.csv', './variable-details.csv')
  )
  variables_file <- data.frame(
    variable = c('sex'),
    label = c('sex'),
    labelLong = c('sex'),
    section = c('Demographics'),
    subject = c('Demographics'),
    variableType = c('Categorical'),
    units = c('N/A'),
    databaseStart = c('cchs'),
    variableStart = c('cchs::Sex'),
    description = c('')
  )
  variable_details_file <- data.frame(
    variable = c('sex'),
    dummyVariable = c('N/A'),
    typeStart = c('cat'),
    typeEnd = c('cat'),
    databaseStart = c('cchs'),
    variableStart = c('cchs::Sex'),
    recEnd = c('copy'),
    numValidCat = c('1'),
    catLabel = c('A1'),
    catLabelLong = c('A1'),
    units = c('N/A'),
    recStart = c('A1'),
    catStartLabel = c('A1'),
    variableStartShortLabel = c('sex'),
    variableStartLabel = c('sex')
  )
  model_steps_file <- data.frame(
    step = c('simple-model'),
    fileType = c('N/A'),
    filePath = c('./simple-model.csv'),
    notes = c('')
  )
  simple_model_file <- data.frame(
    name = c('outputVariableName'),
    value = c('test')
  )

  test_dir <- tempdir()
  write.csv(model_exports_file, file.path(test_dir, '/model-export.csv'))
  write.csv(variables_file, file.path(test_dir, '/variables.csv'))
  write.csv(variable_details_file, file.path(test_dir, '/variable-details.csv'))
  write.csv(model_steps_file, file.path(test_dir, '/model-steps.csv'))
  write.csv(simple_model_file, file.path(test_dir, '/simple-model.csv'))

  actual_pmml <- convert_model_export_to_pmml(
    test_dir,
    file.path(test_dir, '/model-export.csv'),
    database_name = 'cchs',
    custom_function_files = c()
  )

  expected_pmml <- '<PMML version="4.4"
    xmlns="http://www.dmg.org/PMML-4_4"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
    <DataDictionary numberOfFields="1">
      <DataField name="Sex" displayName="sex" optype="categorical" dataType="string">
        <Extension name="variableStartLabel" value="sex"/>
        <Value value="A1" displayValue="A1" property="valid"/>
      </DataField>
    </DataDictionary>
    <TransformationDictionary>
      <DerivedField name="sex" displayName="sex" optype="categorical" dataType="string">
        <Extension name="labelLong" value="sex"/>
        <Extension name="units" value="N/A"/>
        <Apply function="if">
          <Apply function="equal">
            <FieldRef field="Sex"/>
            <Constant dataType="string">A1</Constant>
          </Apply>
          <Constant dataType="string">A1</Constant>
          <Constant dataType="string">NA::b</Constant>
        </Apply>
        <Value value="NA::b" displayValue="Missing" property="missing">
          <Extension name="catLabelLong" value="Missing"/>
        </Value>
      </DerivedField>
    </TransformationDictionary>
    <Header>
      <Application name="pmml" version="0.1.0"/>
    </Header>
    <Output>
      <OutputField name="test"/>
    </Output>
    <SimpleModel/>
  </PMML>'

  expect_equal(
    prettify_xml(toString(actual_pmml)),
    prettify_xml(expected_pmml)
  )
})

test_that("Correctly converts logistic regression", {
  local_mocked_bindings(
    packageVersion = function(package_name) return('0.1.0'),
    .package = 'utils'
  )
  model_exports_file <- data.frame(
    fileType = c("variables", "variable-details", "model-steps"),
    filePath = c("./variables.csv", "./variable-details.csv", "./model-steps.csv")
  )  
  variables_file <- data.frame(
    variable = c("Age", "Sex"),
    label = c("age", "sex"),
    labelLong = c("age", "sex"),
    section = c("Demographics", "Demographics"), 
    subject = c("Demographics", "Demographics"),
    variableType = c("Continuous", "Categorical"),
    units = c("years", "N/A"), 
    databaseStart = c("limesurvey", "limesurvey"),
    variableStart = c("limesurvey::age", "limesurvey::sex"),
    description = c("", "")
  )
  variable_details_file <- data.frame(
    variable = c("Age", "Sex"),
    dummyVariable = c("N/A", "N/A"),
    typeEnd = c("cont", "cat"),
    databaseStart = c("limesurvey", "limesurvey"),
    variableStart = c("limesurvey::age", "limesurvey::sex"),
    typeStart = c("cont", "cat"),
    recEnd = c("copy", "copy"),
    numValidCat = c("N/A", "2"),
    catLabel = c("N/A", "N/A"),
    catLabelLong = c("N/A", "N/A"),
    recStart = c("else", "else"),
    catStartLabel = c("N/A", "N/A"),
    variableStartShortLabel = c("age", "sex"),
    variableStartLabel = c("age", "sex"),
    units = c("years", "N/A")
  )
  model_steps_file <- data.frame(
    step = c("dummy", "logistic-regression"),
    fileType = c("N/A", "N/A"),
    filePath = c("./dummy.csv", "./logistic-regression.csv"),
    notes = c("", "")
  )
  dummy_step_file <- data.frame(
    origVariable = c("sex", "sex"),
    catValue = c(1, 2),
    dummyVariable = c("sex_cat1", "sex_cat2")
  )
  logistic_regression_step_file <- data.frame(
    variable = c("Intercept", "age", "sex_cat1", "sex_cat2"),
    coefficient = c("1", "2", "3", "4"),
    type = c("cont", "cont", "cat", "cat")
  )

  test_dir <- tempdir()
  write.csv(model_exports_file, file.path(test_dir, "/model-export.csv"))
  write.csv(variables_file, file.path(test_dir, "/variables.csv"))
  write.csv(variable_details_file, file.path(test_dir, "/variable-details.csv"))
  write.csv(model_steps_file, file.path(test_dir, "/model-steps.csv"))
  write.csv(dummy_step_file, file.path(test_dir, "/dummy.csv"))
  write.csv(logistic_regression_step_file, file.path(test_dir, "/logistic-regression.csv"))

  actual_pmml <- convert_model_export_to_pmml(
    test_dir,
    file.path(test_dir, "/model-export.csv"),
    database_name = "limesurvey",
    custom_function_files = c()
  )
  
  expected_pmml <- "<PMML version=\"4.4\"
    xmlns=\"http://www.dmg.org/PMML-4_4\"
    xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
    <DataDictionary numberOfFields=\"2\">
      <DataField name=\"age\" displayName=\"age\" optype=\"continuous\" dataType=\"float\">
        <Extension name=\"variableStartLabel\" value=\"age\"/>
      </DataField>
      <DataField name=\"sex\" displayName=\"sex\" optype=\"categorical\" dataType=\"string\">
        <Extension name=\"variableStartLabel\" value=\"sex\"/>
      </DataField>
    </DataDictionary>
    <TransformationDictionary>
      <DerivedField name=\"Age\" displayName=\"age\" optype=\"continuous\" dataType=\"float\">
        <Extension name=\"labelLong\" value=\"age\"/>
        <Extension name=\"units\" value=\"years\"/>
        <FieldRef field=\"age\"/>
      </DerivedField>
      <DerivedField name=\"Sex\" displayName=\"sex\" optype=\"categorical\" dataType=\"string\">
        <Extension name=\"labelLong\" value=\"sex\"/>
        <Extension name=\"units\" value=\"N/A\"/>
        <FieldRef field=\"sex\"/>
      </DerivedField>
      <DerivedField name=\"sex_cat1\" optype=\"categorical\" dataType=\"float\">
        <Apply function=\"if\">
          <Apply function=\"equal\">
            <FieldRef field=\"sex\"/>
            <Constant dataType=\"float\">1</Constant>
          </Apply>
          <Constant dataType=\"float\">1</Constant>
          <Constant dataType=\"float\">0</Constant>
        </Apply>
      </DerivedField>
      <DerivedField name=\"sex_cat2\" optype=\"categorical\" dataType=\"float\">
        <Apply function=\"if\">
          <Apply function=\"equal\">
            <FieldRef field=\"sex\"/>
            <Constant dataType=\"float\">2</Constant>
          </Apply>
          <Constant dataType=\"float\">1</Constant>
          <Constant dataType=\"float\">0</Constant>
        </Apply>
      </DerivedField>
    </TransformationDictionary>
    <Header>
      <Application name=\"pmml\" version=\"0.1.0\"/>
    </Header>
    <RegressionModel functionName=\"regression\" normalizationMethod=\"softmax\" targetFieldName=\"y\">
      <MiningSchema>
        <MiningField name=\"age\"/>
        <MiningField name=\"sex\"/>
        <MiningField name=\"y\" usageType=\"target\"/>
      </MiningSchema>
      <RegressionTable targetCategory=\"yes\" intercept=\"1\">
        <NumericPredictor name=\"age\" coefficient=\"2\"/>
        <NumericPredictor name=\"sex_cat1\" coefficient=\"3\"/>
        <NumericPredictor name=\"sex_cat2\" coefficient=\"4\"/>
      </RegressionTable>
      <RegressionTable targetCategory=\"no\" intercept=\"0\"/>
    </RegressionModel>
  </PMML>"

  expect_equal(
    prettify_xml(toString(actual_pmml)),
    prettify_xml(expected_pmml)
  )
})
