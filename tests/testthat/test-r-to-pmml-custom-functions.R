context("Testing custom functions")

test_that("Simple custom function PMML is correctly generated", { 
    code <- '
    testFunction <- function(testOne) {
      testTwo <- testOne + 1
  
      return(testTwo)
    }
    '

    expected_pmml <- '<PMML>
<LocalTransformations>
<DefineFunction name="testFunction(testTwo)">
<ParameterField name="testOne" dataType="double"/>
<Apply function="+">
<FieldRef field="testOne"/>
<Constant dataType="double">1</Constant>
</Apply>
</DefineFunction>
<DefineFunction name="testFunction">
<ParameterField name="testOne" dataType="double"/>
<Apply function="testFunction(testTwo)">
<FieldRef field="testOne"/>
</Apply>
</DefineFunction>
</LocalTransformations>
</PMML>'

    test_utils_run_generate_pmml_test(code, expected_pmml)
})

test_that("Default function arguments are correctly generated", {
  code <- "
    testFunc <- function(argOne = 1 + 1, argTwo = 'a') {
      test <- argOne + argTwo
    }

    a <- testFunc()
  "

  expected_pmml <- '<PMML>
<LocalTransformations>
<DefineFunction name=\"default(argOne)\">
<ParameterField name=\"argOne\" dataType=\"double\"/>
<ParameterField name=\"argTwo\" dataType=\"double\"/>
<Apply function=\"if\">
<Apply function=\"equal\">
<FieldRef field=\"argOne\"/>
<Constant dataType=\"NA\">NA</Constant>
</Apply>
<Apply function=\"+\">
<Constant dataType=\"double\">1</Constant>
<Constant dataType=\"double\">1</Constant>
</Apply>
<FieldRef field=\"argOne\"/>
</Apply>
</DefineFunction>
<DefineFunction name=\"default(argTwo)\">
<ParameterField name=\"argOne\" dataType=\"double\"/>
<ParameterField name=\"argTwo\" dataType=\"double\"/>
<Apply function=\"if\">
<Apply function=\"equal\">
<FieldRef field=\"argTwo\"/>
<Constant dataType=\"NA\">NA</Constant>
</Apply>
<Constant dataType=\"string\">a</Constant>
<FieldRef field=\"argTwo\"/>
</Apply>
</DefineFunction>
<DefineFunction name=\"testFunc\">
<ParameterField name=\"argOne\" dataType=\"double\"/>
<ParameterField name=\"argTwo\" dataType=\"double\"/>
<Apply function=\"+\">
<Apply function=\"default(argOne)\">
<FieldRef field=\"argOne\"/>
<FieldRef field=\"argTwo\"/>
</Apply>
<Apply function=\"default(argTwo)\">
<FieldRef field=\"argOne\"/>
<FieldRef field=\"argTwo\"/>
</Apply>
</Apply>
</DefineFunction>
<DerivedField name=\"a\" optype=\"continuous\">
<Apply function=\"testFunc\">
<Constant dataType=\"NA\">NA</Constant>
<Constant dataType=\"NA\">NA</Constant>
</Apply>
</DerivedField>
</LocalTransformations></PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml)
})

test_that("No return statement function are correctly generated", {
  code <- '
    a <- function() {
      b <- 1
    }
  '

  expected_pmml <- '<PMML>
<LocalTransformations>
<DefineFunction name="a">
<Constant dataType="double">1</Constant>
</DefineFunction>
</LocalTransformations>
</PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml)
})

test_that("Function with parameters and parameter closing brace on new line", {
  code <- '
    a <- function(arg_one
    ) {
      b <- 1
    }
  '

  expected_pmml <- '
<PMML>
  <LocalTransformations>
    <DefineFunction name="a">
      <ParameterField name="arg_one" dataType="double" />
      <Constant dataType="double">1</Constant>
    </DefineFunction>
  </LocalTransformations>
</PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml)
})

test_that("Custom functions with intermediate variables that use the dollar
          operator to access the column are correctly parsed", {
  code <- '
    a <- function(table) {
      row <- table[table$b == 1, ]
      c <- row$c 
      return(c)
    }
  '

  expected_pmml <- '<PMML>
    <LocalTransformations>
      <DefineFunction name=\"a(row)\">
        <ParameterField name=\"table\" dataType=\"double\"/>
        <MapValues>
          <FieldColumnPair column=\"b\" constant=\"1\"/>
          <TableLocator location=\"local\" name=\"table\"/>
        </MapValues>
      </DefineFunction>
      <DefineFunction name=\"a(c)\">
        <ParameterField name=\"table\" dataType=\"double\"/>
        <MapValues outputColumn=\"c\">
          <TableLocator>
            <Apply function=\"a(row)\">
              <FieldRef field=\"table\"/>
            </Apply>
          </TableLocator>
        </MapValues>
      </DefineFunction>
      <DefineFunction name=\"a\">
        <ParameterField name=\"table\" dataType=\"double\"/>
        <Apply function=\"a(c)\">
          <FieldRef field=\"table\"/>
        </Apply>
      </DefineFunction>
    </LocalTransformations>
  </PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml)
})

test_that("Custom functions with intermediate variables that use non-wildcard
          expressions are correctly parsed", {
  code <- '
    a <- function(table) {
      row <- table
      c <- row[row$b == 1, "c"]
      return(c)
    }
  '

  expected_pmml <- '<PMML>
    <LocalTransformations>
      <DefineFunction name=\"a(row)\">
        <ParameterField name=\"table\" dataType=\"double\"/>
        <FieldRef field=\"table\"/>
      </DefineFunction>
      <DefineFunction name=\"a(c)\">
        <ParameterField name=\"table\" dataType=\"double\"/>
        <MapValues outputColumn=\"c\">
          <FieldColumnPair column=\"index\" constant=\"\"/>
          <TableLocator>
            <Apply function=\"a(row)\">
              <FieldRef field=\"table\"/>
            </Apply>
          </TableLocator>
        </MapValues>
      </DefineFunction>
      <DefineFunction name=\"a\">
        <ParameterField name=\"table\" dataType=\"double\"/>
        <Apply function=\"a(c)\">
          <FieldRef field=\"table\"/>
        </Apply>
      </DefineFunction>
    </LocalTransformations>
  </PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml)
})
