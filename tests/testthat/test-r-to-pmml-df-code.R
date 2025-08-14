context("Data frame code")

test_that("Data frame code within single line functions using table within the function scope are correctly generated", {
  code <- '
       a <- function(b, table) {
        return(table[table$b == b, ]$c)
      } 
  '

  expected_pmml <- '<PMML>
<LocalTransformations>
<DefineFunction name="a">
<ParameterField name="b" dataType="double"/>
<ParameterField name="table" dataType="double"/>
<MapValues outputColumn="c">
<FieldColumnPair column="b" field="b"/>
<TableLocator location="local" name="table"/>
</MapValues>
</DefineFunction>
</LocalTransformations>
</PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml)
})

test_that("Data frame code within single line functions using tables outside the function scope are correctly generated", {
  code <- '
    a <- function(b) {
  return(table[table$c == b, ]$d)
}
'

  expected_pmml <- '<PMML>
<LocalTransformations>
<DefineFunction name="a">
<ParameterField name="b" dataType="double"/>
<MapValues outputColumn="d">
<FieldColumnPair column="c" field="b"/>
<TableLocator location="taxonomy" name="table"/>
</MapValues>
</DefineFunction>
</LocalTransformations>
</PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml)
})

test_that("Retreiving output column from function return", {
 code <- '
a <- function() {
  return(b()$c)
}
'

  expected_pmml <- '<PMML>
<LocalTransformations>
<DefineFunction name="a">
<MapValues outputColumn="c">
<TableLocator>
<Apply function="b"></Apply>
</TableLocator>
</MapValues>
</DefineFunction>
</LocalTransformations>
</PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml)
})

test_that("Data frame query with no output column is correctly generated", {
  code <- '
a <- function(table) {
  return(table[table$b == "1", ])
}
'

  expected_pmml <- '<PMML>
<LocalTransformations>
<DefineFunction name="a">
<ParameterField name="table" dataType="double"/>
<MapValues>
<FieldColumnPair column="b" constant="1"/>
<TableLocator location="local" name="table"/>
</MapValues>
</DefineFunction>
</LocalTransformations>
</PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml)
})

test_that("When multiple expressions are present within a function call and 
          the non-first expression is a data frame access code that uses a 
          table from earlier in the function, the TableLocator node is 
          correctly set.", {
    code <- '
        func <- function(table) {
           var_one <- table[table$a == 1, ] 
           var_two <- var_one[var_one$b == 1, ]$c
           var_three <- var_two + 1
        }
    '

    expected_pmml <- '<PMML>
        <LocalTransformations>
            <DefineFunction name="func(var_one)">
                <ParameterField name="table" dataType="double" />
                <MapValues>
                   <FieldColumnPair column="a" constant="1" />
                   <TableLocator location="local" name="table" />
                </MapValues>
            </DefineFunction>
            <DefineFunction name="func(var_two)">
                <ParameterField name="table" dataType="double" />
                <MapValues outputColumn="c">
                   <FieldColumnPair column="b" constant="1" />
                    <TableLocator>
                   <Apply function="func(var_one)">
                    <FieldRef field="table" />
                   </Apply>
                   </TableLocator>
                </MapValues>
            </DefineFunction>
           <DefineFunction name="func">
                <ParameterField name="table" dataType="double" />
                <Apply function="+">
                    <Apply function="func(var_two)">
                        <FieldRef field="table"/>
                    </Apply>
                    <Constant dataType="double">1</Constant>
                </Apply>
            </DefineFunction>
        </LocalTransformations>
    </PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml)

})

test_that("Data frame code within an else expressions works", {
  code <- '
    a <- function(b, table) {
     if(b == 1) {
       return(1)
     }
     else {
      return(table[table$c == b, ]$d)
     }
}
'

  expected_pmml <- '<PMML>
<LocalTransformations>
  <DefineFunction name="a">
    <ParameterField name="b" dataType="double"/>
    <ParameterField name="table" dataType="double"/>
    <Apply function="if">
      <Apply function="equal">
        <FieldRef field="b"/>
        <Constant dataType="double">1</Constant>
      </Apply>
      <Constant dataType="double">1</Constant>
      <MapValues outputColumn="d">
        <FieldColumnPair column="c" field="b"/>
        <TableLocator location="local" name="table"/>
      </MapValues>
    </Apply>
  </DefineFunction>
</LocalTransformations>
</PMML>'

  test_utils_run_generate_pmml_test(code, expected_pmml)
})

