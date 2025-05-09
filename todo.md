* If the name of the start variable is the same as the value in the variable
  column in the variable details sheet then it throws an error saying it can't
  determine the fromType of a variable so its skipping it. It should instead 
  say that the start variable and end variable have the same name so it can't
  derive it and to rename it to another name.
* Add check to make sure that the variableType column is in the variables sheet
