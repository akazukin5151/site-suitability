The two scripts (Python and R) are independent, run in any order.

The Python script is "stupid lazy" in generating the descriptive stats. This is not an insult: "lazy" means it will skip generating the data if the csv is already present, "stupid" means it does not validate the existing csv, only checks for the existence of the path.

This means whenever you add a new subplot, you have to delete the existing csv to force it to re-calculate
