---
editor_options: 
  markdown: 
    wrap: 72
---

### Phosphorus Notes:

*Client Summary*

We’ve moved to a new Phosphorous analysis method for lakes and streams,
and need to update the Dashboard R scripts to reflect the new method,
while retaining the old data.

For many years we reported “Phosphorous” but now we report “Total
Phosphorus, mixed forms” (method SM4500P-F).

\- We’d like to update the Phosphorus query in R to merge these two,
updating the field called TP_P.\
- The selection lists, graphs and text would need to reflect the new
method, with a note about the previous name.

\- The WQI calculation would get updated to use the new field value.

WQP = NWIS + WQX

**General flow of data acquisition + tidying:**

-   WQP_r_script.R pulls in the data from the remote Water Quality
    Portal.

-   We tidy it to match with a previous internal format. This tidied
    dataframe is saved and used to produce the various dashboard tables.
    Tidied dataframe is named "wqp_data.csv" and can be updated
    manually.

-   The "wqp_data.csv" is prepared and tidied a second time using
    data_prep.R. This second prep allows for a rapid dashboard response,
    avoiding the hangtime of sourcing the external WQP database each
    time the dashboard is launched. The data_prep.R script produces
    multiple RDS tables that are sourced by the dashboard.

**Phosphorus data trace**: The two forms of phosphorus initially pulled
into wqp_data.csv are: "Total Phosphorus, mixed forms" and "Phosphorus".
There are no other observations with case-insensitive "phosphorus"
detected in the initial pull.

In data_prep.R, lines 49 - 56, the following forms of phosphorus that
are detected within wqp_data.csv are **grouped together as "Total
Phosphorus".**

c("Total Phosphorus, mixed forms",'Phosphorus','Total Phosphorus')

*This step appears to incorporate the client's request that the two
previous methods are grouped together as "Total Phosphorus". The only
way phosphorus data could be missing is if phosphorus is defined within
the initial call as something other than the above list. However, the
raw data returned from the call to WQP (wqp_data.csv) does not contain
any other references to the word "phosphorus", case-insensitive, outside
of "Total Phosphorus, mixed forms", and "Phosphorus".*

**Where TP_P comes in:** In the wqi_function.R (reference stop error on
line 48), parameters are referred to as short names. This is where the
TP_P comes in. The wqi_calc will only consider TP_P, which derives from
"Total Phosphorus." As "Total Phosphorus" contains all previous
references to phosphorus, this should contain all observations.

The wqi_function.R calculation applies the function to the tidied
wqp_data.csv. On line 115 in data_prep.R, TP_P is assigned to Total
Phosphorus, which contains all phosphorus references.

All other references throughout the app are for Total Phosphous.

------------------------------------------------------------------------

### E Coli Notes

*Client Summary* We have data in WQX that isn’t making it to the main
table:

In WQX CharacteristicName \| CountOfResultMeasureValue \| MinOfDate \|
MaxOfDate Escherichia coli \| 3365 \| 3/23/1998 \| 10/13/2022 Fecal
Coliform \| 21789 \| 10/17/1974 \| 2/25/2025 Total Coliform \| 1202 \|
10/28/1959 \| 10/13/2022

IN R table: parameter \| CountOfvalue \| MinOfdate_time \|
MaxOfdate_time Escherichia coli \| 512 \| 10/25/1999 \| 9/14/2021 Fecal
Coliform \| 9014 \| 8/15/1983 \| 9/17/2019 6:08:00 PM

------------------------------------------------------------------------

WQP_script.r, wqp_data is not finding "Total Coliform". The initial
readWQPdata() function is not pulling it.
