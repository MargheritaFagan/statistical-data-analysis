/*==============================================================================
  Customer Data Analysis in SAS
  Academic group project

  Purpose:
  - integrate customer, demographic and order data
  - create derived variables
  - aggregate purchasing information at customer level
  - produce descriptive reports and frequency tables
  - assess the association between country and expenditure range

  Required input datasets in the DATA library:
  - customer
  - customer_dim
  - customer_orders

  Before running the script, assign the DATA library to the folder containing
  the input datasets, for example:

      libname data "/path/to/data";
==============================================================================*/


/* Macro for sorting datasets ------------------------------------------------*/

%macro sorting(dset=, by=);
    proc sort data=&dset;
        by &by;
    run;
%mend sorting;


/*==============================================================================
  1. DATA INTEGRATION
==============================================================================*/

/* Sort customer and demographic datasets by customer ID */
%sorting(dset=data.customer, by=Customer_ID);
%sorting(dset=data.customer_dim, by=Customer_ID);

/* Merge customer and demographic information */
data customers_base;
    merge data.customer
          data.customer_dim(
              rename=(
                  Customer_Country   = Country
                  Customer_Gender    = Gender
                  Customer_BirthDate = Birth_Date
              )
          );
    by Customer_ID;
run;


/* Sort datasets before merging with purchase information */
%sorting(dset=customers_base, by=Customer_Name);
%sorting(dset=data.customer_orders, by=Customer_Name);

/* Keep only customers associated with purchase records */
data customers_raw;
    merge customers_base(in=a)
          data.customer_orders(in=b);
    by Customer_Name;

    if a and b;
run;


/*==============================================================================
  2. VARIABLE CREATION
==============================================================================*/

/* Sort data for FIRST./LAST. processing */
%sorting(
    dset=work.customers_raw,
    by=Customer_ID Product_Name
);

/* Create age category and flag variables */
data work.customers_step1;
    set work.customers_raw;
    by Customer_ID Product_Name;

    /* Age category */
    if missing(Customer_Age) then AGE_CAT = 0;
    else if 15 < Customer_Age and Customer_Age <= 30 then AGE_CAT = 1;
    else if Customer_Age > 30 then AGE_CAT = 2;
    else AGE_CAT = 0;

    /* Flag missing customer-group information */
    if missing(Customer_Group) then FLAG01 = "Y";
    else FLAG01 = "N";

    /* Identify first or last record within each customer */
    if first.Customer_ID or last.Customer_ID then FLAG02 = "Yes";
    else FLAG02 = "No";
run;


/* Calculate total expenditure for each customer */
proc means data=work.customers_step1 nway noprint;
    class Customer_ID;
    var Total_Retail_Price;

    output out=work.cost_per_customer(drop=_TYPE_ _FREQ_)
        sum(Total_Retail_Price)=Cost;
run;


/* Sort datasets before merging customer-level expenditure */
%sorting(dset=work.customers_step1, by=Customer_ID);
%sorting(dset=work.cost_per_customer, by=Customer_ID);

/* Add total customer cost and purchase-level budget category */
data work.customers;
    merge work.customers_step1(in=a)
          work.cost_per_customer(in=b);
    by Customer_ID;

    if a;

    if Total_Retail_Price <= 50 then BUDGET_CAT = "<=50";
    else BUDGET_CAT = ">50";

    format Cost Total_Retail_Price dollar12.2;
run;


/*==============================================================================
  3. DESCRIPTIVE REPORTS
==============================================================================*/

ods pdf file="final_report.pdf" style=statistical;


/* Summary of retail price by country */
title "Summary of Total Retail Price by Country";

proc means data=work.customers mean std min max;
    class Country;
    var Total_Retail_Price;
run;


/* Customer-level purchasing summary */
title "Customer-Level Summary: Quantities, Number of Orders and Total Expenditure";

proc means data=work.customers nway noprint;
    class Customer_ID;
    var Quantity Total_Retail_Price;

    output out=work.customer_expenditure(drop=_TYPE_ _FREQ_)
        sum(Quantity)           = Ordered_Quantities
        n(Quantity)             = Total_Num_Orders
        sum(Total_Retail_Price) = Total_Expenditure;
run;


/* Apply labels and formatting */
data work.customer_expenditure;
    set work.customer_expenditure;

    format Total_Expenditure dollar12.2;

    label Ordered_Quantities = "Ordered quantities"
          Total_Num_Orders   = "Total n. of orders"
          Total_Expenditure  = "Total expenditure";
run;


proc print data=work.customer_expenditure label noobs;
    var Customer_ID
        Ordered_Quantities
        Total_Num_Orders
        Total_Expenditure;
run;


/*==============================================================================
  4. COUNTRY AND EXPENDITURE RANGE
==============================================================================*/

/* Retain one country record per customer */
proc sort data=work.customers
          out=work.customer_country(keep=Customer_ID Country)
          nodupkey;
    by Customer_ID;
run;


/* Merge customer expenditure with country information */
%sorting(dset=work.customer_expenditure, by=Customer_ID);

data work.customer_expenditure_country;
    merge work.customer_expenditure(in=a)
          work.customer_country(in=b);
    by Customer_ID;

    if a;
run;


/* Create total-expenditure categories */
data work.customer_expenditure_country;
    set work.customer_expenditure_country;

    length Exp_Range $6;

    if Total_Expenditure < 50 then Exp_Range = "Low";
    else if Total_Expenditure >= 50
            and Total_Expenditure <= 100 then Exp_Range = "Medium";
    else if Total_Expenditure > 100 then Exp_Range = "High";
run;


/* Chi-square test of association */
title "Impact of Country on Expenditure Range: Chi-Square Test";

proc freq data=work.customer_expenditure_country;
    tables Country * Exp_Range / chisq expected;
run;


/* Fisher's exact test as an alternative when expected counts are small */
title "Impact of Country on Expenditure Range: Fisher's Exact Test";

proc freq data=work.customer_expenditure_country;
    tables Country * Exp_Range / expected;
    exact fisher;
run;


/*==============================================================================
  5. COUNTRY AND SUPPLIER FREQUENCIES
==============================================================================*/

title "Country and Supplier Frequencies";

proc freq data=work.customers;
    where not missing(Country)
          and not missing(Supplier);

    tables Country * Supplier;
run;


/* Close report */
ods pdf close;
title;
