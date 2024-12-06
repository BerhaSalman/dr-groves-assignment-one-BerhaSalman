[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/KeEgn-xT)

# Assignment - Groves
For this assignment you will obtain and clean data for your assigned States from IPUMS via API and then merge this with Census Tiger Files using the Census API and create a Map showing the variation of one of the key variables that you create in this assignment. When you cloned this repository you should have acquired a script file called "Assignment_One.R" and a "README.md" file. Your grade will be based on the correct manipulation of the script file and the output produced by the *plot()* command that should appear at the end of your script. You will also be graded on the quality of the code that you write and for covering the basics (like saving your final data in the right place).

# Getting to know your states

![image](https://github.com/user-attachments/assets/32a003a9-5174-4e28-bb85-5988d0794a74)

You have each been assigned a set of states and so now you will need the demographic data for for those states over the decades 1970 through 2020. Since we are dealing with decades, we will utilize data from the U.S. Decennial Census and since we have decades prior to 2000, that rules out the use of the census API (*tidycensus*). An alternative data source for the U.S. Census and many other surveys, demographics, and GIS data is [IPUMS.org](https://www.ipums.org/). They have a collection of "projects" that deal with several different types and scopes of data and the project we will focus on is the NHGIS project for "National Historic Geographic Information Systems" project. Like the census, they too have an API and there is a package that allows us to (somewhat) easily interact with the API through `R` and you need to get that installed on your machine.

> 1. Install in whatever method you choose, the package *ipumsr* and add a line of code to the script to load the library into `R` in addition to the *tidyverse* package. It will be obvious if you do not, because nothing else will work.

## Using the *ipumsr* package

The use of the *ipumsr* package to access the API is not quickly picked up so I have added the code that you need to run in the script that you received with this assignment. Before you can do that, however, just like with the census API, you need to obtain a key and install that within your `R` environment. You will see a line near the top of the script for installing the key which is currently commented out. You need to obtain your key by first [registering for a free IMPUS.org account](https://uma.pop.umn.edu/ihgis/user/new). Once you are registered, you need to apply for an [API key](https://account.ipums.org/api_keys), and then copy and paste that key somewhere you can save it (in a text editor and then save it as a text file) and also in the appropriate place in the script. To install the key into your `R Studio` environment, run that single line of code from the script editor.

```R
 tst <- get_metadata_nhgis("time_series_tables")
```

This will create an object named **tst** which is a mixture of a dataframe with a list (available through *tidyverse*) that contains all of the time series datasets that are available from the NHGIS project. In the console, run the *head()* command on this object and you should see:

```R
  name  description                       geographic_integration sequence time_series      years    geog_levels
  <chr> <chr>                             <chr>                     <dbl> <list>           <list>   <list>     
1 A00   Total Population                  Nominal                    100. <tibble [1 × 3]> <tibble> <tibble>   
2 AV0   Total Population                  Nominal                    100. <tibble [1 × 3]> <tibble> <tibble>   
3 B78   Total Population                  Nominal                    100. <tibble [1 × 3]> <tibble> <tibble>   
4 CL8   Total Population                  Standardized to 2010       100. <tibble [1 × 3]> <tibble> <tibble>   
5 A57   Persons by Urban/Rural Status [4] Nominal                    101. <tibble [4 × 3]> <tibble> <tibble>   
6 A59   Persons by Urban/Rural Status [4] Nominal                    101. <tibble [4 × 3]> <tibble> <tibble>
```

First we see the time series name, which is what we will use to get it from IPUMS, a description, a couple of internal items, and then a set of lists that tell us the specific time series (or data) in each time series, the years for which data exists, and the levels of geography we can get. Also of importance for us is the row number on the left-side. 

Since we are interested in the total population for each state so we will start with the first object here and use the name "A00" to tell the API what series we want, but first we need to ensure it has the years we need. To see this, type the following command into the console: `tst$years[[1]]`. Since the years vector is a list, we use the "\[\[.\]\]" to navigate within a list and the input here is the row number. So this command is telling `R` to show us the first list in the vector of lists named "years" within the object **tst**. Since the output is truncated prior to getting to the years we are interested in, take the suggestion of `R Studio` and instead type: `print(n = 25, tst$years[[1]])` which will display the first 25 rows of this list. We see near the end we have the decades from 1970 on just as we needed`.

> 2. What is the code you would use to find the levels of geography for which population (A00) is available? Place this code in the script in place of the text [Question 2].

The last piece of information we need from this is to know what data (rows) are in each time series. To actually see how this works, type `print(n=25, tst$time_series[[5]])` and see what you get. We see from our output above that row five of the object **tst** is the "Persons by Urban/Rural Status" data and when we run the command we just typed we get:

```R
1 AA    Persons: Urban                                                     1
2 AB    Persons: Urban--Inside urbanized areas                             2
3 AC    Persons: Urban--Outside urbanized areas (in urban clusters)        3
4 AD    Persons: Rural                                                     4
```

This tells me that within this time series there are four sub-measures that measure (1) the number of persons living in an urban area, (2) number of persons inside the urban core, (3) number of persons in the suburbs, and (4) the number of persons in rural areas. More importantly, we see that the code "AA" will denote the first measure, "AB" the second, and so forth. You will will find this output for every variable we will be using in the "README.txt" file for this repository.

> 3. Under the heading "Basic Clean of Data" in the *Assignment One.R* script you will find several lines in the code with the text **[insert code here]**. Replace this with the correct code to calculate the percentage of the population that matches the description in the code comments. The code for percentage under the age of 18 and percentage over the age of 65 is already completed for you to use as a guide.

Next we will get the data from the Census API so that we can make an image of our states. You will find the code for this in the *Assignment One.R* script.

> 4. First, create an object named **state** that contains the FIPS code for your states in the position labeled for Question 4A in the script. Then replace the `"state"` in the geography of the `get_acs()` command with your newly created object so that you only get the information for the states which you were assigned. 

Now we are going to join the two dataframes to create a new dataframe called **core** to be the basis of our visualization. You will find the skeleton of the code in the script for this assignment and you need to decide what file you are starting with and what you are going to join to that. You also need to resolve any issues that exist regarding the common element upon which the join is based. Remember that we are using a left join to ensure that we only keep the information from the other dataframe that match what we have in the main dataframe. Also recall from our in class demonstration that joining order has implications for object type. 
>5. Add a comment under the code that creates **ufo.us** with a brief description of what the preceding code to create **ufo.us** is doing.

> 6. Join the **cen.map** and **dat2** dataframes to create a new dataframe **core** by replacing the bracketed terms in the script file. You will also need to add a new command in one or both of the pipped commands creating **dat2** and/or **cen.map**. Make sure to look at your results because you might find duplicated column vectors (typically denoted with a .x and .y at the end of the name). If there are any, remove them.

> 7. You will now plot two Facet Wraps of the data for your states across the decade. You will see two `ggplot()` commands already sketched out in the script file. You need to find a non-race variable for the first one and one of the race variables for the second. There are several parts within each command that have brackets around the text that you need to fix. I have noted where the item needs to be within quotation marks. 

## Getting the X-Files
Now we want to see if there is anything in the data that might predictive of the number of sightings in your states.  First we need to get our UFO data which is the same that we were using in class and so we need to find the in-class code and copy and paste the code to get the UFO data and clean it.

>8. Copy and Paste the code to import the UFO data and the bridge file from our Groves_R repo that you had forked in class. You will be graded based on if you place the code in the relevant positions within your assignment codes based on what the code is doing. HINT: There is an alternative place to get the code as well. I already have the code in the script to clean the data so make sure you name them the correct object names.

>9. Now you need to add the necessary code to the creation of the **core** object to also join the **ufo.us** code. 

If you completed steps seven through nine correct, you should have a dataframe that has the census values and sightings for your states over the decades 1970 through 2010. If you look at the dataframe, you will noticed, however, that we did no have sightings data for the 2020 decade so we need to remove the rows that correspond to those census values. To do this we can use the `filter()` by adding it as a pipped command to our creation of the **core** dataframe. 

>10. Add a pipped command to the creation of **core** that will remove the rows containing NA or missing data from our dataframe. Also check your dataframe for any redundant vectors and remove those as well. 


## Visualizations

You are going to create two sets of maps that will be displayed as a facet wrap by decade. In your first map under the heading "Non-Race Variable", choose the non-race variable you think is most descriptive of your results. You may need to run your regressions before you decide which to show. For the second map, choose a race variable to display.  I have added the code to save the plots as png files which you should be able to insert into your `LaTeX` script. If you need to adjust the image size or type, see the help for `ggsave()`.

>11. Finish the code and create figures one and two for your presentation.

## Getting Results

 Now we should have a full dataframe ready for analysis. The last part is all pretty straight forward and you can use the code blocks from the notes and the in-class examples to finish this off.  

>11. Create a table named Table 1: Summary Statistics with the summary statistic values for the variables we will be using the analysis. Make sure to replace the vector names with the character string names (I have already helped you out here, you just need to double check the work) and output this as a `LaTeX` file named "table1.txt" (I have it saved as an html right now).

>12. Run three regressions models. The first with only the census characteristics, in the second add decade fixed effects, and in the third, add decade and state fixed effects. 

>13. Create a table named Table 2: Regression Results with the results from all three models. The state fixed effects are omitted so you need to make sure to add something to the line that indicates if the state fixed effects are present. Additionally, using the same methodology you used to replace the vector names in the summary stats table, rename the coefficients using the **var2** object. Right now it is a simple copy of the **var1** object. Make sure to make any changes needed to make sure the names are correct. Also make sure to save this as a `LaTeX` file.
