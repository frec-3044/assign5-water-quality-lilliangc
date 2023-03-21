Water Quality
================
Quinn Thomas

``` r
library(tidyverse)
library(lubridate)
library(purrr)
library(dplyr)
library(base)
```

# Using Application Programming Interfaces (API)

Many sources of data have an application programming interface (API)
that is a set of clearly defined methods of communication. The USGS has
a API that we will use.

Copy and paste the following command into a web browser.

<https://nwis.waterdata.usgs.gov/usa/nwis/uv/?cb_00010=on&format=rdb&site_no=05464420&period=&begin_date=2014-04-01&end_date=2014-09-01>

You will see that it returns a data table and, based the USGS website,
is tab-delimitated. This call to the API specifically asks for a subset
of data from the USGS water quality dataset. To see what subset it is
asking for we need to break down the API into its components.

- the base URL: <https://nwis.waterdata.usgs.gov/usa/nwis/uv/>?  
- the variable of interest: `cb_00010=on` (00010 is water temperature
  and `cb` is require before all variables)
- the format of the table: `format=rdb` (this means tab delimited table,
  rather than, say, a json format)
- the site id: `site_no=05464420`
- the period of time:
  `period=&begin_date=2014-04-01&end_date=2014-09-01` (the period of the
  begin and end date)

Each of these components is combined together with an “&” to create the
full URL.

Our goal is to leverage the API to explore spatial and temporal patterns
in water quality.

To read in data from a single request, we can directly read in the data

``` r
url <- "https://nwis.waterdata.usgs.gov/usa/nwis/uv/?cb_00010=on&format=rdb&site_no=05464420&period=&begin_date=2014-04-01&end_date=2014-09-01"
df <- read_delim(url, delim = "\t")
```

    ## Warning: One or more parsing issues, call `problems()` on your data frame for details,
    ## e.g.:
    ##   dat <- vroom(...)
    ##   problems(dat)

    ## Rows: 14786 Columns: 1
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: "\t"
    ## chr (1): # ---------------------------------- WARNING ----------------------...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(df) 
```

    ## # A tibble: 6 × 1
    ##   # ---------------------------------- WARNING -------------------------------…¹
    ##   <chr>                                                                         
    ## 1 # Some of the data that you have obtained from this U.S. Geological Survey da…
    ## 2 # may not have received Director's approval. Any such data values are qualifi…
    ## 3 # as provisional and are subject to revision. Provisional data are released o…
    ## 4 # condition that neither the USGS nor the United States Government may be hel…
    ## 5 # for any damages resulting from its use.                                     
    ## 6 #                                                                             
    ## # … with abbreviated variable name
    ## #   ¹​`# ---------------------------------- WARNING ----------------------------------------`

Look at the imported data. Did it work? Is there a symbol that
designates a comment in the file?

Now use the comment option in the `read_delim`. You will need to add the
comment symbol below in place of `[INSERT SYMBOL]`

``` r
stream_data <- read_delim(url, 
                          delim = "\t", 
                          comment = "#")
```

    ## Rows: 14759 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: "\t"
    ## chr (6): agency_cd, site_no, datetime, tz_cd, 44003_00010, 44003_00010_cd
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Do the headers look good now?

``` r
head(stream_data)
```

    ## # A tibble: 6 × 6
    ##   agency_cd site_no  datetime         tz_cd `44003_00010` `44003_00010_cd`
    ##   <chr>     <chr>    <chr>            <chr> <chr>         <chr>           
    ## 1 5s        15s      20d              6s    14n           10s             
    ## 2 USGS      05464420 2014-04-01 00:00 CDT   6.7           A               
    ## 3 USGS      05464420 2014-04-01 00:15 CDT   6.6           A               
    ## 4 USGS      05464420 2014-04-01 00:30 CDT   6.6           A               
    ## 5 USGS      05464420 2014-04-01 00:45 CDT   6.6           A               
    ## 6 USGS      05464420 2014-04-01 01:00 CDT   6.6           A

Which column is the temperature observation? Let’s give them better
names. The number below in `rename` is the column number

``` r
stream_data <- stream_data %>% 
    rename(temperature = 5)

# the 5 represents column 5
```

What is the class of the datetime and nitrate columns? Is this what you
expected?

Why might the class be wrong? Look at the top rows of the dataframe and
in the raw data. Why is the first row different that all the other rows?

Now, we need to remove the first row. `slice` is a function that selects
particular rows based on row number. The `-1` value removes the first
row.

``` r
stream_data <- stream_data %>% 
    slice(-1)

head(stream_data)
```

    ## # A tibble: 6 × 6
    ##   agency_cd site_no  datetime         tz_cd temperature `44003_00010_cd`
    ##   <chr>     <chr>    <chr>            <chr> <chr>       <chr>           
    ## 1 USGS      05464420 2014-04-01 00:00 CDT   6.7         A               
    ## 2 USGS      05464420 2014-04-01 00:15 CDT   6.6         A               
    ## 3 USGS      05464420 2014-04-01 00:30 CDT   6.6         A               
    ## 4 USGS      05464420 2014-04-01 00:45 CDT   6.6         A               
    ## 5 USGS      05464420 2014-04-01 01:00 CDT   6.6         A               
    ## 6 USGS      05464420 2014-04-01 01:15 CDT   6.5         A

Now fix the class issue with the `datetime` and `temperature` columns.
What is the format of the`datatime` column? Since it has the year,
month, day, hour, and minute we will use the `ymd_hm()` function (this
is similar to the `ymd()` that we used in the data carpentry module)

``` r
stream_data <- stream_data %>% 
    mutate(datetime = ymd_hm(datetime))
head(stream_data) 
```

    ## # A tibble: 6 × 6
    ##   agency_cd site_no  datetime            tz_cd temperature `44003_00010_cd`
    ##   <chr>     <chr>    <dttm>              <chr> <chr>       <chr>           
    ## 1 USGS      05464420 2014-04-01 00:00:00 CDT   6.7         A               
    ## 2 USGS      05464420 2014-04-01 00:15:00 CDT   6.6         A               
    ## 3 USGS      05464420 2014-04-01 00:30:00 CDT   6.6         A               
    ## 4 USGS      05464420 2014-04-01 00:45:00 CDT   6.6         A               
    ## 5 USGS      05464420 2014-04-01 01:00:00 CDT   6.6         A               
    ## 6 USGS      05464420 2014-04-01 01:15:00 CDT   6.5         A

``` r
# tz = time zone
```

Now lets fix the `temperature` column by converting from character to
numeric using `as.numeric()`

``` r
stream_data <- stream_data |> 
  mutate(temperature = as.numeric(temperature))
```

Finally, remove the temperature missing values using `na.omit()`

``` r
#stream_data <- stream_data |> 
  #na.omit()
```

**Question 1:**

Download data, then combine all data download, import, and cleaning
steps above into a single set of piped commands that creates a data
table named `stream_data`

**Answer 1: code chunk below**

``` r
# installing libraries
library(tidyverse)
library(lubridate)

# downloading data
url <- "https://nwis.waterdata.usgs.gov/usa/nwis/uv/?cb_00010=on&format=rdb&site_no=05464420&period=&begin_date=2014-04-01&end_date=2014-09-01"
df <- read_delim(url, delim = "\t", comment = "#")
```

    ## Rows: 14759 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: "\t"
    ## chr (6): agency_cd, site_no, datetime, tz_cd, 44003_00010, 44003_00010_cd
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# cleaning the data

stream_data <- df |> 
  slice(-1) |> 
  rename(temperature = 5) |> 
  mutate(datetime = ymd_hm(datetime)) |> 
  mutate(temperature = as.numeric(temperature)) 
  
head(stream_data)
```

    ## # A tibble: 6 × 6
    ##   agency_cd site_no  datetime            tz_cd temperature `44003_00010_cd`
    ##   <chr>     <chr>    <dttm>              <chr>       <dbl> <chr>           
    ## 1 USGS      05464420 2014-04-01 00:00:00 CDT           6.7 A               
    ## 2 USGS      05464420 2014-04-01 00:15:00 CDT           6.6 A               
    ## 3 USGS      05464420 2014-04-01 00:30:00 CDT           6.6 A               
    ## 4 USGS      05464420 2014-04-01 00:45:00 CDT           6.6 A               
    ## 5 USGS      05464420 2014-04-01 01:00:00 CDT           6.6 A               
    ## 6 USGS      05464420 2014-04-01 01:15:00 CDT           6.5 A

# Functions

Often you want repeat a task, like downloading from an API, using
slightly different configurations. Rather than copying and pasting the
code for each configuration.

The first step is to create the task that you will be repeatly doing
using a function. Functions can take input arguments and generate
output. For example, here is a function that adds two numbers together.

``` r
add2numbers <- function(a, b){
  c <- a + b
  return(c)
}
```

The function can be called `add2numbers(a = 1 , b = 2)` or just
`add2numbers(1 , 2)`. `a` and `b` are the input arguments and it returns
a single value that as to be assigned to a new object

``` r
new_number <- add2numbers(2, 3)
new_number
```

    ## [1] 5

Functions like this are very powerful for breaking up code into clearly
separated, well-described (particularly when using descriptive verbs in
the function name - like we know what the `add2numbers` does from the
function name) reusable parts. The power of using multiple functions is
that you can update a function in one place, and all the places that it
is called will also be updated. This helps reduce errors in your code
where you changed something in one place but not another.

**Question 2:**

Based on the information provided above, create a function that takes
arguments `variable`, `site_no`, `begin_date`, and `end_date` and
returns the full API URL. Remember to give the function a useful name
that is a verb (e.g., represents the action)

**Answer 2: code chunk below**

``` r
make_url <- function(variable, site_no, begin_date, end_date){
  
  url <-  paste0("https://nwis.waterdata.usgs.gov/usa/nwis/uv/?",
                 variable,"=on&format=rdb&site_no=",
                 site_no,"&period=&begin_date=", 
                 begin_date, "&end_date=", 
                 end_date)
  
  return(url)
}

make_url(variable = "cb_00010", site_no = "05464420", begin_date = "2014-04-01", end_date = "2014-09-01")
```

    ## [1] "https://nwis.waterdata.usgs.gov/usa/nwis/uv/?cb_00010=on&format=rdb&site_no=05464420&period=&begin_date=2014-04-01&end_date=2014-09-01"

Now add your function from Question 2 to the function below where it
says “ADD_YOUR_FUNCTION_FROM_QUESTION_2” (yes - functions can be within
functions). This function takes `site_no` as an argument and returns a
cleaned data table. You will also add your code from Question 1 to where
it says “ADD_YOUR_CODE_FROM_QUESTION_1\_THAT_CLEANS_THE_DATA”.

``` r
get_USGS_temp_data <- function(site_no){
  
  url <- make_url(variable = "cb_00010", site_no = site_no, begin_date = "2014-04-01", end_date = "2014-09-01")
  
  stream_data <- read_delim(file = url, 
                            delim = "\t", 
                            comment = "#")
  
  if(ncol(stream_data) < 5){ 
    #THIS IS NEEDED BECAUSE SOME SITES MAY RETURN EMPTY TABLES
    stream_data <- NULL
  }else{
    stream_data <- stream_data |> 
      slice(-1) |> 
      rename(temperature = 5) |> 
      mutate(datetime = ymd_hm(datetime)) |> 
      mutate(temperature = as.numeric(temperature)) 
  }
  return(stream_data)
  
}
```

# Iteration

Now that we have a function we can reuse, there are powerful tools in R
that make it easy to apply the function repeatably using different
values for the arguments. Here we will use the `map_` family of
functions within the `purrr` package (contained in the Tidyverse).

Back to our `add2numbers()` example. The following code has a vector of
numbers `numbers2add` that we want to add to the number 1 (`b = 1`). The
map function applies the `add2numbers()` function to each number in
`numbers2add` using the other argument a not changing `b = 1`.
Importantly, the first argument of the map function is the vector of
things that you want to iterate over and it has to be the first argument
in the function that you are using. In particular, `numbers2add` are the
values that we want to use for `a` in the `add2numbers(a, b)`. The
second argument is the function name. Any arguments after function name
are other arguments that function uses.

``` r
add2numbers(1, 1)
```

    ## [1] 2

``` r
add2numbers(2, 1)
```

    ## [1] 3

``` r
add2numbers(3, 1)
```

    ## [1] 4

``` r
add2numbers(4, 1)
```

    ## [1] 5

``` r
numbers2add <- c(1,2,3,4)
numbers <- map(numbers2add, add2numbers, b = 1)
numbers
```

    ## [[1]]
    ## [1] 2
    ## 
    ## [[2]]
    ## [1] 3
    ## 
    ## [[3]]
    ## [1] 4
    ## 
    ## [[4]]
    ## [1] 5

Does this match your expected output?

The class of the output are lists. That is because the `map` function
automatically combines the output from the different function calls into
a list. There are other functions in the `map_` family that combines the
output in different ways. For example, the `map_dbl` function returns a
vector of numeric values and is more approproiate for this simple
example.

``` r
numbers2add <- c(1,2,3,4)
numbers <- map_dbl(numbers2add, add2numbers, b = 1)
numbers
```

    ## [1] 2 3 4 5

There are multipe functions in the `map_` family that combine the output
in different ways.

**Question 3**

Run the command `?map` to pull up the help information for the map
function. For each function below describe what the map function will
return

**Answer 3: answers listed below**

``` r
?map_dfc
```

    ## starting httpd help server ... done

- `map()`: transform their input by applying a function to each element
  of a list or atomic vector and returning an object of the same length
  as the input. always returns a list.
- `map_lgl()`: return an atomic vector of the indicated type (or die
  trying). For these functions, .f must return a length-1 vector of the
  appropriate type.
- `map_int()`: return an atomic vector of the indicated type
- `map_dbl()`: return an atomic vector of the indicated type
- `map_chr()`: return an atomic vector of the indicated type
- `map_dfr()`: “map and bind columns”, and it applies a function to each
  element of a list or vector, and then combines the results into a
  single data frame with the columns bound together
- `map_dfc()`: apply a function to each vector and then bind the columns
  together into a single data frame
- `walk()`: calls .f for its side-effect and returns the input .x.

The `map_dfr()` is particularly powerful for working with data frames
because it appends by rows each data frame that is generated by each
call to the function. We use it to apply our function to download and
clean the USGS data over multiple sites. This example uses two site
codes.

``` r
sites <- c("05412500", "05464420")

temperature_data <- map_dfr(sites, get_USGS_temp_data)
```

    ## Rows: 14679 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: "\t"
    ## chr (6): agency_cd, site_no, datetime, tz_cd, 43611_00010, 43611_00010_cd
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 14759 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: "\t"
    ## chr (6): agency_cd, site_no, datetime, tz_cd, 44003_00010, 44003_00010_cd
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Now we can plot the data that was returned by the call to `map_dfr`.
Does it have data from two sites as expected?

``` r
ggplot(temperature_data, aes(x = datetime, y = temperature, color = site_no)) +
  geom_point() + 
  theme_bw() +
  labs(x = "Date", 
       y = "Temperature")
```

![](water-quality_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

# Explore impaired streams and nitrate concentrations at a site

For the second part of this module, we will be further exploring
impaired water bodies near us and federal guidelines for safety using
publicly available EPA water quality data.

**Question 4:** (provide answers)

Explore federal regulatory guidelines. The US Environmental Protection
Agency lists water quality regulations for both Human Drinking Water

<https://www.epa.gov/ground-water-and-drinking-water/national-primary-drinking-water-regulations>

- What is the Maximum Contaminant Level (MCL) for nitrate (reported as
  nitrate -nitrogen) in mg/L.
- What are the potential health impacts of consuming water with
  concentrations above this limit?
- What are the common sources of nitrate in water?

**Answer 4: The maximum contaminant level (MCL) for nitrate is 10 mg/L.
Nitrate can convert hemoglobin, which is the protein responsible for
transporting oxygen in the blood, to methemoglobin. This change reduces
the ability to carry oxygen. Elevated levels of nitrate can also result
in skin appearing bluish or gray, and can lead to health issues such as
weakness, increased heart rate, fatigue, and dizziness. High nitrate
levels in water can stem from various sources such as waste water,
septic systems, animal feedlots, landfills, urban drainage, fertilized
soil runoff, or leakage. **

Add answers to the three questions above.

------------------------------------------------------------------------

During this portion of the module, you will download, clean, and
generate plots that will allow you to further analyze the data,
answering questions as you go.

Watershed managers need to know how often a risk presents itself in a
watershed in order for action to be taken. For financially strapped
government’s, priority is given to problems that have the highest
probability of occurring and which are associated with the most severe
impacts. “Blue-baby syndrome” is a condition that leads to infant
mortality. It is caused by the ingestion of nitrate in drinking water
which subsequently bonds to oxygen sites on hemoglobin in the blood of
the infant. This impairs the circulation of oxygen in the bloodstream
and causes the baby to turn blue. Obviously, society would like to avoid
this outcome.

**Question 5:**

Modify the functions that we created for downloading and cleaning
temperature data to be able to download the nitrate data (variable code
is `cb_99133`).

**Answer 5: code chunk below**

``` r
get_USGS_nitrate_data <- function(site_no){
  
  url <- make_url(variable = "cb_99133", site_no = site_no, begin_date = "2011-01-01", end_date = "2023-01-01")
  
  nitrate_data <- read_delim(file = url, 
                            delim = "\t", 
                            comment = "#")
  
  if(ncol(nitrate_data) < 5){ 
    #THIS IS NEEDED BECAUSE SOME SITES MAY RETURN EMPTY TABLES
    nitrate_data <- NULL
  }else{
    nitrate_data <- nitrate_data |> 
      slice(-1) |> 
      rename(nitrate = 5) |> 
      mutate(datetime = ymd_hm(datetime)) |> 
      mutate(nitrate = as.numeric(nitrate)) |> 
      mutate(site_no = site_no)
  }
  
  Sys.sleep(1) #Add a second delay to prevent errors
  
  
  return(nitrate_data)
  
}
```

**Question 6:**

Download and clean the data from `site_no = 05464420` for the period
between 2011-01-01 and 2023-01-01.

**Answer 6: code chunk below**

``` r
nitrate_data <- get_USGS_nitrate_data(site_no = "05464420")
```

    ## Rows: 235266 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: "\t"
    ## chr (6): agency_cd, site_no, datetime, tz_cd, 44004_99133, 44004_99133_cd
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

**Question 7:**

Create a plot with datetime on the x-axis and nitrate on the y-axis
(this is a time-series plot of nitrate). Then use the `geom_hline()`
function to add a horizontal line at the EPA concentration (the
`epa_limit` variable that you defined above)

**Answer 7: code chunk below**

``` r
nitrate_data |> ggplot(aes(datetime, nitrate)) + 
  geom_line() +
  geom_hline(yintercept = 10) +
  theme_bw() +
  labs(x = "Year",
       y = "Nitrate")
```

![](water-quality_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

**Question 8:**

Does the location exceed the EPA limit?

**Answer 8: On most dates, the nitrate level does not exceed the set 10
mg/l nitrate limit; however, there are some spikes in nitrate that do
exceed the limit, especially in the years 2012-2018. Since Iowa is a
pretty big agricultural state, some of these upticks in nitrate
concentration could be due to increased precipitation and runoff from
farms (where nitrate levels are high) into streams/rivers. **

**Question 9:**

Calculate the number of days per year where the daily mean nitrate is
higher than the EPA limit. You will need to figure out how to answer
this question using skills that you already have (i.e., `group_by`,
`summarize`, `year`). One skill that you may not have used yet is the
[`ifelse()`
function](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/ifelse).
You can use `ifelse` in a mutate to determine if the daily mean nitrate
is above the threshold:
`mutate(above = ifelse(nitrate > epa_limit, 1, 0))`.

**Answer 9: The number of days per year where the daily mean nitrate is
higher than the EPA limit is listed in the “above_epa_count” dataframe.
**

``` r
## lubridate ##
library(lubridate)

epa_limit <- 10

nitrate_cleaned <- nitrate_data |> 
  mutate(year = year(datetime)) |> 
  mutate(day = yday(datetime))  |> 
  select(year, day, nitrate)

## group-by ##
nitrate_over_epa <- nitrate_cleaned |> 
  group_by(year,day) |> 
  summarize(daily_mean_n = mean(nitrate)) |> 
  mutate(above = ifelse(daily_mean_n > epa_limit, 1, 0))
```

    ## `summarise()` has grouped output by 'year'. You can override using the
    ## `.groups` argument.

``` r
above_epa_count <- nitrate_over_epa |> group_by(year) |> summarize(count = sum(above))
```

**Question 10:**

Plot how the number of days per year where the average nitrate
concentration is higher than the EPA limit over the years in the data (x
= year, y = number of days per year). How is it changing over time?

**Answer 10: plot code below. From the years 2012 - 2016, there are
mostly increases in the number of days per year where the average
nitrate concentration is higher than the EPA limit. From between
2016-2018, there is a drastic decrease in days over the nitrate EPA
limit. From that point forward, there is a slight increase then
decrease, and then a drastic increase from 2021-2022. **

``` r
above_epa_count |> ggplot(aes(year, count)) + 
  geom_line() +
  scale_x_continuous(breaks = seq(2012, 2022, by = 1)) +
  theme_bw() +
  labs(x = "Year",
       y ="Number of Days")
```

![](water-quality_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

# Geneate report for an agency

An Iowa agency wants a report on how water quality is changing through
time across the state. Generate a plot that provides number of days per
year where the average nitrate concentration for each site is higher
than the EPA limit over the years in the data. You have all the tools to
need to generate the report except for the list of USGS sites in Iowa.
You can find the list of all sites in a json file in the
`data/site_list.json` with the site codes as a column.

A JSON file is a common format used when requesting data from database
hosted remotely. JSON stands for JavaScript Object Notation. You can
learn more about JSON
[here](https://r4ds.hadley.nz/rectangling.html#json). You will need to
install the `jsonlite` package and use the `read_json()` function to
read in the metadata (you can call this `df_site_list`). Be sure to use
the the argument `simplifyVector = TRUE` so that you covert the JSON to
a dataframe.

**Question 11:**

Your answers to this Question is going to be a mix of code chunks and
text. It should include

- Read in and prepare data for analysis.
- Aggregate (summarize) data to the appropriate time resolution.
- Plot data that includes all sites (e.g. sites should be separate lines
  on the plot)
- Describe the patterns and conclusions from the data.

**Answer 11: Overall the trend of the number of days over the epa limit
for Iowa stays generally constant between the number of 0-100 in all of
the years present in this data set except in 2015 when multiple sites
saw a high and drastic increase in number of days over the epa limit.
Once possible explanation for this drastic increase in nitrate in the
water sources could be due to the record rainfall Iowa saw in 2015
(reference:
<https://www.servicemasterrestore.com/servicemaster-by-rice-des-moines/why-us/blog/2015/december/record-rainfall-hits-iowa-causing-severe-flooding/>).
As rainfall increases, so does runoff from agricultural plots and farms.
This runoff flows into streams that flow into larger rivers and eventual
outputs. Since Iowa is a major agricultural state, the amount of nitrate
is also high, so with high rainfall, an increase in nitrate
contamination in water sources might be expected.**

``` r
## installing packages ##
#install.packages("jsonlite")
library(jsonlite)
```

    ## 
    ## Attaching package: 'jsonlite'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     flatten

``` r
## reading in the data ##
df_site_list <- read_json("C:/Users/lchia/OneDrive/Documents/Spring 2023/EDS Assignments/assign5-water-quality-lilliangc/data/site_list.json", simplifyVector =TRUE)

df_site_list <- df_site_list |> filter(state == "IA")

iowa_sites <- df_site_list$site_id
iowa_data <- map_dfr(iowa_sites, get_USGS_nitrate_data)
```

    ## Rows: 241056 Columns: 6

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: "\t"
    ## chr (6): agency_cd, site_no, datetime, tz_cd, 43610_99133, 43610_99133_cd
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Rows: 235266 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: "\t"
    ## chr (6): agency_cd, site_no, datetime, tz_cd, 44004_99133, 44004_99133_cd
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 104532 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: "\t"
    ## chr (6): agency_cd, site_no, datetime, tz_cd, 247079_99133, 247079_99133_cd
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 317085 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: "\t"
    ## chr (6): agency_cd, site_no, datetime, tz_cd, 44062_99133, 44062_99133_cd
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Rows: 302703 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: "\t"
    ## chr (6): agency_cd, site_no, datetime, tz_cd, 44240_99133, 44240_99133_cd
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 363536 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: "\t"
    ## chr (6): agency_cd, site_no, datetime, tz_cd, 44246_99133, 44246_99133_cd
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Rows: 305983 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: "\t"
    ## chr (6): agency_cd, site_no, datetime, tz_cd, 44262_99133, 44262_99133_cd
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 222217 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: "\t"
    ## chr (6): agency_cd, site_no, datetime, tz_cd, 44294_99133, 44294_99133_cd
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 366409 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: "\t"
    ## chr (6): agency_cd, site_no, datetime, tz_cd, 44302_99133, 44302_99133_cd
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 156097 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: "\t"
    ## chr (6): agency_cd, site_no, datetime, tz_cd, 44654_99133, 44654_99133_cd
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 215047 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: "\t"
    ## chr (6): agency_cd, site_no, datetime, tz_cd, 44719_99133, 44719_99133_cd
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
## cleaning the data up ##
iowa_data_clean <- iowa_data |> 
  mutate(year = year(datetime)) |> 
  mutate(day = yday(datetime))  |> 
  mutate(site_no = as.factor(site_no)) |> 
  select(site_no, year, day, nitrate)

## group-by ##
iowa_over_epa <- iowa_data_clean |> 
  group_by(site_no,year,day) |> 
  summarize(daily_mean_n = mean(nitrate, na.rm = TRUE)) |> 
  mutate(above = ifelse(daily_mean_n > epa_limit, 1, 0))
```

    ## `summarise()` has grouped output by 'site_no', 'year'. You can override using
    ## the `.groups` argument.

``` r
iowa_above_epa_count <- iowa_over_epa |> 
  group_by(site_no, year) |> 
  summarize(count = sum(above, na.rm = TRUE))
```

    ## `summarise()` has grouped output by 'site_no'. You can override using the
    ## `.groups` argument.

``` r
iowa_above_epa_count <- iowa_above_epa_count |> filter(count > 0)

## plot ##
iowa_above_epa_count |> ggplot(aes(year, count, color = site_no)) + 
  geom_line() +
  scale_x_continuous(breaks = seq(2011, 2022, by = 1)) +
  theme_bw() +
  labs(x = "Year",
       y ="Number of Days")
```

![](water-quality_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

# Knitting and committing

Remember to Knit your document as a `github_document` and comment+push
to GitHub your code, knitted document, and any files in the `figure-gfm`
subdirectory that was created when you knitted the document.
