#-----------| Customer Lifetime Value |-----------#

#-------------| Set Workdesk |--------------#

# setwd("/home/mechelle/Google Drive/Work/Syntax/R")

# Authorize the Google Analytics account
# This need not be executed in every session once the token object is created and saved

#-------------| Dependencies |--------------#

# Install Dependencies
source("~/Google Drive/Work/Syntax/R/Snippets/initialDependencies.R")

getAPI <- function(params, unlist=FALSE, debug=FALSE){
  construct <- function (x){
    trim <- function (x) gsub("^\\s+|\\s+$", "", x)
    trim(gsub(", ", "", toString(x)))
  }
  construct <- construct(params)
  url <- construct
  req <- curl_fetch_memory(url, handle = new_handle())
  data <- fromJSON(rawToChar(req$content))
  if(unlist){
    data <- t(unlist(data))
  }
  data <- data.frame(data,stringsAsFactors=FALSE)
  if(debug == TRUE){
    print(paste0("URL: ",url))
    # print(paste0("Content: ",rawToChar(req$content)))
  }
  return(data)
}

#-------------| Authorizations |--------------#

accesstoken <- "https://1603336e2920781a77810966a2318f4d:1827d87cc6e09df685be1058754f2d3d"
hostname <- "@be-mixed.myshopify.com"

tokens <- c(
  # analytics@bvaccel.com
  "/home/mechelle/Google Drive/Work/Syntax/R/GATokens/.gatoken1.rds",
  # analytics2@bvaccel.com
  "/home/mechelle/Google Drive/Work/Syntax/R/GATokens/.gatoken2.rds")

authorize(cache=tokens[2])
view <- "97085486"
profile <- get_profile(53198052,"UA-53198052-2",view)

#-------------| Total Amount of Customers |--------------#

customercount <- c(
  # Acces Token
  accesstoken,

  # Hostname
  hostname,

  # Field
  "/admin/customers/count.json"
)

df_customercount <- data.frame(
  TotalCustomers=as.character(customercount[,1])
)

shopdata <- c(
  # Acces Token
  accesstoken,

  # Hostname
  hostname,

  # Field
  "/admin/shop.json"
)

customercount <- getAPI(customercount)
shopdata <- getAPI(shopdata, unlist=TRUE)

#-------------| Get Customer Data |--------------#

customers <- data.frame(
  ID                  = rep("", 0),
  Email               = rep("", 0),
  AcceptsMarketing    = rep("", 0),
  CreatedAt           = rep("", 0),
  LastOrderDate       = rep("", 0),
  FirstName           = rep("", 0),
  LastName            = rep("", 0),
  OrdersCount         = rep("", 0),
  State               = rep("", 0),
  TotalSpent          = rep("", 0),
  LastOrderID         = rep("", 0),
  Note                = rep("", 0),
  VerifiedEmail       = rep("", 0),
  MultipassIdentifier = rep("", 0),
  TaxExempt           = rep("", 0),
  Tags                = rep("", 0),
  LastOrderName       = rep("", 0),
  stringsAsFactors = FALSE
)

increment <- floor(as.numeric(customercount)/250)+1

# progress bar
pb <- txtProgressBar(min = 0, max = increment, style = 3)
for (i in 1:increment ){
  # print(i)

  data <- c(
    # Acces Token
    accesstoken,

    # Hostname
    hostname,

    # Field
    "/admin/customers.json",

    # Incremental Upload
    "?limit=250&page=",i
  )

  data <- data.frame(getAPI(data))
  data$customers.addresses <- NULL
  data[,18] <- as.numeric()
  data[,19] <- as.numeric()
  colnames(data) <- c(
    "ID",
    "Email",
    "AcceptsMarketing",
    "CreatedAt",
    "LastOrderDate",
    "FirstName",
    "LastName",
    "OrdersCount",
    "State",
    "TotalSpent",
    "LastOrderID",
    "Note",
    "VerifiedEmail",
    "MultipassIdentifier",
    "TaxExempt",
    "Tags",
    "LastOrderName")

  customers <- rbind(data, customers)
  setTxtProgressBar(pb, i)
}
close(pb)

#-------------| Sandbox |--------------#

orderids <- c(
  # Acces Token
  accesstoken,

  # Hostname
  hostname,

  # Field
  "/admin/orders.json?status=any&customer_id=323186949&limit=250"
)

orderdata <- getAPI(orderids, debug = TRUE)

# customer id
customerid <- "323186949"
customers[customers$ID == "323186949",]

# extract from orderdata
customerorders <- orderdata[orderdata$orders.customer$id == customerid,]

x <- customerorders$orders.created_at[which(customerorders$orders.created_at == min(customerorders$orders.created_at))]
firstorderdate <- as_date(sapply(strsplit(as.character(x), "T"),"[[",1))
firstorderid <- customerorders$orders.id[which(customerorders$orders.created_at == min(customerorders$orders.created_at))]

for (i in 1:length(customerorders$orders.created_at)) {
  if(as_date(profile$created[1]) > firstorderdate){
    x <- customerorders$orders.created_at[which(customerorders$orders.created_at == min(customerorders$orders.created_at[customerorders$orders.created_at>x]))]
    firstorderdate <- as_date(sapply(strsplit(as.character(x), "T"),"[[",1))
    firstorderid <- customerorders$orders.id[which(customerorders$orders.created_at == min(customerorders$orders.created_at))]
    print('test')
  }else{
    break
  }
}

firstorderdate <- "2016-01-01"

startDate <- firstorderdate
endDate <- firstorderdate

test <- get_ga(view,
               start.date = startDate,
               end.date = endDate,
               metrics = "ga:sessions",
               dimensions = "ga:transactionId,ga:sourceMedium,ga:channelGrouping",
               sort = "-ga:sessions"
               # filter = paste0("ga:transactionId==",firstorderid)
)

# create failsafe that finds closest date of account creation


# gs_auth(cache = "/home/mechelle/Documents/Scripts/R/GDriveTokens/.gdrive-token1.rds")

# googleSheet <- gs_key("1jbA47fdC_cOR0s86Hb1jnmQ3MIoK_P5NlLmTZF43E3w")

# gs_edit_cells(googleSheet, ws="RAW", input=df_customers, byrow = TRUE, anchor="A1")

# gs_edit_cells(googleSheet, ws="Overview", input=df_customercount, byrow = TRUE, anchor="A1")
