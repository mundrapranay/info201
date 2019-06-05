library(mapdata)
world <- map_data('world')

## Associate the URL IDs with each of the names
## Use the names to plot the associated data
## Should be an easy merge

countries <- c("Argentina", "Australia", "Austria", "Belgium", "Bolivia", "Brazil", "Bulgaria", 
               "Canada", "Chile", "Colombia", "Costa Rica", "Czech Republic", "Denmark", 
               "Dominican Republic", "Ecuador", "El Salvador", "Estonia", "Finland", "France", 
               "Germany", "Greece", "Guatemala", "Honduras", "Hong Kong", "Hungary", "Iceland", 
               "India", "Indonesia", "Ireland", "Israel", "Italy", "Japan", "Latvia", "Lithuania", 
               "Luxembourd", "Malaysia", "Malta", "Mexico", "Netherlands", "New Zealand", "Nicaragua", 
               "Norway", "Panama", "Paraguay", "Peru", "Philippines", "Poland", "Portugal", "Romania", 
               "Singapore", "Slovakia", "South Africa", "Spain", "Sweden", "Switzerland", "Taiwan", 
               "Thailand", "Turkey", "United Kingdom", "United States", "Uruguay", "Vietnam")

url_code <- c("37i9dQZEVXbMMy2roB9myp", "37i9dQZEVXbJPcfkRz0wJ0", "37i9dQZEVXbKNHh6NIXu36", 
              "37i9dQZEVXbJNSeeHswcKB", "37i9dQZEVXbJqfMFK4d691", "37i9dQZEVXbMXbN3EUUhlg", 
              "37i9dQZEVXbNfM2w2mq1B8", "37i9dQZEVXbKj23U1GF4IR", "37i9dQZEVXbL0GavIqMTeb", 
              "37i9dQZEVXbOa2lmxNORXQ", "37i9dQZEVXbMZAjGMynsQX", "37i9dQZEVXbIP3c3fqVrJY", 
              "37i9dQZEVXbL3J0k32lWnN", "37i9dQZEVXbKAbrMR8uuf7", "37i9dQZEVXbJlM6nvL1nD1", 
              "37i9dQZEVXbLxoIml4MYkT", "37i9dQZEVXbLesry2Qw2xS", "37i9dQZEVXbMxcczTSoGwZ", 
              "37i9dQZEVXbIPWwFssbupI", "37i9dQZEVXbJiZcmkrIHGU", "37i9dQZEVXbJqdarpmTJDL", 
              "37i9dQZEVXbLy5tBFyQvd4", "37i9dQZEVXbJp9wcIM9Eo5", "37i9dQZEVXbLwpL8TjsxOG", 
              "37i9dQZEVXbNHwMxAkvmF8", "37i9dQZEVXbKMzVsSGQ49S", "37i9dQZEVXbLZ52XmnySJg", 
              "37i9dQZEVXbObFQZ3JLcXt", "37i9dQZEVXbKM896FDX8L1", "37i9dQZEVXbJ6IpvItkve3", 
              "37i9dQZEVXbIQnj7RRhdSX", "37i9dQZEVXbKXQ4mDTEBXq", "37i9dQZEVXbJWuzDrTxbKS", 
              "37i9dQZEVXbMx56Rdq5lwc", "37i9dQZEVXbKGcyg6TFGx6", "37i9dQZEVXbJlfUljuZExa", 
              "37i9dQZEVXbMD2H5HJqmx9", "37i9dQZEVXbO3qyFxbkOE1", "37i9dQZEVXbKCF6dqVpDkS", 
              "37i9dQZEVXbM8SIrkERIYl", "37i9dQZEVXbISk8kxnzfCq", "37i9dQZEVXbJvfa0Yxg7E7", 
              "37i9dQZEVXbKypXHVwk1f0", "37i9dQZEVXbNOUPGj7tW6T", "37i9dQZEVXbJfdy5b0KP7W", 
              "37i9dQZEVXbNBz9cRCSFkY", "37i9dQZEVXbN6itCcaL3Tt", "37i9dQZEVXbKyJS56d1pgi", 
              "37i9dQZEVXbNZbJ6TZelCq", "37i9dQZEVXbK4gjvS1FjPY", "37i9dQZEVXbKIVTPX9a2Sb", 
              "37i9dQZEVXbMH2jvi6jvjk", "37i9dQZEVXbNFJfN1Vw8d9", "37i9dQZEVXbLoATJ81JYXz", 
              "37i9dQZEVXbJiyhoAPEfMK", "37i9dQZEVXbMnZEatlMSiu", "37i9dQZEVXbMnz8KIWsvf9", 
              "37i9dQZEVXbIVYVBNw9D5K", "37i9dQZEVXbLnolsZ8PSNw", "37i9dQZEVXbLRQDuF5jeBp", 
              "37i9dQZEVXbMJJi3wgRbAy", "37i9dQZEVXbLdGSmz6xilI")
