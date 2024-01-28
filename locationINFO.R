### LOCATION INFO

location <- function()
{
  
  ### INPUTlOCATION OF RECRUITMENT - AREACODE, CITY, COUNTRY
  locationCountry <- readline(prompt = "Type location COUNTRY name: ")
  print(locationCountry)
  
  locationCity <- readline(prompt = "Type location city name: ")
  print(locationCity)
  locationCity
  location <- readline(prompt = "Type specific area name: ")
  ### INPUT 1 -- lOCATION OF RECRUITMENT - AREACODE, CITY, COUNTRY
  #Copy the list of Country names, city and exact locations names into vectors
  countries <- c("Kenya", "Uganda", "Tanzania")
  
  cities <- list("Kenya" =c("Nairobi", "Mombasa"), "Uganda" = c("Kampala", "Mbarara"), "Tanzania" = c("Mwanza", "Dodoma"))
  
  exactArea <- list("Nairobi" =c("eastleigh", "westlands"), "Mombasa" = c("kisauni", "likoni"), "Kampala" = c("kiza", "suba"),"Mbarara" = c("uma", "luki"), "Mwanza" = c("ugunja", "tani"),"Dodoma" = c("konzi", "jiba"))
  
  testFlag <- TRUE
  for (country in countries)
  {
    if(locationCountry == country)
    {
      for(city in cities)
      {
        for(cit in city)
        {
          if(locationCity == cit)
          {
            for (exact in exactArea)
            {
              for (area in exact)
              {
                if(area == location)
                {
                  print(area)
                  locationRiskVal <- "H"
                  testFlag <- FALSE
                  break
                }
                else
                {
                  if(testFlag == FALSE) {
                    print("Do not update again")
                  }
                  else {
                    locationRiskVal <- "M"
                    testFlag = FALSE
                  }
                  
                  break
                }
                
                break
              }
              print("ayeyssea")
            }
          }
          else
          {
            if(testFlag == FALSE) {
              print("Do not update again")
            }
            else {
              locationRiskVal <- "L"
            }
            
          }
          break
        }
        
      }
      break
    }
    else
    {
      locationRiskVal <- ""
    }
    
  }
  
  return (locationRiskVal)
  
}

