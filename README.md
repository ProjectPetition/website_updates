# website_updates
Scripts to update the petition information in the Project Petition website database.

reg.R contains code to test out different forecasting regressions with the We The People bulk download data, and save regression results to a file. scrape.R is run nightly on the projpet server, getting new petition and response data from the WtP API and using the regression results from reg.R to generate signature forecasts for the new petitions.
