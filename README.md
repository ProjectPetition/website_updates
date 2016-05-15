# website updates
Scripts to update the petition information in the Project Petition website database.

`reg.R` contains code to test out different forecasting regressions with the We The People bulk download data, and save regression results to a file. `scrape.R` is run nightly on the projpet server, getting new petition data from the WtP API and using the regression results from `reg.R` to generate signature forecasts for the new petitions. `graph.R` contains code for creating .png graphs that can be displayed on the website.

Run at 12:05 am every night with the cron command

`5 0 * * * /usr/bin/Rscript /network/rit/lab/projpet/will/website_updates/scrape.R`
