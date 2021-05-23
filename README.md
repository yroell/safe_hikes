# Safe Hikes
Hike recommender in Colorado that gives safety recommendations for each hike

# Website
https://yroell.shinyapps.io/safe_hikes/

# Business Objective:
Create a hike recommender that gives safety suggestions. A major concern of hiking is safety and this is especially true for beginners. This differs from other hike recommenders because of the safety suggestions, which is currently not implemented in any hiking app. 

In 2020, there were more than 230 million visitors to the US national parks with over 8 million overnight stays. While most of these visits and overnight stays are in safe and populated areas, many people will go deeper into the backcountry. On average, 800 hikers are injured and 150 die each year in national parks. While this is not a large number compared to the total visits, the general reason for injury or death is lack of knowledge. For many of the hikers that are stuck in the backcountry, they are lucky enough to get a search and rescue team. The national park service records around 3000 search and rescue incidents each year. However, each trip is a substantial amount of money. The canyon parks of Utah alone spend more than 200 thousand dollars every year on search and rescue services. This website will ideally reduce the number of people that get hurt and have to use search and rescue. The goal is to get beginner hikers more informed and prepare them with the major concerns of the hike before they go outside.

# Data Ingestion:
A few data sources were used. The trail data comes from AllTrails. The weather data comes from the National Weather Service. The sheriff numbers come from the County Sheriffs of Colorado. Safety suggestions come from personal knowledge and various hiking resources.

The AllTrails data is acquired by scraping the webpages for each hike. This information includes elevation and distance. The GPX file for each hike was downloaded to be included in the interactive map. The weather data is acquired by using the weather service's API to get the weather and temperature for the next two days at each location. The sheriff number's are linked to a county map of Colorado and the first waypoint from the GPX file is used to determine which number should be reported.

# Visualizations:
The website has an interactive map that uses leaflet to display the spatial information and a line graph that displays the distance and elevation for each hike using base R.

# Machine Learning and Analysis:
A hike is recommended by taking in the user inputs and determining which hike is the closest to these inputs. The data from AllTrails is clustered together using k-means before the website was deployed. This was to reduce the amount of processing that needs to happen when the user is on the website. On the website, a distance matrix is created using the cluster centroids and the user inputs. Once the cluster that is most similar is chosen, another distance matrix is used to determine which hikes in the cluster are closest. The recommended hike is a random choice of the top three most similar hikes.

# Interactive Website:
The website allows the user to set inputs and a hike will be suggested which is all interactive. An interactive map is also included which allows the user to zoom in and out around the hike. This is deployed using ShinyApps.

# Script Details:
Three scripts are included: alltrails.R, website_gpx.R, and app.R. In the alltrails script, the code is scraping the hike webpages for the necessary data and clusters the data. This data is saved as an RData format to be uploaded into the website. The website_gpx script handles all the spatial data manipulation and creates the dataframe displayed when a hike is recommended. This data is saved as an RData format to be uploaded into the website. The dispalyed website comes from the app script. This handles all of the interactivity using the ShinyApp format and visualizations.
