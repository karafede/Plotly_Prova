
# install.packages("plotly")
# install.packages("devtools")
# devtools::install_github("ropensci/plotly")

library(plotly)
library(ggplot2)
library(reshape)
library(curl)

p <- plot_ly(midwest, x = percollege, color = state, type = "box")
p

## Not run: 
# You need a plotly username and API key to communicate with the plotly API.

# If you already have a username and API key, please create the following
# environment variables:
# signup("karafede", "karafede@hotmail.com", save = TRUE)
Sys.setenv("plotly_username" = "karafede")
Sys.setenv("plotly_api_key" = "v516efgsn7")

p <- plot_ly(midwest, x = percollege, color = state, type = "box")
plotly_POST(p, filename = "home/prova_FK/midwest-boxplots")

#### ggplot & ggplotly #########################################################################

set.seed(100)
d <- diamonds[sample(nrow(diamonds), 1000), ]
plot_ly(d, x = carat, y = price, text = paste("Clarity: ", clarity),
        mode = "markers", color = carat, size = carat)

p <- ggplot(data = d, aes(x = carat, y = price))+
  geom_point(aes(text = paste("Clarity:", clarity)), size = 2) +
  geom_smooth(aes(colour = cut, fill = cut), size=0.5) + facet_wrap(~ cut)

(gg <- ggplotly(p))
gg
plotly_POST(gg, filename = "home/prova_FK/midwest-boxplots")


###### Boxplots ###################################################################

p <- plot_ly(midwest, x = percollege, color = state, type = "box")
p

#### Time series plots ########################################################## 

p <- plot_ly(economics, x = date, y = unemploy / pop)
p
m <- loess(unemploy / pop ~ as.numeric(date), data = economics)
p <- plot_ly(economics, x = date, y = unemploy / pop, name = "raw")
p <- add_trace(p, y = fitted(m), name = "loess")
p


### 3D plot ##################################################################

p <- plot_ly(z = volcano, type = "surface")
p


### Contour plots ###########################################################

p <- plot_ly(z = volcano, type = "contour")
p

### Color mapping by cluster ###################################################

p <- plot_ly(iris, x = Petal.Length, y = Petal.Width,
             color = Species, mode = "markers")
p


###################################################################################
###################################################################################

df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2011_february_us_airport_traffic.csv')
df$hover <- with(df, paste(airport, city, state, "Arrivals: ", cnt))

# marker styling
m <- list(
  colorbar = list(title = "Incoming flights February 2011"),
  size = 8, opacity = 0.8, symbol = 'square'
)

# geo styling
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  subunitcolor = toRGB("gray85"),
  countrycolor = toRGB("gray85"),
  countrywidth = 0.5,
  subunitwidth = 0.5
)

p <- plot_ly(df, lat = lat, lon = long, text = hover, color = cnt,
        type = 'scattergeo', locationmode = 'USA-states', mode = 'markers',
        marker = m) %>%
  layout(title = 'Most trafficked US airports<br>(Hover for airport)', geo = g)

plotly_POST(p, filename = "home/prova_FK/geo_MAP_FK")


###############################################################################

# US map small multiples

df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/1962_2006_walmart_store_openings.csv')

# common map properties
g <- list(scope = 'usa', showland = T, landcolor = toRGB("gray90"), showcountries = F, subunitcolor = toRGB("white"))

# year text labels
yrs <- unique(df$YEAR)
id <- seq_along(yrs)
df2 <- data.frame(
  YEAR = yrs,
  id = id
)

#  id for anchoring traces on different plots
df$id <- as.integer(factor(df$YEAR))

p <- plot_ly(df, type = 'scattergeo', lon = LON, lat = LAT, group = YEAR,
             geo = paste0("geo", id), showlegend = F,
             marker = list(color = toRGB("blue"), opacity = 0.5)) %>%
  add_trace(lon = -78, lat = 47, mode = 'text', group = YEAR,
            geo = paste0("geo", id), text = YEAR, data = df2) %>%
  layout(title = 'New Walmart Stores per year 1962-2006<br> Source: <a href="http://www.econ.umn.edu/~holmes/data/WalMart/index.html">University of Minnesota</a>',
         geo = g,
         autosize = F,
         width = 1000,
         height = 900,
         hovermode = F)

q <- subplot(p, nrows = 9)

plotly_POST(q, filename = "home/prova_FK/geo_MAP_FK")

#####################################################################################

df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv")
df$hover <- with(df, paste(state, '<br>', "Beef", beef, "Dairy", dairy, "<br>",
                           "Fruits", total.fruits, "Veggies", total.veggies,
                           "<br>", "Wheat", wheat, "Corn", corn))
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

p <- plot_ly(df, z = total.exports, text = hover, locations = code, type = 'choropleth',
        locationmode = 'USA-states', color = total.exports, colors = 'Purples',
        marker = list(line = l), colorbar = list(title = "Millions USD")) %>%
  layout(title = '2011 US Agriculture Exports by State<br>(Hover for breakdown)', geo = g)

plotly_POST(p, filename = "home/prova_FK/geo_MAP_FK")

######################################################################################

df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_ebola.csv')
# restrict from June to September
df <- subset(df, Month %in% 6:9)
# ordered factor variable with month abbreviations
df$abbrev <- ordered(month.abb[df$Month], levels = month.abb[6:9])
# September totals
df9 <- subset(df, Month == 9)

# common plot options
g <- list(
  scope = 'africa',
  showframe = F,
  showland = T,
  landcolor = toRGB("grey90")
)

g1 <- c(
  g,
  resolution = 50,
  showcoastlines = T,
  countrycolor = toRGB("white"),
  coastlinecolor = toRGB("white"),
  projection = list(type = 'Mercator'),
  list(lonaxis = list(range = c(-15, -5))),
  list(lataxis = list(range = c(0, 12))),
  list(domain = list(x = c(0, 1), y = c(0, 1)))
)

g2 <- c(
  g,
  showcountries = F,
  bgcolor = toRGB("white", alpha = 0),
  list(domain = list(x = c(0, .6), y = c(0, .6)))
)

p <- plot_ly(df, type = 'scattergeo', mode = 'markers', locations = Country,
        locationmode = 'country names', text = paste(Value, "cases"),
        color = as.ordered(abbrev), marker = list(size = Value/50), inherit = F) %>%
  add_trace(type = 'scattergeo', mode = 'text', geo = 'geo2', showlegend = F,
            lon = 21.0936, lat = 7.1881, text = 'Africa') %>%
  add_trace(type = 'choropleth', locations = Country, locationmode = 'country names',
            z = Month, colors = "black", showscale = F, geo = 'geo2', data = df9) %>%
  layout(title = 'Ebola cases reported by month in West Africa 2014<br> Source: <a href="https://data.hdx.rwlabs.org/dataset/rowca-ebola-cases">HDX</a>',
         geo = g1, geo2 = g2)

plotly_POST(p, filename = "home/prova_FK/geo_MAP_FK")

##################################################################################

df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')

# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

p <- plot_ly(df, z = GDP..BILLIONS., text = COUNTRY, locations = CODE, type = 'choropleth',
        color = GDP..BILLIONS., colors = 'Blues', marker = list(line = l),
        colorbar = list(tickprefix = '$', title = 'GDP Billions US$')) %>%
  layout(title = '2014 Global GDP<br>Source:<a href="https://www.cia.gov/library/publications/the-world-factbook/fields/2195.html">CIA World Factbook</a>',
         geo = g)

plotly_POST(p, filename = "home/prova_FK/geo_MAP_FK")

###################################################################################


df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_us_cities.csv')
df$hover <- paste(df$name, "Population", df$pop/1e6, " million")

df$q <- with(df, cut(pop, quantile(pop)))
levels(df$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
df$q <- as.ordered(df$q)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)

p <- plot_ly(df, lon = lon, lat = lat, text = hover,
        marker = list(size = sqrt(pop/10000) + 1),
        color = q, type = 'scattergeo', locationmode = 'USA-states') %>%
  layout(title = '2014 US city populations<br>(Click legend to toggle)', geo = g)

plotly_POST(p, filename = "home/prova_FK/geo_MAP_FK")
