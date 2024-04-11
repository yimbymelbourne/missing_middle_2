## Assumptions used in regression (03run_regression.R)

### Cleaning

-   SA3 areas with less than 3 observations were allocated to a neighbouring SA3 (Only a single SA3)
-   Records with a missing SA1 value were removed from the dataset (only \~20 records)
-   Records removed if their lga_name_2022 is not in inner_lga (Melbourne, Yarra, Port Phillip, Stonnington, Maribyrnong) or middle_lgas (Boroondara, Darebin, Glen Eira, Merri-bek, Banyule, Bayside, Hobsons Bay, Kingston, Manningham, Monash, Moonee Valley, Whitehorse, Maroondah, Brimbank)
-   Any record with feature_preventing_development (equals TRUE) is excluded from the dataset
-   Only records with zoning_permits_housing = "Housing permitted" are kept
-   Only records with zone_short in one of ("Neighbourhood residential", "General residential", "Residential growth", "Residential Growth" or "Mixed use")
-   If the minimum of the walking distance to the tram and train is N/A - recording as the largest factor (\>2km).
-   Where walkability metrics are missing the mean of the local area is used
-   Outliers for walkability metrics, distance to closest rail, lot size and distance to cbd are 95% winsorised.
-   Any "Acreage" property types are omitted from the dataset
-   Any records with a missing sold date are omitted
-   Only records with sale dates between 2010 and 2018 are included (No data after 2018 is in the dataset)


### General feature engineering

-   The minimum of the walking distance to the tram and train is calculated as one variable for regression.
-   Outliers for walkability metrics, distance to closest rail, lot size and distance to cbd are 95% winsorised and saved as "wins\_{colname}".
-   Winsorised features are also transformed via log and inverse functions into new features
-   A new feature is made factorising lot sizes into the following categories: [0, 300), [300, 500), [500, 750), [750, 1000), [1000, 1250), and [1250, Inf).
-   All walkability metrics and distance to rail are factorised in the following categories: [0, 100), [100, 200), [200, 300), [300, 400), [400, 500), [500, 800), and [800, Inf).
-   Bedrooms and bathrooms features are factorised in the following categories: "1", "2", "3 or more".
-   Traffic pollution is factored into categories from 0 to 1 with a step of 0.1 and a category of [1, Inf).

#### House-specific feature engineering
- Only included categories "House", "Land" and "Townhouse"
- Only included lot_size > 150, lot_size < 3000
- price_per_sqm outliers beyond 3sd are removed

#### Apartment-specific feature engineering
- Only included category "Apartment" and "Unit"
- Only included apartments part of a group > 6
- Only included apartments priced between $25k and $2m


### Regression assumptions
- The validation process includes 10 repetitions of the training
- The training percentage of data is 99% vs testing at 1%
- The target for the house model is price per sqm, whereas the target for the apartment model is sale price.
- The apartment target is log-transformed while the house target is not.

## Questions
- In general when creating factors, many of the splits aren't equidistant. Was this through experimentation? Is this a split which helps create roughly equal groups?
- 65: I can see the merit in taking the minimum of the closest rail or tram line but in general I'd have thought the value of being closer to rail is higher than tram (which would be reflected in property value). Not sure how much it was experimented with but I think more feature engineering here could be valuable.
- 98: The comment refers to excluding units but acreage is excluded. Probably intended but just confirming.
- 108: I think starting 2010 is fine although GFC would have still partially impacted the market. Looking at the count of year values makes me think 2018 is only a partial dataset. Bigger question is that 2018 is quite old. I'm not sure of the data source but is there any way to get a more recent data supply? How confident are we that data from 7-15 years ago is a relevant descriptor of 2024's prices? It's better than nothing but seems to me a big caveat on any specific conclusions we draw.
- 133: There is an attempt from the >6 dwellings estimate to exclude townhouses from units which makes sense. Would townhouses be in the houses category though? Or are they excluded altogether?
- 145: The default train_test_pct is 0.99 which seems really high to me. I'm used to using more like 0.7-0.9 at the most. Did you find any issues with overfitting or wider variance of performance metrics? I think if there was a bit more testing pct you probably wouldn't need to worry about as many repetitions although 10 doesn't hurt and it doesnt take too long to run.
- 216: I'm just wondering why the house datasets price per sqm didn't need to be logged whereas the apartment datasets sale price was logged. From what I can see with hist() they are both heavily right skewed and I would've thought both would benefit from being logged.
