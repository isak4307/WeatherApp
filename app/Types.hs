{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- \|
-- Module      : Types
-- Description : Contains all datatypes and classes created.
--               All modified instances and lenses are also created here.
module Types where

import Control.Lens (makeLenses)
import Data.Aeson (FromJSON)
import Data.Aeson.Types (FromJSON (parseJSON), withObject, (.:), (.:?))
import Data.Text (Text, pack)
import Data.Time (UTCTime)
import Servant.Client (ClientError)
import Text.Megaparsec (Parsec)

-- | Weather data from JSON according to the met api
data WeatherData = WeatherData
  { _weatherType :: Maybe Text,
    _properties :: Maybe Properties
  }

instance FromJSON WeatherData where
  parseJSON = withObject "WeatherData" $ \v ->
    WeatherData
      <$> v .:? "type"
      <*> v .:? "properties"

data Geometry = Geometry
  { _geoType :: Maybe Text,
    _coordinates :: Maybe [Double]
  }

instance FromJSON Geometry where
  parseJSON = withObject "Geometry" $ \v ->
    Geometry
      <$> v .:? "type"
      <*> v .:? "coordinates"

data TimeSeries = TimeSeries
  { _time :: Maybe UTCTime,
    _timeData :: Maybe TimeSeriesData
  }

instance FromJSON TimeSeries where
  parseJSON = withObject "TimeSeries" $ \v ->
    TimeSeries
      <$> v .:? "time"
      <*> v .:? "data"

data TimeSeriesData = TimeSeriesData
  { _instant :: Maybe InstantData,
    _next12Hours :: Maybe SumDetail,
    _next1Hour :: Maybe SumDetail,
    _next6Hours :: Maybe SumDetail
  }

instance FromJSON TimeSeriesData where
  parseJSON = withObject "TimeSeriesData" $ \v ->
    TimeSeriesData
      <$> v .:? "instant"
      <*> v .:? "next_12_hours"
      <*> v .:? "next_1_hours"
      <*> v .:? "next_6_hours"

data SumDetail = SumDetail
  { _summary :: Maybe Summary,
    _details :: Maybe SummaryDetails
  }

instance FromJSON SumDetail where
  parseJSON = withObject "SumDetail" $ \v ->
    SumDetail
      <$> v .:? "summary"
      <*> v .:? "details"

newtype Summary = Summary
  { _symbolCode :: Maybe Text
  }

instance FromJSON Summary where
  parseJSON = withObject "Summary" $ \v -> Summary <$> v .:? "symbol_code"

newtype InstantData = InstantData
  { _detail :: Maybe InstantDetails
  }

instance FromJSON InstantData where
  parseJSON = withObject "InstantData" $ \v -> InstantData <$> v .:? "details"

data Properties = Properties
  { _meta :: Maybe Meta,
    _timeseries :: Maybe [TimeSeries]
  }

instance FromJSON Properties where
  parseJSON = withObject "Properties" $ \v ->
    Properties
      <$> v .:? "meta"
      <*> v .:? "timeseries"

data Meta = Meta
  { _updatedAt :: Maybe UTCTime,
    _units :: Maybe Units
  }

instance FromJSON Meta where
  parseJSON = withObject "Meta" $ \v ->
    Meta
      <$> v .:? "updated_at"
      <*> v .:? "units"

data Units = Units
  { _airPressureAtSeaLevelU :: Maybe Text,
    _airTemperatureU :: Maybe Text
  }

instance FromJSON Units where
  parseJSON = withObject "Units" $ \v ->
    Units
      <$> v .:? "air_pressure_at_sea_level"
      <*> v .:? "air_temperature"

data InstantDetails = InstantDetails
  { _airPressureAtSeaLevel :: Maybe Double,
    _airTemperature :: Maybe Double,
    _cloudAreaFraction :: Maybe Double,
    _windFromDirection :: Maybe Double,
    _windSpeed :: Maybe Double
  }

instance FromJSON InstantDetails where
  parseJSON = withObject "InstantDetails" $ \v ->
    InstantDetails
      <$> v .:? "air_pressure_at_sea_level"
      <*> v .:? "air_temperature"
      <*> v .:? "cloud_area_fraction"
      <*> v .:? "wind_from_direction"
      <*> v .:? "wind_speed"

data SummaryDetails = SummaryDetails
  { _probabilityOfPrecipitation :: Maybe Double,
    _precipitationAmount :: Maybe Double,
    _precipitationAmountMax :: Maybe Double,
    _precipitationAmountMin :: Maybe Double,
    _probabilityOfThunder :: Maybe Double,
    _airTemperatureMax :: Maybe Double,
    _airTemperatureMin :: Maybe Double
  }

instance FromJSON SummaryDetails where
  parseJSON = withObject "SummaryDetails" $ \v ->
    SummaryDetails
      <$> v .:? "probability_of_precipitation"
      <*> v .:? "precipitation_amount"
      <*> v .:? "precipitation_amount_max"
      <*> v .:? "precipitation_amount_min"
      <*> v .:? "probability_of_thunder"
      <*> v .:? "air_temperature_max"
      <*> v .:? "air_temperature_min"

-- | GeoLocation data type
data GeoLocation = GeoLocation
  { name :: Text,
    lat :: Double,
    lon :: Double
  }

instance FromJSON GeoLocation where
  parseJSON = withObject "GeoLocation" $ \v ->
    GeoLocation
      <$> v .: "name"
      <*> v .: "lat"
      <*> v .: "lon"

-- | GeoLocation data type from JSON according to the nominatim API
data NominatimResponse = NominatimResponse
  { placeId :: Int,
    latResponse :: Text,
    lonResponse :: Text,
    displayName :: Text
  }

instance FromJSON NominatimResponse where
  parseJSON = withObject "NominatimResponse" $ \v ->
    NominatimResponse
      <$> v .: "place_id"
      <*> v .: "lat"
      <*> v .: "lon"
      <*> v .: "display_name"

-- Sunset/Sunrise

-- | Sunset and Sunrise datatype
data Sun = Sun
  { cityS :: Text,
    countryS :: Maybe Text,
    dateS :: Text
  }

-- | Sunset and Sunrise datatype for JSON from the met API
newtype SunData = SunData {_sunProperties :: Maybe SunProperties}

instance FromJSON SunData where
  parseJSON = withObject "SunData" $ \v ->
    SunData
      <$> v .:? "properties"

data SunProperties = SunProperties
  { _sunBody :: Maybe Text,
    _sunrise :: Maybe Sunrise,
    _sunset :: Maybe Sunset
  }

instance FromJSON SunProperties where
  parseJSON = withObject "properties" $ \v ->
    SunProperties
      <$> v .:? "body"
      <*> v .:? "sunrise"
      <*> v .:? "sunset"

data Sunrise = Sunrise
  { _sunriseTime :: Maybe UTCTime,
    _sunriseAzimuth :: Maybe Double
  }

instance FromJSON Sunrise where
  parseJSON = withObject "sunrise" $ \v ->
    Sunrise
      <$> v .:? "time"
      <*> v .:? "azimuth"

data Sunset = Sunset
  { _sunsetTime :: Maybe UTCTime,
    _sunsetAzimuth :: Maybe Double
  }

instance FromJSON Sunset where
  parseJSON = withObject "sunrise" $ \v ->
    Sunset
      <$> v .:? "time"
      <*> v .:? "azimuth"

-- | Data types Commands
data Cmd
  = Quit
  | Help
  | WeatherCmd Text
  | LocationCmd Text
  | MinMaxWeatherCmd Text
  | WeekWeatherCmd Text
  | SunCmd Text
  deriving (Eq, Show)

-- | Error Types
type Parser = Parsec PError Text

-- | Custom subcategories for different error types
data PError
  = EmptyInput
  | UnknownCommand Text
  | ArgumentError Text
  | InvalidFormat Text
  deriving (Ord, Eq, Show)

-- | Custom error types
data ErrorTypes
  = APIError ClientError
  | MissingVal Text
  | ParseErr PError
  deriving (Eq)

-- | Weather data type
data Weather = Weather
  { city :: Text,
    country :: Maybe Text,
    date :: Maybe UTCTime
  }

-- | Location data type
data Location = Location
  { cityL :: Text,
    countryL :: Maybe Text
  }
  deriving (Show)

-- | Converts datatype to a text (is very similar to class Show)
class ToText a where
  toText :: a -> Text

instance ToText InstantDetails where
  toText inDetails =
    "------------------------------------ \n"
      <> "- Temperature: "
      <> maybe (toText $ MissingVal "Temperature not available") (\t -> pack (show t) <> "°C") (_airTemperature inDetails)
      <> "\n"
      <> "- Wind: "
      <> maybe (toText $ MissingVal "Wind not available") (\w -> pack (show w) <> " km/h") (_windSpeed inDetails)
      <> "\n"
      <> "- Wind Direction: "
      <> maybe (toText $ MissingVal "wind direction not available") (\d -> (windDir . degToDir) d <> " (" <> pack (show d) <> "°)") (_windFromDirection inDetails)
      <> "\n"
      <> "- Cloud Coverage: "
      <> maybe (toText $ MissingVal "CloudCoverage not available") (\c -> pack (show c) <> "%") (_cloudAreaFraction inDetails)
      <> "\n"
      <> "- Air Pressure: "
      <> maybe (toText $ MissingVal "Airpressure not available") (\p -> pack (show p) <> " hPa") (_airPressureAtSeaLevel inDetails)
      <> "\n------------------------------------"

instance ToText GeoLocation where
  toText (GeoLocation name' lat' lon') =
    name' <> "\n(lat,lon) ==> (" <> pack (show lat') <> ", " <> pack (show lon') <> ")"

instance ToText Sun where
  toText (Sun city' (Just country') date') = "City: " <> city' <> "\n Country: " <> country' <> "\n Date: " <> pack (show date')
  toText (Sun city' Nothing date') = "City: " <> city' <> "\n Date: " <> pack (show date')

instance ToText Sunrise where
  toText sunrise =
    "Sunrise at time: "
      <> maybe (toText $ MissingVal "Date not available") (\t -> pack (show t) <> "\n") (_sunriseTime sunrise)
      <> maybe (toText $ MissingVal "Sunrise direction not available") (\dir -> "Sunrise direction: " <> degToDir dir <> " (" <> pack (show dir) <> "°)") (_sunriseAzimuth sunrise)

instance ToText Sunset where
  toText sunset =
    "Sunset at time: "
      <> maybe (toText $ MissingVal "Date not available") (\t -> pack (show t) <> "\n") (_sunsetTime sunset)
      <> maybe (toText $ MissingVal "Sunset direction not available") (\dir -> "Sunset direction: " <> degToDir dir <> " (" <> pack (show dir) <> "°)") (_sunsetAzimuth sunset)

instance ToText PError where
  toText (UnknownCommand cmd) = "Unknown command => " <> cmd
  toText EmptyInput = "EmptyInput => No input provided."
  toText (ArgumentError txt) = "ArgumentError => " <> txt
  toText (InvalidFormat txt) = "InvalidFormat => The following argument " <> txt <> " is invalid"

instance ToText ErrorTypes where
  toText (MissingVal msg) = "Missing value => " <> msg
  toText (ParseErr err) = "Parse error => " <> toText err
  toText (APIError err) = "API error => " <> pack (show err)

instance ToText Weather where
  toText (Weather city' (Just country') (Just date')) = "City: " <> city' <> "\n Country: " <> country' <> "\n Time: " <> pack (show date')
  toText (Weather city' Nothing (Just date')) = "City: " <> city' <> "\nTime: " <> pack (show date')
  toText (Weather city' (Just country') Nothing) = "City: " <> city' <> "\nCountry: " <> country'
  toText (Weather city' _ _) = "City: " <> city'

instance ToText Location where
  toText (Location city' (Just country')) = "City: " <> city' <> " Country: " <> country'
  toText (Location city' _) = "City: " <> city'

instance ToText UTCTime where
  toText t = pack $ show t

-- | Converts the direction degree to text directions
degToDir :: Double -> Text
degToDir direction
  | direction >= 0 && direction < 90 = "(north)"
  | direction >= 90 && direction < 180 = "(east)"
  | direction >= 180 && direction < 270 = "(south)"
  | direction >= 270 && direction < 360 = "(west)"
  | otherwise = toText $ InvalidFormat "Invalid input :"

-- | Adds a directional arrow for where the wind comes from
windDir :: Text -> Text
windDir t
  | "(north)" == t = "↓ " <> t
  | "(east)" == t = "← " <> t
  | "(south)" == t = "↑ " <> t
  | "(west)" == t = "→ " <> t
  | otherwise = ""

-- | Create lenses for the custom datatypes
makeLenses ''WeatherData
makeLenses ''Geometry
makeLenses ''Properties
makeLenses ''Meta
makeLenses ''Units
makeLenses ''TimeSeries
makeLenses ''TimeSeriesData
makeLenses ''InstantData
makeLenses ''InstantDetails
makeLenses ''SummaryDetails
makeLenses ''Summary
makeLenses ''SumDetail
makeLenses ''SunData
makeLenses ''Sunrise
makeLenses ''Sunset
makeLenses ''SunProperties