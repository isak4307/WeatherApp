{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens (makeLenses)
import Data.Aeson (FromJSON)
import Data.Aeson.Types (FromJSON (parseJSON), withObject, (.:), (.:?))
import Data.Text (Text, pack, unpack)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Servant.Client (ClientError)
import Text.Megaparsec (Parsec)

-- | Weather data from JSON according to the met api
data WeatherData = WeatherData
  { _weatherType :: Maybe Text,
    _properties :: Maybe Properties
  }
  deriving (Show, Generic)

instance FromJSON WeatherData where
  parseJSON = withObject "WeatherData" $ \v ->
    WeatherData
      <$> v .:? "type"
      <*> v .:? "properties"

data Geometry = Geometry
  { _geoType :: Maybe Text,
    _coordinates :: Maybe [Double]
  }
  deriving (Show, Generic)

instance FromJSON Geometry where
  parseJSON = withObject "Geometry" $ \v ->
    Geometry
      <$> v .:? "type"
      <*> v .:? "coordinates"

data TimeSeries = TimeSeries
  { _time :: Maybe UTCTime,
    _timeData :: Maybe TimeSeriesData
  }
  deriving (Show, Generic)

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
  deriving (Show, Generic)

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
  deriving (Show, Generic)

instance FromJSON SumDetail where
  parseJSON = withObject "SumDetail" $ \v ->
    SumDetail
      <$> v .:? "summary"
      <*> v .:? "details"

newtype Summary = Summary
  { _symbolCode :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON Summary where
  parseJSON = withObject "Summary" $ \v -> Summary <$> v .:? "symbol_code"

newtype InstantData = InstantData
  { _detail :: Maybe InstantDetails
  }
  deriving (Show, Generic)

instance FromJSON InstantData where
  parseJSON = withObject "InstantData" $ \v -> InstantData <$> v .:? "details"

data Properties = Properties
  { _meta :: Maybe Meta,
    _timeseries :: Maybe [TimeSeries]
  }
  deriving (Show, Generic)

instance FromJSON Properties where
  parseJSON = withObject "Properties" $ \v ->
    Properties
      <$> v .:? "meta"
      <*> v .:? "timeseries"

data Meta = Meta
  { _updatedAt :: Maybe UTCTime,
    _units :: Maybe Units
  }
  deriving (Show, Generic)

instance FromJSON Meta where
  parseJSON = withObject "Meta" $ \v ->
    Meta
      <$> v .:? "updated_at"
      <*> v .:? "units"

data Units = Units
  { _airPressureAtSeaLevelU :: Maybe Text,
    _airTemperatureU :: Maybe Text
  }
  deriving (Show, Generic)

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
  deriving (Generic)

instance FromJSON InstantDetails where
  parseJSON = withObject "InstantDetails" $ \v ->
    InstantDetails
      <$> v .:? "air_pressure_at_sea_level"
      <*> v .:? "air_temperature"
      <*> v .:? "cloud_area_fraction"
      <*> v .:? "wind_from_direction"
      <*> v .:? "wind_speed"

instance Show InstantDetails where
  show inDetails =
    "- Temperature: "
      <> maybe (show $ MissingVal "Temperature not available") (\t -> show t <> "°C") (_airTemperature inDetails)
      <> "\n"
      <> "- Wind: "
      <> maybe (show $ MissingVal "Wind not available") (\w -> show w <> " km/h") (_windSpeed inDetails)
      <> "\n"
      <> "- WindDirection: "
      -- Have to unpack instead of show for utf-8 to work (arrows)
      <> maybe (show $ MissingVal "Winddirection not available") (\d -> unpack (windDir d) <> " (" <> show d <> "°)") (_windFromDirection inDetails)
      <> "\n"
      <> "- Cloud Coverage: "
      <> maybe (show $ MissingVal "CloudCoverage not available") (\c -> show c <> "%") (_cloudAreaFraction inDetails)
      <> "\n"
      <> "- Air Pressure: "
      <> maybe (show $ MissingVal "Airpressure not available") (\p -> show p <> " hPa") (_airPressureAtSeaLevel inDetails)

-- | Converts the windirection degree to text directions
windDir :: Double -> Text
windDir direction
  | direction >= 0 && direction < 90 = "↓ (north)"
  | direction >= 90 && direction < 180 = "← (east)"
  | direction >= 180 && direction < 270 = "↑ (south)"
  | direction >= 270 && direction < 360 = "→ (west)"
  | otherwise = pack $ show (InvalidFormat "Invalid input :")

data SummaryDetails = SummaryDetails
  { _probabilityOfPrecipitation :: Maybe Double,
    _precipitationAmount :: Maybe Double,
    _precipitationAmountMax :: Maybe Double,
    _precipitationAmountMin :: Maybe Double,
    _probabilityOfThunder :: Maybe Double,
    _airTemperatureMax :: Maybe Double,
    _airTemperatureMin :: Maybe Double
  }
  deriving (Show, Generic)

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

-- | GeoLocation data types
data GeoLocation = GeoLocation
  { name :: Text,
    lat :: Double,
    lon :: Double
  }
  deriving (Generic)

instance FromJSON GeoLocation where
  parseJSON = withObject "GeoLocation" $ \v ->
    GeoLocation
      <$> v .: "name"
      <*> v .: "lat"
      <*> v .: "lon"

instance Show GeoLocation where
  show (GeoLocation name' lat' lon') =
    unpack name'
      <> "\n"
      <> "(lat,lon) ==> ("
      <> show lat'
      <> ", "
      <> show lon'
      <> ")"

data NominatimResponse = NominatimResponse
  { placeId :: Int,
    latResponse :: Text,
    lonResponse :: Text,
    displayName :: Text
  }
  deriving (Show, Generic)

instance FromJSON NominatimResponse where
  parseJSON = withObject "NominatimResponse" $ \v ->
    NominatimResponse
      <$> v .: "place_id"
      <*> v .: "lat"
      <*> v .: "lon"
      <*> v .: "display_name"

-- | Data types Commands
data Cmd
  = Quit
  | Help
  | WeatherCmd Text
  | LocationCmd Text
  | MinMaxWeatherCmd Text
  | WeekWeatherCmd Text
  deriving (Show, Eq)

-- | Error Types
type Parser = Parsec PError Text

data PError
  = EmptyInput
  | UnknownCommand Text
  | ArgumentError Text
  | InvalidFormat Text
  deriving (Ord, Eq)

instance Show PError where
  show EmptyInput = "EmptyInput"
  show (UnknownCommand v) = "UnknownCommand: " <> unpack v
  show (ArgumentError v) = "ArgumentError: " <> unpack v
  show (InvalidFormat v) = "InvalidFormat: " <> unpack v

data ErrorTypes
  = APIError ClientError
  | MissingVal Text
  | ParseErr PError
  deriving (Eq)

instance Show ErrorTypes where
  show (APIError c) = "APIError: " <> show c
  show (MissingVal val) = "MissingVal: " <> unpack val
  show (ParseErr pe) = "ParseError ==> " <> show pe

-- | Input weather data type
data Weather = Weather
  { city :: Text,
    country :: Maybe Text,
    date :: Maybe UTCTime
  }

instance Show Weather where
  show (Weather city' (Just country') (Just date')) = "City: " <> unpack city' <> "\n Country: " <> unpack country' <> "\n Time: " <> show date'
  show (Weather city' Nothing (Just date')) = "City: " <> unpack city' <> "\nTime: " <> show date'
  show (Weather city' (Just country') Nothing) = "City: " <> unpack city' <> "\nCountry: " <> unpack country'
  show (Weather city' _ _) = "City: " <> unpack city'

-- | Location data type
data Location = Location
  { cityL :: Text,
    countryL :: Maybe Text
  }

instance Show Location where
  show (Location city' (Just country')) = "City: " <> unpack city' <> " Country: " <> unpack country'
  show (Location city' _) = "City:" <> unpack city'

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