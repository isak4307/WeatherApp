{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Text (Text, unpack)
import qualified Network.HTTP.Client.TLS as NT
import Servant
  ( Get,
    Header,
    JSON,
    Proxy (..),
    QueryParam,
    throwError,
    (:>),
  )
import Servant.Client (BaseUrl (..), ClientM, Scheme (Https), client, mkClientEnv, runClientM)
import Types

-- | Datatype used to construct the GEOAPI link
type GeoAPI =
  "search"
    :> QueryParam "city" Text
    :> QueryParam "country" Text
    :> QueryParam "format" Text
    :> Header "User-Agent" Text
    :> Get '[JSON] [NominatimResponse]

-- | Datatype used to construct the WeatherAPI link
type WeatherAPI =
  "weatherapi"
    :> "locationforecast"
    :> "2.0"
    :> "compact"
    :> QueryParam "lat" Double
    :> QueryParam "lon" Double
    :> Header "User-Agent" Text
    :> Get '[JSON] WeatherData

-- Weather

-- | Creates the appropriate link for the get request adding the queryparams and the user-agent values
requestConstructor :: Maybe Double -> Maybe Double -> Maybe Text -> ClientM WeatherData
requestConstructor = client (Proxy :: Proxy WeatherAPI)

-- | The base url for the weather api
baseWeatherURL :: BaseUrl
baseWeatherURL = BaseUrl {baseUrlScheme = Https, baseUrlHost = "api.met.no", baseUrlPort = 443, baseUrlPath = ""}

-- | Fetch the weather data
fetchWeather :: Weather -> ExceptT ErrorTypes IO WeatherData
fetchWeather weather = do
  loc <- fetchGeoLocation $ Location (city weather) (country weather)
  case loc of
    [] -> throwError $ MissingVal "Was unable to fetch the locations"
    -- Take the first response and its lat lon
    geoLoc : _ -> do
      mn <- liftIO NT.newTlsManager
      let req = requestConstructor (Just $ lat geoLoc) (Just $ lon geoLoc) (Just "https://git.app.uib.no/Isak.Yau")
      let envClient = mkClientEnv mn baseWeatherURL
      -- runClientM is a Either ClientError a
      result <- liftIO $ runClientM req envClient
      case result of
        Left err -> throwE $ APIError err
        Right v -> return v

-- Geo Locations

-- | Creates the appropriate link for the get request adding the queryparams and the user-agent values
requestGeoConstructor :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ClientM [NominatimResponse]
requestGeoConstructor = client (Proxy :: Proxy GeoAPI)

-- | The base url for the weather api
baseGeoURL :: BaseUrl
baseGeoURL = BaseUrl {baseUrlScheme = Https, baseUrlHost = "nominatim.openstreetmap.org", baseUrlPort = 443, baseUrlPath = ""}

-- | Fetches the GeoLocation value given a Location name
fetchGeoLocation :: Location -> ExceptT ErrorTypes IO [GeoLocation]
fetchGeoLocation loc = do
  mn <- liftIO NT.newTlsManager
  let req = requestGeoConstructor (Just $ cityL loc) (countryL loc) (Just "jsonv2") (Just "https://git.app.uib.no/Isak.Yau")
  let envClient = mkClientEnv mn baseGeoURL
  result <- liftIO $ runClientM req envClient
  case result of
    Left err -> throwE $ APIError err
    Right response ->
      if null response
        then
          throwError $ MissingVal "Unable to fetch the given location"
        else
          return $ convertToGeoLocation <$> response

-- | Helper function to round to 4 decimals for the latitude and longitude values
round4Dec :: Double -> Double
round4Dec x = fromIntegral (round (x * 10000)) / 10000

-- | Helper function to convert from NominatimResponse datatype to GeoLocation
convertToGeoLocation :: NominatimResponse -> GeoLocation
convertToGeoLocation nomRes = GeoLocation (displayName nomRes) (round4Dec $ read $ unpack $ latResponse nomRes) (round4Dec $ read $ unpack $ lonResponse nomRes)
