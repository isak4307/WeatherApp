# Weather Application
This is a weather application that can display weather data through a discord bot or through a ghci terminal.

## Data Sources (Credits):
  This application uses data from the following sources:
  - Weather data from [MET Norway](https://www.met.no/)
    Which is licensed under [Norwegian License for Open Government Data (NLOD) 2.0](https://data.norge.no/nlod/en/2.0) and [Creative Commons 4.0 BY International](https://creativecommons.org/licenses/by/4.0/)
    For more information visit: https://api.met.no/doc/License

  - Geographical data from [Nominatim/OpenStreetMap](https://nominatim.org/)
    Which is licensed under [ODbl](https://opendatacommons.org/licenses/odbl/)
    For more information visit: https://www.openstreetmap.org/copyright

  By using this application, you hereby comply with their Terms of Serivce as well as mine.

## Build
This application is built on Cabal

## Features
The application can run through a discord bot or through the ghci terminal.
### Terminal
The default is that it runs through the terminal, thus all you need to do is build the project (cabal build) and run it (cabal run)
**NB**: If your terminal isn't using utf-8 or you get this error: *<stdout>: commitBuffer: invalid argument (cannot encode character ...)*, then you have to change the encoding to utf-8. To solve this in Windows,  write *chcp 65001* in the terminal (for MAC and Linux IDK)
### Discord bot
1. To make it run through the discord bot you first have to create a txt-file called *token* in the root directory. 
2. Add the discord bot token. Without a token, the application won't be able to run.
3. Run the code by uncommenting *discord* and comment out *terminal* in [Main.hs](app/Main.hs)

## Commands
Here are the available commands:
- weather: Display the weather given a city, (optionally) a country and (optionally) a date.
The date format needs to be in ISO8601 => YYYY-MM-DDTHH:mm:ssZ
If no date has been inputted, the default value would be the current date and the nearest hour  
**!weather City, ?Country, Date**
- location: Display the latitude and longitude values given a city (and optionally a country) rounded to 4 decimals.  
  **!location City,?Country**
- minMax: Get the highest and lowest temperature given city,country and date
The date format needs to be in ISO8601 => YYYY-MM-DDTHH:mm:ssZ  
**!minMax City,Country,date**
- quit: Exit the application (currently doesn't work in discord bot)
**!quit**
- help: Display all available commands
**!help**




