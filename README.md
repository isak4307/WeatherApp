# Weather Application
This is a weather application that can display weather data through a discord bot or through a ghci terminal.

## Data Sources (Credits):
  This application uses data from the following sources:
  - Weather data and sunset/sunrise data from [MET Norway](https://www.met.no/)
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
The default is that it runs through the terminal, thus all you need to do is build the project (*cabal build*) and run it (*cabal run*)  
**Warning**: If your terminal isn't using utf-8 or you get this error: *<stdout>: commitBuffer: invalid argument (cannot encode character ...)*, then you have to change the encoding to utf-8. To solve this in Windows,  write *chcp 65001* in the terminal.  
For Mac and Linux, it shouldn't be a problem since its standard is utf-8  
If the issue still persists. Then get rid of the arrow characters in the function [winDir](app\Types.hs).
### Discord bot
Here is a detailed step by step tutorial on setting up the discord bot.  
If you already have a bot set up and have its token, you can skip to step **7**
1. To make it run through the discord bot you first have to create a discord bot [here](https://discord.com/developers/applications)
2. Then you need to go to installation page, and check of *User Install* and *Guild Install*
3. Afterwards in the same page, enable *application.commands* scope for User Install.  
   Whilst for Guild Install, it should have *application.commands* and *bot*
4. Add *Send Messages*, *Manage Messages*, *Read Message History* and *View Audit Log* under the Permissions section in the Installation page
5. Finally, invite the bot to your server or to your direct messages.
6. Grab the token that should be in the Bot page. If you don't have it, then reset the token.
7. Create a txt-file called *token* in the root directory. 
8. Add the discord bot token to the file. Without a token, the application won't be able to run.
9. Run the code by uncommenting *discord* and comment out *terminal* in [Main.hs](app/Main.hs)

## Bugs and Issues  
There is a problem when handling utf-8 characters in the terminal.  
Meaning, if the input value contains such characters (example: "ÆØÅ"), they won't be parsed correctly and the code treats them as empty characters.  
This issue only affects users using the terminal. The discord bot is able to handle such characters.
### Update  
It seems that the issue is only in my vsc terminal? Tried testing it in another terminal such as git bash, and it worked perfectly fine.  
Still not sure how to solve this issue, but at least this might indicate that this is not a program issue, but rather individual terminal issue.  

Another small issue is that the discord bot won't be able to shutdown/terminate when the command *!quit* is written to it.  

## Commands
Here are the available commands:
- weather: Display the weather given a city, (optionally) a country and (optionally) a date.  
  The date format needs to be in ISO8601 => YYYY-MM-DDTHH:mm:ssZ.  
  If no date has been inputted, the default value would be the current date and the nearest hour.  
**!weather City, ?Country, ?Date**

- location: Display the latitude and longitude values given a city and (optionally) a country rounded to 4 decimals.
**!location City, ?Country**

- minMax: Get the highest and lowest temperature given city, (optionally) a country and (optionally) date for the day.  
  The date format needs to be in ISO8601 =>YYYY-MM-DDTHH:mm:ssZ.      
**!minMax City, ?Country, Date**

- week: Display the weather data for the next 7 days given a city, (optionally) a country and (optionally) a date.  
  The date format needs to be in ISO8601 =>YYYY-MM-DDTHH:mm:ssZ.  
  The weather data is rounded to the nearest quarter of the day, if it doesn't have the weather data for the exact time.  
**!week City, ?Country, ?Date**

- sun: Get the sunrise and sunset time and its direction given the city, (optionally) a country and a date.  
  The date format is different from the other ones and needs to be like this: YYYY-MM-DD.  
**!sun City,?Country, Date**

- quit: Exit the application (currently doesn't work if the user runs it through discord bot)  
**!quit**
- help: Display all available commands.  
  **!help**
