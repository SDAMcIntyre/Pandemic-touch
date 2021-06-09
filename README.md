# Pandemic touch
 
## Notes from data collection

9th November 2020: issues with Danish language survey, questions Q11 and Q50.  Ignore Danish respondents’ responses prior to this. Fix published at 12:15 Swedish time. 

23rd March 2021, 16:16 CET: added question about vaccinations (own and household members’) to the survey in all languages. 

The location question is there several times since that was the only way to make the alphabetisation of countries to make sense in different language settings. Participants only saw one of those depending on their language setting.

## Data processing steps

- exclude those who did not give consent (7 excluded)

- exclude those who did not finish the survey (168 exlcuded, too strict?)

- set data to NA for Q11 and Q50 when language is Danish and time < 12:15 9th Nov 2020 (8 excluded)

- calculate age from year of birth and survey completion year