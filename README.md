# Pandemic touch
 
## Notes from data collection

9th November 2020: issues with Danish language survey, questions Q11 and Q50.  Ignore Danish respondents’ responses prior to this. Fix published at 12:15 Swedish time. 

23rd March 2021, 16:16 CET: added question about vaccinations (own and household members’) to the survey in all languages. 

The location question is there several times since that was the only way to make the alphabetisation of countries to make sense in different language settings. Participants only saw one of those depending on their language setting.

Apparent duplicate questions:

- Q20 is shown to people who live alone, Q65 is shown to people who do NOT live alone

- Q21 is shown to people who live alone, Q66 is shown to people who do NOT live alone

- Q22 is shown to people who live alone, Q67 is shown to people who do NOT live alone

- Q45 is shown to people who do NOT live alone, Q63 is shown to people who live alone

Q27 scale: no!, no, more or less, yes, yes!

## Data processing steps in process_data.R

- merge country of residence questions 

- merge duplicate questions for those living alone/not alone

- exclude those who did not give consent (7 excluded)

- exclude those who did not finish the survey (168 exlcuded, too strict?)

- set data to NA for Q11 and Q50 when language is Danish and time < 12:15 9th Nov 2020 (8 cases)

- calculate age from year of birth and survey completion year

- collapse n. cohabiting categories: 4, 5, > 5 to > 3 (n = 24, 4 and 5)

- recode ordinal scales as factors with the correct order
    + Number Cohabiting
    + Wanted Touch ...

### To do

- recode more ordinal scales as factors with the correct order

- do something with free-text options

## Analysis 
Planned analyses: https://docs.google.com/document/d/12VXfi6zQshHtaW3PI8TcdygAFz4hHToMb_zJvOmUaFc/edit 

- analysis_cohabitation-and-wanting-touch.R: Individuals living alone during lockdowns will be more lonely and touch-hungry than those living with others (but may also be experiencing less unwanted touch)

### To do

- video-related questions (especially live alone / with someone comparisons, but also others)