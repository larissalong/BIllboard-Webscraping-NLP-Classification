# Billboard-Webscraping-NLP-Project

## Objectives: 
We are a group of music lovers from MS Business Analytics Program 2018, UC San Diego. By studying the lyrics of hot 100 songs on Billboard year-end from year 1968 to 2017, we would like to answer the below questions:

1. How did music topics change over time? 

2. Are pop song lyrics getting more repetitive?

3. How did sentiment on popular song lyrics change over time?

4. Can we classify the genre of a song based on its lyrics? 


## Data sources: 
1. Song name: Hot 100 ranking: https://en.wikipedia.org/wiki/Billboard_Year-End
2. Song lyrics: 
    https://www.azlyrics.com/
    
    http://www.metrolyrics.com/

3. Song genre: Google search

## Web scraping process:
1. Hot 100 ranking: Extract datatable from wikipedia by year 
2. Lyrics: from the above two websites
3. Genre: google search to fetch the below variables:
    
    a. Song name
    
    b. Artist name
    
    c. Lyrics
    
    d. Genre: Pop, Rock, R&B, Soul, Hip-hop, Rap, Country, Dance, Alternative-indie, Blues, Punk, Metal, etc
    
    e. Ranking
    
    f. Ranking Year

## Text mining:
We conducted text mining by different dimensions and metrics:

1. Dimension: By music genre / decades / ranking

2. Metrics: 
    
    a. Word use: frequency / repetition / diversity
    
    b. Top Artists based on ranking
    
    c. Sentiment analysis
    
    d. Part-of-speech tagging
    
    e. Topic modeling
    
    f. Repetition
 
## Classification:
To predict whether a song belongs to a particular song genre or not solely based on lyrics, we used the below approaches to conduct feature engineering:
1. Use word count of the lyrics as predictors
2. Change word count to binary 0 / 1 (i.e., whether a certain word appears or not), then re-run models to  see if any improvement on accuracy
3. Use Word2vec to predict 2-level / multiple-level classifications 

We used SVM and logistic regression models in this project.

## For more details:
Please see:
1. web scraping code in billboard.R
2. text mining code in lyrics text mining.R
3. classification code in model.R
4. Presentation deck in Text Mining Project Deck
