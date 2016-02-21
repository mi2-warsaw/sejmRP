library(jsonlite)

# lista posłów
url <- 'http://mamprawowiedziec.pl/api/json/list/mpw/sejm'
document <- jsonlite::fromJSON(txt = url)

poslowie <- list()

for (id in document[[1]]) {
  document2 <- jsonlite::fromJSON(txt = paste0("http://mamprawowiedziec.pl/api/json/field/mpw/osoba/",id))
  poslowie[[id]] <- document2
}

df <- data.frame(id = names(poslowie),
            nazwisko = sapply(poslowie, function(x) x$nazwisko),
           imie = sapply(poslowie, function(x) x$imie))

rownames(df) <- paste(df$imie, df$nazwisko)

head(df,15)

surnames <- sort(unique(allStatements$surname_name))

kluby <- sapply(surnames, function(sn) {
  nam1 <- strsplit(sn, split=" ")[[1]]
  wm <- sapply(rownames(df), function(mc) {
    nam2 <- strsplit(mc, split=" ")[[1]]
    length(intersect(nam1, nam2))
  }) 
  which.max(wm)
})

slownikSurnamesMPW <- data.frame(surnames, df[kluby,])


save(slownikSurnamesMPW, file="slownikSurnamesMPW.rda")
