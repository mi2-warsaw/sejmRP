# install.packages("SPARQL")
library(SPARQL)

endpoint <- "http://linkedpolitics.ops.few.vu.nl/sparql/"

q <- "SELECT ?text ?date ?agendaitemnr ?speechnr
WHERE { 
?sessionday rdf:type lpv_eu:SessionDay .
?sessionday dcterms:date ?date.  
?sessionday dcterms:hasPart ?agendaitem.
?agendaitem dcterms:hasPart ?speech.

?agendaitem lpv:number ?agendaitemnr.
?speech lpv:number ?speechnr.
?speech lpv:spokenText ?text.
FILTER ( ?date >= '2009-05-06'^^xsd:date && ?date <= '2010-05-06'^^xsd:date ) 
FILTER(langMatches(lang(?text), 'en'))

} ORDER BY ?date ?agendaitemnr ?speechnr LIMIT 100"
res <- SPARQL(endpoint, q)$results
res$date <- as.Date(as.POSIXct(res$date, origin = "1970-01-01"))


q <- "SELECT ?partyname (COUNT(DISTINCT ?speech) AS ?speechno)
WHERE {
<http://purl.org/linkedpolitics/eu/plenary/2010-12-16_AgendaItem_4> dcterms:hasPart ?speech.
?speech lpv:spokenAs ?function.
?function lpv:institution ?party.
?party rdf:type lpv:EUParty.
?party lpv:acronym ?partyname.
} GROUP BY ?partyname"
res2 <- SPARQL(endpoint, q)$results



q <- "SELECT ?partyname (COUNT(DISTINCT ?speech) AS ?speechno)
WHERE {
?ai rdf:type <http://purl.org/linkedpolitics/vocabulary/eu/plenary/AgendaItem>.
?ai dcterms:hasPart ?speech.
?speech lpv:speaker ?speaker.
?speaker lpv:countryOfRepresentation ?country.
?country rdfs:label ?label.
filter(?label='Poland'@en)}"
res3 <- SPARQL(endpoint, q)$results
