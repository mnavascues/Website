source("fun.R")

# read file exported from Zotero
production = read.csv("database.csv")
production = production[order(-production$Publication.Year), ]

extra_fields = extract_extra_fields(production$Extra)
production = cbind(production, GoogleScholar = unlist(extra_fields$GoogleScholar))
production = cbind(production, Organiser = unlist(extra_fields$Organiser))
pos = which(production$DOI=="")
for (i in pos){
  if (is.na(production$DOI[i]) | production$DOI[i]==""){
    production$DOI[i] = extra_fields$DOI[i]
  }
}


make_list(table = production,
          item_type = "preprint",
          qmd_file = "_preprints.qmd",
          comment = "Preprints")

make_list(table = production,
          item_type = "journalArticle",
          qmd_file = "_journal_articles.qmd",
          comment = "Journal Articles")

make_list(table = production,
          item_type = "bookSection",
          qmd_file = "_book_chapters.qmd",
          comment = "Book Chapters")

make_list(table = production,
          item_type = "thesis",
          qmd_file = "_thesis.qmd",
          comment = "Thesis")

make_list(table = production,
          item_type = "conferencePaper",
          qmd_file = "_conference_proceedings.qmd",
          comment = "Conference Proceedings")

make_list(table = production,
          item_type = "computerProgram",
          qmd_file = "_software.qmd",
          comment = "Software")

make_list(table = production,
          item_type = "dataset",
          qmd_file = "_data.qmd",
          comment = "Data")

make_list(table = production,
          item_type = "presentation",
          type = c("Oral presentation","Invited oral presentation"),
          first_author = T,
          qmd_file = "_oral_presentations.qmd",
          comment = "Oral Presentations")

make_list(table = production,
          item_type = "presentation",
          type = c("Oral presentation","Invited oral presentation"),
          first_author = F,
          qmd_file = "_oral_presentations_coauthor.qmd",
          comment = "Coauthor in Oral Presentations")

make_list(table = production,
          item_type = "presentation",
          type = "Poster",
          qmd_file = "_posters.qmd",
          comment = "Posters")


unique(production$Item.Type)
unique(production$Type)
