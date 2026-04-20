to_date = function(x) {
  # If the string already looks like a full date, keep it
  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", x)) return(as.Date(x))
  # If we have year‑month only, append "-01"
  if (grepl("^\\d{4}-\\d{2}$", x)) return(as.Date(paste0(x, "-01")))
  # Otherwise it's just a year – append "-01-01"
  as.Date(paste0(x, "-01-01"))
}

split_authors = function(txt) {
  authors = strsplit(txt, ";")[[1]]
  authors = trimws(authors)
  
  get_initials = function(name) {
    name_parts = strsplit(name, "\\s+")[[1]]
    initials = sapply(name_parts, function(pal) {
      subp = strsplit(pal, "-")[[1]]
      initials_plus_dot = paste0(substr(subp, 1, 1), ".")
      paste(initials_plus_dot, collapse = "-")
    })
    paste0(initials, collapse = "")
  }
  
  res = lapply(authors, function(a) {
    parts = strsplit(a, ",")[[1]]
    surname = trimws(parts[1])
    name = ifelse(length(parts) > 1, trimws(parts[2]), "")

    initials = if (nzchar(name)) get_initials(name) else ""
    
    data.frame(
      author = a,
      surname = surname,
      name = name,
      initials = initials,
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, res)
}

make_list = function(table, item_type, type=NA, first_author=NA, focus_author="Navascués, Miguel de", qmd_file, comment){
  write(paste0("<!-- ", comment, " -->"), file = qmd_file)
  write("", file = qmd_file, append = T)
  sub_table = table[table$Item.Type %in% item_type, ]
  if (any(!is.na(type))){
    sub_table = sub_table[sub_table$Type %in% type, ]
  }
  for (i in seq_along(sub_table$Key)){
    authors = split_authors(sub_table$Author[i])
    include_item=F
    add_asterisk=F
    if (is.na(first_author)){
      include_item=T
    }else if (first_author==T & authors$author[1]==focus_author){
      include_item=T
    }else if (first_author==F & authors$author[1]!=focus_author){ 
      include_item=T
      add_asterisk=T
    }
    if (include_item){    
      reference = "1. "
      reference = paste0(reference, "**", sub_table$Title[i], "**")
      if (!endsWith(sub_table$Title[i], "?")) reference = paste0(reference, ".")
      reference = paste0(reference, " ")
      for (j in seq_along(authors$author)){
        if (authors$author[j]==focus_author){
          reference = paste0(reference, "_", authors$initials[j], " ", authors$surname[j], "_")
        }else{
          reference = paste0(reference, authors$initials[j], " ", authors$surname[j])
        }
        if (item_type=="presentation" & j==1 & add_asterisk){
          reference = paste0(reference,"\\* ")
        }
        if (item_type=="thesis" & j==1 & j!=length(authors$author)){
          reference = paste0(reference," supervised by ")
        }else if (j!=length(authors$author)){
          reference = paste0(reference,", ")
        }
      }
      reference = paste0(reference, " (", sub_table$Publication.Year[i], ")")
      
      if (item_type=="thesis"){
        reference = paste0(reference, " ", sub_table$Type[i])
        reference = paste0(reference, ". ", sub_table$Publisher[i], ", ")
        reference = paste0(reference, sub_table$Place[i])
      }
      
      if (item_type=="bookSection" | item_type=="conferencePaper" | item_type=="presentation"){
        reference = paste0(reference, " in ")
        editors = split_authors(sub_table$Editor[i])
        for (j in seq_along(editors$author)){
          if (editors$author[j]==focus_author){
            reference = paste0(reference, "_", authors$initials[j], " ", authors$surname[j], "_")
          }else{
            reference = paste0(reference, editors$initials[j], " ", editors$surname[j])
          }
          if (j!=length(editors$author)) reference = paste0(reference,", ")
        }
        if (length(editors$author)>2){
          reference = paste0(reference, " (eds.) ")
        }else if (length(editors$author)==1){
          reference = paste0(reference, " (ed.) ")
        }else{
          reference = paste0(reference, " ")
        }
        if (item_type=="presentation"){
          reference = paste0(reference, "_", sub_table$Meeting.Name[i], "_, ")
          reference = paste0(reference, sub_table$Organiser[i], ", ")
        }else{
          reference = paste0(reference, "_", sub_table$Publication.Title[i], "_, ")
          reference = paste0(reference, sub_table$Publisher[i], ", ")
        }
         reference = paste0(reference, sub_table$Place[i])
      }
      
      if (sub_table$DOI[i]!="" & !is.na(sub_table$DOI[i])){
        reference = paste0(reference, " [doi:", sub_table$DOI[i],
                           "](http://doi.org/", sub_table$DOI[i], " \"",
                           sub_table$Publication.Title[i],"\")")
      }else if (sub_table$Url[i]!="" & !is.na(sub_table$Url[i])){
        reference = paste0(reference, " [", sub_table$Url[i],
                           "](", sub_table$Url[i], ")")
      }
      reference = paste0(reference, ".")

      if (item_type=="presentation" & sub_table$Type[i]=="Invited oral presentation"){
        reference = paste0(reference, " **Invited**.")
      }

      if (item_type=="journalArticle" | item_type=="preprint"){
        GoogleScholarCode = strsplit(sub_table$Extra[i],":")[[1]][2]
        reference = paste0(reference,
                           " Citations: [`{r} publicationsMNavascues$cites[publicationsMNavascues$pubid==\"",
                           sub_table$GoogleScholar[i],
                           "\"]`](https://scholar.google.com/scholar?oi=bibs&hl=en&cites=`{r} ",
                           "publicationsMNavascues$cid[publicationsMNavascues$pubid==\"",
                           sub_table$GoogleScholar[i], "\"]` \"Google Scholar\").")
      }
      
      write(reference, file = qmd_file, append = T)
      write("", file = qmd_file, append = T)
    }
  }
}



extract_extra_fields = function(x){
  pieces <- strsplit(x, ";\\s*")         
  kv_list <- lapply(pieces, function(p) {
    kv <- strsplit(p, ":\\s*", perl = TRUE)   
    setNames(
      sapply(kv, `[`, 2),                   
      sapply(kv, `[`, 1)                 
    )
  })
  all_keys <- sort(unique(unlist(lapply(kv_list, names))))
  out_df <- as.data.frame(
    t(sapply(kv_list, function(kv) {
      v <- rep(NA_character_, length(all_keys))
      idx <- match(names(kv), all_keys)
      v[idx] <- kv
      v
    })),
    stringsAsFactors = FALSE
  )
  colnames(out_df) <- all_keys
  out_df
}
