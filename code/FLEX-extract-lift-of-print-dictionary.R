## Note: these texts have no translation elements, so I manually added it for escaping error:
### Kah yõkõ' kėrape ka'daih (2f6dc8ff-b718-4687-ac85-344a19959037)
### Yãh yah bėhu̇dah ayam (a4c92df3-3d45-4e2b-acb5-7c0fd99e9d48)
### This has been git tracked now (1 Jan. 2025)


library(tidyverse)
library(xml2)

flexdb <- read_xml("data-input/Enggano-Learner-Dictionary.lift")
flexdb

# MAIN ENTRY ======

# extract only the <entry>
myentry <- flexdb %>% 
  xml_find_all("entry")

# determine the length of children for each <entry>
myentry_length <- myentry %>% 
  xml_length()
myentry_length

# get the id for each <entry> node
myentry_id <- myentry %>% 
  xml_attr("id")
myentry_guid <- myentry |> 
  xml_attr("guid")
myentry_id_num <- myentry_id %>% str_extract("(?<=_).+$")
myentry_id_form <- myentry_id %>% str_extract("^.+(?=_)")

# get the dates
dateCreated <- myentry |> 
  xml_attr("dateCreated")
dateModified <- myentry |> 
  xml_attr("dateModified")

## get the CHILDREN elements of the main entry ======
## get the unique elements name of the <entry> node
myentry_children_name <- myentry %>% 
  map(~xml_children(.)) %>% 
  map(~xml_name(.)) %>% 
  unlist() %>% 
  unique()
myentry_children_name
# [1] "lexical-unit" (DONE) "trait" (DONE) "relation" (DONE) "etymology" (DONE)    "sense"    (DONE)    "field"        "note"   (DONE)      "citation" 


# LEXICAL-UNIT: extract the <lexical-unit> using purrr::map() for each <entry> =====

## gather the lexical unit =======
lu_form <- myentry %>% 
  # .[test_range] %>% 
  map(~xml_find_all(., "lexical-unit/form[@lang=\"eno\"]/text")) %>% 
  map(~xml_text(.))

### check the length of the form/text elements
lu_form_length <- lu_form |> map_int(length)

### duplicate the entry id as many as the number of elements of form/text
lu_form_entry_id <- rep(myentry_id_num, lu_form_length)
lu_form_entry_guid <- rep(myentry_guid, lu_form_length)
lu_form_entry_id_orig <- rep(myentry_id, lu_form_length)

### create the tibble for the lu form
lu_form_df <- lu_form |> 
  map(~tibble(form = .)) |> 
  list_rbind()

### create the tibble for the lu form
lu_form_df <- lu_form_df |> 
  mutate(entry_id_orig = lu_form_entry_id_orig,
         entry_id = lu_form_entry_id,
         entry_guid = lu_form_entry_guid)

## gather the language attribute of the form ======
lu_text_source <- myentry %>% 
  map(~xml_find_all(., "lexical-unit/form[@lang=\"eno\"]")) %>% 
  map(~xml_attr(., "lang"))
lu_text_source_length <- lu_text_source |> map_int(length)
lu_text_source_entry_id <- rep(myentry_id_num, lu_form_length)
lu_text_source_entry_id_orig <- rep(myentry_id, lu_form_length)
lu_text_source_entry_guid <- rep(myentry_guid, lu_form_length)
lu_text_source_vect <- unlist(lu_text_source)
lu_text_source_df <- tibble(lang = lu_text_source_vect,
                            entry_id = lu_text_source_entry_id,
                            entry_id_orig = lu_text_source_entry_id_orig,
                            entry_guid = lu_text_source_entry_guid)

## gather the entry order =====
entry_attr <- myentry |> xml_attrs()
entry_attr_df <- entry_attr |> 
  map(~tibble(attr = ., types = names(.))) |> 
  map(~pivot_wider(., names_from = types, values_from = attr)) |> 
  list_rbind() |> 
  mutate(entry_guid = guid) |> 
  rename(entry_id = guid)

## gather the lexical unit TRAIT =====
lu_trait <- myentry %>% 
  map(~xml_find_first(., "trait[@name='morph-type']")) %>% 
  map(~xml_attr(., "value"))
lu_trait_vect <- rep(unlist(lu_trait), lu_form_length)

## gather the LU form, id, language source, lu_order, and trait
lu_form_df <- tibble(entry_id = lu_text_source_entry_id,
                     entry_id_orig = lu_text_source_entry_id_orig,
                     entry_guid = lu_text_source_entry_guid,
                     form = unlist(lu_form),
                     lang = unlist(lu_text_source),
                     trait = lu_trait_vect) |> 
  left_join(select(entry_attr_df, entry_id, order),
            by = join_by("entry_id"))

# RELATION: extract the Relation elements within <entry> element =====

myrelation <- myentry |> 
  map(~xml_find_all(., "relation"))

relation_order <- myrelation |> 
  map(~map(., ~xml_attr(., "order"))) |> 
  map(unlist) |> 
  modify_if(~is_null(.) == TRUE, ~NA)
relation_order_length <- map_int(relation_order, length)
relation_ref <- myrelation |> 
  map(~map(., ~xml_attr(., "ref"))) |> 
  map(unlist) |> 
  modify_if(~is_null(.) == TRUE, ~NA)
relation_ref_length <- map_int(relation_ref, length)
relation_type <- myrelation |> 
  map(~map(., ~xml_attr(., "type"))) |> 
  map(unlist) |> 
  modify_if(~is_null(.) == TRUE, ~NA)
relation_type_length <- map_int(relation_type, length)
relation_is_primary <- myrelation |> 
  map(~map(., ~xml_find_all(., "trait[@name='is-primary']"))) |> 
  map(~map(., ~xml_attr(., "value"))) |> 
  map(~modify_if(., ~length(.) == 0, ~NA)) |> 
  map(unlist) |> 
  modify_if(~is_null(.) == TRUE, ~NA)
relation_is_primary_length <- map_int(relation_is_primary, length)
relation_complex_form_type <- myrelation |> 
  map(~map(., ~xml_find_all(., "trait[@name='complex-form-type']"))) |> 
  map(~map(., ~xml_attr(., "value"))) |> 
  map(~modify_if(., ~length(.) == 0, ~NA)) |> 
  map(unlist) |> 
  modify_if(~is_null(.) == TRUE, ~NA)
relation_complex_form_type_length <- map_int(relation_complex_form_type, length)
relation_variant_type <- myrelation |> 
  map(~map(., ~xml_find_all(., "trait[@name='variant-type']"))) |> 
  map(~map(., ~xml_attr(., "value"))) |> 
  map(~modify_if(., ~length(.) == 0, ~NA)) |> 
  map(unlist) |> 
  modify_if(~is_null(.) == TRUE, ~NA)
relation_variant_type_length <- map_int(relation_variant_type, length)

relation_all_df <- tibble(entry_id = rep(lu_text_source_entry_id, relation_ref_length),
                            entry_id_orig = rep(lu_text_source_entry_id_orig, relation_ref_length),
                            entry_guid = rep(lu_text_source_entry_guid, relation_ref_length))
rel_tb <- pmap(list(a = relation_order,
          b = relation_type,
          c = relation_ref,
          d = relation_is_primary,
          e = relation_complex_form_type,
          f = relation_variant_type),
     \(a, b, c, d, e, f)
     tibble(rel_order = a,
            rel_type = b,
            rel_ref = c,
            rel_primacy = d,
            rel_compform = e,
            rel_variant = f)) |> 
  list_rbind() |> 
  mutate(across(where(is.character), ~replace_na(., "")))
relation_all_df <- relation_all_df |> bind_cols(rel_tb)
lu_form_df <- lu_form_df |> 
  left_join(relation_all_df)

# SENSE: extract children elements within <sense>, including sense id attributes =====

# s <- senses[c(424, 1248, 248, 271, 1365, 1299)]
## find all <sense> nodes =====
senses <- myentry %>% 
  map(~xml_find_all(., "sense"))
names(senses) <- myentry_id_num

s <- senses

s |> 
  map(~map(., ~xml_attr(., "id"))) |> 
  map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
  map(~map(., \(x) replace(x, nchar(x) == 0, NA))) -> s_id_interim
s_id_interim |> 
  map(unlist) |> 
  map(~tibble(sense_id = .)) -> s_id

s |> 
  map(~map(., ~xml_attr(., "order"))) |> 
  map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
  map(~map(., \(x) replace(x, nchar(x) == 0, NA))) |> 
  map(unlist) |> 
  map(~tibble(sense_order = .)) -> s_order

s |> 
  map(~map(., ~xml_find_all(., "grammatical-info"))) |> 
  map(~map(., ~xml_attr(., "value"))) |> 
  map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
  map(~map(., \(x) replace(x, nchar(x) == 0, NA))) |> 
  map(unlist) |> 
  map(~tibble(sense_gram = .)) -> s_gram

s |> 
  map(~map(., ~xml_find_all(., "illustration"))) |> 
  map(~map(., ~xml_attr(., "href"))) |> 
  map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
  map(~map(., \(x) replace(x, nchar(x) == 0, NA))) |> 
  map(unlist) |> 
  map(~tibble(pict = .)) -> s_pict

s |> 
  map(~map(., ~xml_find_all(., "gloss[@lang=\"en\"]/text"))) |> 
  map(~map(., ~xml_text(.))) |> 
  map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
  map(~map(., \(x) replace(x, nchar(x) == 0, NA))) |> 
  map(unlist) |> 
  map(~tibble(sense_gloss_en = .)) -> s_ge

s |> 
  map(~map(., ~xml_find_all(., "reversal/form[@lang='en']/text"))) |> 
  map(~map(., ~xml_text(.))) |> 
  map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
  map(~map(., \(x) replace(x, nchar(x) == 0, NA))) |> 
  map(~map(., ~str_c(., collapse = " ; "))) |> 
  map(unlist) |> 
  map(~tibble(rev_en = .)) -> s_rev_en

s |> 
  map(~map(., ~xml_find_all(., "gloss[@lang=\"id\"]/text"))) |> 
  map(~map(., ~xml_text(.))) |> 
  map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
  map(~map(., \(x) replace(x, nchar(x) == 0, NA))) |> 
  map(unlist) |> 
  map(~tibble(sense_gloss_idn = .)) -> s_gn

s |> 
  map(~map(., ~xml_find_all(., "reversal/form[@lang='id']/text"))) |> 
  map(~map(., ~xml_text(.))) |> 
  map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
  map(~map(., \(x) replace(x, nchar(x) == 0, NA))) |> 
  map(~map(., ~str_c(., collapse = " ; "))) |> 
  map(unlist) |> 
  map(~tibble(rev_id = .)) -> s_rev_id

# s |> 
#   map(~map(., ~xml_find_all(., "definition[@lang=\"en\"]/text"))) |> 
#   map(~map(., ~xml_text(.))) |> 
#   map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
#   map(~map(., \(x) replace(x, nchar(x) == 0, NA))) |> 
#   map(unlist) |> 
#   map(~tibble(sense_definition_en = .)) -> s_de
# 
# s |> 
#   map(~map(., ~xml_find_all(., "definition[@lang=\"id\"]/text"))) |> 
#   map(~map(., ~xml_text(.))) |> 
#   map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
#   map(~map(., \(x) replace(x, nchar(x) == 0, NA))) |> 
#   map(unlist) |> 
#   map(~tibble(sense_definition_idn = .)) -> s_dn

s_df <- pmap(list(a = s_id, b = s_order, c = s_gram, d = s_ge, e = s_gn, 
                  # f = s_de, g = s_dn, 
                  h = s_rev_en, ii = s_rev_id, pict = s_pict),
             \(a, b, c, d, e, h, ii, pict) bind_cols(a, b, c, d, e, h, ii, pict))
s_df <- map2(.x = s_df, .y = names(s_df), ~mutate(.x, entry_id = .y)) |> 
  list_rbind()

lu_form_df <- lu_form_df |> 
  left_join(s_df)

## EXAMPLE ======
# s_id_indices <- c(1448, 1454, 1462, 1257, 1263)
# s_id_mini <- s_id_interim[s_id_indices]
# s_ex <- s[s_id_indices] |> 
s_ex_eno <- s |> 
  map(~map(., ~xml_find_all(., "example/form[@lang=\"eno\"]"))) |> 
  map(~map(., ~xml_text(.))) |> 
  map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
  map(~map(., \(x) replace(x, nchar(x) == 0, NA)))

# s_ex_eng <- s[s_id_indices] |> 
s_ex_eno_source <- s |> 
  map(~map(., ~xml_find_all(., "example[@source]"))) |> 
  map(~map(., ~xml_attr(., "source"))) |> 
  map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
  map(~map(., \(x) replace(x, nchar(x) == 0, NA)))

s_ex_eng <- s |>
  map(~map(., ~xml_find_all(., "example/translation[@type=\"Free translation\"]"))) |> 
  map(~map(., ~xml_find_first(., "form[@lang=\"en\"]"))) |> 
  map(~map(., ~xml_text(.))) |> 
  map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
  map(~map(., \(x) replace(x, nchar(x) == 0, NA)))

s_ex_idn <- s |>
  map(~map(., ~xml_find_all(., "example/translation[@type=\"Free translation\"]"))) |> 
  map(~map(., ~xml_find_first(., "form[@lang=\"id\"]"))) |> 
  map(~map(., ~xml_text(.))) |> 
  map(~map(., \(x) if(identical(x, character(0))) NA else x)) |> 
  map(~map(., \(x) replace(x, nchar(x) == 0, NA)))

s_ex_eno_df <- map2(s_ex_eno, s_id_interim, 
                    ~tibble(x_eno = .x, sense_id = .y)) |> 
  map(unnest_longer, c(x_eno, sense_id))
s_ex_eno_df <- map2(s_ex_eno_df, names(s_ex_eno_df), 
                    ~mutate(.x, entry_id = .y))

# s_ex_eno_source_df <- map2(s_ex_eno_source, s_id_interim, 
#                            ~tibble(x_eno_source = .x, sense_id = .y)) |>
#   map(unnest_longer, c(x_eno_source, sense_id))
# s_ex_eno_source_df <- map2(s_ex_eno_source_df, names(s_ex_eno_source_df), 
#                            ~mutate(.x, entry_id = .y))
s_ex_eno_source_df <- map(s_ex_eno_source, ~tibble(x_eno_source = .x)) |> 
  map(unnest_longer, x_eno_source)

# s_ex_eng_df <- map2(s_ex_eng, s_id_interim, 
#                     ~tibble(x_eng = .x, sense_id = .y)) |> 
#   map(unnest_longer, c(x_eng, sense_id))
# s_ex_eng_df <- map2(s_ex_eng_df, names(s_ex_eng_df), 
#                     ~mutate(.x, entry_id = .y))
s_ex_eng_df <- map(s_ex_eng, ~tibble(x_eng = .x)) |> 
  map(unnest_longer, x_eng)

# s_ex_idn_df <- map2(s_ex_idn, s_id_interim, 
#                     ~tibble(x_idn = .x, sense_id = .y)) |> 
#   map(unnest_longer, c(x_idn, sense_id))
# s_ex_idn_df <- map2(s_ex_idn_df, names(s_ex_idn_df), 
#                     ~mutate(.x, entry_id = .y))
s_ex_idn_df <- map(s_ex_idn, ~tibble(x_idn = .x)) |> 
  map(unnest_longer, x_idn)

# map2(s_ex_idn_df[c(899, 1298)], names(s_ex_idn_df)[c(899, 1298)], ~mutate(.x, entry_id = .y))
# map2(map2(s_ex_idn_df[c(899, 1298)], names(s_ex_idn_df)[c(899, 1298)], ~mutate(.x, entry_id = .y)), map2(s_ex_eno_df[c(899, 1298)], names(s_ex_eno_df)[c(899, 1298)], ~mutate(.x, entry_id = .y)), ~left_join(.x, .y, by = join_by("entry_id")))

ex_l <- list(a = s_ex_eno_source_df,
             b = s_ex_eno_df,
             c = s_ex_idn_df,
             d = s_ex_eng_df)

s_ex_all_df <- pmap(ex_l, \(a, b, c, d) bind_cols(a, b, c, d)) |> 
  map(~select(., entry_id, sense_id, everything(.))) |> 
  map(~mutate(., across(where(is.logical), ~as.character(.)))) |> 
  list_rbind()

# ETYMOLOGY ====
etym <- myentry |> 
  map(~xml_find_all(., "etymology"))
names(etym) <- myentry_id_num

etym_df <- etym |> 
  map(~xml_find_all(., "form[@lang='eno']/text")) |> 
  map(~xml_text(.)) |> 
  map(\(x) if(identical(x, character(0))) NA else x) |> 
  map(\(x) replace(x, nchar(x) == 0, NA)) |> 
  map(\(x) tibble(etym_proto = x))
etym_df <- etym_df |> 
  map2(.y = names(etym_df), ~mutate(.x, entry_id = .y)) |> 
  list_rbind()

etym_borrowed <- etym |> 
  map(~xml_find_all(., "field/form[@lang='en']/text/span[@lang='eno']")) |> 
  map(~xml_text(.)) |> 
  map(\(x) if(identical(x, character(0))) NA else x) |> 
  map(\(x) replace(x, nchar(x) == 0, NA)) |> 
  map(\(x) tibble(etym_borrowed = x))
etym_borrowed <- etym_borrowed |> 
  map2(.y = names(etym_borrowed), ~mutate(.x, entry_id = .y)) |> 
  list_rbind()

etym_df1 <- etym |> 
  map(~xml_find_all(., "form[@lang=\"id\"]/text")) |> 
  map(~xml_text(.)) |> 
  map(\(x) if(identical(x, character(0))) NA else x) |> 
  map(\(x) replace(x, nchar(x) == 0, NA)) |> 
  map(\(x) tibble(etym = x))
etym_df1 <- etym_df1 |> 
  map2(.y = names(etym_df1), ~mutate(.x, entry_id = .y)) |> 
  list_rbind()

etym_type <- etym |> 
  map(~xml_attr(., "type")) |> 
  map(\(x) if(identical(x, character(0))) NA else x) |> 
  map(\(x) replace(x, nchar(x) == 0, NA)) |> 
  map(\(x) tibble(etym_type = x))
etym_type <- etym_type |> 
  map2(.y = names(etym_type), ~mutate(.x, entry_id = .y)) |> 
  list_rbind()

lu_form_df <- lu_form_df |> 
  left_join(etym_type) |> 
  left_join(etym_df) |> 
  left_join(etym_borrowed)

# VARIANT: extract <variant> using purrr::map() for each <entry> ======
# var_form <- myentry %>% 
#   map(~xml_find_all(., "variant/form[@lang=\"eno\"]/text")) %>% 
#   map(~xml_text(.)) |> 
#   map(\(x) replace(x, nchar(x) == 0, NA)) |>
#   map(\(x) if(identical(x, character(0))) NA else x) |> 
#   map(~paste(., collapse = " ; ")) |> 
#   unlist()
# 
# var_traits <- myentry %>% 
#   map(~xml_find_all(., "variant/trait[@name=\"morph-type\"]")) %>% 
#   map(~xml_attr(., "value")) |> 
#   map(\(x) replace(x, nchar(x) == 0, NA)) |>
#   map(\(x) if(identical(x, character(0))) NA else x) |> 
#   map(~paste(., collapse = " ; ")) |> 
#   unlist()
# 
# variants_df <- tibble(variant_form = var_form, 
#                       variant_trait = var_traits, 
#                       entry_id = myentry_id_num) |> 
#   mutate(across(matches("var"), ~replace(., . == "NA", NA)))
# 
# lu_form_df <- lu_form_df |> 
#   left_join(variants_df) |> 
#   arrange(form, order)

lu_form_df <- lu_form_df |> 
  left_join(s_ex_all_df) |> 
  distinct()


# NOTE: extract the <note> for the lexeme entry ======
## re-run later
notes <- myentry |>
  map(~xml_find_all(., 'note'))
names(notes) <- myentry_id_num
notes_children <- notes |>
  map(~xml_children(.)) |>
  map(~xml_name(.)) |>
  unlist() |>
  unique()
notes_children
# # [1] "form"
notes_grandchilren <- notes |>
  map(~xml_find_all(., 'form')) |>
  map(~xml_children(.)) |>
  map(~xml_name(.)) |>
  unlist() |>
  unique()
notes_grandchilren
# # [1] "text"
notes_text_lang <- notes |>
  map(~xml_find_all(., 'form')) |>
  map(~xml_attr(., 'lang'))
notes_text_lang |> unlist() |> unique()
# # [1] "en" --- SO I DO NOT NEED TO MAKE A COLUMN FOR LANGUAGE OF THE NOTE
notes_text_en <- notes |>
  map(~xml_find_all(., 'form[@lang="en"]/text')) |>
  map(~xml_text(.))
notes_text_id <-  notes |>
  map(~xml_find_all(., 'form[@lang="id"]/text')) |>
  map(~xml_text(.))
notes_text_df <- pmap(list(notes_text_en, notes_text_id, names(notes_text_en)), \(a, b, c) tibble(entry_id = c, notes_eng = a, notes_idn = b)) |> 
  list_rbind() # |> 
  # mutate(notes_lexeme = if_else(str_detect(notes_lexeme, "^\\("),
  #                               str_replace_all(notes_lexeme, "(^\\(|\\)$)", ""),
  #                               notes_lexeme))
notes_text_df

lu_form_df <- lu_form_df |> 
  left_join(notes_text_df)

# CITATION forms ====
cit <- myentry |>
  map(~xml_find_all(., 'citation'))
names(cit) <- myentry_id_num
cit_form <- cit |>
  map(~xml_find_all(., 'form[@lang="eno"]/text')) |>
  map(~xml_text(.))
cit_form_df <- pmap(list(cit_form, names(cit_form)), \(a, b) tibble(entry_id = b, cit_form = a)) |> 
  list_rbind()

lu_form_df <- lu_form_df |> 
  left_join(cit_form_df) |> 
  distinct()

dateCreated_df <- tibble(dateCreated, entry_id = lu_form_entry_guid)
dateModified_df <- tibble(dateModified, entry_id = lu_form_entry_guid)

lu_form_df <- lu_form_df |> 
  mutate(across(where(is.character), ~replace_na(., ""))) |> 
  left_join(dateCreated_df) |> 
  left_join(dateModified_df) |> 
  select(-entry_id) |> 
  select(dateCreated, dateModified, entry_id = entry_id_orig, everything())

write_rds(lu_form_df, "data-output/Enggano-Learner-Dictionary.rds")
write_tsv(lu_form_df, "data-output/Enggano-Learner-Dictionary.tsv")
write_csv(lu_form_df, "data-output/Enggano-Learner-Dictionary.csv")
