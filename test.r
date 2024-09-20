deepl_translate <-
function (path, out_path, yaml_fields = c("title", "description"), 
    glossary_name = NULL, source_lang = NULL, target_lang = NULL, 
    formality = c("default", "more", "less", "prefer_more", "prefer_less")) 
{
    if (!file.exists(path)) {
        cli::cli_abort("Can't find {.field path} {.val {path}}.")
    }
    source_lang_code <- examine_source_lang(source_lang)
    target_lang_code <- examine_target_lang(target_lang)
    glossary_id <- examine_glossary(glossary_name = glossary_name, 
        source_lang_code = source_lang_code, target_lang_code = target_lang_code)
    formality <- rlang::arg_match(formality, values = c("default", 
        "more", "less", "prefer_more", "prefer_less"))
    temp_markdown_file <- withr::local_tempfile()
    markdown_lines <- brio::read_lines(path)
    shortcodes_indices <- grep("^\\{\\{<.*>\\}\\}", markdown_lines)
    shortcodes_present <- length(shortcodes_indices) > 0
    if (shortcodes_present) {
        shortcodes <- markdown_lines[shortcodes_indices]
        shortcodes <- purrr::map_chr(shortcodes, translate_shortcode, 
            glossary_name = glossary_name, source_lang = source_lang, 
            target_lang = target_lang, formality = formality)
        markdown_lines[shortcodes_indices] <- sprintf("`%s`", 
            purrr::map_chr(shortcodes, digest::digest))
        brio::write_lines(markdown_lines, temp_markdown_file)
        wool <- tinkr::yarn$new(path = temp_markdown_file)
    } else {
        wool <- tinkr::yarn$new(path = path)
    }
    yaml <- yaml::yaml.load(wool$yaml, handlers = list(seq = function(x) x))
    if (!is.null(yaml_fields) && !is.null(yaml)) {
        for (yaml_field in yaml_fields) {
            if (is_non_empty_string(yaml[[yaml_field]])) {
                yaml[[yaml_field]] <- purrr::map_chr(yaml[[yaml_field]], 
                  deepl_translate_markdown_string, glossary_name = glossary_name, 
                  source_lang = source_lang, target_lang = target_lang, 
                  formality = formality)
            }
        }
        yaml_file <- withr::local_tempfile()
        yaml::write_yaml(x = yaml, file = yaml_file, handlers = list(logical = yaml::verbatim_logical))
        wool$yaml <- c("---", brio::read_lines(yaml_file), "---")
    }
    split_size <- 10
    children_pods <- split(xml2::xml_children(wool[["body"]]), 
        ceiling(seq_along(xml2::xml_children(wool[["body"]]))/split_size))
    translated_children_pods <- purrr::map(children_pods, babeldown:::translate_part, 
        glossary_id = glossary_id, source_lang = source_lang, 
        target_lang = target_lang, formality = formality, glossary_name = glossary_name)
    wool[["body"]] <- fakify_xml(translated_children_pods)
    temp_markdown_file <- withr::local_tempfile()
    wool$write(temp_markdown_file)
    markdown_lines <- brio::read_lines(temp_markdown_file)
    if (shortcodes_present) {
        for (shortcode in shortcodes) {
            digested_shortcode <- sprintf("`%s`", digest::digest(shortcode))
            markdown_lines[markdown_lines == digested_shortcode] <- shortcode
        }
    }
    brio::write_lines(markdown_lines, out_path)
}


babeldown:::translate_part
function(
    xml, glossary_id, source_lang, target_lang, formality,
    glossary_name) {
    temp_file <- withr::local_tempfile()
    file.create(temp_file)
    woolish <- tinkr::yarn$new(path = temp_file)
    woolish$body <- babeldown:::fakify_xml(xml)
    woolish$body <- tinkr::protect_math(woolish$body)
    woolish$body <- tinkr::protect_curly(woolish$body)
    curlies <- xml2::xml_find_all(woolish$body, "//*[@curly]")
    purrr::walk(curlies, babeldown:::protect_curly)
    maths <- xml2::xml_find_all(woolish$body, "//*[@asis='true']")
    purrr::walk(maths, babeldown:::protect_math)
    contain_square_brackets <- xml2::xml_find_all(
        woolish$body,
        "//*[contains(text(), \"[\") and contains(text(), \"]\")]"
    )
    contain_square_brackets <- contain_square_brackets[!xml2::xml_name(contain_square_brackets) %in%
        c("code", "code_block")]
    if (length(contain_square_brackets) > 0) {
        purrr::walk(contain_square_brackets, babeldown:::protect_squaries)
    }
    non_code_blocks <- xml2::xml_find_all(woolish$body, "//d1:code_block[@language='block']")
    purrr::walk(non_code_blocks, protect_non_code_block)
    .translate <- function(text, glossary_id, source_lang, target_lang,
                           formality) {
        body_params <- purrr::compact(list(
            text = text, source_lang = source_lang,
            target_lang = target_lang, tag_handling = "xml",
            non_splitting_tags = "text,softbreak", formality = formality,
            glossary_id = glossary_id, ignore_tags = "code,code_block,curly,math,notranslate"
        ))
        doc <- deepl_form_request("translate", !!!body_params)
        doc$translations[[1]]$text
    }
    translate <- memoise::memoise(.translate)
    woolish$body <- xml2::read_xml(translate(as.character(woolish$body),
        source_lang = source_lang, target_lang = target_lang,
        formality = formality, glossary_id = glossary_id
    ))
    curlies <- xml2::xml_find_all(woolish$body, "//*[@curly]")
    purrr::walk(curlies, translate_alt_curly,
        glossary_name = glossary_name,
        source_lang = source_lang, target_lang = target_lang,
        formality = formality
    )
    purrr::walk(curlies, unprotect_curly)
    maths <- xml2::xml_find_all(woolish$body, "//d1:math")
    purrr::walk(maths, unprotect_math)
    notranslates <- xml2::xml_find_all(woolish$body, ".//d1:notranslate")
    purrr::walk(notranslates, unprotect_notranslate)
    nested_text_nodes <- xml2::xml_find_all(woolish$body, ".//d1:squary/d1:text")
    nested_parents <- xml2::xml_parent(nested_text_nodes)
    purrr::walk(nested_parents, untangle_text)
    squaries <- c(
        xml2::xml_find_all(woolish$body, ".//d1:squary"),
        xml2::xml_find_all(woolish$body, ".//squary")
    )
    purrr::walk(squaries, unprotect_squary)
    nested_text_nodes <- xml2::xml_find_all(woolish$body, ".//d1:text/d1:text")
    nested_parents <- xml2::xml_parent(nested_text_nodes)
    purrr::walk(nested_parents, untangle_text)
    non_code_blocks <- xml2::xml_find_all(woolish$body, "//d1:non_code_block")
    purrr::walk(non_code_blocks, unprotect_non_code_block)
    woolish[["body"]]
}