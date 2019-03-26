QUERY_STEM = 'https://oeis.org/search?fmt=text&q='

oeis = function(sequence, top_result = TRUE, all_results = FALSE) {
  if (is.character(sequence)) {
    # confirmed: there doesn't appear to be a way to supply multiple
    #   IDs, e.g. as comma-separated id:A0,A1 or id:A0,id:A1
    if (length(sequence) == 1L) {
      query_url = sprintf('%sid:%s', QUERY_STEM, sequence)
      result = readLines(query_url, warn = FALSE)
      return(parse_text(result))
    } else lapply(sequence, oeis,
                  top_result = top_result, all_results = all_results)
  } else {
    if (length(sequence) == 1L) {
      query_url = sprintf('%sid:A%d', QUERY_STEM, sequence)
      result = readLines(query_url, warn = FALSE)
    } else {
      query_str = paste(sequence, collapse = ',')
      query_url = sprintf('%s%s', QUERY_STEM, query_str)
      result = readLines(query_url, warn = FALSE)

      if (top_result) {
        return(parse_text(result, first_only = TRUE))
      }
      output = parse_text(result)
      if (all_results) {
        n = get_n_results(result)
        next_page_starts = 10L * (1:((n - 1L) %/% 10L))
        output = unlist(c(output, lapply(next_page_starts, function(d) {
          query_url = sprintf('%s%s&start=%d', QUERY_STEM, query_str, d)
          parse_text(readLines(query_url, warn = FALSE))
        })), recursive = FALSE)
      }
      return(output)
    }
  }
}

get_n_results = function(txt) {
  idx = grep('^Showing ', txt)
  as.integer(gsub('.*\\s([0-9]+)$', '\\1', txt[idx]))
}

# reference here:
#   https://oeis.org/eishelp1.html
parse_text = function(txt, first_only = FALSE) {
  # all entries required to start with
  new_entry_idx = grep('^%I', txt)

  seq_ids = get_seq_no(txt[new_entry_idx])
  id_lengths = nchar(seq_ids)

  entry_seq = if (first_only) 1L else seq_along(new_entry_idx)
  entries = lapply(entry_seq, function(ii) {
    next_idx = -2L +
      if (ii == length(new_entry_idx)) length(txt) else new_entry_idx[ii + 1L]
    parse_entry(txt[new_entry_idx[ii]:next_idx], id_lengths[ii])
  })
  names(entries) = seq_ids[entry_seq]

  class(entries) = 'oeis'

  entries
}

get_seq_no = function(txt) {
  gsub('%I (A[0-9]+)\\s?.*', '\\1', txt)
}

NAME_MAPPING = c(
  "%I" = 'alternative_sequence_id',
  "%S" = 'terms_1',
  "%T" = 'terms_2',
  "%U" = 'terms_3',
  "%N" = 'sequence_name',
  '%D' = 'reference_details',
  '%H' = 'related_links',
  '%F' = 'formula',
  '%Y' = 'crossref_sequences',
  "%A" = 'author',
  '%O' = 'offset',
  '%p' = 'code_maple',
  '%t' = 'code_mathematica',
  '%o' = 'code_other_language',
  '%E' = 'extensions_errata',
  "%e" = 'examples',
  "%K" = 'keywords',
  '%C' = 'comments'
)

parse_entry = function(txt, k) {
  key = substring(txt, 1L, 2L)
  val = substring(txt, 5L + k, nchar(txt))

  grouped = tapply(val, key, identity, simplify = FALSE)
  names(grouped) = NAME_MAPPING[names(grouped)]

  entries = names(grouped)

  terms_idx = grep('terms', names(grouped))
  grouped$terms =
    as.integer(unlist(strsplit(unlist(grouped[terms_idx]), ',', fixed = TRUE)))
  grouped = grouped[-terms_idx]

  # R-ifying other entries
  ## required entries
  grouped$offset = as.integer(strsplit(grouped$offset, ',', fixed = TRUE)[[1L]])
  grouped$keywords = unlist(strsplit(grouped$keywords, ',', fixed = TRUE))

  return(grouped)
}

