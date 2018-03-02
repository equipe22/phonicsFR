#'soundex2
#'@description Transforms text according to the french adapted soundex2 algorithm proposed by Florent Carlier
#'@param txt character string
#'@example soundex('AMOXICILLINE')
#'@export
soundex2 <- function(txt) {
  r <- stringr::str_replace_all(txt, '[- ]', '')
  r <- stringr::str_to_upper(r)

  # Etape : Remplacement groupes lettres par correspondance
  r <- stringr::str_replace_all(r, 'GUI','KI')
  r <- stringr::str_replace_all(r,'GUE','KE')
  r <- stringr::str_replace_all(r,'GA' ,'KA')
  r <- stringr::str_replace_all(r,'GO' ,'KO')
  r <- stringr::str_replace_all(r,'GU' ,'K')
  r <- stringr::str_replace_all(r,'CA' ,'KA')
  r <- stringr::str_replace_all(r,'CO' ,'KO')
  r <- stringr::str_replace_all(r,'CU' ,'KU')
  r <- stringr::str_replace_all(r,'Q'  ,'K')
  r <- stringr::str_replace_all(r,'CC' ,'K')
  r <- stringr::str_replace_all(r,'CK' ,'K')

  # Etape : Remplcaement des voyelles
  r <- paste0(stringr::str_sub(r,1, 1), chartr('AEIOU','AAAAA',stringr::str_sub(r,2)))

  # Etape : Remplacement des Prefixe

  r <- stringr::str_replace_all(r,'^MAC', 'MCC')
  r <- stringr::str_replace_all(r,'^SCH', 'SSS')
  r <- stringr::str_replace_all(r,'^ASA', 'AZA')
  r <- stringr::str_replace_all(r,'^KN','NN')
  r <- stringr::str_replace_all(r,'^PH','FF')
  r <- stringr::str_replace_all(r,'^PF','FF')

  # Etape H -> '' sauf SH et CH
  r <- stringr::str_replace_all(r,'([^CS])H', '\\1')

  # Etape Y -> '' sauf A
  r <- stringr::str_replace_all(r,'([^A])Y', '\\1')

  # Etape : Suppression des terminaisons A, D, T, S
  r <- stringr::str_replace_all(r,'(.*)[ADTS]$','\\1')

  # Etape : Suppression des A sauf En-tete
  r <- paste0(stringr::str_sub(r,1, 1), stringr::str_replace_all(stringr::str_sub(r,2), 'A',''))

  # Supprimer les lettres dupliquées
  r <- stringr::str_replace_all(r, '([[:alpha:]])\\1+', '\\1')

  stringr::str_pad(r, 4, 'right', ' ')

}

