#'phonex
#'@description Transforms text according to the french adapted phonex algorithm proposed by Frédéric BROUARD. This work is based on the python implementation provided by  Christian Pennaforte
#'@param txt a character string
#'@param char_to_remove characters to remove from the initial text (defaults: ' -.+*/,:;_')
#'@param convert_to_num if TRUE, converts the processed text to a base22 number (as in the original algorithm). if FALSE, keeps the last output before this conversion (defaults: TRUE)
#'@example phonex('AMOXICILLINE')
#'@export
#'
phonex <- function(txt, char_to_remove = ' -.+*/,:;_', convert_to_num = TRUE) {

  if (!is.character(txt)) stop('txt must be of type character')

 # r <- iconv(r, to='ASCII//TRANSLIT')
  r <- stringr::str_to_lower(txt)
  r <- chartr('àâäãéèêëìîïòôöõùûüñy','AAAAYYYYIIIOOOOUUUNI', r)
  r <- stringr::str_replace_all(r, stringr::str_interp("[${char_to_remove}]"), "")
  r <- stringr::str_to_upper(r)

  if (stringr::str_detect(r,'[:digit:]')) stop('the input text cannot contain digits.\n you can add [:digits:] to the char_to_remove list if needed')
  if (stringr::str_detect(r,'[ -.+*/,:;_]')) stop('the input text cannot contain special characters.\n you can add them to the char_to_remove list if needed')


  #1 remplacer les y par des i
  #r <- stringr::str_replace_all(r, 'Y', 'I')

  #2 supprimer les h qui ne sont pas précédées de c ou de s ou de p
  r <- stringr::str_replace_all(r, '([^PCS])H', '\\1')
  r <- stringr::str_replace_all(r, '^H', '') # ajouté la suppression des H en début de phrase

  #3 remplacement du ph par f
  r = stringr::str_replace_all(r, 'PH', 'F')

  #4 remplacer les groupes de lettres suivantes :
  r = stringr::str_replace_all(r, 'G(AI?[NM])','K\\1')


  #5 remplacer les occurrences suivantes, si elles sont suivies par une lettre a, e, i, o, ou u :
  r = stringr::str_replace_all(r, '[AE]I[NM]([AEIOU])','YN\\1')



  #6 remplacement de groupes de 3 lettres (sons 'o', 'oua', 'ein') :
  r = stringr::str_replace_all(r, 'EAU','O')
  r = stringr::str_replace_all(r, 'OUA','2')
  r = stringr::str_replace_all(r, 'EIN','4')
  r = stringr::str_replace_all(r, 'AIN','4')
  r = stringr::str_replace_all(r, 'EIM','4')
  r = stringr::str_replace_all(r, 'AIM','4')

  #7 remplacement du son É:
  #r = stringr::str_replace_all(r, 'É','Y') #CP : déjà fait en étape 0
  #r = stringr::str_replace_all(r, 'È','Y') #CP : déjà fait en étape 0
  #r = stringr::str_replace_all(r, 'Ê','Y') #CP : déjà fait en étape 0
  r = stringr::str_replace_all(r, 'AI','Y')
  r = stringr::str_replace_all(r, 'EI','Y')
  r = stringr::str_replace_all(r, 'ER','YR')
  r = stringr::str_replace_all(r, 'ESS','YS')
  r = stringr::str_replace_all(r, 'ET','YT') #CP : différence entre la version Delphi et l'algo
  r = stringr::str_replace_all(r, 'EZ','YZ')


  #8 remplacer les groupes de 2 lettres suivantes (son â..anâ.. et â..inâ..), sauf sâ..il sont suivi par une lettre a, e, i o, u ou un son 1 Ã  4 :
 r = stringr::str_replace_all(r, 'AN([^AEIOU1234N])','1\\1') # ajouté n à la liste des exclusions
 r = stringr::str_replace_all(r, 'ON([^AEIOU1234N])','1\\1') # ajouté n à la liste des exclusions
 r = stringr::str_replace_all(r, 'AM([^AEIOU1234M])','1\\1') # ajouté n à la liste des exclusions
 r = stringr::str_replace_all(r, 'EN([^AEIOU1234N])','1\\1') # ajouté n à la liste des exclusions
 r = stringr::str_replace_all(r, 'EM([^AEIOU1234M])','1\\1') # ajouté n à la liste des exclusions
 r = stringr::str_replace_all(r, 'IN([^AEIOU1234N])','4\\1') # ajouté n à la liste des exclusions

 #9 remplacer les s par des z sâ..ils sont suivi et prÃ©cÃ©dÃ©s des lettres a, e, i, o,u ou dâ..un son 1 Ã  4
 r = stringr::str_replace_all(r, '([AEIOUY1234])S([AEIOUY1234])','\\1Z\\2')

 #10 remplacer les groupes de 2 lettres suivants :
 r = stringr::str_replace_all(r,'OE','E')
 r = stringr::str_replace_all(r,'EU','E')
 r = stringr::str_replace_all(r,'AU','O')
 r = stringr::str_replace_all(r,'OI','2')
 r = stringr::str_replace_all(r,'OY','2')
 r = stringr::str_replace_all(r,'OU','3')

 #12 remplacer le c par un s s'il est suivi d'un e ou d'un i
 #CP : à mon avis, il faut inverser 11 et 12 et ne pas faire la dernière ligne du 11 # DONE
 r = stringr::str_replace_all(r,'C([EI])','S\\1')


 #11 remplacer les groupes de lettres suivants
 r = stringr::str_replace_all(r,'SCH','5') # changé l'ordre
 r = stringr::str_replace_all(r,'CH','5')
 r = stringr::str_replace_all(r,'SH','5')
 r = stringr::str_replace_all(r,'SS','S')
 # r = stringr::str_replace_all(r,'SC','S') #CP : problème pour PASCAL, mais pas pour PISCINE ?

 #13 remplacer les lettres ou groupe de lettres suivants :

 r = stringr::str_replace_all(r,'QU','K') # changé l'ordre
 r = stringr::str_replace_all(r,'GU','K') # changé l'ordre
 r = stringr::str_replace_all(r,'C','K')
 r = stringr::str_replace_all(r,'Q','K')
 r = stringr::str_replace_all(r,'GA','KA')
 r = stringr::str_replace_all(r,'GO','KO')
 r = stringr::str_replace_all(r,'GY','KY')

 #14 remplacer les lettres suivante :
 r = stringr::str_replace_all(r,'A','O')
 r = stringr::str_replace_all(r,'D','T')
 r = stringr::str_replace_all(r,'P','T')
 r = stringr::str_replace_all(r,'J','G')
 r = stringr::str_replace_all(r,'B','F')
 r = stringr::str_replace_all(r,'V','F')
 r = stringr::str_replace_all(r,'M','N')

 #15 Supprimer les lettres dupliquées
 r <- stringr::str_replace_all(r, '([[:alpha:]])\\1+', '\\1')

 #16 Supprimer les terminaisons suivantes : t, x # ajouté: enlever e si précédé de K
 r <- stringr::str_replace_all(r,'(.*)[TX]$','\\1')
 r <- stringr::str_replace_all(r, '(.*[K])E', '\\1') # ajouté

 if (!convert_to_num) {
   return(r)
 } else {

   #17 Affecter à chaque lettre le code numérique correspondant en partant de la dernière lettre
   num = c('1','2','3','4','5','E','F','G','H','I','K','L','N','O','R','S','T','U','W','X','Y','Z')
   l = rep(NA, nchar(r))

   ll <- stringr::str_split(r,'')[[1]]
   for (i in seq_len(length(ll))) {
     l[[i]] <- which(num == ll[i])
   }

   #18 Convertissez les codes numériques ainsi obtenu en un nombre de base 22 exprimé en virgule flottante.
   res=0.
   i=1
   for (n in l) {
     res = n*22**-i+res
   }
   i=i+1

   return(res)

 }

}

