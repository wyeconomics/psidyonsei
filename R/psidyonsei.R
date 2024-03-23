#' Vectorize Variables
#'
#' Converts copied variables to a vector
#' @param
#' @return A vector of given variables
#' @examples
#'
#' @export
load_var <- function(){
  varlist=c()
  repeat{
    x=readline('Enter ! when finished : ')
    if(x=='!'){break}
    varlist=append(varlist, x)
  }
  return(varlist)
}

#' Help search PSID variables
#'
#' After you created variable list using 'load_var', use 'var_search' to help search in the PSID website.
#' @param varlist A Vector of Variables copied from PSID codebook
#' @return well-organized string of vectors to your clipboard
#' @examples
#' var_search(c("state || [68]V93 [69]V537 [70]V1103 [71]V1803 [72]V2403 [73]V3003 [74]V3403 [75]V3803 [76]V4303 [77]V5203 [78]V5703 [79]V6303 [80]V6903 [81]V7503 [82]V8203 [83]V8803 [84]V10003 [85]V11103 [86]V12503 [87]V13703 [88]V14803 [89]V16303 [90]V17703 [91]V19003 [92]V20303 [93]V21603 [94]ER4156 [95]ER6996 [96]ER9247 [97]ER12221 [99]ER13004 [01]ER17004 [03]ER21003 [05]ER25003 [07]ER36003 [09]ER42003 [11]ER47303 [13]ER53003 [15]ER60003 [17]ER66003 [19]ER72003 [21]ER78003"))
#' @export
var_search <- function(varlist){
  varsearch=varlist %>% stringr::str_split_i(' \\|\\| ', -1) %>%
    stringr::str_remove_all(pattern='\\[\\d\\d\\]') %>%  paste(collapse=' ')
  clipr::write_clip(varsearch)
  return(varsearch)
}

#' Alternative psid_read() function
#'
#' If 'psid_read' function from the 'psid_read' package causes an error, try this instead.(check psid_read help file)
#' @param
#' @return well-organized string of vectors to your clipboard
#' @examples
#'
#' @export
psid_read2 <- function(indir,str_df, idvars=NA, type, filename=NA){
  return(psidread::psid_read(indir=indir, str_df=str_df, idvars=idvars, type=type, filename=filename) %>% mutate(ER30001=pid%/%1000))
}

