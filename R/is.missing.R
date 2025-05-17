is.missing <- function(x)
{
    if(base::is.null(x) || base::is.na(x) || base::is.nan(x) || x=="") {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
