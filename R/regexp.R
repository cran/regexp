make.regex <- function(n = 1) .Call("REGEXP_make_regex_t", n)
make.regmatch <- function(n = 1) .Call("REGEXP_make_regmatch_t", n)
regex.nsub <- function(re, i = 0) .Call("REGEXP_regex_t_re_nsub", re, i)
regmatch.so <- function(rm, i = 0) .Call("REGEXP_regmatch_t_rm_so", rm, i)
regmatch.eo <- function(rm, i = 0) .Call("REGEXP_regmatch_t_rm_eo", rm, i)
regcomp <- function(pat, flags = REG.EXTENDED) {
    #**** without interrupts??
    rex <- make.regex()
    result <- .Call("REGEXP_regcomp", rex, pat, flags)
    if (result != 0)
        .Call("REGEXP_regerror", result, rex)
    .Call("REGEXP_register", rex)
    rex
}
regexec <- function(rex, str, flags = 0) {
    nmatch <- regex.nsub(rex) + 1
    rm <- make.regmatch(nmatch)
    result <- .Call("REGEXP_regexec", rex, str, nmatch, rm, flags)
    if (result == 0) {
        val <- matrix(integer(2 * nmatch), nmatch)
        for (i in 1:nmatch) {
            val[i, 1] <- regmatch.so(rm, i - 1)
            val[i, 2] <- regmatch.eo(rm, i - 1)
        }
        val
    }
    else if (result == REG.NOMATCH)
        NULL
    else
        .Call("REGEXP_regerror", result, rex)
}
deHex <- function(str) .Call("REGEXP_deHex", str)
.First.lib <- function(lib, pkg) {
    library.dynam( "regexp", pkg, lib )
    pkgname <- paste("package", pkg, sep = ":")
    .Call("REGEXP_init", pos.to.env(match(pkgname, search())))
}
compile.regex <- function(pat, extended=TRUE, ignore.case=FALSE) {
    flags <- if (extended) REG.EXTENDED else 0
    if (ignore.case) flabs <- flags + REG.ICASE
    regcomp(pat, flags)
}
get.substrings <- function(pairs, str) {
    if (is.null(pairs)) character(0)
    else apply(pairs, 1, function(i, s) substr(s, i[1] + 1, i[2]), str)
}
regexp <- function(pat, str, index.only=FALSE, ...) {
    rex <- compile.regex(pat, ...)
    pairs <- regexec(rex, str, 0)
    if (index.only) pairs
    else get.substrings(pairs, str)        
}
regsub <- function(pat, str, sub, all=FALSE, ...) {
    strcat <- function(...) paste(..., sep="")
    rex <- compile.regex(pat, ...)
    head <- ""
    tail <- str
    repeat {
        val <- regexec(rex, tail, 0)
        if (is.null(val))
            return(strcat(head, tail))
        sval <- if (is.character(sub)) sub else sub(get.substrings(val, tail))
        tail.head <- substr(tail, 1, val[1,1])
        head <- strcat(head, tail.head, sval)
        tail <- substr(tail, val[1,2] + 1, nchar(tail))
        if (! all)
            return(strcat(head, tail))
    }
}
url.parts <- function(url) {
    val <- regexp("^(([^:]+)://)?([^:/]+)(:([0-9]+))?(/.*)", url)
    if (length(val) == 0)
        stop(paste("not a valid URL:", url))
    structure(val[c(3,4,6,7)], names=c("protocol","host","port","path"))
}
    
url.decode <- function(url)
    regsub("%([0-9a-hA-H][0-9a-hA-H])", regsub("\\+", url, " ", all=TRUE),
           function(s) deHex(s[2]), all=TRUE)
