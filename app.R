# =============================================================================
#  INNOVATECH EMPLOYER BRAND DASHBOARD                                 app.R
# -----------------------------------------------------------------------------
#  Question : how do we build an employer brand that employees live, outsiders
#             believe and candidates choose?
#  Answers  : 1 How big is the problem?      -> Tabs 2, 3, 6
#             2 Where is it concentrated?    -> Tabs 4, 5, 6, 8, 9, 10
#             3 What drives it?              -> Tabs 5, 6
#             4 What would it cost to fix?   -> Tabs 7, 11
#  Privacy  : the CSV files are read by R on this computer only.
#  How to run: open this file in RStudio and press "Run App".
#
#  Packages : shiny, bslib, dplyr, plotly, DT are required.
#             tidytext (Bing lexicon, stop words) and topicmodels (LDA) are
#             optional - the app uses built-in fallbacks when they are missing.
#
#  Naming   : the SCREEN (ui) and the CALCULATOR (server) use exactly the names
#             in the brief. Extra names used by this app:
#               dept, grd, dr, stat, adj      global filters
#               rm_emp ... rm_ja              remove buttons
#               tk_<chart or table name>      takeaways
#               hdr_status                    header "files loaded" meter
# =============================================================================


# -----------------------------------------------------------------------------
# 0. PACKAGES
# -----------------------------------------------------------------------------
needed  <- c("shiny", "bslib", "dplyr", "plotly", "DT")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing))
  stop("Install these packages first, then run the app again: install.packages(c(",
       paste0("'", missing, "'", collapse = ", "), "))")

library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(DT)

USE_TIDYTEXT    <- requireNamespace("tidytext", quietly = TRUE)
USE_TOPICMODELS <- USE_TIDYTEXT && requireNamespace("topicmodels", quietly = TRUE)

options(shiny.maxRequestSize = 100 * 1024^2)   # allow CSV files up to 100 MB


# -----------------------------------------------------------------------------
# 1. SETTINGS
# -----------------------------------------------------------------------------
MIN_N <- 5          # groups smaller than this are hidden

# Colour meaning is the same everywhere:
# violet = brand, teal = good, saffron = watch, rose = risk, sky = information
COL <- c(violet = "#6D4AFF", teal = "#14B8A6", saffron = "#F59E0B", rose = "#F43F5E",
         sky = "#0EA5E9", ink = "#24163A")
PAL <- c("#6D4AFF", "#14B8A6", "#F59E0B", "#F43F5E", "#0EA5E9",
         "#8B5CF6", "#EC4899", "#84CC16", "#64748B", "#F97316")
SCALE_GOOD_RISK <- list(c(0, "#14B8A6"), c(0.5, "#F6F4FB"), c(1, "#F43F5E"))
SCALE_RISK      <- list(c(0, "#F8F7FC"), c(0.5, "#FDA4AF"), c(1, "#BE123C"))
SCALE_SHARE     <- list(c(0, "#F4F1FD"), c(1, "#6D4AFF"))
FONT_BODY <- "Plus Jakarta Sans, Segoe UI, Helvetica, Arial, sans-serif"

# The six datasets, in the order of the upload cards.
DS <- list(
  emp = list(label = "Employee Master", ctrl = "f_emp", file = "Innovatech_Employee_Master.csv",
             key = "employee_id", simulated = FALSE,
             cols = c("employee_id", "date_of_birth", "age", "department", "grade", "education",
                      "date_of_joining", "yos", "prior_experience", "total_experience", "compa_ratio",
                      "num_promotions", "Appraisal 2024", "Appraisal 2023", "Appraisal 2022",
                      "Appraisal 2021", "Appraisal 2020", "last_working_day", "Employee Status"),
             need_all = "employee_id", need_any = c("last_working_day", "Employee Status")),
  gd  = list(label = "Glassdoor Reviews", ctrl = "f_gd", file = "Innovatech_Glassdoor.csv",
             key = "review_id", simulated = FALSE,
             cols = c("review_id", "date", "overall_rating", "review_text"),
             need_all = character(0), need_any = c("review_text", "overall_rating")),
  li  = list(label = "LinkedIn Posts", ctrl = "f_li", file = "Innovatech_LinkedIn.csv",
             key = "post_id", simulated = FALSE,
             cols = c("post_id", "date", "post_type", "likes", "comments", "shares"),
             need_all = "post_type", need_any = c("likes", "comments", "shares")),
  jp  = list(label = "Job Postings", ctrl = "f_jp", file = "Innovatech_JobPostings.csv",
             key = "job_id", simulated = FALSE,
             cols = c("job_id", "date_posted", "job_title", "vmv_alignment_score", "time_to_fill"),
             need_all = "job_id", need_any = c("time_to_fill", "vmv_alignment_score")),
  lc  = list(label = "LinkedIn Comments", ctrl = "f_lc", file = "Innovatech_LinkedIn_Comments_SIMULATED.csv",
             key = "comment_id", simulated = TRUE,
             cols = c("comment_id", "post_id", "date", "commenter_type", "comment_text"),
             need_all = "comment_text", need_any = character(0)),
  ja  = list(label = "Job Advertisement Text", ctrl = "f_ja", file = "Innovatech_JobAds_Text_SIMULATED.csv",
             key = "job_id", simulated = TRUE,
             cols = c("job_id", "job_title", "ad_text"),
             need_all = c("job_id", "ad_text"), need_any = character(0))
)
KEYS <- names(DS)

NUMERIC_COLS <- c("age", "yos", "prior_experience", "total_experience", "compa_ratio",
                  "num_promotions", "overall_rating", "vmv_alignment_score", "time_to_fill",
                  "likes", "comments", "shares")
DATE_COLS    <- c("date_of_birth", "date_of_joining", "last_working_day", "date", "date_posted")

READABLE <- c(age = "Age", yos = "Years of service", prior_experience = "Prior experience",
              total_experience = "Total experience", compa_ratio = "Compa ratio",
              num_promotions = "Promotions", last_appraisal = "Latest appraisal",
              appraisal_change = "Change in appraisal", department = "Department",
              grade = "Grade", education = "Education")
readable <- function(v) ifelse(v %in% names(READABLE), READABLE[v], v)

# Choices for the attrition heatmap axes
AXIS_CHOICES <- c("Department" = "department", "Grade" = "grade", "Education" = "education",
                  "Tenure band" = "yos_band", "Age band" = "age_band",
                  "Compa-ratio band" = "compa_band", "Promotions" = "promo_band",
                  "Latest appraisal" = "appr_band")


# -----------------------------------------------------------------------------
# 2. TEXT RESOURCES
# -----------------------------------------------------------------------------
NEGATORS <- c("not", "no", "never", "dont", "doesnt", "didnt", "isnt", "wasnt", "cant",
              "cannot", "wont", "hardly", "without", "arent", "werent", "nobody", "nothing")

SNOWBALL <- c("i","me","my","myself","we","our","ours","ourselves","you","your","yours",
  "yourself","yourselves","he","him","his","himself","she","her","hers","herself","it","its",
  "itself","they","them","their","theirs","themselves","what","which","who","whom","this",
  "that","these","those","am","is","are","was","were","be","been","being","have","has","had",
  "having","do","does","did","doing","would","should","could","ought","a","an","the","and",
  "but","if","or","because","as","until","while","of","at","by","for","with","about",
  "against","between","into","through","during","before","after","above","below","to",
  "from","up","down","in","out","on","off","over","under","again","further","then","once",
  "here","there","when","where","why","how","all","any","both","each","few","more","most",
  "other","some","such","only","own","same","so","than","too","very","will","can","just")
COMPANY_WORDS <- c("innovatech", "company", "pros", "cons", "advice", "also", "get", "got",
                   "one", "really", "lot", "lots", "etc", "even", "still", "make", "made",
                   "way", "thing", "things", "us", "im", "ive")
if (USE_TIDYTEXT) {
  sw <- tidytext::stop_words
  SNOWBALL <- unique(c(SNOWBALL, sw$word[sw$lexicon == "snowball"]))
}
STOP_WORDS <- setdiff(unique(c(gsub("'", "", SNOWBALL), COMPANY_WORDS)), NEGATORS)

# Built-in sentiment words (always used; merged with Bing when tidytext exists)
POS_OWN <- c("good","great","excellent","amazing","awesome","best","love","friendly","supportive",
  "helpful","flexible","growth","learning","opportunity","opportunities","fair","transparent",
  "innovative","inspiring","rewarding","collaborative","happy","enjoy","nice","positive","strong",
  "stable","recognition","respect","empowering","competitive","fantastic","wonderful","brilliant",
  "smart","talented","caring","inclusive","diverse","fun","exciting","proud","trust","generous",
  "encouraging","appreciated","motivated","efficient","clear","kind","perks","progressive","easy",
  "satisfied","solid","success","successful","congratulations","congrats","thanks","thank","glad",
  "valued","welcoming","balanced","healthy","secure","safe","ethical","honest","passionate")
NEG_OWN <- c("bad","poor","terrible","awful","worst","worse","hate","toxic","stressful","stress",
  "pressure","overworked","burnout","underpaid","unfair","politics","political","micromanagement",
  "micromanage","slow","bureaucracy","bureaucratic","lack","lacking","chaos","chaotic",
  "disorganized","layoffs","layoff","insecure","insecurity","favoritism","favouritism","biased",
  "bias","boring","rigid","hostile","rude","disappointing","frustrating","frustrated","low",
  "limited","stagnant","outdated","unclear","confusing","ignored","overtime","exhausting",
  "mediocre","horrible","unprofessional","unsupportive","inflexible","issues","issue","problem",
  "problems","concern","unhappy","sad","angry","useless","waste","delay","delays","nepotism",
  "blame","fear","unpaid","exploitative","stressed","demotivated","demotivating","quit",
  "harassment","discrimination")
POS_WORDS <- POS_OWN; NEG_WORDS <- NEG_OWN; LEXICON_NAME <- "built-in word list"
if (USE_TIDYTEXT) {
  bing <- tryCatch(tidytext::get_sentiments("bing"), error = function(e) NULL)
  if (!is.null(bing)) {
    drop_words <- c("like", "work", "well", "right", "pro", "pros", "con", "cons", "support")
    POS_WORDS <- unique(c(setdiff(bing$word[bing$sentiment == "positive"], drop_words), POS_OWN))
    NEG_WORDS <- unique(c(setdiff(bing$word[bing$sentiment == "negative"], drop_words), NEG_OWN))
    POS_WORDS <- setdiff(POS_WORDS, NEG_OWN); NEG_WORDS <- setdiff(NEG_WORDS, POS_OWN)
    LEXICON_NAME <- "Bing lexicon (tidytext) plus a built-in list"
  }
}
POS_WORDS <- gsub("'", "", POS_WORDS); NEG_WORDS <- gsub("'", "", NEG_WORDS)

# Themes searched in reviews, comments and ads
THEMES <- list(
  "Pay & benefits"    = "salar|pay|paid|compensat|bonus|benefit|perk|increment|hike|incentive",
  "Growth & learning" = "growth|grow\\b|career|promot|learn|training|skill|develop|mentor",
  "Culture & people"  = "culture|team|colleague|people|environment|friendly|inclusi|divers|respect",
  "Work-life balance" = "balance|hours|overtime|flexib|remote|hybrid|stress|burnout|weekend|workload",
  "Leadership"        = "manager|management|leader|boss|senior|micromanag|director",
  "Innovation & tech" = "innovat|technolog|tech\\b|ai\\b|cutting.edge|product|engineer",
  "Purpose & values"  = "mission|vision|value|purpose|impact|integrity|trust|ethic",
  "Job security"      = "secur|stable|stabilit|layoff|lay.off|restructur")
VALUES_RX <- "\\b(mission|vision|values?|purpose|culture|integrity|innovat\\w*|inclusi\\w*|divers\\w*)"

# Emoji: detection ranges, characters to strip, and a small scored list
EMOJI_RX    <- "[\\x{1F300}-\\x{1FAFF}\\x{2600}-\\x{27BF}\\x{2B50}\\x{2B55}]"
EMOJI_STRIP <- "[\\x{FE0F}\\x{200D}\\x{1F3FB}-\\x{1F3FF}]"
EMOJI_POS <- c("\U0001F600","\U0001F603","\U0001F604","\U0001F601","\U0001F60A","\U0001F642",
               "\U0001F60D","\U0001F970","\U0001F60E","\U0001F44D","\U0001F44F","\U0001F64C",
               "\U0001F389","\U0001F4AF","\U00002764","\U0001F499","\U0001F49A","\U00002728",
               "\U0001F680","\U0001F525","\U0001F3C6","\U0001F4AA","\U0001F929","\U0001F602",
               "\U0001F64F","\U0001F31F","\U00002B50","\U0001F44C","\U0001F91D","\U0001F973")
EMOJI_NEG <- c("\U0001F621","\U0001F620","\U0001F61E","\U0001F622","\U0001F62D","\U0001F612",
               "\U0001F644","\U0001F44E","\U0001F494","\U0001F624","\U0001F629","\U0001F62B",
               "\U0001F92C","\U0001F615","\U0001F61F","\U0001F62C","\U0001F611","\U0001F613")

DASH  <- "\u2014"   # shown when a number is not available
RUPEE <- "\u20b9"


# -----------------------------------------------------------------------------
# 3. SMALL HELPERS
# -----------------------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
norm_name <- function(x) gsub("[^a-z0-9]", "", tolower(x))
has_col   <- function(d, v) is.data.frame(d) && v %in% names(d) && any(!is.na(d[[v]]))
as_text   <- function(x) { x <- iconv(as.character(x), "UTF-8", "UTF-8", sub = " "); x[is.na(x)] <- ""; x }
bold      <- function(x) paste0("<b>", x, "</b>")

num_parse <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  suppressWarnings(as.numeric(gsub("[^0-9eE.+-]", "", trimws(as.character(x)))))
}
# Try common date formats, keep the one that reads the most values;
# also read Excel serial numbers; blank years outside 1940-2100.
date_parse <- function(x) {
  if (inherits(x, "Date")) return(x)
  x <- sub("[ T][0-9]{1,2}:[0-9]{2}.*$", "", trimws(as.character(x)))
  formats <- c("%Y-%m-%d", "%d-%m-%Y", "%d/%m/%Y", "%m/%d/%Y", "%Y/%m/%d", "%d-%b-%Y",
               "%d %b %Y", "%b %d, %Y", "%d.%m.%Y", "%d-%b-%y", "%d/%m/%y", "%m/%d/%y")
  best <- as.Date(rep(NA_character_, length(x))); best_n <- 0
  for (f in formats) {
    d <- suppressWarnings(as.Date(x, format = f)); n <- sum(!is.na(d))
    if (n > best_n) { best <- d; best_n <- n }
  }
  if (best_n == 0) {
    s <- suppressWarnings(as.numeric(x))
    if (any(!is.na(s))) best <- as.Date(s, origin = "1899-12-30")
  }
  yr <- as.numeric(format(best, "%Y"))
  best[!is.na(yr) & (yr < 1940 | yr > 2100)] <- NA
  best
}
# Appraisals: numbers as they are; text ratings mapped to 1-5
appraisal_parse <- function(x) {
  n <- num_parse(x); given <- !is.na(x) & nzchar(trimws(as.character(x)))
  if (!any(given) || mean(!is.na(n[given])) >= 0.6) return(n)
  s <- tolower(as.character(x)); out <- rep(NA_real_, length(s))
  out[grepl("outstanding|exceptional", s)] <- 5
  out[is.na(out) & grepl("exceed", s)] <- 4
  out[is.na(out) & grepl("meet", s)] <- 3
  out[is.na(out) & grepl("below|improve", s)] <- 2
  out[is.na(out) & grepl("unsatisf|poor", s)] <- 1
  out
}
# Mean or median, as chosen in the sidebar
avg <- function(x, how = "mean") {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  if (identical(how, "median")) median(x) else mean(x)
}
show_num <- function(x, digits = 1, suffix = "", prefix = "") {
  if (is.null(x) || !length(x) || !is.finite(x[1])) return(DASH)
  paste0(prefix, formatC(x[1], format = "f", digits = digits, big.mark = ","), suffix)
}
signed   <- function(x, digits = 0) if (length(x) && is.finite(x)) sprintf(paste0("%+.", digits, "f"), x) else "n/a"
share_of <- function(a, b) if (isTRUE(b > 0)) 100 * a / b else NA_real_
levels_of <- function(x) if (is.factor(x)) levels(droplevels(x)) else sort(unique(as.character(x)))
# Long table (row, col, value) -> matrix for heatmaps
to_matrix <- function(df, r, c, v) {
  rs <- levels_of(df[[r]]); cs <- levels_of(df[[c]])
  m <- matrix(if (is.character(df[[v]])) NA_character_ else NA_real_, length(rs), length(cs),
              dimnames = list(rs, cs))
  m[cbind(match(as.character(df[[r]]), rs), match(as.character(df[[c]]), cs))] <- df[[v]]
  m
}
# VMV score -> 0-100 using the maximum value in the file
vmv_factor <- function(x) {
  mx <- suppressWarnings(max(x, na.rm = TRUE))
  if (!is.finite(mx)) return(1)
  if (mx <= 1) 100 else if (mx <= 5) 20 else if (mx <= 10) 10 else 1
}


# -----------------------------------------------------------------------------
# 4. TEXT ANALYTICS
# -----------------------------------------------------------------------------
tokenise <- function(x) {
  x <- tolower(as_text(x))
  x <- gsub("http\\S+|www\\.\\S+", " ", x, perl = TRUE)
  x <- gsub("['\u2019]", "", x)
  x <- gsub("[^a-z]+", " ", x)
  lapply(strsplit(trimws(x), "\\s+"), function(t) t[nchar(t) > 1])
}
# Positive and negative word counts per text. A negator up to two words
# before a sentiment word flips it ("not good" counts as negative).
polarity_counts <- function(tokens) {
  if (!length(tokens)) return(matrix(0, 2, 0))
  vapply(tokens, function(t) {
    if (!length(t)) return(c(0, 0))
    s <- ifelse(t %in% POS_WORDS, 1, ifelse(t %in% NEG_WORDS, -1, 0))
    flip <- c("", t)[seq_along(t)] %in% NEGATORS | c("", "", t)[seq_along(t)] %in% NEGATORS
    s[flip] <- -s[flip]
    c(sum(s > 0), sum(s < 0))
  }, numeric(2))
}
# Sentiment = (positive - negative words) / (positive + negative), -1..+1;
# 0 when the text has no sentiment words. Label by sign.
score_texts <- function(d, col) {
  tokens <- tokenise(d[[col]]); m <- polarity_counts(tokens)
  d$pos_n <- m[1, ]; d$neg_n <- m[2, ]
  total   <- d$pos_n + d$neg_n
  d$sent  <- ifelse(total > 0, (d$pos_n - d$neg_n) / total, 0)
  d$lab   <- ifelse(d$sent > 0, "Positive", ifelse(d$sent < 0, "Negative", "Neutral"))
  list(d = d, tokens = tokens)
}
# Net Sentiment Score = % positive - % negative (-100..+100, higher = better)
nss <- function(lab) {
  lab <- lab[!is.na(lab)]
  if (!length(lab)) return(NA_real_)
  100 * (sum(lab == "Positive") - sum(lab == "Negative")) / length(lab)
}
# Word runs (n-grams) after removing stop words
ngrams <- function(tokens, n) unlist(lapply(tokens, function(t) {
  t <- t[!t %in% STOP_WORDS]; L <- length(t)
  if (L < n) return(character(0))
  if (n == 1) return(t)
  do.call(paste, lapply(0:(n - 1), function(j) t[(1 + j):(L - n + 1 + j)]))
}), use.names = FALSE)
# Share of texts mentioning each theme, and the net sentiment of those texts
theme_table <- function(text, lab, source) {
  low <- tolower(as_text(text))
  do.call(rbind, lapply(names(THEMES), function(th) {
    hit <- grepl(paste0("\\b(", THEMES[[th]], ")"), low, perl = TRUE)
    data.frame(source = source, theme = th, n_docs = length(low), n_hit = sum(hit),
               share = if (length(low)) 100 * mean(hit) else NA_real_,
               net = if (any(hit) && !is.null(lab)) nss(lab[hit]) else NA_real_,
               stringsAsFactors = FALSE)
  }))
}

# Fallback topic model: collapsed Gibbs sampling LDA in base R
lda_base <- function(docs, V, k, iters = 60, alpha = 0.1, beta = 0.01) {
  set.seed(42); D <- length(docs)
  z <- lapply(docs, function(w) sample.int(k, length(w), replace = TRUE))
  ndk <- matrix(0, D, k); nkw <- matrix(0, k, V); nk <- numeric(k)
  for (d in seq_len(D)) for (i in seq_along(docs[[d]])) {
    t <- z[[d]][i]; w <- docs[[d]][i]
    ndk[d, t] <- ndk[d, t] + 1; nkw[t, w] <- nkw[t, w] + 1; nk[t] <- nk[t] + 1
  }
  for (it in seq_len(iters)) for (d in seq_len(D)) {
    w <- docs[[d]]; zz <- z[[d]]
    for (i in seq_along(w)) {
      t <- zz[i]; wi <- w[i]
      ndk[d, t] <- ndk[d, t] - 1; nkw[t, wi] <- nkw[t, wi] - 1; nk[t] <- nk[t] - 1
      t <- sample.int(k, 1L, prob = (ndk[d, ] + alpha) * (nkw[, wi] + beta) / (nk + V * beta))
      zz[i] <- t; ndk[d, t] <- ndk[d, t] + 1; nkw[t, wi] <- nkw[t, wi] + 1; nk[t] <- nk[t] + 1
    }
    z[[d]] <- zz
  }
  list(theta = (ndk + alpha) / rowSums(ndk + alpha), phi = (nkw + beta) / rowSums(nkw + beta))
}
# Topic model: words of 3+ letters, not stop words, in 3+ reviews and in no more
# than half of reviews (max 1,500 words). topicmodels Gibbs LDA when available.
fit_topics <- function(tokens, k) {
  tokens <- lapply(tokens, function(t) t[!t %in% STOP_WORDS & nchar(t) >= 3])
  df <- table(unlist(lapply(tokens, unique)))
  vocab <- names(df)[df >= 3 & df <= 0.5 * length(tokens)]
  if (length(vocab) > 1500) vocab <- names(sort(df[vocab], decreasing = TRUE))[1:1500]
  tokens <- lapply(tokens, function(t) t[t %in% vocab])
  keep <- which(lengths(tokens) >= 2)
  if (length(keep) < 20 || length(vocab) < 10) return(NULL)
  tokens <- tokens[keep]
  if (USE_TOPICMODELS) {
    res <- tryCatch({
      counts <- data.frame(doc = rep(as.character(seq_along(tokens)), lengths(tokens)),
                           term = unlist(tokens), stringsAsFactors = FALSE) %>% count(doc, term)
      dtm <- tidytext::cast_dtm(counts, doc, term, n)
      fit <- topicmodels::LDA(dtm, k = k, method = "Gibbs",
                              control = list(seed = 42, iter = 500, burnin = 100))
      post <- topicmodels::posterior(fit)
      theta <- post$topics[order(as.integer(rownames(post$topics))), , drop = FALSE]
      list(theta = theta, phi = post$terms, vocab = colnames(post$terms), keep = keep,
           engine = "topicmodels (Gibbs LDA)")
    }, error = function(e) NULL)
    if (!is.null(res)) return(res)
  }
  if (length(tokens) > 800) {
    set.seed(42); s <- sort(sample(length(tokens), 800)); tokens <- tokens[s]; keep <- keep[s]
  }
  vocab2 <- sort(unique(unlist(tokens)))
  r <- lda_base(lapply(tokens, function(t) match(t, vocab2)), length(vocab2), k)
  list(theta = r$theta, phi = r$phi, vocab = vocab2, keep = keep,
       engine = "built-in LDA (topicmodels not installed)")
}


# -----------------------------------------------------------------------------
# 5. READING AND CLEANING A FILE
# -----------------------------------------------------------------------------
read_csv_file <- function(path) {
  d <- tryCatch(suppressWarnings(read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
         na.strings = c("", "NA", "N/A", "NULL", "-", "--", "#N/A"), encoding = "UTF-8",
         strip.white = TRUE, comment.char = "")), error = function(e) NULL)
  if (!is.null(d)) names(d) <- trimws(gsub("^\ufeff|^\u00ef\u00bb\u00bf", "", names(d)))  # strip BOM
  d
}

# Employee Master extras: leaver flag, latest appraisal, change, bands
add_employee_fields <- function(d) {
  notes <- character(0)
  # Leaver rule: last_working_day filled = 1, blank = 0
  if ("last_working_day" %in% names(d)) {
    d$left <- as.integer(!is.na(d$last_working_day))
  } else {
    # Fallback only when last_working_day is absent: read Employee Status words
    s <- tolower(trimws(d[["Employee Status"]]))
    d$left <- NA_integer_
    d$left[grepl("exit|left|leaver|resign|terminat|separat|inactive|former|quit|retire", s)] <- 1L
    d$left[is.na(d$left) & grepl("active|current|employed|working|present|existing", s)] <- 0L
    known <- tapply(d$left, s, function(z) z[1])
    if (length(known) == 2 && sum(is.na(known)) == 1)
      d$left[!is.na(s) & s == names(known)[is.na(known)]] <- 1L - known[!is.na(known)][[1]]
  }
  if ("Employee Status" %in% names(d)) {
    st <- as.character(d[["Employee Status"]]); vals <- sort(unique(st[!is.na(st)]))
    notes <- c(notes, paste0("Status values: ", paste(vapply(vals, function(v)
      sprintf("%s = %d (%d with exit date)", v, sum(st == v, na.rm = TRUE),
              sum(st == v & d$left == 1, na.rm = TRUE)), character(1)), collapse = "; "), "."))
  }
  notes <- c(notes, sprintf("Leavers: %d of %d (%.1f%%).", sum(d$left == 1, na.rm = TRUE), nrow(d),
                            100 * mean(d$left == 1, na.rm = TRUE)))
  # Fill blank yos only from date_of_joining to last_working_day (or today)
  if (all(c("yos", "date_of_joining") %in% names(d))) {
    blank <- is.na(d$yos) & !is.na(d$date_of_joining)
    end <- if ("last_working_day" %in% names(d)) d$last_working_day else as.Date(rep(NA_character_, nrow(d)))
    end[is.na(end)] <- Sys.Date()
    d$yos[blank] <- as.numeric(end[blank] - d$date_of_joining[blank]) / 365.25
    if (any(blank)) notes <- c(notes, sprintf("%d blank yos filled from date_of_joining.", sum(blank)))
  }
  # Appraisals: blanks mean "not yet joined" and are never filled
  appr <- grep("^Appraisal\\s*[0-9]{4}$", names(d), value = TRUE)
  if (length(appr)) {
    appr <- appr[order(-as.numeric(gsub("\\D", "", appr)))]          # newest year first
    A <- as.matrix(d[, appr, drop = FALSE])
    d$last_appraisal <- apply(A, 1, function(r) { r <- r[!is.na(r)]; if (length(r)) r[1] else NA_real_ })
    prev <- apply(A, 1, function(r) { r <- r[!is.na(r)]; if (length(r) > 1) r[2] else NA_real_ })
    d$appraisal_change <- d$last_appraisal - prev
    d$appr_band <- factor(round(d$last_appraisal))
  }
  if ("yos" %in% names(d))
    d$yos_band <- cut(d$yos, c(-Inf, 1, 3, 5, 10, Inf), right = FALSE,
                      labels = c("<1 yr", "1-3 yrs", "3-5 yrs", "5-10 yrs", "10+ yrs"))
  if ("age" %in% names(d))
    d$age_band <- cut(d$age, c(-Inf, 25, 30, 35, 45, Inf), right = FALSE,
                      labels = c("<25", "25-29", "30-34", "35-44", "45+"))
  if ("compa_ratio" %in% names(d))
    d$compa_band <- cut(d$compa_ratio, c(-Inf, 0.9, 1, 1.1, Inf), right = FALSE,
                        labels = c("<0.90", "0.90-0.99", "1.00-1.09", "1.10+"))
  if ("num_promotions" %in% names(d)) {
    p <- round(d$num_promotions)
    d$promo_band <- factor(ifelse(is.na(p), NA, ifelse(p >= 3, "3+", as.character(p))),
                           levels = c("0", "1", "2", "3+"))
  }
  list(d = d, notes = notes)
}

# Validate and clean one uploaded file. Errors stop with a plain message.
clean_file <- function(key, raw) {
  spec <- DS[[key]]; expected <- spec$cols; found <- names(raw)
  # Wrong box? Compare the headers with every dataset's expected columns.
  fit <- vapply(KEYS, function(k) mean(norm_name(DS[[k]]$cols) %in% norm_name(found)), numeric(1))
  best <- names(which.max(fit))
  if (best != key && fit[[best]] >= 0.6 && fit[[best]] > fit[[key]])
    stop(sprintf("Wrong box: this looks like the %s file (%d of %d of its columns found). Upload it in the '%s' box, and upload %s here.",
                 DS[[best]]$label, round(fit[[best]] * length(DS[[best]]$cols)), length(DS[[best]]$cols),
                 DS[[best]]$label, spec$file))
  map <- match(norm_name(found), norm_name(expected))       # file column -> expected column
  if (all(is.na(map)))
    stop(sprintf("None of the expected columns were found. Expected %s with: %s.", spec$file, paste(expected, collapse = ", ")))
  # Schema report for the "detected" table
  schema <- do.call(rbind, lapply(seq_along(expected), function(i) {
    j <- which(map == i)[1]
    data.frame(Dataset = spec$label, Column = expected[i],
               Status = if (is.na(j)) "Missing" else if (found[j] == expected[i]) "Found" else paste0("Found as '", found[j], "'"),
               stringsAsFactors = FALSE)
  }))
  extra <- found[is.na(map)]
  if (length(extra)) schema <- rbind(schema, data.frame(Dataset = spec$label, Column = extra,
                                                        Status = "Extra (ignored)", stringsAsFactors = FALSE))
  use <- which(!is.na(map) & !duplicated(map))
  d <- raw[, use, drop = FALSE]; names(d) <- expected[map[use]]
  # Required columns
  lacking <- setdiff(spec$need_all, names(d))
  if (length(lacking)) stop("Required column(s) missing: ", paste(lacking, collapse = ", "))
  if (length(spec$need_any) && !any(spec$need_any %in% names(d)))
    stop("Need at least one of: ", paste(spec$need_any, collapse = ", "))
  # Types
  for (v in names(d)) {
    if (v %in% NUMERIC_COLS) d[[v]] <- num_parse(d[[v]])
    else if (grepl("^Appraisal", v)) d[[v]] <- appraisal_parse(d[[v]])
    else if (v %in% DATE_COLS) d[[v]] <- date_parse(d[[v]])
    else { x <- trimws(as_text(d[[v]])); x[x == ""] <- NA; d[[v]] <- x }
  }
  # Impossible values are blanked (never invented) and noted
  notes <- character(0)
  blank_if <- function(v, bad, why) {
    if (!v %in% names(d)) return(invisible())
    b <- !is.na(d[[v]]) & bad(d[[v]])
    if (any(b)) { d[[v]][b] <<- NA; notes <<- c(notes, sprintf("%d %s values blanked (%s).", sum(b), v, why)) }
  }
  blank_if("overall_rating", function(x) x < 1 | x > 5, "outside 1-5")
  blank_if("compa_ratio", function(x) x <= 0 | x > 5, "not plausible")
  blank_if("age", function(x) x < 15 | x > 80, "outside 15-80")
  for (v in c("likes", "comments", "shares", "num_promotions", "yos", "prior_experience", "time_to_fill"))
    blank_if(v, function(x) x < 0, "negative")
  # Empty rows, duplicate rows, blank IDs, duplicate IDs (keep the first)
  n0 <- nrow(d)
  d <- d[rowSums(!is.na(d)) > 0, , drop = FALSE]
  d <- d[!duplicated(d), , drop = FALSE]
  dup_rows <- n0 - nrow(d); blank_ids <- 0; dup_ids <- 0
  if (spec$key %in% names(d)) {
    blank_ids <- sum(is.na(d[[spec$key]])); d <- d[!is.na(d[[spec$key]]), , drop = FALSE]
    dup <- duplicated(d[[spec$key]]); dup_ids <- sum(dup); d <- d[!dup, , drop = FALSE]
  }
  if (!nrow(d)) stop("No usable rows remain after cleaning.")
  schema$Type <- ""; schema$`Missing %` <- NA_real_
  for (i in seq_len(nrow(schema))) if (schema$Column[i] %in% names(d)) {
    schema$Type[i] <- class(d[[schema$Column[i]]])[1]
    schema$`Missing %`[i] <- round(100 * mean(is.na(d[[schema$Column[i]]])), 1)
  }
  if (key == "emp") { x <- add_employee_fields(d); d <- x$d; notes <- c(notes, x$notes) }
  if (spec$simulated) notes <- c(notes, "Simulated file: findings are illustrative.")
  list(d = d, schema = schema, notes = notes,
       removed = sprintf("%d duplicate rows, %d duplicate IDs, %d blank IDs", dup_rows, dup_ids, blank_ids))
}


# -----------------------------------------------------------------------------
# 6. ATTRITION MODEL
# -----------------------------------------------------------------------------
# Logistic regression, stratified 70/30 split (seed 42), AUC on the 30% test set.
fit_attrition <- function(emp) {
  d <- emp[!is.na(emp$left), ]
  if (nrow(d) < 50 || sum(d$left == 1) < 10 || sum(d$left == 0) < 10) return(NULL)
  nums <- intersect(c("age", "yos", "prior_experience", "compa_ratio", "num_promotions",
                      "last_appraisal", "appraisal_change"), names(d))
  nums <- nums[vapply(nums, function(v) mean(!is.na(d[[v]])) >= 0.5 && isTRUE(sd(d[[v]], na.rm = TRUE) > 0), logical(1))]
  for (v in nums) d[[v]][is.na(d[[v]])] <- median(d[[v]], na.rm = TRUE)       # median-impute
  facs <- intersect(c("department", "grade", "education"), names(d))
  for (v in facs) {
    x <- as.character(d[[v]]); x[is.na(x)] <- "Unknown"
    small <- names(which(table(x) < 20)); x[x %in% small] <- "Other"          # lump small levels
    d[[v]] <- relevel(factor(x), ref = names(which.max(table(x))))             # most common = reference
  }
  facs <- facs[vapply(facs, function(v) nlevels(d[[v]]) > 1, logical(1))]
  vars <- c(nums, facs)
  if (!length(vars)) return(NULL)
  set.seed(42)
  train_rows <- unlist(lapply(split(seq_len(nrow(d)), d$left),
                              function(ix) ix[sample.int(length(ix), floor(0.7 * length(ix)))]))
  train <- d[train_rows, ]; test <- d[-train_rows, ]
  model <- suppressWarnings(glm(reformulate(vars, "left"), data = train, family = binomial()))
  p_test <- suppressWarnings(predict(model, test, type = "response"))
  # Benchmark rule: mean of standardised low pay, few promotions, low appraisal, short tenure
  rule_parts <- lapply(intersect(c("compa_ratio", "num_promotions", "last_appraisal", "yos"), nums),
                       function(v) -as.numeric(scale(d[[v]])))
  rule <- if (length(rule_parts)) rowMeans(do.call(cbind, rule_parts)) else rep(NA_real_, nrow(d))
  d$prob <- suppressWarnings(predict(model, d, type = "response"))
  # Effects: numeric = coefficient x SD (effect of +1 SD); factor = level vs reference
  cf <- coef(model)[-1]; cf <- cf[!is.na(cf)]
  eff <- data.frame(term = names(cf), effect = as.numeric(cf), stringsAsFactors = FALSE)
  for (i in seq_len(nrow(eff))) {
    v <- eff$term[i]
    if (v %in% nums) { eff$effect[i] <- eff$effect[i] * sd(train[[v]]); eff$term[i] <- paste0(readable(v), " (+1 SD)") }
    else { f <- facs[startsWith(v, facs)][1]
      if (!is.na(f)) eff$term[i] <- paste0(readable(f), ": ", substring(v, nchar(f) + 1), " vs ", levels(train[[f]])[1]) }
  }
  eff$odds <- exp(eff$effect)
  eff <- head(eff[order(-abs(eff$effect)), ], 15)
  list(model = model, scored = d, nums = nums, facs = facs, n_test = nrow(test),
       auc_model = auc_score(test$left, p_test), auc_rule = auc_score(test$left, rule[-train_rows]),
       roc_model = roc_points(test$left, p_test), roc_rule = roc_points(test$left, rule[-train_rows]),
       effects = eff)
}
auc_score <- function(y, p) {
  ok <- !is.na(y) & !is.na(p); y <- y[ok]; p <- p[ok]
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  if (!n1 || !n0) return(NA_real_)
  (sum(rank(p)[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
roc_points <- function(y, p) {
  ok <- !is.na(y) & !is.na(p); y <- y[ok][order(p[ok], decreasing = TRUE)]
  if (!length(y)) return(data.frame(fpr = numeric(0), tpr = numeric(0)))
  out <- data.frame(fpr = c(0, cumsum(y == 0) / sum(y == 0)), tpr = c(0, cumsum(y == 1) / sum(y == 1)))
  out[unique(round(seq(1, nrow(out), length.out = min(nrow(out), 400)))), ]
}


# -----------------------------------------------------------------------------
# 7. SCREEN BUILDING BLOCKS
# -----------------------------------------------------------------------------
# A chart card: Enlarge icon (full_screen) and a takeaway footer
chart_card <- function(id, title, height = "340px")
  card(full_screen = TRUE, card_header(title), plotlyOutput(id, height = height),
       card_footer(class = "tk", uiOutput(paste0("tk_", id))))
table_card <- function(id, title)
  card(full_screen = TRUE, card_header(title), DTOutput(id),
       card_footer(class = "tk", uiOutput(paste0("tk_", id))))
two_up <- function(a, b) layout_columns(col_widths = c(6, 6), a, b)
number_row <- function(...) {
  ids <- c(...)
  do.call(layout_columns, c(list(col_widths = rep(12 / length(ids), length(ids))), lapply(ids, uiOutput)))
}
insight_slot <- function(id) div(class = "ins", uiOutput(id))
upload_card <- function(key) {
  s <- DS[[key]]
  card(card_header(s$label),
       fileInput(s$ctrl, NULL, accept = c(".csv", "text/csv"), placeholder = "No file"),
       div(class = "hint", "Expects ", tags$b(s$file), br(),
           "Columns: ", paste(head(s$cols, 6), collapse = ", "), if (length(s$cols) > 6) ", ..." else ""),
       actionButton(paste0("rm_", key), "Remove file", class = "btn-sm btn-rm mt-2"))
}
# Number box: label, value, data-driven subtitle, tone (blue/teal/amber/red)
number_box <- function(label, value, sub = "", tone = "blue")
  div(class = paste("vbox", tone), div(class = "vlab", label), div(class = "vval", value),
      div(class = "vsub", HTML(sub)))
# Insight box: four parts
insight_box <- function(read, shows, means, caveat)
  tagList(tags$h6("What this tab tells you"),
          tags$p(tags$b("How to read: "), HTML(read)),
          tags$p(tags$b("What it shows: "), HTML(shows)),
          tags$p(tags$b("What it means for the business: "), HTML(means)),
          tags$p(class = "cav", tags$b("What it does not prove: "), HTML(caveat)))
waiting_box <- function(what) insight_box(paste0("Upload ", what, " in Tab 1."), "Results appear here once the file is loaded.", DASH, DASH)
need_file <- function(key) paste0("Upload ", DS[[key]]$label, " (", DS[[key]]$file, ") in Tab 1. ",
                                  "If it is uploaded, the current filters may have removed all rows.")

CSS <- "
:root{--ink:#24163A;--ink2:#3B3452;--muted:#6B6485;--violet:#6D4AFF;--violet2:#8B5CF6;--teal:#14B8A6;
      --saffron:#F59E0B;--rose:#F43F5E;--canvas:#F4F2FA;--line:#E7E3F3;--tint:#F6F3FF}
body{background:var(--canvas);color:var(--ink2);font-family:'Plus Jakarta Sans','Segoe UI',sans-serif}
h1,h2,h3,h4,h5,h6{font-family:'Fraunces',Georgia,serif;color:var(--ink)}

/* Header: dark band with three slowly drifting colour blobs */
.navbar.navbar-static-top{padding:0 !important;background:transparent !important;border:0 !important;min-height:0}
.navbar.navbar-static-top>.container-fluid{padding:0}
.hero{position:relative;overflow:hidden;width:100%;background:#1E1233;color:#fff;padding:26px 34px 22px}
.blob{position:absolute;border-radius:50%;filter:blur(70px);pointer-events:none;animation:drift 18s ease-in-out infinite alternate}
.blob.b1{width:440px;height:440px;background:#6D4AFF;top:-220px;left:-60px;opacity:.55}
.blob.b2{width:360px;height:360px;background:#14B8A6;top:-170px;right:12%;opacity:.4;animation-duration:23s}
.blob.b3{width:320px;height:320px;background:#EC4899;bottom:-230px;left:42%;opacity:.38;animation-duration:27s}
@keyframes drift{0%{transform:translate(0,0) scale(1)}100%{transform:translate(70px,40px) scale(1.18)}}
.hero-row{position:relative;display:flex;justify-content:space-between;align-items:flex-end;gap:24px;flex-wrap:wrap}
.hero-title{font-family:'Fraunces',Georgia,serif;font-weight:600;font-size:clamp(28px,3.1vw,44px);line-height:1.04;
  letter-spacing:-.015em;margin:0;color:#fff;animation:rise .8s cubic-bezier(.2,.7,.2,1) both}
.hero-sub{color:#D9D2F0;max-width:62ch;margin:8px 0 14px;font-size:15px;animation:rise .8s .12s cubic-bezier(.2,.7,.2,1) both}
.chips{display:flex;gap:8px;flex-wrap:wrap}
.chip{display:inline-flex;align-items:center;gap:8px;padding:6px 13px;border-radius:999px;font-size:13px;font-weight:600;
  color:#F3EFFF;background:rgba(255,255,255,.08);border:1px solid rgba(255,255,255,.18);
  animation:rise .7s cubic-bezier(.2,.7,.2,1) both;transition:transform .2s,background .2s}
.chip::before{content:'';width:8px;height:8px;border-radius:50%;background:var(--dot);box-shadow:0 0 10px var(--dot)}
.chip.c1{--dot:#5EEAD4;animation-delay:.25s}.chip.c2{--dot:#FBBF24;animation-delay:.37s}.chip.c3{--dot:#FB7FA8;animation-delay:.49s}
.chip:hover{background:rgba(255,255,255,.16);transform:translateY(-2px)}
.meter{min-width:240px;padding:12px 16px;border-radius:16px;background:rgba(255,255,255,.07);
  border:1px solid rgba(255,255,255,.18);backdrop-filter:blur(8px);animation:rise .8s .3s both}
.meter-l{font-size:12.5px;color:#CFC6EC}
.meter-n{font-family:'Fraunces',Georgia,serif;font-size:30px;font-weight:600;line-height:1.1;color:#fff}
.meter-n small{font-size:15px;font-weight:400;color:#CFC6EC}
.bars{display:flex;gap:5px;margin-top:8px}
.bars i{flex:1;height:6px;border-radius:3px;background:rgba(255,255,255,.18);transition:background .4s}
.bars i.ok{background:#5EEAD4}.bars i.err{background:#FB7185}.bars i.busy{background:#FBBF24}
@keyframes rise{from{opacity:0;transform:translateY(12px)}to{opacity:1;transform:none}}

/* Sidebar */
.bslib-sidebar-layout>.sidebar{background:#fff;border-right:1px solid var(--line) !important}
.side-h{font-family:'Fraunces',Georgia,serif;font-size:17px;font-weight:600;color:var(--ink);margin:4px 0 6px}
.sidebar hr{border-top:1px dashed #D9D1F3;opacity:1}
.sidebar label{font-weight:600;font-size:13px;color:var(--ink2)}
.form-control,.selectize-input{border-radius:10px !important;border-color:#DDD6F3 !important}
.form-control:focus,.selectize-input.focus{border-color:var(--violet) !important;box-shadow:0 0 0 3px rgba(109,74,255,.18) !important}
.form-check-input:checked{background-color:var(--violet);border-color:var(--violet)}
.irs--shiny .irs-bar{background:linear-gradient(90deg,var(--violet),var(--teal));border:0}
.irs--shiny .irs-single{background:var(--ink)}
.irs--shiny .irs-handle{border:3px solid var(--violet);background:#fff;transition:transform .15s}
.irs--shiny .irs-handle:hover{transform:scale(1.12)}
.selectize-control.multi .selectize-input>div{background:var(--tint);color:#4C2FD6;border-radius:6px}

/* Tabs: white pill bar, violet active pill, content fades up on switch */
.nav-tabs{border:0;gap:6px;flex-wrap:wrap;background:#fff;padding:6px;border-radius:14px;
  box-shadow:0 1px 0 var(--line),0 10px 24px -18px rgba(36,22,58,.45)}
.nav-tabs .nav-link{border:0 !important;border-radius:10px;color:#4B4463;font-weight:600;font-size:13.5px;padding:8px 14px;
  transition:background .2s,color .2s,box-shadow .2s}
.nav-tabs .nav-link:hover{background:#F1EDFF;color:#4C2FD6}
.nav-tabs .nav-link.active{background:linear-gradient(135deg,var(--violet),var(--violet2));color:#fff !important;
  box-shadow:0 8px 18px -8px rgba(109,74,255,.8)}
.tab-content{padding-top:16px}
.tab-pane.active{animation:fadeUp .38s cubic-bezier(.2,.7,.2,1) both}
@keyframes fadeUp{from{opacity:0;transform:translateY(8px)}to{opacity:1;transform:none}}

/* Cards lift on hover */
.card{border:1px solid var(--line);border-radius:16px;background:#fff;box-shadow:0 1px 2px rgba(36,22,58,.05);
  transition:transform .25s cubic-bezier(.2,.7,.2,1),box-shadow .25s,border-color .25s}
.card:hover{transform:translateY(-3px);border-color:#CFC4FF;box-shadow:0 18px 34px -18px rgba(36,22,58,.4)}
.card.bslib-full-screen,.card.bslib-full-screen:hover{transform:none}
.card-header{background:transparent;border-bottom:1px solid #F0EDF8;font-weight:700;font-size:14.5px;color:var(--ink);padding:12px 16px}
.card-header::before{content:'';display:inline-block;width:4px;height:15px;border-radius:2px;margin-right:9px;
  vertical-align:-2px;background:linear-gradient(var(--violet),var(--teal))}
.bslib-full-screen-enter{color:var(--violet) !important;background:#fff !important;border-radius:8px}

/* Number boxes: gradient top bar, corner glow, value pops in on change */
.vbox{position:relative;overflow:hidden;height:100%;min-height:112px;padding:15px 18px;border-radius:16px;
  border:1px solid var(--line);background:radial-gradient(circle at 100% 0,var(--glow) 0,transparent 48%),#fff;
  transition:transform .25s cubic-bezier(.2,.7,.2,1),box-shadow .25s}
.vbox::before{content:'';position:absolute;left:0;right:0;top:0;height:4px;background:var(--bar)}
.vbox:hover{transform:translateY(-3px);box-shadow:0 18px 34px -18px rgba(36,22,58,.4)}
.vbox.blue{--bar:linear-gradient(90deg,#6D4AFF,#8B5CF6);--glow:rgba(109,74,255,.12)}
.vbox.teal{--bar:linear-gradient(90deg,#14B8A6,#5EEAD4);--glow:rgba(20,184,166,.13)}
.vbox.amber{--bar:linear-gradient(90deg,#F59E0B,#FBBF24);--glow:rgba(245,158,11,.14)}
.vbox.red{--bar:linear-gradient(90deg,#F43F5E,#FB7185);--glow:rgba(244,63,94,.13)}
.vlab{font-size:13px;font-weight:600;color:var(--muted)}
.vval{font-family:'Fraunces',Georgia,serif;font-size:34px;font-weight:600;line-height:1.15;color:var(--ink);
  animation:pop .55s cubic-bezier(.2,.9,.2,1.2) both}
.vsub{font-size:12.5px;color:#4B4463}
@keyframes pop{0%{opacity:0;transform:translateY(6px) scale(.96)}100%{opacity:1;transform:none}}

/* Takeaways: four rows with coloured pill labels */
.tk{font-size:13px;background:#FAF8FF;border-top:1px dashed #E2DBF7;color:var(--ink2);padding:10px 16px}
.tk4{display:grid;gap:7px}
.tkr{display:grid;grid-template-columns:148px 1fr;gap:10px;align-items:baseline;line-height:1.45}
.tkl{justify-self:start;font-size:11.5px;font-weight:700;padding:2px 9px;border-radius:999px;white-space:nowrap}
.r1 .tkl{background:#EEE9FF;color:#4C2FD6}.r2 .tkl{background:#E0F2FE;color:#0369A1}
.r3 .tkl{background:#FEF3C7;color:#92400E}.r4 .tkl{background:#CCFBF1;color:#0F766E}
.r4 .tkt{font-weight:600;color:var(--ink)}
@media (max-width:768px){.tkr{grid-template-columns:1fr;gap:2px}}

/* Insight box: dark panel with a violet glow */
.ins{position:relative;overflow:hidden;background:var(--ink);color:#E9E4F7;border-radius:18px;padding:20px 24px;
  margin:6px 0 22px;font-size:14px;line-height:1.55}
.ins::after{content:'';position:absolute;width:320px;height:320px;right:-120px;top:-160px;border-radius:50%;
  background:radial-gradient(circle,rgba(109,74,255,.55),transparent 65%);pointer-events:none}
.ins h6{position:relative;color:#fff;font-size:19px;font-weight:600;margin-bottom:12px}
.ins p{position:relative;margin-bottom:9px}
.ins p b{color:#fff}
.ins p:nth-of-type(1)>b:first-child{color:#C4B5FD}
.ins p:nth-of-type(2)>b:first-child{color:#5EEAD4}
.ins p:nth-of-type(3)>b:first-child{color:#FBBF24}
.ins p.cav>b:first-child{color:#FDA4AF}
.ins .cav{color:#F3D5DC;margin-bottom:0}

/* Uploads */
.hint{font-size:11.5px;color:var(--muted);line-height:1.4}
.btn-file{background:var(--ink) !important;color:#fff !important;border:0 !important;transition:background .2s}
.btn-file:hover{background:#3A2760 !important}
.progress{height:8px !important;border-radius:4px;background:#EEEAF7}
.progress-bar{background:linear-gradient(90deg,var(--violet),var(--teal)) !important;font-size:0}
.btn-rm{border:1px solid #F5C2CD;color:#BE123C;background:#fff;border-radius:9px;font-weight:600;transition:background .2s,transform .2s}
.btn-rm:hover{background:#FFF1F3;color:#9F1239;transform:translateY(-1px)}

/* Tables */
table.dataTable{font-size:13px}
table.dataTable thead th{background:var(--tint) !important;color:var(--ink);font-weight:700;border-bottom:2px solid #E2DBF7 !important}
table.dataTable tbody tr:hover>*{background:#F1EDFF !important;box-shadow:none !important}
.page-item.active .page-link{background:var(--violet);border-color:var(--violet)}
.page-link{color:var(--violet)}
.shiny-notification{border-radius:12px;border:1px solid var(--line)}

/* Accessibility */
a:focus-visible,button:focus-visible,.nav-link:focus-visible{outline:3px solid rgba(109,74,255,.5);outline-offset:2px}
@media (prefers-reduced-motion: reduce){*,*::before,*::after{animation:none !important;transition:none !important}}
@media (max-width:768px){.hero{padding:20px 18px}.meter{min-width:0;width:100%}}
"


# -----------------------------------------------------------------------------
# 8. SCREEN (UI)
# -----------------------------------------------------------------------------
ui <- page_sidebar(
  title = div(class = "hero",
    span(class = "blob b1"), span(class = "blob b2"), span(class = "blob b3"),
    div(class = "hero-row",
      div(h1(class = "hero-title", "Innovatech employer brand"),
          p(class = "hero-sub", "Is the brand we promise the brand people experience? Load the six files and every tab answers with your own numbers."),
          div(class = "chips",
              span(class = "chip c1", "Employees live it"),
              span(class = "chip c2", "Outsiders believe it"),
              span(class = "chip c3", "Candidates choose it"))),
      uiOutput("hdr_status"))),
  window_title = "Innovatech employer brand",
  theme = bs_theme(version = 5, primary = "#6D4AFF", secondary = "#24163A", success = "#14B8A6",
                   warning = "#F59E0B", danger = "#F43F5E", info = "#0EA5E9", bg = "#FFFFFF", fg = "#24163A",
                   base_font = font_collection("Plus Jakarta Sans", "Segoe UI", "Helvetica Neue", "Arial", "sans-serif"),
                   heading_font = font_collection("Fraunces", "Georgia", "serif")),
  fillable = FALSE,
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Fraunces:opsz,wght@9..144,400;9..144,600;9..144,700&family=Plus+Jakarta+Sans:wght@400;500;600;700&display=swap"),
    tags$style(HTML(CSS))),

  sidebar = sidebar(width = 320,
    div(class = "side-h", "Filters for every tab"),
    selectInput("dept", "Department (blank = all)", choices = NULL, multiple = TRUE),
    selectInput("grd", "Grade (blank = all)", choices = NULL, multiple = TRUE),
    dateRangeInput("dr", "Date range (reviews, posts, comments, postings)", start = "2022-08-01", end = Sys.Date()),
    radioButtons("stat", "Averages shown as", c("Mean" = "mean", "Median" = "median"), inline = TRUE),
    checkboxInput("adj", "Like-for-like (grade & tenure / same month / same job title)", FALSE),
    conditionalPanel("input.tabs == 't2'", hr(), div(class = "side-h", "EBI weights"),
      sliderInput("w1", "Employee voice (Glassdoor)", 0, 100, 25),
      sliderInput("w2", "Retention (Employee Master)", 0, 100, 25),
      sliderInput("w3", "Outsider reputation (LinkedIn comments)", 0, 100, 25),
      sliderInput("w4", "Candidate pull (VMV in postings)", 0, 100, 25)),
    conditionalPanel("input.tabs == 't4'", hr(), div(class = "side-h", "Word settings"),
      sliderInput("n", "Word-run length (1 = single words)", 1, 3, 2),
      radioButtons("src", "Review part", c("Pros" = "Pros", "Cons" = "Cons"), inline = TRUE),
      selectInput("grp", "Reviewer group", c("All", "Promoters (4-5)", "Passives (3)", "Detractors (1-2)")),
      sliderInput("top", "How many to show", 5, 40, 15)),
    conditionalPanel("input.tabs == 't5'", hr(), div(class = "side-h", "Topic settings"),
      sliderInput("k", "Number of topics", 2, 8, 4),
      helpText("The topic model re-runs when topics or filters change.")),
    conditionalPanel("input.tabs == 't6'", hr(), div(class = "side-h", "Heatmap"),
      selectInput("ax", "Rows", choices = AXIS_CHOICES, selected = "department"),
      selectInput("ay", "Columns", choices = AXIS_CHOICES, selected = "grade")),
    conditionalPanel("input.tabs == 't7'", hr(), div(class = "side-h", "What-if levers"),
      sliderInput("fl", "Flight-risk threshold (probability)", 0.05, 0.95, 0.30, step = 0.05),
      sliderInput("cov", "Coverage of at-risk employees (%)", 0, 100, 50),
      numericInput("miny", "Minimum years of service", 1, min = 0, max = 40),
      numericInput("sal", paste0("Average annual salary, ", RUPEE, " (optional)"), NA, min = 0),
      sliderInput("mult", "Replacement cost (x annual salary)", 0.25, 3, 1, step = 0.25),
      sliderInput("pp", "Pay uplift for those covered (%)", 0, 20, 8)),
    conditionalPanel("input.tabs == 't11'", hr(), div(class = "side-h", "Plan settings"),
      selectInput("pf", "Focus departments (blank = all)", choices = NULL, multiple = TRUE),
      sliderInput("pcov", "Programme coverage (%)", 0, 100, 60),
      numericInput("cs", paste0("Cost per employee covered, ", RUPEE, " (optional)"), NA, min = 0),
      sliderInput("td", "Time to deliver (months)", 3, 24, 12),
      sliderInput("nssg", "Target gain in review net sentiment (points)", 0, 60, 20),
      sliderInput("tgt", "Target Employer Brand Index", 40, 95, 70))
  ),

  navset_tab(id = "tabs",
    nav_panel("1 Data", value = "t1",
      layout_columns(col_widths = c(4, 4, 4), upload_card("emp"), upload_card("gd"), upload_card("li")),
      layout_columns(col_widths = c(4, 4, 4), upload_card("jp"), upload_card("lc"), upload_card("ja")),
      table_card("status", "Upload status"),
      table_card("detected", "Detected columns and verified joins")),
    nav_panel("2 Brand Index", value = "t2",
      number_row("v_ebi", "v_gap", "v_nss"),
      two_up(chart_card("radar", "EBI pillars (0-100, higher = better)"),
             chart_card("sdh", "Say-do heatmap: what each voice talks about")),
      insight_slot("ins_ebi")),
    nav_panel("3 Sentiment", value = "t3",
      two_up(chart_card("ridge", "Review sentiment by star rating"), chart_card("trend", "Sentiment and rating over time")),
      two_up(chart_card("pc", "Words driving positive and negative tone"), chart_card("bing", "Positive vs negative words by source")),
      insight_slot("ins_sent")),
    nav_panel("4 Words", value = "t4",
      two_up(chart_card("ngbar", "Most frequent word runs"), chart_card("cloud", "Word cloud")),
      chart_card("net", "Word-pair network", height = "460px"),
      insight_slot("ins_ng")),
    nav_panel("5 Topics", value = "t5",
      two_up(chart_card("lt", "Top words per topic", "420px"), chart_card("lr", "Topic share by star rating", "420px")),
      two_up(chart_card("lc", "Topic size and tone"), chart_card("ly", "Topic share by year")),
      insight_slot("ins_lda")),
    nav_panel("6 Attrition", value = "t6",
      number_row("auc_g", "auc_r"),
      chart_card("heat", "Where leaving is concentrated", "420px"),
      two_up(chart_card("roc", "How well each method ranks leavers (ROC)"), chart_card("imp", "What is associated with leaving")),
      table_card("risk", "Current employees by estimated flight risk (group planning only)"),
      insight_slot("ins_attr")),
    nav_panel("7 What-if lab", value = "t7",
      number_row("r1", "r2", "r3"),
      two_up(chart_card("sens", "Sensitivity: result vs pay uplift"), chart_card("cb", "Cost vs benefit")),
      insight_slot("ins_rx")),
    nav_panel("8 LinkedIn", value = "t8",
      two_up(chart_card("we", "Engagement per post by type"), chart_card("mix", "Content mix")),
      two_up(chart_card("tr2", "Posting and engagement trend"), chart_card("vmv", "Values talk in comments")),
      insight_slot("ins_li")),
    nav_panel("9 Emoji", value = "t9",
      number_row("e1", "e2", "e3"),
      two_up(chart_card("e_top", "Most used emoji"), chart_card("e_cmp", "Emoji use by commenter type")),
      two_up(chart_card("e_sc", "Do emoji agree with the words?"), table_card("e_tb", "Emoji table")),
      insight_slot("ins_emo")),
    nav_panel("10 Job ads", value = "t10",
      number_row("j1", "j2", "j3"),
      two_up(chart_card("j_ti", "Time to fill by job title"), chart_card("j_hi", "Spread of time to fill")),
      two_up(chart_card("j_vm", "Values alignment vs time to fill"), chart_card("j_tt", "What job ads talk about")),
      table_card("j_tb", "Postings"),
      insight_slot("ins_ja")),
    nav_panel("11 Action plan", value = "t11",
      number_row("p1", "p2", "p3", "p4"),
      two_up(chart_card("p_ebi", "EBI path to target"), chart_card("p_lev", "Retention levers: exits avoided")),
      two_up(chart_card("p_tl", "Delivery timeline"), chart_card("p_cor", "How employee factors move together")),
      table_card("p_tb", "Action plan"),
      insight_slot("ins_p"))
  )
)


# -----------------------------------------------------------------------------
# 9. CALCULATOR (server)
# -----------------------------------------------------------------------------
server <- function(input, output, session) {

  # ===========================================================================
  # 9.1 Storage: one independent slot per dataset
  # ===========================================================================
  store <- reactiveValues(emp = NULL, gd = NULL, li = NULL, jp = NULL, lc = NULL, ja = NULL,
                          schema = list(), version = 0,
                          status = data.frame(key = KEYS,
                                              Dataset = vapply(DS, `[[`, "", "label"),
                                              Control = vapply(DS, `[[`, "", "ctrl"),
                                              Status = "Not Uploaded", File = "", Rows = NA_integer_,
                                              Columns = NA_integer_, Removed = "", Notes = "",
                                              stringsAsFactors = FALSE, row.names = NULL))
  set_status <- function(key, status, file = "", rows = NA, cols = NA, removed = "", notes = "") {
    s <- store$status; i <- match(key, s$key)
    s$Status[i] <- status; s$File[i] <- file; s$Rows[i] <- rows; s$Columns[i] <- cols
    s$Removed[i] <- removed; s$Notes[i] <- notes
    store$status <- s
  }
  load_file <- function(key, info) {
    set_status(key, "Processing", info$name, notes = "Reading file...")
    withProgress(message = paste("Processing", DS[[key]]$label), value = 0.4, {
      result <- tryCatch({
        if (!grepl("\\.csv$", info$name, ignore.case = TRUE))
          stop("This is not a .csv file. Save it as CSV and upload it again.")
        raw <- read_csv_file(info$datapath)
        if (is.null(raw) || !nrow(raw) || !ncol(raw)) stop("The file could not be read or is empty.")
        out <- clean_file(key, raw); out$ncol <- ncol(raw); out
      }, error = function(e) e)
      if (inherits(result, "error")) {
        store[[key]] <- NULL; store$schema[[key]] <- NULL
        set_status(key, "Error", info$name, notes = conditionMessage(result))
      } else {
        store[[key]] <- result$d; store$schema[[key]] <- result$schema
        set_status(key, "Analysed", info$name, nrow(result$d), result$ncol, result$removed,
                   if (length(result$notes)) paste(result$notes, collapse = " ") else "OK")
      }
    })
    store$version <- store$version + 1
  }
  for (k in KEYS) local({
    key <- k
    observeEvent(input[[DS[[key]]$ctrl]], load_file(key, input[[DS[[key]]$ctrl]]))
    observeEvent(input[[paste0("rm_", key)]], {
      store[[key]] <- NULL; store$schema[[key]] <- NULL
      set_status(key, "Not Uploaded", notes = "Removed")
      store$version <- store$version + 1
    }, ignoreInit = TRUE)
  })

  # ===========================================================================
  # 9.2 Filter choices follow the loaded data
  # ===========================================================================
  observeEvent(store$version, {
    e <- store$emp
    depts  <- if (has_col(e, "department")) sort(unique(e$department)) else character(0)
    grades <- if (has_col(e, "grade")) sort(unique(e$grade)) else character(0)
    updateSelectInput(session, "dept", choices = depts, selected = intersect(input$dept, depts))
    updateSelectInput(session, "grd", choices = grades, selected = intersect(input$grd, grades))
    updateSelectInput(session, "pf", choices = depts, selected = intersect(input$pf, depts))
    axes <- AXIS_CHOICES[vapply(AXIS_CHOICES, function(v) has_col(e, v), logical(1))]
    if (length(axes) >= 2) {
      updateSelectInput(session, "ax", choices = axes, selected = if (isTRUE(input$ax %in% axes)) input$ax else axes[1])
      updateSelectInput(session, "ay", choices = axes, selected = if (isTRUE(input$ay %in% axes)) input$ay else axes[2])
    }
    dates <- c(if (has_col(store$gd, "date")) store$gd$date, if (has_col(store$li, "date")) store$li$date,
               if (has_col(store$lc, "date")) store$lc$date, if (has_col(store$jp, "date_posted")) store$jp$date_posted)
    dates <- dates[!is.na(dates)]
    if (length(dates)) updateDateRangeInput(session, "dr", start = min(dates), end = max(dates))
  })

  # ===========================================================================
  # 9.3 Filtered data (each dataset works on its own)
  # ===========================================================================
  in_dates <- function(d, col) {
    if (is.null(d)) return(NULL)
    if (col %in% names(d) && length(input$dr) == 2 && !any(is.na(input$dr)))
      d <- d[is.na(d[[col]]) | (d[[col]] >= input$dr[1] & d[[col]] <= input$dr[2]), , drop = FALSE]
    if (nrow(d)) d else NULL
  }
  emp <- reactive({
    d <- store$emp; if (is.null(d)) return(NULL)
    if (length(input$dept) && "department" %in% names(d)) d <- d[d$department %in% input$dept, , drop = FALSE]
    if (length(input$grd) && "grade" %in% names(d)) d <- d[d$grade %in% input$grd, , drop = FALSE]
    if (nrow(d)) d else NULL
  })
  gd <- reactive(in_dates(store$gd, "date"))
  li <- reactive(in_dates(store$li, "date"))
  jp <- reactive(in_dates(store$jp, "date_posted"))
  lc <- reactive(in_dates(store$lc, "date"))       # LinkedIn comments data ("lc", shared name on purpose)
  ja <- reactive(store$ja)                         # ad text has no date

  gd_scored <- reactive({ d <- gd(); if (has_col(d, "review_text")) score_texts(d, "review_text") else NULL })
  lc_scored <- reactive({ d <- lc(); if (has_col(d, "comment_text")) score_texts(d, "comment_text") else NULL })
  ja_scored <- reactive({ d <- ja(); if (has_col(d, "ad_text")) score_texts(d, "ad_text") else NULL })

  # Joins are used only when the IDs really overlap
  joins <- reactive({
    jj <- if (has_col(store$jp, "job_id") && has_col(store$ja, "job_id")) sum(unique(store$jp$job_id) %in% store$ja$job_id) else NA
    pj <- if (has_col(store$lc, "post_id") && has_col(store$li, "post_id")) sum(unique(store$lc$post_id) %in% store$li$post_id) else NA
    list(job = isTRUE(jj > 0), post = isTRUE(pj > 0), jj = jj, pj = pj)
  })

  # ===========================================================================
  # 9.4 Output helpers
  # ===========================================================================
  style_plot <- function(p, x_title = NULL, y_title = NULL) p %>%
    layout(xaxis = list(title = x_title, gridcolor = "#EEEAF7", zeroline = FALSE, automargin = TRUE, linecolor = "#E2DBF7"),
           yaxis = list(title = y_title, gridcolor = "#EEEAF7", zeroline = FALSE, automargin = TRUE, linecolor = "#E2DBF7"),
           font = list(family = FONT_BODY, size = 12, color = "#3B3452"), colorway = PAL,
           hoverlabel = list(bgcolor = "#24163A", bordercolor = "#6D4AFF", font = list(color = "#FFFFFF", family = FONT_BODY)),
           legend = list(orientation = "h", x = 0, y = -0.22), margin = list(l = 10, r = 10, t = 10, b = 10),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)") %>%
    config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d"))
  chart <- function(id, build) output[[id]] <- renderPlotly(build())
  # Takeaway: a function returning c(reads, suggests, means, do_next), or one line
  takeaway <- function(id, build) output[[paste0("tk_", id)]] <- renderUI({
    x <- tryCatch(build(), error = function(e) NULL)
    if (is.null(x) || !length(x) || !nzchar(x[1]))
      return(span(class = "text-muted", "Takeaway appears when data is available."))
    if (length(x) == 1) return(HTML(paste0("<b>Takeaway:</b> ", x)))
    labels <- c("Reads", "Suggests", "Means for Innovatech", "Do next")
    div(class = "tk4", lapply(1:4, function(i)
      div(class = paste0("tkr r", i), span(class = "tkl", labels[i]), span(class = "tkt", HTML(x[i])))))
  })
  quiet <- function(r) tryCatch(r(), error = function(e) NULL)
  dtable <- function(d, n = 10) datatable(d, rownames = FALSE, filter = "top",
                                          options = list(pageLength = n, scrollX = TRUE, dom = "ftip"))

  # Fixed meaning and action for a weak EBI pillar
  WEAK_MEANS <- c(
    Voice      = "employees are not living the brand, and they are the source candidates trust most",
    Retention  = "people leave faster than the brand story suggests, raising hiring cost and losing know-how",
    Reputation = "outsiders are not convinced, so the employer brand has little pull on LinkedIn",
    Candidate  = "job ads do not carry the company's values, so candidates see a generic employer")
  WEAK_DO <- c(
    Voice      = "Fix the top employee complaint (Tabs 3-5) before spending on brand campaigns.",
    Retention  = "Target the attrition hotspot and the top risk factor in Tab 6.",
    Reputation = "Shift LinkedIn content to the best-performing format (Tab 8) and reply to comments.",
    Candidate  = "Rewrite low-scoring job ads around evidenced values (Tab 10).")

  # Header meter ("hdr_status"): files loaded, one bar per file
  output$hdr_status <- renderUI({
    s <- store$status
    cls <- c("Analysed" = "ok", "Error" = "err", "Processing" = "busy", "Not Uploaded" = "")[s$Status]
    div(class = "meter", div(class = "meter-l", "Files loaded"),
        div(class = "meter-n", sum(s$Status == "Analysed"), tags$small(" of 6")),
        div(class = "bars", lapply(seq_len(nrow(s)), function(i)
          tags$i(class = unname(cls[i]), title = paste(s$Dataset[i], "-", s$Status[i])))))
  })

  # ===========================================================================
  # TAB 1  DATA
  # ===========================================================================
  output$status <- renderDT({
    datatable(store$status[, -1], rownames = FALSE, options = list(dom = "t", ordering = FALSE)) %>%
      formatStyle("Status", fontWeight = "700",
        backgroundColor = styleEqual(c("Not Uploaded", "Processing", "Analysed", "Error"), c("#F1F0F6", "#FEF3C7", "#CCFBF1", "#FFE4E6")),
        color = styleEqual(c("Not Uploaded", "Processing", "Analysed", "Error"), c("#6B6485", "#92400E", "#0F766E", "#BE123C")))
  })
  takeaway("status", function() {
    s <- store$status; a <- sum(s$Status == "Analysed")
    errs <- s$Dataset[s$Status == "Error"]; todo <- s$Dataset[s$Status == "Not Uploaded"]
    c(paste0(a, " of 6 files analysed", if (length(errs)) paste0("; errors in ", bold(paste(errs, collapse = ", "))) else "", "."),
      if (a < 6) "Part of the evidence is missing, so tabs that need those files are blank or partial." else "All evidence sources are in, so every tab uses the full picture.",
      if (a < 6) "Conclusions so far rest on part of the evidence; the Brand Index may move when the rest is added." else "Results combine what employees, outsiders and candidates say.",
      if (length(errs)) "Read the Notes column, fix the flagged file(s) and upload them again."
      else if (length(todo)) paste0("Upload: ", paste(todo, collapse = ", "), ".") else "Go to Tab 2 to read the Brand Index.")
  })
  schema_table <- reactive({
    j <- joins()
    join_rows <- data.frame(
      Dataset = c("Job Postings + Job Ad Text", "LinkedIn Posts + Comments"),
      Column  = c("JOIN on job_id", "JOIN on post_id"),
      Status  = c(if (is.na(j$jj)) "Waiting for both files" else if (j$job) sprintf("Confirmed: %d job_ids match", j$jj) else "Not confirmed: no matching job_id",
                  if (is.na(j$pj)) "Waiting for both files" else if (j$post) sprintf("Confirmed: %d post_ids match", j$pj) else "Not confirmed: no matching post_id"),
      Type = "", `Missing %` = NA_real_, check.names = FALSE, stringsAsFactors = FALSE)
    rbind(do.call(rbind, store$schema), join_rows)
  })
  output$detected <- renderDT(datatable(schema_table(), rownames = FALSE, options = list(pageLength = 25, dom = "ftip")))
  takeaway("detected", function() {
    d <- schema_table(); j <- joins(); miss <- sum(d$Status == "Missing")
    cover <- if (!is.na(j$jj) && !is.null(store$jp)) share_of(j$jj, nrow(store$jp)) else NA
    c(paste0(if (miss) paste(miss, "expected column(s) missing") else "All expected columns found", ". ",
             if (!is.na(j$jj)) sprintf("Job join: %d IDs. ", j$jj) else "",
             if (!is.na(j$pj)) sprintf("Post join: %d IDs. ", j$pj) else "",
             if ("last_working_day" %in% d$Column) "A mostly blank last_working_day is normal: blank = still employed." else ""),
      if (miss) "Calculations that need a missing column are skipped." else "The data matches the brief, so the calculations can be trusted.",
      if (is.finite(cover)) sprintf("Ad-wording findings speak for about %.0f%% of postings, not all hiring.", cover)
      else "Joined analyses appear once both files of a pair are loaded.",
      "When presenting job-ad or comment findings, state how many records they rest on.")
  })

  # ===========================================================================
  # TAB 2  BRAND INDEX
  # EBI (0-100, higher = better) = weighted average of the pillars that have data:
  #   Voice      = mean((rating - 1) / 4 x 100, (review NSS + 100) / 2)
  #   Retention  = 100 - attrition %
  #   Reputation = (LinkedIn comment NSS + 100) / 2
  #   Candidate  = VMV alignment rescaled to 0-100
  # Gap = mean(Reputation, Candidate) - mean(Voice, Retention); positive = over-promising
  # ===========================================================================
  pillars <- reactive({
    how <- input$stat
    out <- c(Voice = NA_real_, Retention = NA_real_, Reputation = NA_real_, Candidate = NA_real_)
    g <- gd(); gs <- gd_scored()
    rating_part <- if (has_col(g, "overall_rating")) (avg(g$overall_rating, how) - 1) / 4 * 100 else NA_real_
    review_nss  <- if (!is.null(gs)) nss(gs$d$lab) else NA_real_
    parts <- c(rating_part, if (is.finite(review_nss)) (review_nss + 100) / 2 else NA)
    if (any(is.finite(parts))) out["Voice"] <- mean(parts, na.rm = TRUE)
    e <- emp()
    if (has_col(e, "left") && sum(!is.na(e$left)) >= MIN_N) out["Retention"] <- 100 - 100 * mean(e$left, na.rm = TRUE)
    l <- lc_scored()
    if (!is.null(l) && nrow(l$d) >= MIN_N) out["Reputation"] <- (nss(l$d$lab) + 100) / 2
    j <- jp()
    if (has_col(j, "vmv_alignment_score"))
      out["Candidate"] <- min(100, avg(j$vmv_alignment_score, how) * vmv_factor(store$jp$vmv_alignment_score))
    attr(out, "rating_part") <- rating_part; attr(out, "review_nss") <- review_nss
    out
  })
  weights <- reactive(c(Voice = input$w1, Retention = input$w2, Reputation = input$w3, Candidate = input$w4))
  ebi_from <- function(p, w) {
    ok <- is.finite(p)
    if (!any(ok) || sum(w[ok]) == 0) NA_real_ else sum(w[ok] * p[ok]) / sum(w[ok])
  }
  ebi <- reactive(ebi_from(pillars(), weights()))
  gap <- reactive({
    p <- pillars()
    inside <- mean(p[c("Voice", "Retention")], na.rm = TRUE); outside <- mean(p[c("Reputation", "Candidate")], na.rm = TRUE)
    if (is.finite(inside) && is.finite(outside)) outside - inside else NA_real_
  })

  output$v_ebi <- renderUI({
    v <- ebi(); p <- pillars(); n <- sum(is.finite(p))
    number_box("Employer Brand Index", show_num(v),
      if (!n) "Upload data in Tab 1." else sprintf("0-100, higher = stronger. %d of 4 pillars. Weakest: <b>%s</b>.", n, names(which.min(p[is.finite(p)]))),
      if (is.finite(v) && v < 50) "red" else "blue")
  })
  output$v_gap <- renderUI({
    v <- gap()
    number_box("Promise - experience gap", show_num(v, 1, " pts"),
      if (!is.finite(v)) "Needs an inside source (Glassdoor or Employee Master) and an outside one (LinkedIn comments or Job Postings)."
      else if (v > 10) "The outside story is clearly rosier than what employees live: <b>over-promising</b>."
      else if (v > 0) "The outside story is slightly ahead of the inside experience." else "Employees rate it at least as well as outsiders.",
      if (is.finite(v) && v > 10) "amber" else "teal")
  })
  output$v_nss <- renderUI({
    g <- gd_scored(); v <- if (!is.null(g)) nss(g$d$lab) else NA
    number_box("Review net sentiment", show_num(v, 0),
      if (is.null(g)) need_file("gd") else sprintf("%% positive - %% negative reviews. %.0f%% of %d reviews are negative.", 100 * mean(g$d$lab == "Negative"), nrow(g$d)),
      if (is.finite(v) && v < 0) "red" else "teal")
  })

  chart("radar", function() {
    p <- pillars(); have <- p[is.finite(p)]
    validate(need(length(have) > 0, "Upload Glassdoor, Employee Master, LinkedIn Comments or Job Postings to build the index."))
    th <- c(names(have), names(have)[1]); r <- c(as.numeric(have), as.numeric(have)[1])
    plot_ly(type = "scatterpolar", mode = "lines+markers", r = r, theta = th, fill = "toself", name = "Now",
            line = list(color = COL[["violet"]]), fillcolor = "rgba(109,74,255,0.20)",
            hovertemplate = "%{theta}: %{r:.1f}<extra></extra>") %>%
      add_trace(r = rep(input$tgt, length(th)), theta = th, mode = "lines", fill = "none", name = "Target",
                line = list(color = COL[["rose"]], dash = "dot"), hoverinfo = "skip") %>%
      layout(polar = list(bgcolor = "rgba(0,0,0,0)", radialaxis = list(range = c(0, 100), gridcolor = "#E9E4F7"),
                          angularaxis = list(gridcolor = "#E9E4F7")),
             showlegend = FALSE, paper_bgcolor = "rgba(0,0,0,0)", font = list(family = FONT_BODY, color = "#3B3452"),
             hoverlabel = list(bgcolor = "#24163A", bordercolor = "#6D4AFF", font = list(color = "#FFFFFF")),
             margin = list(l = 40, r = 40, t = 20, b = 20)) %>%
      config(displaylogo = FALSE)
  })
  takeaway("radar", function() {
    p <- pillars(); have <- p[is.finite(p)]; if (!length(have)) return(NULL)
    weak <- names(which.min(have)); spread <- max(have) - min(have)
    c(sprintf("Strongest pillar is %s (%.0f); weakest is %s (%.0f), a %.0f-point spread. The dotted ring is the target (%d).",
              bold(names(which.max(have))), max(have), bold(weak), min(have), spread, as.integer(input$tgt)),
      if (spread > 20) "The brand is uneven: strong in one area, clearly weak in another." else "The brand is fairly balanced across the pillars measured.",
      paste0("A weak ", weak, " pillar means ", WEAK_MEANS[[weak]], "."),
      WEAK_DO[[weak]])
  })

  say_do <- reactive({
    parts <- list()
    a <- ja(); if (has_col(a, "ad_text")) parts$a <- theme_table(a$ad_text, NULL, "Job ads (we say)")
    l <- lc_scored(); if (!is.null(l)) parts$l <- theme_table(l$d$comment_text, l$d$lab, "LinkedIn comments (outsiders)")
    g <- gd_scored(); if (!is.null(g)) parts$g <- theme_table(g$d$review_text, g$d$lab, "Glassdoor (employees)")
    if (!length(parts)) return(NULL)
    t <- do.call(rbind, parts); t$theme <- factor(t$theme, levels = rev(names(THEMES))); t
  })
  worst_theme <- reactive({
    t <- say_do(); if (is.null(t)) return(NULL)
    g <- t[grepl("Glassdoor", t$source) & !is.na(t$net) & t$n_hit >= MIN_N, ]
    if (nrow(g)) g[which.min(g$net), ] else NULL
  })
  chart("sdh", function() {
    t <- say_do(); validate(need(!is.null(t), "Upload Job Ad Text, LinkedIn Comments or Glassdoor Reviews."))
    t$hover <- sprintf("%s<br>%s<br>Mentioned in %.0f%% of %d texts<br>Net sentiment when mentioned: %s",
                       t$source, t$theme, t$share, t$n_docs, ifelse(is.na(t$net), "n/a", sprintf("%+.0f", t$net)))
    z <- to_matrix(t, "theme", "source", "share"); h <- to_matrix(t, "theme", "source", "hover")
    plot_ly(z = z, x = colnames(z), y = rownames(z), type = "heatmap", text = h, hoverinfo = "text",
            colorscale = SCALE_SHARE, colorbar = list(title = "% texts")) %>% style_plot()
  })
  takeaway("sdh", function() {
    t <- say_do(); if (is.null(t)) return(NULL); w <- worst_theme()
    if (is.null(w)) {
      b <- t[which.max(t$share), ]
      return(c(sprintf("Most talked-about theme: %s (%.0f%% of %s).", bold(b$theme), b$share, b$source),
               "This theme dominates what that voice talks about.",
               "Without Glassdoor reviews we cannot yet check whether employees experience what is promised.",
               "Upload Glassdoor Reviews to compare what we say with what employees live."))
    }
    ad <- t[grepl("Job ads", t$source) & t$theme == w$theme, ]
    overclaim <- nrow(ad) > 0 && ad$share > w$share
    c(paste0(sprintf("Employees are most negative about %s (net %s, in %.0f%% of reviews)", bold(w$theme), signed(w$net), w$share),
             if (nrow(ad)) sprintf("; job ads mention it in %.0f%% of ads.", ad$share) else "."),
      if (overclaim) "The company promotes this theme more than employees experience it: a say-do gap."
      else "Employees' biggest frustration is not a headline promise in the ads, but it is a real experience problem.",
      if (overclaim) "Candidates drawn in by this promise may leave early when reality differs, hurting retention and future reviews."
      else "The problem sits inside the employee experience and will keep pulling ratings down until it is fixed.",
      if (overclaim) paste0("Fix ", w$theme, " internally first; until then, tone down ", w$theme, " claims in ads and posts.")
      else paste0("Give ", w$theme, " an owner and track its net sentiment every quarter."))
  })

  output$ins_ebi <- renderUI({
    p <- pillars(); v <- ebi(); g <- gap(); w <- worst_theme()
    if (!is.finite(v)) return(waiting_box("at least one dataset"))
    weak <- names(which.min(p[is.finite(p)]))
    insight_box(
      "Each pillar is scored 0-100 (higher = better); the EBI is their weighted average. Move the weight sliders to see how sensitive it is. In the heatmap, darker cells mean a theme is mentioned more by that voice.",
      paste0("EBI is <b>", show_num(v), "</b>; the weakest pillar is <b>", weak, "</b> (", show_num(p[[weak]]), ").",
             if (is.finite(g)) paste0(" The outside view is <b>", show_num(abs(g)), " points ", if (g > 0) "above" else "below", "</b> the inside experience.") else "",
             if (!is.null(w)) paste0(" Employees are most negative about <b>", w$theme, "</b>.") else ""),
      paste0("Spend first where the brand is weakest (", weak, ").",
             if (is.finite(g) && g > 10) " A large positive gap means the message is ahead of reality: fix the employee experience before amplifying the brand, or new hires will leave disappointed."
             else " Keep the outside story honest and tied to what employees actually experience."),
      "Weights are a judgement, not a law. Glassdoor and LinkedIn are self-selected samples. LinkedIn comments and ad text are simulated, so their pillars are illustrative. Pillars come from different sources, so part of any gap is a measurement difference.")
  })

  # ===========================================================================
  # TAB 3  SENTIMENT
  # ===========================================================================
  by_rating <- reactive({
    g <- gd_scored(); validate(need(!is.null(g), need_file("gd")))
    d <- g$d
    d$grp <- if (has_col(d, "overall_rating")) ifelse(is.na(d$overall_rating), "Unrated", paste0(round(d$overall_rating), " star")) else "All reviews"
    d <- d[d$grp %in% names(which(table(d$grp) >= MIN_N)), ]
    validate(need(nrow(d) > 0, "Too few reviews per rating (need 5+).")); d
  })
  chart("ridge", function() {
    d <- by_rating()
    plot_ly(d, x = ~sent, y = ~grp, split = ~grp, type = "violin", orientation = "h", side = "positive",
            points = FALSE, meanline = list(visible = TRUE), showlegend = FALSE) %>%
      style_plot("Sentiment (-1 = negative, +1 = positive)", NULL)
  })
  takeaway("ridge", function() {
    d <- by_rating(); s <- tapply(d$sent, d$grp, avg, how = input$stat); neg <- 100 * mean(d$lab == "Negative")
    c(if (length(s) > 1) sprintf("Sentiment runs from %.2f (%s) to %.2f (%s); %.0f%% of reviews read negative.", min(s), names(which.min(s)), max(s), names(which.max(s)), neg)
      else sprintf("Sentiment is %.2f; %.0f%% of reviews read negative.", s[[1]], neg),
      if (length(s) > 1 && max(s) - min(s) > 0.5) "Words and stars agree: unhappy reviewers say clearly why." else "Words differ little by rating, so star ratings alone hide what people feel.",
      if (neg >= 30) "Close to a third or more of public reviews read negative, and this is what candidates see first on Glassdoor." else "Most public reviews read neutral or positive, a base the brand can build on.",
      "Read the 1-2 star reviews in Tab 4 (Cons, Detractors) to list issues that can be fixed.")
  })

  by_month <- reactive({
    g <- gd_scored(); validate(need(!is.null(g) && has_col(g$d, "date"), "Needs Glassdoor Reviews with dates."))
    d <- g$d[!is.na(g$d$date), ]; if (!"overall_rating" %in% names(d)) d$overall_rating <- NA_real_
    m <- d %>% mutate(month = as.Date(format(date, "%Y-%m-01"))) %>% group_by(month) %>%
      summarise(n = n(), s = avg(sent, input$stat), r = avg(overall_rating, input$stat), .groups = "drop") %>%
      filter(n >= MIN_N)
    validate(need(nrow(m) >= 2, "Need two or more months with 5+ reviews.")); m
  })
  chart("trend", function() {
    m <- by_month()
    plot_ly(m, x = ~month) %>%
      add_lines(y = ~s, name = paste("Sentiment", input$stat), line = list(color = COL[["violet"]]), text = ~paste("Reviews:", n)) %>%
      add_lines(y = ~r, name = paste("Rating", input$stat), yaxis = "y2", line = list(color = COL[["saffron"]], dash = "dot")) %>%
      style_plot(NULL, "Sentiment") %>%
      layout(yaxis2 = list(overlaying = "y", side = "right", title = "Rating (1-5)", range = c(1, 5)))
  })
  takeaway("trend", function() {
    m <- by_month(); h <- max(1, min(6, floor(nrow(m) / 2)))
    a <- mean(head(m$s, h)); b <- mean(tail(m$s, h))
    dir <- if (b - a > 0.05) "up" else if (b - a < -0.05) "down" else "flat"
    c(sprintf("Sentiment moved from %.2f (first %d months) to %.2f (last %d months).", a, h, b, h),
      switch(dir, up = "Employee mood is improving.", down = "Employee mood is getting worse.", flat = "Employee mood has barely moved."),
      switch(dir, up = "Recent changes may be working and are worth protecting.",
                  down = "A worsening experience usually shows up next in lower ratings and higher attrition.",
                  flat = "Current initiatives are not moving how employees feel."),
      switch(dir, up = "Find out what changed and scale it; keep tracking monthly.",
                  down = "Check what happened around the turning point (policy, layoffs, appraisals) and act on it.",
                  flat = "Launch one visible fix for the top complaint and watch this line respond."))
  })

  top_words <- reactive({
    g <- gd_scored(); validate(need(!is.null(g), need_file("gd")))
    w <- unlist(g$tokens)
    pos <- head(sort(table(w[w %in% POS_WORDS]), decreasing = TRUE), 10)
    neg <- head(sort(table(w[w %in% NEG_WORDS]), decreasing = TRUE), 10)
    validate(need(length(pos) + length(neg) > 0, "No sentiment words found."))
    rbind(data.frame(word = names(pos), n = as.numeric(pos), tone = rep("Positive", length(pos)), stringsAsFactors = FALSE),
          data.frame(word = names(neg), n = -as.numeric(neg), tone = rep("Negative", length(neg)), stringsAsFactors = FALSE))
  })
  chart("pc", function() {
    d <- top_words()
    plot_ly(d, x = ~n, y = ~reorder(word, n), color = ~tone, colors = c(Negative = COL[["rose"]], Positive = COL[["teal"]]),
            type = "bar", orientation = "h", text = ~paste(word, abs(n)), hoverinfo = "text", textposition = "none") %>%
      style_plot("Times used (negative words to the left)", NULL)
  })
  takeaway("pc", function() {
    d <- top_words(); ng <- d[d$tone == "Negative", ]; ps <- d[d$tone == "Positive", ]
    nw <- if (nrow(ng)) paste0("'", ng$word[1], "'") else NA; pw <- if (nrow(ps)) paste0("'", ps$word[1], "'") else NA
    c(paste0(if (!is.na(nw)) sprintf("Top negative word: %s (%d uses). ", bold(nw), as.integer(-ng$n[1])) else "",
             if (!is.na(pw)) sprintf("Top positive word: %s (%d uses).", bold(pw), as.integer(ps$n[1])) else ""),
      "These single words shape the tone of reviews more than any others.",
      paste0(if (!is.na(pw)) paste0(pw, " is a strength employees already name. ") else "",
             if (!is.na(nw)) paste0(nw, " is a credibility risk if the brand claims the opposite.") else ""),
      paste0(if (!is.na(pw)) paste0("Use ", pw, " in job ads and posts. ") else "",
             if (!is.na(nw)) paste0("Find out what sits behind ", nw, " with a pulse survey or focus group.") else ""))
  })

  word_balance <- reactive({
    rows <- list()
    g <- gd_scored(); if (!is.null(g)) rows$g <- data.frame(src = "Glassdoor (employees)", pos = sum(g$d$pos_n), neg = sum(g$d$neg_n))
    l <- lc_scored(); if (!is.null(l)) rows$l <- data.frame(src = "LinkedIn comments*", pos = sum(l$d$pos_n), neg = sum(l$d$neg_n))
    a <- ja_scored(); if (!is.null(a)) rows$a <- data.frame(src = "Job ads*", pos = sum(a$d$pos_n), neg = sum(a$d$neg_n))
    validate(need(length(rows) > 0, "Upload Glassdoor Reviews, LinkedIn Comments or Job Ad Text."))
    d <- do.call(rbind, rows); tot <- pmax(d$pos + d$neg, 1)
    d$pos_pct <- 100 * d$pos / tot; d$neg_pct <- 100 * d$neg / tot; d
  })
  chart("bing", function() {
    d <- word_balance()
    plot_ly(d, x = ~src, y = ~pos_pct, type = "bar", name = "Positive words", marker = list(color = COL[["teal"]])) %>%
      add_trace(y = ~neg_pct, name = "Negative words", marker = list(color = COL[["rose"]])) %>%
      style_plot("* simulated source", "% of sentiment words") %>% layout(barmode = "stack")
  })
  takeaway("bing", function() {
    d <- word_balance(); i <- which.max(d$neg_pct)
    if (nrow(d) == 1) return(c(sprintf("%.0f%% of sentiment words in %s are negative.", d$neg_pct, d$src),
      if (d$neg_pct > 40) "Tone is heavily negative." else "Tone leans positive.",
      "One source alone cannot show a gap between voices.", "Upload a second text source to compare inside and outside tone."))
    inside <- grepl("Glassdoor", d$src[i])
    c(sprintf("%s is the most negative voice (%.0f%% negative words) vs %.0f%% for %s.", bold(d$src[i]), d$neg_pct[i], min(d$neg_pct), d$src[which.min(d$neg_pct)]),
      if (inside) "Employees sound more negative than the outside story: the brand looks better from outside than inside." else "An outside voice is the most negative, so the outside story is the weaker one.",
      if (inside) "A gap between inside and outside tone is the classic sign of an over-promised brand." else "Outsiders are the harder audience to convince.",
      if (inside) "Close the gap from the inside: fix experience issues before amplifying brand messages." else "Improve outward content and engagement (Tab 8).")
  })

  output$ins_sent <- renderUI({
    g <- gd_scored(); if (is.null(g)) return(waiting_box("Glassdoor Reviews"))
    v <- nss(g$d$lab); l <- lc_scored(); lv <- if (!is.null(l)) nss(l$d$lab) else NA
    agree <- if (has_col(g$d, "overall_rating")) suppressWarnings(cor(g$d$sent, g$d$overall_rating, use = "complete.obs")) else NA
    insight_box(
      "The violin chart shows how positive review wording is at each star rating (the line marks the average). The trend shows the direction over time. Bars left and right are the words that pull tone down or lift it.",
      paste0("Review net sentiment is <b>", show_num(v, 0), "</b>", if (is.finite(lv)) paste0(" vs <b>", show_num(lv, 0), "</b> for LinkedIn comments") else "", ".",
             if (is.finite(agree)) paste0(" Word tone and star ratings correlate at r = ", show_num(agree, 2), if (agree < 0.3) " (weak: treat word scores with care)." else ".") else ""),
      if (is.finite(lv) && lv - v > 15) "Outsiders sound much happier than employees. Treat the negative words here as HR's to-do list and avoid amplifying messages employees contradict."
      else "The top negative words are a ready-made list of issues to explore in pulse surveys or focus groups.",
      paste0("Sentiment uses the ", LEXICON_NAME, "; it misses sarcasm and context. Review timing can be driven by events such as appraisals or layoffs. A dip is a prompt to investigate, not proof of a cause."))
  })

  # ===========================================================================
  # TAB 4  WORDS
  # ===========================================================================
  review_parts <- reactive({
    g <- gd(); if (!has_col(g, "review_text")) return(NULL)
    text <- as_text(g$review_text); low <- tolower(text)
    group <- if (has_col(g, "overall_rating"))
      ifelse(is.na(g$overall_rating), "Unrated", ifelse(g$overall_rating >= 4, "Promoters (4-5)",
             ifelse(g$overall_rating >= 3, "Passives (3)", "Detractors (1-2)"))) else rep("Unrated", nrow(g))
    p_at <- regexpr("\\bpros?\\s*[:\\-]", low, perl = TRUE); c_at <- regexpr("\\bcons?\\s*[:\\-]", low, perl = TRUE)
    labelled <- p_at > 0 & c_at > p_at
    out <- list()
    if (any(labelled)) {
      i <- which(labelled)
      pros <- substr(text[i], p_at[i] + attr(p_at, "match.length")[i], c_at[i] - 1)
      cons <- substr(text[i], c_at[i] + attr(c_at, "match.length")[i], nchar(text[i]))
      cons <- sub("(?i)\\badvice\\b.*$", "", cons, perl = TRUE)          # cut "Advice" off Cons
      out$labelled <- data.frame(part = rep(c("Pros", "Cons"), each = length(i)), text = c(pros, cons),
                                 grp = group[c(i, i)], stringsAsFactors = FALSE)
    }
    i <- which(!labelled)
    if (length(i)) {
      sentences <- strsplit(text[i], "(?<=[.!?;])\\s+|\\n+", perl = TRUE)
      s <- unlist(sentences); owner <- rep(i, lengths(sentences))
      if (length(s)) {
        m <- polarity_counts(tokenise(s)); net <- m[1, ] - m[2, ]
        part <- ifelse(net > 0, "Pros", ifelse(net < 0, "Cons", NA))
        out$split <- data.frame(part = part, text = s, grp = group[owner], stringsAsFactors = FALSE)[!is.na(part), ]
      }
    }
    if (!length(out)) return(NULL)
    r <- do.call(rbind, out); attr(r, "labelled_share") <- mean(labelled); r
  })
  chosen_tokens <- reactive({
    p <- review_parts(); validate(need(!is.null(p), need_file("gd")))
    p <- p[p$part == input$src, ]
    if (input$grp != "All") p <- p[p$grp == input$grp, ]
    validate(need(nrow(p) >= MIN_N, "Too few review parts for this group (need 5+). Try 'All' or the other review part."))
    tokenise(p$text)
  })
  top_runs <- reactive({
    tk <- chosen_tokens(); runs <- ngrams(tk, input$n)
    validate(need(length(runs) > 0, "No word runs of this length. Try a shorter run."))
    tb <- head(sort(table(runs), decreasing = TRUE), input$top)
    data.frame(term = names(tb), n = as.numeric(tb), share = 100 * as.numeric(tb) / length(tk), stringsAsFactors = FALSE)
  })
  chart("ngbar", function() {
    d <- top_runs()
    plot_ly(d, x = ~n, y = ~reorder(term, n), type = "bar", orientation = "h",
            marker = list(color = if (input$src == "Pros") COL[["teal"]] else COL[["rose"]]),
            text = ~sprintf("%.1f%% of review parts", share), hoverinfo = "x+text", textposition = "none") %>%
      style_plot("Times mentioned", NULL)
  })
  takeaway("ngbar", function() {
    d <- top_runs(); t1 <- paste0("'", d$term[1], "'"); cons <- input$src == "Cons"
    c(sprintf("In %s (%s), %s leads with %d mentions (%.0f%% of review parts).", tolower(input$src), input$grp, bold(t1), as.integer(d$n[1]), d$share[1]),
      if (cons) paste0(t1, " is the most common complaint for this group.") else paste0(t1, " is what this group values most."),
      if (cons) paste0("Fixing ", t1, " would shrink the biggest single source of negative reviews.") else paste0(t1, " is a genuine selling point that employees already vouch for."),
      if (cons) paste0("Give ", t1, " an owner and a 90-day fix; switch to Promoters to see whether they mention it too.")
      else paste0("Feature ", t1, " in job ads and LinkedIn posts with real employee stories."))
  })
  chart("cloud", function() {
    d <- top_runs(); i <- seq_len(nrow(d)); angle <- i * 2.4; radius <- sqrt(i)     # spiral layout
    size <- 12 + 26 * (d$n - min(d$n)) / max(1, max(d$n) - min(d$n))
    plot_ly(x = radius * cos(angle), y = radius * sin(angle), text = d$term, type = "scatter", mode = "text",
            textfont = list(size = size, color = rep(PAL, length.out = nrow(d))),
            hovertext = paste(d$term, d$n), hoverinfo = "text") %>%
      style_plot() %>% layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
  })
  takeaway("cloud", function() {
    d <- top_runs(); s3 <- sum(head(d$n, 3)) / sum(d$n)
    c(sprintf("The top 3 terms make up %.0f%% of the mentions shown.", 100 * s3),
      if (s3 > 0.4) "A few issues dominate the conversation." else "Opinions are spread across many issues.",
      if (s3 > 0.4) "Fixing a small number of things could move sentiment noticeably." else "There is no single fix; improvement will need several smaller changes.",
      if (s3 > 0.4) paste0("Focus budget on the top 3: ", paste(head(d$term, 3), collapse = ", "), ".") else "Group the terms into themes (Tab 5) and prioritise by tone.")
  })
  pair_network <- reactive({
    pairs <- ngrams(chosen_tokens(), 2); validate(need(length(pairs) > 0, "Not enough text to link word pairs."))
    tb <- head(sort(table(pairs), decreasing = TRUE), input$top)
    ends <- do.call(rbind, strsplit(names(tb), " ", fixed = TRUE))
    edges <- data.frame(a = ends[, 1], b = ends[, 2], n = as.numeric(tb), stringsAsFactors = FALSE)
    nodes <- unique(c(edges$a, edges$b))
    list(edges = edges, nodes = nodes, degree = as.numeric(table(factor(c(edges$a, edges$b), levels = nodes))))
  })
  chart("net", function() {
    x <- pair_network(); ang <- 2 * pi * seq_along(x$nodes) / length(x$nodes)   # nodes on a circle
    nx <- cos(ang); ny <- sin(ang); ia <- match(x$edges$a, x$nodes); ib <- match(x$edges$b, x$nodes)
    plot_ly() %>%
      add_trace(x = as.vector(rbind(nx[ia], nx[ib], NA)), y = as.vector(rbind(ny[ia], ny[ib], NA)), type = "scatter",
                mode = "lines", line = list(color = "#D8D0F2", width = 1.5), hoverinfo = "none", showlegend = FALSE) %>%
      add_trace(x = nx, y = ny, type = "scatter", mode = "markers+text", text = x$nodes, textposition = "top center",
                marker = list(size = 8 + 4 * x$degree, color = COL[["violet"]]),
                hovertext = paste(x$nodes, "- linked", x$degree, "times"), hoverinfo = "text", showlegend = FALSE) %>%
      style_plot() %>% layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
  })
  takeaway("net", function() {
    x <- pair_network(); i <- which.max(x$degree); hub <- paste0("'", x$nodes[i], "'")
    c(sprintf("%s is the hub word, linked to %d others.", bold(hub), as.integer(x$degree[i])),
      paste0("Many ", tolower(input$src), " connect through ", hub, ": it is a root theme, not a side issue."),
      paste0("A change to ", hub, " would ripple into the issues linked to it."),
      paste0("Treat ", hub, " as a priority theme in the action plan (Tab 11)."))
  })
  output$ins_ng <- renderUI({
    p <- review_parts(); if (is.null(p)) return(waiting_box("Glassdoor Reviews"))
    d <- quiet(top_runs)
    insight_box(
      "Longer bars mean a phrase is used more. A word run of 2 is a pair such as 'work life'. The network links words that appear side by side; bigger dots connect to more words. Switch Pros/Cons and reviewer group in the sidebar.",
      paste0(if (!is.null(d)) paste0("Top ", tolower(input$src), " for ", input$grp, ": <b>", paste(head(d$term, 3), collapse = "</b>, <b>"), "</b>. ") else "",
             sprintf("%.0f%% of reviews carried Pros/Cons labels; the rest were split by sentence tone.", 100 * attr(p, "labelled_share"))),
      if (input$src == "Cons") "These phrases are the experience gaps to close before advertising the brand harder. Compare Detractors with Promoters to see what separates them."
      else "These phrases are authentic strengths to feature in job ads and LinkedIn content, because employees already believe them.",
      "Frequency is not importance: a rare issue can matter more than a common one. The sentence-tone split is approximate. Reviewer groups come from star ratings because the file has no reviewer-type column.")
  })

  # ===========================================================================
  # TAB 5  TOPICS
  # ===========================================================================
  topics <- reactive({
    g <- gd_scored(); validate(need(!is.null(g), need_file("gd")))
    k <- input$k
    res <- withProgress(message = "Finding topics...", value = 0.5, fit_topics(g$tokens, k))
    validate(need(!is.null(res), "Need at least 20 reviews with shared vocabulary to find topics."))
    top <- do.call(rbind, lapply(seq_len(k), function(t) {
      o <- order(-res$phi[t, ])[1:8]
      data.frame(topic = t, term = res$vocab[o], weight = res$phi[t, o], stringsAsFactors = FALSE)
    }))
    labels <- vapply(seq_len(k), function(t) paste0("T", t, ": ", paste(top$term[top$topic == t][1:3], collapse = " / ")), "")
    meta <- g$d[res$keep, ]; th <- res$theta
    tone <- vapply(seq_len(k), function(t) sum(th[, t] * meta$sent) / sum(th[, t]), numeric(1))
    list(theta = th, top = top, labels = labels, meta = meta, k = k, tone = tone,
         share = colMeans(th) * 100, engine = res$engine)
  })
  chart("lt", function() {
    L <- topics(); d <- L$top[ave(L$top$weight, L$top$topic, FUN = seq_along) <= 6, ]
    d$label <- factor(L$labels[d$topic], levels = L$labels); d$row <- paste0("T", d$topic, "  ", d$term)
    plot_ly(d, x = ~weight, y = ~reorder(row, -topic * 10 + weight), color = ~label, colors = PAL[seq_len(L$k)],
            type = "bar", orientation = "h") %>% style_plot("Word weight within topic", NULL)
  })
  takeaway("lt", function() {
    L <- topics()
    c(paste0(L$k, " topics found (", L$engine, "): ", paste(bold(L$labels), collapse = "; "), "."),
      paste0("Reviews cluster into ", L$k, " distinct conversations; each label shows its three strongest words."),
      "These are the main things people say about working at Innovatech: the raw material for the employer value proposition.",
      "Give each topic a business name, then use the other topic charts to see which ones hurt ratings.")
  })
  topic_by_star <- reactive({
    L <- topics(); validate(need(has_col(L$meta, "overall_rating"), "Needs overall_rating in Glassdoor Reviews."))
    r <- round(L$meta$overall_rating); stars <- sort(unique(r[!is.na(r)]))
    stars <- stars[vapply(stars, function(s) sum(r == s, na.rm = TRUE) >= MIN_N, logical(1))]
    validate(need(length(stars) > 0, "Too few rated reviews per star level."))
    z <- t(vapply(stars, function(s) colMeans(L$theta[!is.na(r) & r == s, , drop = FALSE]) * 100, numeric(L$k)))
    list(z = z, stars = stars, r = r, L = L)
  })
  chart("lr", function() {
    x <- topic_by_star()
    plot_ly(z = x$z, x = x$L$labels, y = paste(x$stars, "star"), type = "heatmap",
            colorscale = list(c(0, "#F4F1FD"), c(1, "#8B5CF6")), colorbar = list(title = "% share"),
            hovertemplate = "%{y}<br>%{x}<br>%{z:.1f}%<extra></extra>") %>% style_plot()
  })
  takeaway("lr", function() {
    x <- topic_by_star(); r <- x$r; th <- x$L$theta
    low <- !is.na(r) & r <= 2; high <- !is.na(r) & r >= 4
    if (sum(low) < MIN_N || sum(high) < MIN_N) return(NULL)
    diff <- colMeans(th[low, , drop = FALSE]) - colMeans(th[high, , drop = FALSE]); i <- which.max(diff)
    c(sprintf("%s is %.0f points more common in 1-2 star than in 4-5 star reviews.", bold(x$L$labels[i]), 100 * diff[i]),
      "Unhappy employees talk about this topic far more than happy ones.",
      "It is a likely driver of low ratings and a weak employee voice.",
      "Make it a top-3 fix and check whether the same issue appears among the attrition drivers (Tab 6).")
  })
  chart("lc", function() {                       # chart "lc" in the Topics tab
    L <- topics(); d <- data.frame(label = L$labels, share = L$share, tone = L$tone)
    plot_ly(d, x = ~share, y = ~tone, text = ~label, type = "scatter", mode = "markers+text", textposition = "top center",
            marker = list(size = ~8 + share, color = ~tone, cmin = -1, cmax = 1, line = list(color = "#FFFFFF", width = 1.5),
                          colorscale = list(c(0, "#F43F5E"), c(0.5, "#E9E4F7"), c(1, "#14B8A6")))) %>%
      style_plot("Share of review content (%)", "Average tone (-1 to +1)")
  })
  takeaway("lc", function() {
    L <- topics(); b <- which.max(L$share); w <- which.min(L$tone)
    c(sprintf("Biggest topic: %s (%.0f%% of content, tone %+.2f). Most negative: %s (tone %+.2f).", bold(L$labels[b]), L$share[b], L$tone[b], bold(L$labels[w]), L$tone[w]),
      if (b == w) "The biggest topic is also the most negative: the main conversation is a complaint." else "The most negative topic is not the biggest one.",
      if (b == w) "This is the single largest threat to the employer brand." else "The most negative topic is a hidden risk: smaller, but strongly felt.",
      paste0("Prioritise ", L$labels[w], " for fixes", if (b != w && L$tone[b] > 0) paste0("; use ", L$labels[b], " as a positive brand message") else "", "."))
  })
  topic_by_year <- reactive({
    L <- topics(); validate(need(has_col(L$meta, "date"), "Needs dates in Glassdoor Reviews."))
    y <- format(L$meta$date, "%Y"); years <- names(which(table(y) >= MIN_N))
    validate(need(length(years) >= 2, "Need two or more years with 5+ reviews."))
    m <- t(vapply(years, function(v) colMeans(L$theta[!is.na(y) & y == v, , drop = FALSE]) * 100, numeric(L$k)))
    list(m = m, years = years, L = L)
  })
  chart("ly", function() {
    x <- topic_by_year(); p <- plot_ly()
    for (t in seq_len(x$L$k)) p <- add_lines(p, x = x$years, y = x$m[, t], name = x$L$labels[t], line = list(color = PAL[t]))
    style_plot(p, NULL, "% share")
  })
  takeaway("ly", function() {
    x <- topic_by_year(); change <- x$m[nrow(x$m), ] - x$m[1, ]; i <- which.max(change); bad <- x$L$tone[i] < 0
    c(sprintf("Fastest-growing topic from %s to %s: %s (%+.0f points).", x$years[1], tail(x$years, 1), bold(x$L$labels[i]), change[i]),
      "This topic is becoming a bigger part of what employees discuss.",
      if (bad) "A growing negative topic is an early warning of falling ratings." else "A growing positive topic is an emerging strength.",
      if (bad) "Act before it spreads, and brief leaders on the trend." else "Amplify it in employer-brand content.")
  })
  output$ins_lda <- renderUI({
    L <- quiet(topics); if (is.null(L)) return(waiting_box("Glassdoor Reviews"))
    w <- which.min(L$tone); b <- which.max(L$share)
    insight_box(
      "A topic is a group of words that tend to appear together; name each one from its top words. In the bubble chart, further right means discussed more and higher means more positive.",
      paste0("Most discussed: <b>", L$labels[b], "</b>. Most negative: <b>", L$labels[w], "</b> (tone ", show_num(L$tone[w], 2), "). Based on ", nrow(L$theta), " reviews using ", L$engine, "."),
      "The most negative sizeable topic is the strongest candidate driver of a weak employee voice. Prioritise it in the action plan and check it against the attrition drivers in Tab 6.",
      "Topics are statistical groupings, not facts, and shift with the number of topics. Re-run with 3-6 topics and trust only themes that keep appearing. Topic tone is associated with ratings, not proven to cause them.")
  })

  # ===========================================================================
  # TAB 6  ATTRITION
  # Attrition rate = leavers / employees x 100 (higher = worse)
  # The model uses the whole Employee Master and re-runs only when that file changes.
  # ===========================================================================
  model <- reactive({
    e <- store$emp; validate(need(has_col(e, "left"), need_file("emp")))
    m <- fit_attrition(e)
    validate(need(!is.null(m), "The model needs 50+ employees with at least 10 leavers and 10 stayers, and usable predictors."))
    m
  })
  output$auc_g <- renderUI({
    m <- quiet(model)
    number_box("Model accuracy (AUC)", show_num(m$auc_model, 2),
      if (is.null(m)) "Upload Employee Master (50+ rows, 10+ leavers)." else
        sprintf("Tested on %d unseen employees. %s", m$n_test,
                if (isTRUE(m$auc_model >= 0.8)) "Strong ranking of leavers." else if (isTRUE(m$auc_model >= 0.7)) "Useful ranking of leavers." else "Weak: use with care."), "blue")
  })
  output$auc_r <- renderUI({
    m <- quiet(model)
    number_box("Simple rule accuracy (AUC)", show_num(m$auc_rule, 2),
      if (is.null(m) || !is.finite(m$auc_rule)) "Low pay + few promotions + low appraisal + short tenure." else
        sprintf("The model is %s points %s than this common-sense rule.", show_num(abs(100 * (m$auc_model - m$auc_rule)), 0),
                if (m$auc_model >= m$auc_rule) "better" else "worse"), "teal")
  })

  # Like-for-like: expected rate from a glm on grade + tenure band; cell = actual - expected
  hotspots <- reactive({
    e <- emp(); validate(need(has_col(e, "left"), need_file("emp")))
    ax <- input$ax; ay <- input$ay
    validate(need(has_col(e, ax) && has_col(e, ay), "Choose two fields that exist in the file."),
             need(ax != ay, "Pick two different fields for rows and columns."))
    d <- e[!is.na(e$left) & !is.na(e[[ax]]) & !is.na(e[[ay]]), ]
    validate(need(nrow(d) >= MIN_N, "Too few employees for this selection."))
    adjusted <- isTRUE(input$adj); d$expected <- mean(d$left)
    if (adjusted) {
      controls <- setdiff(intersect(c("grade", "yos_band"), names(d)), c(ax, ay))
      controls <- controls[vapply(controls, function(v) length(unique(na.omit(d[[v]]))) > 1, logical(1))]
      if (length(controls)) {
        dd <- d[, c("left", controls), drop = FALSE]
        for (v in controls) { x <- as.character(dd[[v]]); x[is.na(x)] <- "Unknown"; dd[[v]] <- factor(x) }
        d$expected <- suppressWarnings(predict(glm(left ~ ., data = dd, family = binomial()), dd, type = "response"))
      }
    }
    cells <- d %>% group_by(X = .data[[ax]], Y = .data[[ay]]) %>%
      summarise(n = n(), rate = 100 * mean(left), exp = 100 * mean(expected), .groups = "drop")
    cells$value <- if (adjusted) cells$rate - cells$exp else cells$rate
    cells$value[cells$n < MIN_N] <- NA
    list(cells = cells, adjusted = adjusted, overall = 100 * mean(d$left), ax = ax, ay = ay)
  })
  chart("heat", function() {
    x <- hotspots(); c0 <- x$cells; xs <- as.character(c0$X); ys <- as.character(c0$Y)
    c0$hover <- ifelse(c0$n < MIN_N, sprintf("%s | %s<br>Only %d people - hidden", xs, ys, c0$n),
      sprintf("%s | %s<br>%d people<br>Attrition %.1f%%%s", xs, ys, c0$n, c0$rate,
              if (x$adjusted) sprintf("<br>Expected for this grade/tenure mix %.1f%%<br>Difference %+.1f pts", c0$exp, c0$rate - c0$exp) else ""))
    z <- to_matrix(c0, "X", "Y", "value"); h <- to_matrix(c0, "X", "Y", "hover")
    plot_ly(z = z, x = colnames(z), y = rownames(z), type = "heatmap", text = h, hoverinfo = "text",
            colorscale = if (x$adjusted) SCALE_GOOD_RISK else SCALE_RISK, zmid = if (x$adjusted) 0 else NULL,
            colorbar = list(title = if (x$adjusted) "pts vs expected" else "Attrition %")) %>%
      style_plot(names(AXIS_CHOICES)[AXIS_CHOICES == x$ay], names(AXIS_CHOICES)[AXIS_CHOICES == x$ax])
  })
  takeaway("heat", function() {
    x <- hotspots(); ok <- x$cells[!is.na(x$cells$value), ]
    if (!nrow(ok)) return("All cells have fewer than 5 people - choose broader fields.")
    b <- ok[which.max(ok$value), ]; cell <- paste(b$X, "|", b$Y)
    ratio <- b$rate / max(x$overall, 0.01)
    concentrated <- if (x$adjusted) b$value > 5 else ratio > 1.5
    c(if (x$adjusted) sprintf("Biggest hotspot after allowing for grade and tenure: %s at %.1f%% vs %.1f%% expected (%+.1f pts, %d people).", bold(cell), b$rate, b$exp, b$value, b$n)
      else sprintf("Hotspot: %s at %.1f%% attrition (%d people) vs %.1f%% overall, %.1fx the average.", bold(cell), b$rate, b$n, x$overall, ratio),
      if (concentrated) "Leaving is concentrated in specific groups, not spread evenly." else "Leaving is fairly evenly spread across groups.",
      if (concentrated) "A targeted fix in this group would cut attrition far more cheaply than a company-wide programme." else "Attrition is a company-wide issue rather than a pocket problem.",
      if (concentrated) paste0("Run stay interviews in ", cell, " and check its pay and promotions",
                               if (!x$adjusted) "; tick Like-for-like to confirm it is not just its grade and tenure mix" else "", ".")
      else "Look at company-wide drivers in the 'What is associated with leaving' chart.")
  })
  chart("roc", function() {
    m <- model()
    p <- plot_ly() %>%
      add_lines(data = m$roc_model, x = ~fpr, y = ~tpr, name = sprintf("Model (AUC %.2f)", m$auc_model), line = list(color = COL[["violet"]])) %>%
      add_lines(x = c(0, 1), y = c(0, 1), name = "Coin toss", line = list(color = "#9CA3AF", dash = "dash"))
    if (is.finite(m$auc_rule))
      p <- add_lines(p, data = m$roc_rule, x = ~fpr, y = ~tpr, name = sprintf("Simple rule (AUC %.2f)", m$auc_rule), line = list(color = COL[["teal"]]))
    style_plot(p, "Stayers wrongly flagged (share)", "Leavers correctly flagged (share)")
  })
  takeaway("roc", function() {
    m <- model(); r <- m$roc_model; i <- which(r$fpr >= 0.2)[1]; if (is.na(i)) return(NULL)
    caught <- 100 * r$tpr[i]
    c(sprintf("Flagging the riskiest employees until 20%% of stayers are flagged catches about %s; a coin toss would catch 20%%.", bold(sprintf("%.0f%% of leavers", caught))),
      if (caught >= 50) "The model picks out most likely leavers early." else "The model only partly separates leavers from stayers.",
      if (caught >= 50) "HR can focus retention effort on a small high-risk group instead of everyone." else "Risk scores are rough: useful for sizing groups, not for targeting.",
      "Use risk bands to size retention budgets by department, never to single out individuals.")
  })
  chart("imp", function() {
    d <- model()$effects; validate(need(nrow(d) > 0, "The model produced no usable effects."))
    plot_ly(d, x = ~effect, y = ~reorder(term, abs(effect)), type = "bar", orientation = "h",
            marker = list(color = ifelse(d$effect > 0, COL[["rose"]], COL[["teal"]])),
            text = ~sprintf("Odds x %.2f", odds), hoverinfo = "y+text", textposition = "none") %>%
      style_plot("Effect on odds of leaving (right = higher risk)", NULL)
  })
  takeaway("imp", function() {
    d <- model()$effects; up <- d[d$effect > 0, ]; down <- d[d$effect < 0, ]
    risk_f <- if (nrow(up)) up$term[1] else NA; keep_f <- if (nrow(down)) down$term[1] else NA
    c(paste0(if (!is.na(risk_f)) sprintf("Strongest risk factor: %s (odds x%.2f). ", bold(risk_f), up$odds[1]) else "",
             if (!is.na(keep_f)) sprintf("Strongest protective factor: %s (odds x%.2f).", bold(keep_f), down$odds[1]) else ""),
      paste0(if (!is.na(risk_f)) paste0("'", risk_f, "' goes with more leaving") else "No clear risk factor stands out",
             if (!is.na(keep_f)) paste0("; '", keep_f, "' goes with staying.") else "."),
      "These are the factors most linked to attrition in Innovatech's own data, so they are the most promising retention levers.",
      if (!is.na(keep_f) && grepl("Compa|Promotion", keep_f)) paste0("Test '", keep_f, "' as a lever in the What-if lab (Tab 7) and the lever chart (Tab 11).")
      else "Review the policy behind the top risk factor and test pay and promotion levers in Tabs 7 and 11.")
  })
  risk_list <- reactive({
    m <- model(); e <- emp(); validate(need(!is.null(e), "No employees match the filters."))
    a <- m$scored[m$scored$left == 0 & m$scored$employee_id %in% e$employee_id, c("employee_id", "prob"), drop = FALSE]
    validate(need(nrow(a) > 0, "No current employees match the filters."))
    show <- intersect(c("employee_id", "department", "grade", "yos", "compa_ratio", "num_promotions", "last_appraisal"), names(store$emp))
    a <- merge(a, store$emp[, show, drop = FALSE], by = "employee_id")
    a$risk_pct <- round(100 * a$prob, 1); a$prob <- NULL
    a$band <- cut(a$risk_pct, c(-Inf, 20, 40, Inf), labels = c("Low", "Medium", "High"))
    for (v in intersect(c("yos", "compa_ratio", "last_appraisal"), names(a))) a[[v]] <- round(a[[v]], 2)
    a[order(-a$risk_pct), ]
  })
  output$risk <- renderDT(dtable(risk_list()))
  takeaway("risk", function() {
    a <- risk_list(); high <- a[a$band == "High", ]; pct_high <- share_of(nrow(high), nrow(a))
    dep <- if (nrow(high) && "department" %in% names(high)) names(which.max(table(high$department))) else NA
    c(sprintf("%d of %d current employees (%.0f%%) are high risk (40%%+)%s.", nrow(high), nrow(a), pct_high,
              if (!is.na(dep)) paste0("; most are in ", bold(dep)) else ""),
      if (isTRUE(pct_high > 15)) "A sizeable group is at high risk of leaving." else "High-risk employees are a small minority.",
      paste0("Expected exits are concentrated", if (!is.na(dep)) paste0(" in ", dep) else "", "; each exit adds replacement and vacancy cost."),
      paste0("Brief ", if (!is.na(dep)) paste0(dep, " leadership") else "department heads",
             " with group-level numbers and plan stay conversations. For group planning only, never for decisions about an individual."))
  })
  output$ins_attr <- renderUI({
    m <- quiet(model); if (is.null(m)) return(waiting_box("Employee Master"))
    e <- emp(); rate <- if (!is.null(e)) 100 * mean(e$left, na.rm = TRUE) else NA
    insight_box(
      "The heatmap shows attrition % for each pair of groups (darker red = more leaving); cells under 5 people are hidden. Tick Like-for-like to compare each cell with what its grade and tenure mix would predict. AUC runs from 0.5 (coin toss) to 1 (perfect) and shows how well a method ranks leavers above stayers.",
      paste0("Attrition in the current filter is <b>", show_num(rate), "%</b> (about 11% with no filters). Model AUC <b>", show_num(m$auc_model, 2),
             "</b> vs <b>", show_num(m$auc_rule, 2), "</b> for the simple rule. Strongest associations: <b>", paste(head(m$effects$term, 3), collapse = "</b>; <b>"), "</b>."),
      "Aim retention money at the hotspot cells and the top risk factors rather than across the board. If the model barely beats the simple rule, act on the rule: it is easier to explain to managers.",
      "These are associations, not causes: low pay may travel with a poor manager, for example. Leaver records are a snapshot at exit, which can flatter accuracy. Flight-risk scores are for <b>group planning only, never for decisions about individuals</b>, and must be used under HR governance with respect for privacy.")
  })

  # ===========================================================================
  # TAB 7  WHAT-IF LAB
  # Covered = top cov% (by risk) of current employees with yos >= miny and risk >= fl
  # r1 Leavers avoided = sum(old risk - new risk with compa_ratio x (1 + pp/100))
  # r2 Cost            = covered x salary x pp/100        (one year)
  # r3 Net saving      = r1 x mult x salary - r2           (higher = better)
  # Without a salary, money is in salary-years (salary = 1).
  # ===========================================================================
  what_if <- reactive({
    m <- model()
    validate(need("compa_ratio" %in% m$nums, "The what-if lab needs compa_ratio in Employee Master (it is the pay lever)."))
    has_salary <- isTRUE(is.finite(input$sal) && input$sal > 0); salary <- if (has_salary) input$sal else 1
    min_yos <- if (isTRUE(is.finite(input$miny))) input$miny else 0
    current <- m$scored[m$scored$left == 0 & m$scored$employee_id %in% (emp()$employee_id %||% character(0)), ]
    if ("yos" %in% names(current)) current <- current[current$yos >= min_yos, ]
    eligible <- current[current$prob >= input$fl, ]
    n_cov <- floor(nrow(eligible) * input$cov / 100)
    covered <- eligible[order(-eligible$prob), ][seq_len(n_cov), , drop = FALSE]
    run <- function(uplift) {
      if (n_cov == 0) return(c(avoided = 0, cost = 0))
      nd <- covered; nd$compa_ratio <- nd$compa_ratio * (1 + uplift / 100)
      new_risk <- suppressWarnings(predict(m$model, nd, type = "response"))
      c(avoided = sum(covered$prob - new_risk), cost = n_cov * salary * uplift / 100)
    }
    now <- run(input$pp); benefit <- now[["avoided"]] * input$mult * salary
    list(n_eligible = nrow(eligible), n_cov = n_cov, base = sum(covered$prob), avoided = now[["avoided"]],
         cost = now[["cost"]], benefit = benefit, net = benefit - now[["cost"]], run = run,
         salary = salary, has_salary = has_salary, unit = if (has_salary) RUPEE else "salary-years")
  })
  money <- function(w, x) {
    if (is.null(w)) return(DASH)
    if (w$has_salary) show_num(x, 0, prefix = RUPEE) else show_num(x, 1, " salary-yrs")
  }
  output$r1 <- renderUI({
    w <- quiet(what_if)
    number_box("Leavers avoided (expected)", show_num(w$avoided, 1),
      if (is.null(w)) "Upload Employee Master (needs compa_ratio)." else
        sprintf("%d of %d eligible employees covered; %.0f%% fewer expected exits among them.", w$n_cov, w$n_eligible,
                if (w$base > 0) 100 * w$avoided / w$base else 0), "teal")
  })
  output$r2 <- renderUI({
    w <- quiet(what_if)
    number_box("Programme cost (1 year)", money(w, w$cost),
      if (!is.null(w) && !w$has_salary) paste0("Enter an average salary in the sidebar to see ", RUPEE, ".") else "Pay uplift x salary x people covered.", "amber")
  })
  output$r3 <- renderUI({
    w <- quiet(what_if)
    number_box("Net saving", money(w, w$net),
      if (!is.null(w) && w$cost > 0) sprintf("Returns %.2f for every 1 spent: %s.", w$benefit / w$cost, if (w$net >= 0) "pays back" else "does not pay back")
      else "Replacement cost avoided minus programme cost.",
      if (!is.null(w) && w$net < 0) "red" else "blue")
  })
  sensitivity <- reactive({
    w <- what_if(); uplifts <- 0:20
    v <- t(vapply(uplifts, w$run, numeric(2)))
    d <- data.frame(pp = uplifts, cost = v[, 2], benefit = v[, 1] * input$mult * w$salary)
    d$net <- d$benefit - d$cost
    list(d = d, w = w)
  })
  chart("sens", function() {
    x <- sensitivity(); d <- x$d
    plot_ly(d, x = ~pp) %>%
      add_lines(y = ~benefit, name = "Benefit", line = list(color = COL[["teal"]])) %>%
      add_lines(y = ~cost, name = "Cost", line = list(color = COL[["saffron"]])) %>%
      add_lines(y = ~net, name = "Net", line = list(color = COL[["violet"]], width = 3)) %>%
      style_plot("Pay uplift (%)", paste0("Amount (", x$w$unit, ")")) %>%
      layout(shapes = list(list(type = "line", x0 = input$pp, x1 = input$pp, yref = "paper", y0 = 0, y1 = 1,
                                line = list(dash = "dot", color = "#9CA3AF"))))
  })
  takeaway("sens", function() {
    x <- sensitivity(); d <- x$d; best <- d[which.max(d$net), ]
    if (x$w$n_cov == 0) return(c("Nobody qualifies at these settings.", "The risk threshold or minimum tenure is too strict.",
                                 "No pay programme can be costed yet.", "Lower the risk threshold or minimum tenure in the sidebar."))
    if (best$pp == 0) return(c("The net line falls from the start: no pay uplift pays back.",
                               "At these settings extra pay costs more than the exits it prevents.",
                               "Money spent on raises here would not be recovered through lower attrition.",
                               "Try a narrower target group, or a non-pay lever such as promotions (Tab 11)."))
    c(sprintf("Net saving peaks at a %s uplift (%s); your current %d%% gives %s.", bold(paste0(best$pp, "%")), money(x$w, best$net),
              as.integer(input$pp), money(x$w, d$net[d$pp == input$pp])),
      "Returns rise up to that point and then fall: beyond it, extra pay costs more than it saves.",
      "There is a sweet spot for pay; spending above it wastes retention budget.",
      sprintf("Set the uplift near %d%% and pilot it before rolling it out.", as.integer(best$pp)))
  })
  chart("cb", function() {
    w <- what_if()
    d <- data.frame(item = factor(c("Cost", "Benefit", "Net"), levels = c("Cost", "Benefit", "Net")), v = c(-w$cost, w$benefit, w$net))
    plot_ly(d, x = ~item, y = ~v, type = "bar",
            marker = list(color = c(COL[["saffron"]], COL[["teal"]], if (w$net >= 0) COL[["violet"]] else COL[["rose"]])),
            text = ~formatC(v, format = "f", digits = if (w$has_salary) 0 else 1, big.mark = ","), textposition = "outside") %>%
      style_plot(NULL, paste0("Amount (", w$unit, ")"))
  })
  takeaway("cb", function() {
    w <- what_if()
    if (w$cost == 0) return(c("No cost yet.", "The pay uplift is 0% or nobody qualifies.", "There is nothing to compare.", "Set a pay uplift above 0% in the sidebar."))
    c(sprintf("Spending %s avoids about %.1f exits worth %s: net %s.", money(w, w$cost), w$avoided, money(w, w$benefit), bold(money(w, w$net))),
      if (w$net >= 0) "The programme pays for itself." else "The programme costs more than it saves.",
      sprintf("Every 1 spent returns %.2f in avoided replacement cost.", w$benefit / w$cost),
      if (w$net >= 0) "Build a business case for a pilot in the Tab 6 hotspot." else "Narrow coverage, lower the uplift, or combine pay with a non-pay lever.")
  })
  output$ins_rx <- renderUI({
    w <- quiet(what_if); if (is.null(w)) return(waiting_box("Employee Master (it needs compa_ratio, the pay lever)"))
    insight_box(
      "Choose who qualifies (risk threshold, minimum tenure, coverage) and the size of the pay uplift. The lab re-scores those people with the higher pay and counts the drop in expected exits. The sensitivity chart shows where extra pay stops paying back.",
      paste0("Covering <b>", w$n_cov, "</b> people with a <b>", input$pp, "%</b> uplift avoids about <b>", show_num(w$avoided, 1), "</b> exits; net result <b>", money(w, w$net), "</b>.",
             if (!w$has_salary) " Money is in salary-years until you enter a salary." else ""),
      if (w$avoided <= 0) "In this data, higher pay is not linked to less leaving, so a pay rise is unlikely to fix attrition. Look at promotions, managers or the topics in Tab 5 instead."
      else if (w$net >= 0) "A targeted uplift for high-risk employees looks self-funding. Pilot it in the Tab 6 hotspot and measure actual exits before scaling."
      else "At these settings a pay uplift costs more than it saves. Narrow the group or combine it with non-pay levers.",
      "This assumes the past link between pay and leaving would hold after a raise, which may not be true. Risk covers the data period, not exactly one year. Salary and replacement cost are your inputs. Raises recur every year while their effect may fade.")
  })

  # ===========================================================================
  # TAB 8  LINKEDIN
  # Engagement = likes + comments + shares (no follower data, so not reach-adjusted)
  # Like-for-like: engagement index = post engagement / median post that month x 100
  # ===========================================================================
  posts <- reactive({
    d <- li(); if (is.null(d)) return(NULL)
    d$eng <- rowSums(d[, intersect(c("likes", "comments", "shares"), names(d)), drop = FALSE], na.rm = TRUE)
    if (has_col(d, "date")) {
      month <- format(d$date, "%Y-%m"); month[is.na(month)] <- "unknown"
      med <- ave(d$eng, month, FUN = function(x) median(x, na.rm = TRUE))
      d$eng_index <- ifelse(med > 0, 100 * d$eng / med, NA)
    } else d$eng_index <- NA_real_
    d
  })
  by_type <- reactive({
    d <- posts(); validate(need(!is.null(d), need_file("li")))
    use_index <- isTRUE(input$adj) && any(is.finite(d$eng_index))
    g <- d %>% filter(!is.na(post_type)) %>% group_by(post_type) %>%
      summarise(n = n(), v = avg(if (use_index) eng_index else eng, input$stat), .groups = "drop") %>% filter(n >= MIN_N)
    validate(need(nrow(g) > 0, "Too few posts per type (need 5+)."))
    g$share <- 100 * g$n / sum(g$n)
    list(g = g, use_index = use_index)
  })
  chart("we", function() {
    x <- by_type()
    plot_ly(x$g, x = ~v, y = ~reorder(post_type, v), type = "bar", orientation = "h", marker = list(color = COL[["violet"]]),
            text = ~paste(n, "posts"), hoverinfo = "x+text", textposition = "none") %>%
      style_plot(if (x$use_index) paste(input$stat, "engagement index (100 = typical post that month)") else paste(input$stat, "engagement per post"), NULL)
  })
  takeaway("we", function() {
    g <- by_type()$g; b <- g[which.max(g$v), ]; w <- g[which.min(g$v), ]
    if (nrow(g) == 1) return(sprintf("Only one post type (%s) has 5+ posts; post more formats to compare.", b$post_type))
    under <- b$share < 25
    c(sprintf("%s posts earn %.1fx the engagement of %s posts and make up %.0f%% of posting.", bold(b$post_type), b$v / max(w$v, 1e-9), w$post_type, b$share),
      if (under) "The best-performing format is under-used." else "The best format is already a large part of the mix.",
      if (under) "Innovatech is leaving engagement, and employer-brand reach, on the table." else "Further gains will come from content quality, not format.",
      if (under) paste0("Shift a bigger share of posts to ", b$post_type, "; test 2-3 extra posts a month.") else paste0("Test new topics within ", b$post_type, " posts."))
  })
  chart("mix", function() {
    d <- posts(); validate(need(!is.null(d), need_file("li")))
    g <- d %>% filter(!is.na(post_type)) %>% count(post_type)
    plot_ly(g, labels = ~post_type, values = ~n, type = "pie", hole = 0.6, textinfo = "label+percent",
            marker = list(colors = PAL, line = list(color = "#FFFFFF", width = 3)),
            hoverlabel = list(bgcolor = "#24163A", font = list(color = "#FFFFFF"))) %>%
      layout(showlegend = FALSE, paper_bgcolor = "rgba(0,0,0,0)", font = list(family = FONT_BODY, color = "#3B3452")) %>%
      config(displaylogo = FALSE)
  })
  takeaway("mix", function() {
    d <- posts(); g <- sort(table(d$post_type), decreasing = TRUE); most <- names(g)[1]
    best <- tryCatch({ x <- by_type()$g; x$post_type[which.max(x$v)] }, error = function(e) NA)
    same <- isTRUE(best == most)
    c(sprintf("Most posted type: %s (%.0f%% of %d posts).", bold(most), 100 * g[[1]] / sum(g), as.integer(sum(g))),
      if (is.na(best)) "Engagement by type is not available yet." else if (same) "The most posted format is also the most engaging."
      else paste0("The most posted format is not the most engaging one (", best, ")."),
      if (same) "Posting effort is well matched to what the audience responds to." else "Posting effort is going into a lower-return format.",
      if (same) "Keep the mix and refresh the topics." else if (!is.na(best)) paste0("Rebalance the content calendar toward ", best, " posts.") else "Check engagement by post type above.")
  })
  post_trend <- reactive({
    d <- posts(); validate(need(!is.null(d) && has_col(d, "date"), "Needs LinkedIn Posts with dates."))
    g <- d %>% filter(!is.na(date)) %>% mutate(month = as.Date(format(date, "%Y-%m-01"))) %>% group_by(month) %>%
      summarise(n = n(), v = avg(eng, input$stat), .groups = "drop")
    validate(need(nrow(g) >= 2, "Need posts in two or more months.")); g
  })
  chart("tr2", function() {
    g <- post_trend()
    plot_ly(g, x = ~month) %>%
      add_bars(y = ~n, name = "Posts", marker = list(color = "#DCD3FA")) %>%
      add_lines(y = ~v, name = paste(input$stat, "engagement"), yaxis = "y2", line = list(color = COL[["violet"]])) %>%
      style_plot(NULL, "Posts") %>% layout(yaxis2 = list(overlaying = "y", side = "right", title = "Engagement"))
  })
  takeaway("tr2", function() {
    g <- post_trend(); h <- max(1, min(3, floor(nrow(g) / 2))); a <- mean(head(g$v, h)); b <- mean(tail(g$v, h))
    change <- share_of(b - a, a)
    dir <- if (!is.finite(change)) "flat" else if (change > 10) "up" else if (change < -10) "down" else "flat"
    c(sprintf("Engagement per post moved from %.0f to %.0f (first vs last %d months, %s%%).", a, b, h, signed(change)),
      switch(dir, up = "Content is resonating more over time.", down = "Engagement per post is falling.", flat = "Engagement is broadly flat."),
      switch(dir, up = "The employer brand is reaching and engaging more people.", down = "The employer brand is reaching fewer people per post.",
                  flat = "Current content holds attention but is not growing it."),
      switch(dir, up = "Keep the approach and track applications, not just likes.", down = "Review recent content and frequency; bring back the formats that worked earlier.",
                  flat = "Test one new format or theme a month and compare."))
  })
  values_talk <- reactive({
    l <- lc_scored(); validate(need(!is.null(l), "Upload LinkedIn Comments to see values talk."))
    d <- l$d; d$values_hit <- grepl(VALUES_RX, tolower(d$comment_text), perl = TRUE)
    by <- "commenter_type"; joined <- FALSE
    if (joins()$post && has_col(store$li, "post_type")) {          # verified post_id join
      d <- merge(d, store$li[, c("post_id", "post_type")], by = "post_id"); by <- "post_type"; joined <- TRUE
    }
    validate(need(has_col(d, by), "Comments need commenter_type (or a confirmed post_id join)."))
    g <- d %>% filter(!is.na(.data[[by]])) %>% group_by(grp = .data[[by]]) %>%
      summarise(n = n(), share = 100 * mean(values_hit), tone = nss(lab[values_hit]), .groups = "drop") %>% filter(n >= MIN_N)
    validate(need(nrow(g) > 0, "Too few comments per group (need 5+)."))
    list(g = g, joined = joined, n = nrow(d))
  })
  chart("vmv", function() {
    x <- values_talk()
    plot_ly(x$g, x = ~grp, y = ~share, type = "bar", marker = list(color = COL[["sky"]]),
            text = ~sprintf("%d comments<br>Net sentiment of values comments: %s", n, ifelse(is.na(tone), "n/a", sprintf("%+.0f", tone))),
            hoverinfo = "x+y+text", textposition = "none") %>%
      style_plot(if (x$joined) sprintf("Post type (joined on post_id, %d comments)", x$n) else "Commenter type",
                 "% comments mentioning mission/values/culture")
  })
  takeaway("vmv", function() {
    x <- values_talk(); b <- x$g[which.max(x$g$share), ]; critical <- is.finite(b$tone) && b$tone < 0
    c(sprintf("%s draws the most values talk (%.0f%% of its comments, net tone %s).", bold(b$grp), b$share, signed(b$tone)),
      "Mission, values and culture resonate most with this group.",
      if (critical) "But the tone is negative: people are questioning the values, which is a warning sign." else "This audience connects with the company's purpose.",
      paste0(if (critical) "Back the values claims with evidence for " else "Aim purpose-led content at ", b$grp, ". (Illustrative: comments are simulated.)"))
  })
  output$ins_li <- renderUI({
    x <- quiet(by_type); if (is.null(x)) return(waiting_box("LinkedIn Posts (and Comments)"))
    b <- x$g[which.max(x$g$v), ]
    insight_box(
      "Engagement adds likes, comments and shares. Compare post types by bar length; the doughnut shows how often each type is posted. Tick Like-for-like to compare each post with others from the same month.",
      paste0("<b>", b$post_type, "</b> posts earn the most engagement but are <b>", show_num(b$share, 0), "%</b> of posting.",
             if (joins()$post) sprintf(" Comments are linked to posts on post_id (%d matching posts).", joins()$pj) else ""),
      if (b$share < 25) paste0("Quick win: shift the content mix toward ", b$post_type, " posts and feature real employees, so the outside story matches the inside one.")
      else "The mix already favours the best format; next, test which topics within it engage most.",
      "There is no follower or impression data, so this is raw engagement, not reach. A format may do well because of its timing or topic. Likes are not applications. Comments are simulated.")
  })

  # ===========================================================================
  # TAB 9  EMOJI  (uses the LinkedIn comments data "lc")
  # e1 = % comments with an emoji;  e2 = distinct emoji;
  # e3 = (positive - negative emoji) / scored emoji x 100  (higher = better)
  # ===========================================================================
  emoji <- reactive({
    l <- lc_scored(); if (is.null(l)) return(NULL)
    d <- l$d; text <- gsub(EMOJI_STRIP, "", d$comment_text, perl = TRUE)
    found <- regmatches(text, gregexpr(EMOJI_RX, text, perl = TRUE))
    d$n_emoji <- lengths(found)
    d$emoji_score <- vapply(found, function(e) { p <- sum(e %in% EMOJI_POS); n <- sum(e %in% EMOJI_NEG)
      if (p + n == 0) NA_real_ else (p - n) / (p + n) }, numeric(1))
    list(d = d, per_comment = found, all = unlist(found))
  })
  output$e1 <- renderUI({
    e <- emoji()
    number_box("Comments with emoji", if (is.null(e)) DASH else show_num(100 * mean(e$d$n_emoji > 0), 1, "%"),
               if (is.null(e)) need_file("lc") else sprintf("%d of %d comments. Simulated data.", sum(e$d$n_emoji > 0), nrow(e$d)), "blue")
  })
  output$e2 <- renderUI({
    e <- emoji()
    number_box("Distinct emoji", if (is.null(e)) DASH else length(unique(e$all)),
               if (is.null(e) || !length(e$all)) "" else sprintf("%d emoji used in total; top one: %s", length(e$all), names(which.max(table(e$all)))), "teal")
  })
  output$e3 <- renderUI({
    e <- emoji(); a <- if (is.null(e)) character(0) else e$all; p <- sum(a %in% EMOJI_POS); n <- sum(a %in% EMOJI_NEG)
    number_box("Emoji net sentiment", if (p + n == 0) DASH else show_num(100 * (p - n) / (p + n), 0),
               if (p + n == 0) "No scored emoji yet." else sprintf("%d positive vs %d negative emoji.", p, n),
               if (p + n > 0 && p < n) "red" else "amber")
  })
  emoji_top <- reactive({
    e <- emoji(); validate(need(!is.null(e) && length(e$all) > 0, "No emoji found in LinkedIn comments yet."))
    tb <- head(sort(table(e$all), decreasing = TRUE), 15)
    d <- data.frame(em = names(tb), n = as.numeric(tb), stringsAsFactors = FALSE)
    d$tone <- ifelse(d$em %in% EMOJI_POS, "Positive", ifelse(d$em %in% EMOJI_NEG, "Negative", "Unscored"))
    d$share <- 100 * d$n / length(e$all); d
  })
  chart("e_top", function() {
    d <- emoji_top()
    plot_ly(d, x = ~n, y = ~reorder(em, n), color = ~tone, type = "bar", orientation = "h",
            colors = c(Negative = COL[["rose"]], Positive = COL[["teal"]], Unscored = "#9CA3AF")) %>%
      style_plot("Times used", NULL) %>% layout(yaxis = list(tickfont = list(size = 18)))
  })
  takeaway("e_top", function() {
    d <- emoji_top(); tone <- d$tone[1]
    c(sprintf("%s alone is %.0f%% of all emoji; the top 3 make up %.0f%%.", d$em[1], d$share[1], sum(head(d$share, 3))),
      switch(tone, Positive = "Reactions are dominated by a positive emoji.", Negative = "Reactions are dominated by a negative emoji.",
             "The most used emoji has no fixed meaning."),
      if (tone == "Negative") "Outsiders are reacting badly: a reputation risk." else "Emoji give a quick read of how outsiders feel about the brand.",
      "Track the top emoji monthly as an early warning, and read the comments that carry negative emoji.")
  })
  emoji_groups <- reactive({
    e <- emoji(); validate(need(!is.null(e) && has_col(e$d, "commenter_type"), "Needs commenter_type in LinkedIn Comments."))
    g <- e$d %>% filter(!is.na(commenter_type)) %>% group_by(commenter_type) %>%
      summarise(n = n(), use = 100 * mean(n_emoji > 0), tone = 100 * mean(emoji_score, na.rm = TRUE), .groups = "drop") %>%
      filter(n >= MIN_N)
    validate(need(nrow(g) > 0, "Too few comments per commenter type (need 5+)."))
    g$tone[!is.finite(g$tone)] <- NA; g
  })
  chart("e_cmp", function() {
    g <- emoji_groups()
    plot_ly(g, x = ~commenter_type, y = ~use, type = "bar", name = "% comments with emoji", marker = list(color = COL[["violet"]])) %>%
      add_trace(y = ~tone, name = "Emoji tone (-100 to +100)", marker = list(color = COL[["saffron"]])) %>%
      style_plot(NULL, "Value") %>% layout(barmode = "group")
  })
  takeaway("e_cmp", function() {
    g <- emoji_groups(); b <- g[which.max(g$use), ]; scored <- g[!is.na(g$tone), ]
    cold <- if (nrow(scored)) scored$commenter_type[which.min(scored$tone)] else NA
    c(paste0(sprintf("%s use emoji most (%.0f%% of comments).", bold(b$commenter_type), b$use),
             if (!is.na(cold)) sprintf(" Least positive emoji tone: %s (%+.0f).", bold(cold), min(scored$tone)) else ""),
      "Groups differ in how expressive and how positive they are.",
      if (!is.na(cold)) paste0(cold, " is the least warm audience for the brand.") else "No group stands out as negative.",
      if (!is.na(cold)) paste0("Tailor content for ", cold, " and reply to their comments personally.") else "Keep monitoring by group.")
  })
  emoji_vs_words <- reactive({
    e <- emoji(); validate(need(!is.null(e), need_file("lc")))
    d <- e$d[is.finite(e$d$emoji_score), ]
    validate(need(nrow(d) >= MIN_N, "Need 5+ comments with scored emoji."))
    d$agree <- sign(d$sent) == sign(d$emoji_score) | d$sent == 0; d
  })
  chart("e_sc", function() {
    d <- emoji_vs_words(); set.seed(1)
    plot_ly(x = d$sent + runif(nrow(d), -0.04, 0.04), y = d$emoji_score + runif(nrow(d), -0.04, 0.04),
            type = "scatter", mode = "markers", color = if (has_col(d, "commenter_type")) d$commenter_type else I(COL[["violet"]]),
            colors = PAL, marker = list(opacity = 0.6), text = substr(d$comment_text, 1, 120), hoverinfo = "text") %>%
      style_plot("Word sentiment", "Emoji sentiment")
  })
  takeaway("e_sc", function() {
    d <- emoji_vs_words(); agree <- 100 * mean(d$agree)
    c(sprintf("Emoji agree with the words (or the words are neutral) in %.0f%% of comments; %d comments send mixed signals.", agree, sum(!d$agree)),
      if (agree >= 80) "Emoji and words mostly tell the same story." else "Many comments send mixed signals, such as a smiley on a complaint.",
      "Word-based sentiment can miss sarcasm or polite criticism hidden behind emoji.",
      "Read the mixed-signal comments before reporting outsider reputation as positive.")
  })
  emoji_table <- reactive({
    e <- emoji(); validate(need(!is.null(e) && length(e$all) > 0, "No emoji found yet."))
    tb <- sort(table(e$all), decreasing = TRUE)
    in_comments <- vapply(names(tb), function(x) sum(vapply(e$per_comment, function(v) x %in% v, logical(1))), numeric(1))
    data.frame(Emoji = names(tb), Uses = as.numeric(tb), Comments = in_comments,
               `Share of emoji %` = round(100 * as.numeric(tb) / sum(tb), 1),
               Tone = ifelse(names(tb) %in% EMOJI_POS, "+1", ifelse(names(tb) %in% EMOJI_NEG, "-1", "unscored")),
               check.names = FALSE, stringsAsFactors = FALSE)
  })
  output$e_tb <- renderDT(dtable(emoji_table(), 8))
  takeaway("e_tb", function() {
    d <- emoji_table(); unscored <- sum(d$`Share of emoji %`[d$Tone == "unscored"])
    c(sprintf("%d distinct emoji; %.0f%% of uses are unscored.", nrow(d), unscored),
      if (unscored > 50) "Most emoji used have no fixed positive or negative meaning." else "Most emoji used carry a clear tone.",
      "Emoji alone cannot be the main measure of reputation.",
      "Use emoji as a supporting signal alongside comment sentiment (Tab 3).")
  })
  output$ins_emo <- renderUI({
    e <- emoji(); if (is.null(e)) return(waiting_box("LinkedIn Comments"))
    p <- sum(e$all %in% EMOJI_POS); n <- sum(e$all %in% EMOJI_NEG)
    insight_box(
      "Emoji are a quick read of feeling. The scatter compares each comment's words with its emoji: points top-left or bottom-right are mixed signals, such as a smiley on a complaint.",
      paste0(show_num(100 * mean(e$d$n_emoji > 0), 0), "% of comments use emoji. ",
             if (p + n > 0) paste0("Scored emoji are ", show_num(100 * p / (p + n), 0), "% positive.") else "None of the emoji used are on the scored list."),
      "Emoji tone is a cheap early-warning signal for outsider reputation. Track it monthly and read the mixed-signal comments, which often hide criticism behind polite emoji.",
      "Emoji meaning varies by culture and context, and only common emoji are scored. Comments are public, self-selected and simulated, so findings are illustrative.")
  })

  # ===========================================================================
  # TAB 10  JOB ADS
  # Time to fill in days (lower = better); VMV alignment rescaled 0-100 (higher = better)
  # Like-for-like: time to fill / median for the same job title
  # ===========================================================================
  postings <- reactive({
    p <- jp(); if (is.null(p)) return(NULL)
    if (has_col(p, "vmv_alignment_score"))
      p$vmv100 <- pmin(100, p$vmv_alignment_score * vmv_factor(store$jp$vmv_alignment_score))
    if (has_col(p, "time_to_fill") && has_col(p, "job_title")) {
      title <- ifelse(is.na(p$job_title), "unknown", p$job_title)
      med <- ave(p$time_to_fill, title, FUN = function(x) median(x, na.rm = TRUE))
      p$ttf_ratio <- ifelse(med > 0, p$time_to_fill / med, NA)
    }
    p
  })
  output$j1 <- renderUI({
    p <- postings(); j <- joins()
    number_box("Job postings", if (is.null(p)) DASH else format(nrow(p), big.mark = ","),
      if (is.null(p)) need_file("jp") else if (!is.na(j$jj)) sprintf("%d (%.0f%%) have ad text to analyse.", j$jj, share_of(j$jj, nrow(store$jp)))
      else "Upload Job Ad Text to analyse wording.", "blue")
  })
  output$j2 <- renderUI({
    p <- postings(); v <- if (has_col(p, "time_to_fill")) avg(p$time_to_fill, input$stat) else NA
    number_box(paste(if (input$stat == "median") "Median" else "Mean", "time to fill"), show_num(v, 0, " days"),
      if (is.finite(v)) sprintf("Lower is better. %.0f%% of postings take over 60 days.", 100 * mean(p$time_to_fill > 60, na.rm = TRUE)) else "Lower is better.", "amber")
  })
  output$j3 <- renderUI({
    p <- postings(); v <- if (has_col(p, "vmv100")) avg(p$vmv100, input$stat) else NA
    number_box("Values (VMV) alignment", show_num(v, 0),
      if (is.finite(v)) sprintf("0-100. %.0f%% of postings score below 50.", 100 * mean(p$vmv100 < 50, na.rm = TRUE)) else "0-100, higher = ads reflect mission, vision and values.", "teal")
  })
  by_title <- reactive({
    p <- postings(); validate(need(has_col(p, "time_to_fill") && has_col(p, "job_title"), "Needs job_title and time_to_fill in Job Postings."))
    g <- p %>% filter(!is.na(job_title)) %>% group_by(job_title) %>%
      summarise(n = n(), v = avg(time_to_fill, input$stat), .groups = "drop") %>%
      filter(n >= MIN_N) %>% arrange(desc(n)) %>% head(15)
    validate(need(nrow(g) > 0, "Too few postings per job title (need 5+).")); g
  })
  chart("j_ti", function() {
    g <- by_title()
    plot_ly(g, x = ~v, y = ~reorder(job_title, v), type = "bar", orientation = "h", marker = list(color = COL[["saffron"]]),
            text = ~paste(n, "postings"), hoverinfo = "x+text", textposition = "none") %>%
      style_plot(paste(input$stat, "days to fill"), NULL)
  })
  takeaway("j_ti", function() {
    g <- by_title(); s <- g[which.max(g$v), ]; f <- g[which.min(g$v), ]; ratio <- s$v / max(f$v, 1e-9)
    c(sprintf("%s is slowest to fill (%.0f days), %.1fx longer than %s (%.0f days).", bold(s$job_title), s$v, ratio, f$job_title, f$v),
      if (ratio > 1.5) "Some roles are much harder to fill than others." else "Hiring speed is similar across roles.",
      "Slow roles carry vacancy cost, overload existing teams and signal weak pull for those jobs.",
      paste0("Review pay, requirements and ad wording for ", s$job_title, ", and use referrals and employee stories for it."))
  })
  chart("j_hi", function() {
    p <- postings(); validate(need(has_col(p, "time_to_fill"), "Needs time_to_fill in Job Postings."))
    plot_ly(x = p$time_to_fill, type = "histogram", marker = list(color = COL[["violet"]], line = list(color = "#FFFFFF", width = 1))) %>%
      style_plot("Days to fill", "Number of postings")
  })
  takeaway("j_hi", function() {
    p <- postings(); x <- p$time_to_fill[is.finite(p$time_to_fill)]; if (!length(x)) return(NULL)
    md <- median(x); p90 <- unname(quantile(x, 0.9)); long_tail <- p90 > 2 * md
    c(sprintf("Half of postings fill within %.0f days; 1 in 10 takes over %.0f days.", md, p90),
      if (long_tail) "There is a long tail of very slow hires." else "Hiring times are fairly consistent.",
      if (long_tail) "The slow tail drives a large share of total vacancy cost." else "Delays are a general pace issue, not a few outliers.",
      sprintf("Investigate postings taking over %.0f days first (sort the Postings table by time_to_fill).", p90))
  })
  vmv_speed <- reactive({
    p <- postings(); validate(need(has_col(p, "vmv100") && has_col(p, "time_to_fill"), "Needs vmv_alignment_score and time_to_fill."))
    use_ratio <- isTRUE(input$adj) && has_col(p, "ttf_ratio")
    d <- data.frame(x = p$vmv100, y = if (use_ratio) p$ttf_ratio else p$time_to_fill,
                    title = if ("job_title" %in% names(p)) p$job_title else NA)
    d <- d[is.finite(d$x) & is.finite(d$y), ]
    validate(need(nrow(d) >= MIN_N, "Too few postings with both values."))
    list(d = d, r = suppressWarnings(cor(d$x, d$y)), use_ratio = use_ratio)
  })
  chart("j_vm", function() {
    x <- vmv_speed(); d <- x$d; line <- lm(y ~ x, d); xs <- range(d$x)
    plot_ly(d, x = ~x, y = ~y, type = "scatter", mode = "markers", text = ~title, name = "Postings",
            marker = list(color = COL[["violet"]], opacity = 0.6)) %>%
      add_lines(x = xs, y = predict(line, data.frame(x = xs)), name = sprintf("Trend (r = %.2f)", x$r),
                line = list(color = COL[["rose"]]), inherit = FALSE) %>%
      style_plot("VMV alignment (0-100)", if (x$use_ratio) "Time to fill vs same-title median (1 = typical)" else "Days to fill")
  })
  takeaway("j_vm", function() {
    x <- vmv_speed(); d <- x$d; hi <- d$x >= median(d$x); r <- x$r
    dir <- if (!is.finite(r) || abs(r) < 0.1) "none" else if (r < 0) "faster" else "slower"
    c(sprintf("Correlation r = %.2f. Better-aligned half: %s %.1f; less-aligned half: %.1f%s.", r, input$stat,
              avg(d$y[hi], input$stat), avg(d$y[!hi], input$stat), if (x$use_ratio) " (ratio to title median)" else " days"),
      switch(dir, faster = "Ads that reflect the company's values tend to fill faster.", slower = "Ads that reflect the company's values tend to fill slower.",
             none = "Values alignment has almost no link with hiring speed."),
      switch(dir, faster = "A values-led employer brand seems to help attract candidates.", slower = "Values messaging may sit on harder roles, or may not be resonating.",
             none = "Values wording alone is not what speeds up hiring."),
      switch(dir, faster = "Raise VMV alignment in the lowest-scoring ads.", slower = "Tick Like-for-like to check whether high-VMV ads are simply for scarcer roles.",
             none = "Focus on pay, requirements and channels, and keep values wording honest."))
  })
  ad_themes <- reactive({
    a <- ja(); validate(need(has_col(a, "ad_text"), need_file("ja"))); p <- postings()
    if (joins()$job && has_col(p, "time_to_fill")) {                 # verified job_id join
      m <- merge(a[, c("job_id", "ad_text")], p[, c("job_id", "time_to_fill")], by = "job_id")
      m <- m[is.finite(m$time_to_fill), ]
      if (nrow(m) >= 2 * MIN_N) {
        cut_off <- median(m$time_to_fill)
        return(list(d = rbind(theme_table(m$ad_text[m$time_to_fill <= cut_off], NULL, "Faster to fill"),
                              theme_table(m$ad_text[m$time_to_fill > cut_off], NULL, "Slower to fill")),
                    joined = TRUE, n = nrow(m)))
      }
    }
    list(d = theme_table(a$ad_text, NULL, "All ads"), joined = FALSE, n = nrow(a))
  })
  chart("j_tt", function() {
    x <- ad_themes(); d <- x$d
    if (x$joined)
      plot_ly(d, x = ~share, y = ~theme, color = ~source, colors = c(COL[["teal"]], COL[["rose"]]), type = "bar", orientation = "h") %>%
        style_plot(sprintf("%% of ads mentioning theme (%d joined ads)", x$n), NULL) %>% layout(barmode = "group")
    else
      plot_ly(d, x = ~share, y = ~reorder(theme, share), type = "bar", orientation = "h", marker = list(color = COL[["teal"]])) %>%
        style_plot("% of ads mentioning theme", NULL)
  })
  takeaway("j_tt", function() {
    x <- ad_themes(); d <- x$d
    if (x$joined) {
      fast <- d[d$source == "Faster to fill", ]; slow <- d[d$source == "Slower to fill", ]
      diff <- fast$share - slow$share; i <- which.max(diff); th <- fast$theme[i]
      return(c(sprintf("Ads that fill faster mention %s %.0f points more often (%d postings with ad text).", bold(th), diff[i], x$n),
               paste0("Faster-filling ads talk more about ", th, "."),
               paste0("Candidates may respond to ", th, ": it could be part of Innovatech's pull."),
               paste0("Test adding evidenced ", th, " content to slow-filling ads. (Illustrative: ad text is simulated.)")))
    }
    b <- d[which.max(d$share), ]; w <- d[which.min(d$share), ]
    c(sprintf("Ads talk most about %s (%.0f%%) and least about %s (%.0f%%).", bold(b$theme), b$share, bold(w$theme), w$share),
      paste0("The ads lean heavily on ", b$theme, " and rarely mention ", w$theme, "."),
      paste0("If employees value ", w$theme, ", the ads are missing a selling point."),
      "Compare with what employees praise (Tab 4, Pros) and add it to the ads.")
  })
  postings_table <- reactive({
    p <- postings(); a <- ja(); validate(need(!is.null(p) || !is.null(a), "Upload Job Postings and/or Job Ad Text."))
    if (is.null(p)) { d <- a; d$ad_words <- lengths(tokenise(d$ad_text)); d$ad_text <- substr(d$ad_text, 1, 120); return(d) }
    d <- p[, intersect(c("job_id", "job_title", "date_posted", "vmv_alignment_score", "time_to_fill"), names(p)), drop = FALSE]
    if (joins()$job) {
      x <- a[, c("job_id", "ad_text")]; x$ad_words <- lengths(tokenise(x$ad_text))
      x$themes <- vapply(tolower(x$ad_text), function(s) sum(vapply(THEMES, function(rx) grepl(paste0("\\b(", rx, ")"), s, perl = TRUE), logical(1))), numeric(1))
      d <- merge(d, x[, c("job_id", "ad_words", "themes")], by = "job_id", all.x = TRUE)
    }
    d
  })
  output$j_tb <- renderDT(dtable(postings_table()))
  takeaway("j_tb", function() {
    d <- postings_table(); with_text <- if ("ad_words" %in% names(d)) sum(!is.na(d$ad_words)) else NA
    c(paste0(format(nrow(d), big.mark = ","), " postings in the current date range", if (!is.na(with_text)) sprintf("; %d have ad text.", with_text) else "."),
      "This is the record-level evidence behind the job-ad charts.",
      if (!is.na(with_text)) sprintf("Ad-wording findings rest on %.0f%% of these postings.", share_of(with_text, nrow(d))) else "Without ad text, only speed and VMV scores can be analysed.",
      "Sort by time_to_fill to find the slowest postings and read their ads.")
  })
  output$ins_ja <- renderUI({
    p <- postings(); a <- ja(); if (is.null(p) && is.null(a)) return(waiting_box("Job Postings and/or Job Ad Text"))
    x <- quiet(vmv_speed); r <- if (!is.null(x)) x$r else NA
    insight_box(
      "Bars compare typical days to fill by job title (titles with 5+ postings). The scatter asks whether ads that reflect our values fill faster; a downward trend line would say yes. The theme chart shows what our ads promise.",
      paste0(if (is.finite(r)) paste0("Values alignment and time to fill correlate at <b>r = ", show_num(r, 2), "</b>. ") else "",
             if (joins()$job) sprintf("Ad text is linked to postings on job_id for %d postings (about %.0f%%).", joins()$jj, share_of(joins()$jj, nrow(store$jp)))
             else "Ad text and postings are analysed separately (join not confirmed)."),
      if (is.finite(r) && r < -0.2) "Rewriting low-alignment ads around real, evidenced values could shorten hiring. Pilot it on the slowest job titles first."
      else "Values wording alone does not seem to speed hiring. Focus on the slowest titles' pay, requirements and sourcing, and keep ad promises consistent with what employees say (Tab 2).",
      "Time to fill depends on role scarcity, pay and the market, not only the ad. Ad text is simulated and covers only matched postings, so wording findings are illustrative. Correlation is not causation.")
  })

  # ===========================================================================
  # TAB 11  ACTION PLAN
  # p1 Current EBI
  # p2 Projected EBI: review NSS rises by nssg x pcov/100, Voice recalculated, other pillars flat
  # p3 Cost = cs x current employees in focus departments x pcov/100
  # p4 Months to target = td x (tgt - current) / (projected - current), straight line
  # ===========================================================================
  plan <- reactive({
    p <- pillars(); w <- weights(); now <- ebi_from(p, w)
    rating_part <- attr(p, "rating_part"); review_nss <- attr(p, "review_nss"); later <- p
    if (is.finite(review_nss)) {
      new_nss <- min(100, review_nss + input$nssg * input$pcov / 100)
      later["Voice"] <- mean(c(rating_part, (new_nss + 100) / 2), na.rm = TRUE)
    }
    projected <- ebi_from(later, w)
    e <- store$emp; n_current <- NA
    if (has_col(e, "left")) {
      if (length(input$pf) && "department" %in% names(e)) e <- e[e$department %in% input$pf, ]
      n_current <- sum(e$left == 0, na.rm = TRUE)
    }
    has_cost <- isTRUE(is.finite(input$cs) && input$cs > 0)
    cost <- if (has_cost && is.finite(n_current)) input$cs * n_current * input$pcov / 100 else NA
    months <- if (!is.finite(now)) NA else if (now >= input$tgt) 0
              else if (is.finite(projected) && projected > now && projected >= input$tgt) input$td * (input$tgt - now) / (projected - now) else NA
    list(pillars = p, now = now, projected = projected, n_current = n_current, cost = cost,
         has_cost = has_cost, months = months, review_nss = review_nss)
  })
  output$p1 <- renderUI({
    x <- plan()
    number_box("Current EBI", show_num(x$now),
      if (is.finite(x$now)) sprintf("%.0f points %s the target of %d.", abs(input$tgt - x$now), if (x$now < input$tgt) "below" else "above", as.integer(input$tgt)) else "Upload data in Tab 1.", "blue")
  })
  output$p2 <- renderUI({
    x <- plan()
    number_box("Projected EBI", show_num(x$projected),
      if (is.finite(x$review_nss)) sprintf("%s points if review sentiment rises %d pts across %d%% coverage.", signed(x$projected - x$now, 1), as.integer(input$nssg), as.integer(input$pcov))
      else "Needs Glassdoor review text to project.", "teal")
  })
  output$p3 <- renderUI({
    x <- plan()
    number_box("Programme cost", if (x$has_cost) show_num(x$cost, 0, prefix = RUPEE) else DASH,
      if (!x$has_cost) "Enter a cost per employee in the sidebar." else if (!is.finite(x$n_current)) "Needs Employee Master."
      else sprintf("%s current employees x %d%% coverage.", format(x$n_current, big.mark = ","), as.integer(input$pcov)), "amber")
  })
  output$p4 <- renderUI({
    x <- plan()
    number_box("Months to target", if (isTRUE(x$months == 0)) "Met" else show_num(x$months, 1),
      if (is.na(x$months)) "Target not reached with these settings: raise coverage or the sentiment gain."
      else sprintf("Target EBI %d within the %d-month plan.", as.integer(input$tgt), as.integer(input$td)),
      if (is.na(x$months)) "red" else "teal")
  })
  chart("p_ebi", function() {
    x <- plan(); validate(need(is.finite(x$now), "Upload data in Tab 1 so the Brand Index can be calculated."))
    m <- 0:input$td; y <- x$now + (x$projected - x$now) * m / input$td
    plot_ly(x = m, y = y, type = "scatter", mode = "lines+markers", name = "Projected EBI", line = list(color = COL[["violet"]])) %>%
      add_lines(x = range(m), y = rep(input$tgt, 2), name = "Target", line = list(color = COL[["rose"]], dash = "dash")) %>%
      style_plot("Month", "EBI (0-100)") %>%
      layout(yaxis = list(range = c(max(0, min(y, input$tgt) - 10), min(100, max(y, input$tgt) + 10))))
  })
  takeaway("p_ebi", function() {
    x <- plan(); if (!is.finite(x$now)) return(NULL)
    if (isTRUE(x$months == 0)) return(c(sprintf("The EBI (%.1f) already meets the target of %d.", x$now, as.integer(input$tgt)),
      "The brand is at the level set.", "The next gains come from protecting what works.", "Set a higher target to plan the next step."))
    if (is.na(x$months)) return(c(sprintf("At these settings the EBI reaches %.1f, short of %d by %.1f points.", x$projected, as.integer(input$tgt), input$tgt - x$projected),
      "Better review sentiment alone is not enough to reach the target.", "The plan needs more than one lever to lift the brand.",
      "Add retention and candidate-pull actions, or raise coverage and sentiment gain in the sidebar."))
    c(sprintf("The EBI rises from %.1f to %.1f, reaching the target of %d in about %s.", x$now, x$projected, as.integer(input$tgt), bold(sprintf("%.1f months", x$months))),
      "The target is achievable within the plan if sentiment improves as assumed.",
      "A stronger brand should show up as better reviews, faster hiring and fewer exits.",
      "Lock in owners and quarterly checkpoints so the assumed sentiment gain is tracked.")
  })
  levers <- reactive({
    m <- model()
    a <- m$scored[m$scored$left == 0 & m$scored$employee_id %in% (emp()$employee_id %||% character(0)), ]
    if (length(input$pf) && "department" %in% names(store$emp))
      a <- a[a$employee_id %in% store$emp$employee_id[store$emp$department %in% input$pf], ]
    validate(need(nrow(a) > 0, "No current employees in the focus departments."))
    a <- a[order(-a$prob), ][seq_len(floor(nrow(a) * input$pcov / 100)), , drop = FALSE]
    validate(need(nrow(a) > 0, "Coverage is 0%."))
    avoided <- function(nd) sum(a$prob - suppressWarnings(predict(m$model, nd, type = "response")))
    pay <- function(nd) { nd$compa_ratio <- nd$compa_ratio * 1.05; nd }
    promote <- function(nd) { z <- nd$num_promotions == 0; nd$num_promotions[z] <- 1; nd }
    out <- list()
    if ("compa_ratio" %in% m$nums) out[["Pay +5%"]] <- avoided(pay(a))
    if ("num_promotions" %in% m$nums) out[["Promote the never-promoted"]] <- avoided(promote(a))
    if (length(out) == 2) out[["Both"]] <- avoided(promote(pay(a)))
    validate(need(length(out) > 0, "Needs compa_ratio or num_promotions in Employee Master."))
    data.frame(lever = names(out), v = unlist(out), n = nrow(a), stringsAsFactors = FALSE)
  })
  chart("p_lev", function() {
    d <- levers()
    plot_ly(d, x = ~v, y = ~reorder(lever, v), type = "bar", orientation = "h", marker = list(color = COL[["teal"]]),
            text = ~sprintf("%.1f", v), textposition = "outside") %>%
      style_plot(sprintf("Expected exits avoided among the top-risk %d%% (%d people)", as.integer(input$pcov), d$n[1]), NULL)
  })
  takeaway("p_lev", function() {
    d <- levers(); single <- d[d$lever != "Both", ]; b <- single[which.max(single$v), ]
    c(sprintf("%s is the stronger single lever (%.1f exits avoided among %d people)%s.", bold(b$lever), b$v, b$n,
              if ("Both" %in% d$lever) sprintf("; both together avoid %.1f", d$v[d$lever == "Both"]) else ""),
      paste0(b$lever, " avoids more exits than the alternative in this data."),
      "Retention budget goes further with this lever.",
      paste0("Pilot '", b$lever, "' in the focus departments first and compare actual exits after 6 months."))
  })
  phases <- reactive({
    td <- input$td
    data.frame(phase = c("Diagnose and agree EVP", "Quick wins (content mix, ad copy)", "Fix pay and career gaps",
                         "Refresh brand story", "Measure and adjust"),
               start = c(0, 0.10, 0.20, 0.30, 0.70) * td, end = c(0.15, 0.35, 0.75, 0.85, 1.00) * td,
               stringsAsFactors = FALSE)
  })
  chart("p_tl", function() {
    ph <- phases()
    plot_ly(ph, x = ~end - start, y = ~factor(phase, levels = rev(phase)), base = ~start, type = "bar", orientation = "h",
            marker = list(color = PAL[1:5]), text = ~sprintf("Month %.0f-%.0f", start, end), hoverinfo = "y+text", textposition = "none") %>%
      style_plot("Month", NULL)
  })
  takeaway("p_tl", function() {
    ph <- phases()
    c(sprintf("Quick wins land by month %.0f; pay and career fixes run to month %.0f; the brand story is refreshed after that work is under way.", ph$end[2], ph$end[3]),
      "Fixes to the employee experience come before the brand is re-told.",
      "Promising change before it happens would widen the say-do gap.",
      "Hold the external brand refresh until the fixes show up in review sentiment.")
  })
  correlations <- reactive({
    e <- emp(); validate(need(!is.null(e) && nrow(e) >= 30, "Needs Employee Master with 30+ people in the filter."))
    v <- intersect(c("age", "yos", "prior_experience", "total_experience", "compa_ratio", "num_promotions", "last_appraisal", "left"), names(e))
    v <- v[vapply(v, function(x) isTRUE(sd(e[[x]], na.rm = TRUE) > 0), logical(1))]
    validate(need(length(v) >= 2, "Not enough numeric fields to correlate."))
    list(m = suppressWarnings(cor(e[, v], use = "pairwise.complete.obs")), v = v)
  })
  chart("p_cor", function() {
    x <- correlations(); lab <- ifelse(x$v == "left", "Left (1 = yes)", readable(x$v))
    plot_ly(z = x$m, x = lab, y = lab, type = "heatmap", zmin = -1, zmax = 1, colorscale = SCALE_GOOD_RISK,
            hovertemplate = "%{y} vs %{x}: %{z:.2f}<extra></extra>") %>% style_plot()
  })
  takeaway("p_cor", function() {
    x <- correlations(); if (!"left" %in% x$v) return(NULL)
    r <- x$m["left", setdiff(x$v, "left")]; i <- which.max(abs(r)); v <- readable(names(r)[i])
    c(sprintf("%s moves most with leaving (r = %+.2f).", bold(v), r[i]),
      if (r[i] < 0) paste0("Higher ", v, " goes with fewer exits.") else paste0("Higher ", v, " goes with more exits."),
      "It is a strong candidate lever, but correlation is not causation.",
      paste0("Test ", v, " in the What-if lab or a small pilot before any company-wide change."))
  })
  plan_table <- reactive({
    x <- plan(); p <- x$pillars; validate(need(any(is.finite(p)), "Upload data in Tab 1 to build the plan."))
    w <- worst_theme(); m <- quiet(model)
    driver <- if (!is.null(m) && any(m$effects$effect > 0)) m$effects$term[m$effects$effect > 0][1] else NA
    hot <- tryCatch({ h <- hotspots()$cells; h <- h[!is.na(h$value), ]; b <- h[which.max(h$value), ]; paste(b$X, "|", b$Y) }, error = function(e) NA)
    best_post <- tryCatch({ g <- by_type()$g; g$post_type[which.max(g$v)] }, error = function(e) NA)
    rows <- list(
      Voice      = c("Fix what employees complain about most",
                     if (!is.null(w)) paste0("Most negative theme: ", w$theme, " (net ", signed(w$net), ")") else "See Tabs 3-5",
                     "Review net sentiment", "HR business partners + leaders", "Months 1-9"),
      Retention  = c("Targeted pay and career moves for high-risk groups",
                     paste0(if (!is.na(driver)) paste0("Top risk factor: ", driver) else "See Tab 6", if (!is.na(hot)) paste0("; hotspot: ", hot) else ""),
                     "Attrition % (like-for-like)", "Total Rewards + Talent", "Months 2-9"),
      Reputation = c("Post more of the best-engaging format and reply to comments",
                     if (!is.na(best_post)) paste0("Best format: ", best_post) else "See Tab 8",
                     "Comment net sentiment, engagement index", "Employer brand team", "Months 1-4"),
      Candidate  = c("Rewrite ads around real, evidenced values and benefits", "See Tab 10 theme coverage",
                     "VMV score, time to fill", "Talent acquisition", "Months 1-6"))
    d <- do.call(rbind, lapply(names(rows), function(k) data.frame(
      Pillar = k, `Score now` = round(p[[k]], 1),
      Status = if (!is.finite(p[[k]])) "No data" else if (p[[k]] < input$tgt) "Below target" else "On target",
      Action = rows[[k]][1], Evidence = rows[[k]][2], KPI = rows[[k]][3], Owner = rows[[k]][4], When = rows[[k]][5],
      check.names = FALSE, stringsAsFactors = FALSE)))
    d[order(d$`Score now`, na.last = TRUE), ]                 # weakest first
  })
  output$p_tb <- renderDT(datatable(plan_table(), rownames = FALSE, options = list(dom = "t", ordering = FALSE)))
  takeaway("p_tb", function() {
    d <- plan_table(); below <- d[d$Status == "Below target", ]
    if (!nrow(below)) return(c("All pillars with data are on target.", "The brand is healthy on every measured pillar.",
                               "The risk now is complacency.", "Keep the KPIs and review them quarterly."))
    c(sprintf("%d of %d pillars are below target; the weakest is %s (%.0f).", nrow(below), sum(d$Status != "No data"), bold(below$Pillar[1]), below$`Score now`[1]),
      "The plan has to start where the brand is weakest.",
      paste0("A weak ", below$Pillar[1], " pillar means ", WEAK_MEANS[[below$Pillar[1]]], "."),
      "Confirm an owner and KPI for each row and review progress quarterly.")
  })
  output$ins_p <- renderUI({
    x <- plan(); if (!is.finite(x$now)) return(waiting_box("data"))
    insight_box(
      "Set coverage, cost, delivery time and target in the sidebar. The line shows the EBI path if review sentiment improves as set; the lever chart compares retention moves; the table turns the evidence into owners and KPIs.",
      paste0("EBI moves from <b>", show_num(x$now), "</b> to <b>", show_num(x$projected), "</b>",
             if (is.na(x$months)) paste0("; the target of ", input$tgt, " is <b>not reached</b>.")
             else if (x$months == 0) "; the target is already met." else paste0(", reaching ", input$tgt, " in about <b>", show_num(x$months, 1), " months</b>."),
             if (x$has_cost) paste0(" Estimated cost: <b>", show_num(x$cost, 0, prefix = RUPEE), "</b>.") else ""),
      "Sequence matters: fix the lived experience (pay, careers, the top complaint) before amplifying the brand, so new joiners find what they were promised. Review the KPIs quarterly and move budget to what works.",
      "The projection models only the review-sentiment lever; other pillars are held flat and gains are assumed to be linear. Correlations show what moves together, not what causes what. Act ethically: use group-level insight, be open with employees, and never use flight-risk scores to judge or penalise individuals.")
  })
}

shinyApp(ui, server)
