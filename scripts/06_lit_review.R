# Lit Review Table

# Load libs 
library(xtable)
library(readr)

# read csv
lit <- read_csv("data/dotters_lit.csv")
lit_small <- lit[, 1:7]

print(
        xtable(lit_small,
          digits = 3,
          align = c("p{0.05\\textwidth}", "p{0.07\\textwidth}", "p{0.05\\textwidth}", "p{0.1\\textwidth}", "p{0.4\\textwidth}", "p{0.1\\textwidth}", "p{0.1\\textwidth}", "p{0.2\\textwidth}"),
          caption = "Literature Review", 
          label = "tab:lit"), 
        include.rownames = FALSE,
        include.colnames = TRUE, 
        size="\\tiny", 
        type = "latex", 
        sanitize.text.function=identity,
        #sanitize.text.function = function(x){x},
        caption.placement = "top",
        table.placement = "!htb",
        tabular.environment='longtable',
        floating = FALSE,
        booktabs = TRUE,
        hline.after = c(-1:nrow(lit_small)),
        file = "tabs/appendix_lit_review.tex")
