##header includes:
- \PassOptionsToPackage{dvipsnames}{xcolor}
- \usepackage{xcolor}
- \definecolor{BrickRed}{HTML}{B22222}
- \usepackage{tikz}
- "\\usepackage{tikz-cd}"
- \usepackage{pgfplots}
- \usepackage[margin=1in]{geometry}
- \usepackage{bm}
- \usetikzlibrary{arrows.meta}
- \usepackage{titling}
- "\\pretitle{\\begin{center} \\includegraphics[width=2in,height=2in]{Ku-ucph-logo-svg.svg.png}\\LARGE\\\\}"
- \posttitle{\par\vspace{1.0em}\end{center}}
- \usepackage{fancyhdr}
- \pagestyle{fancy}
- \fancyhf{}
- \lhead{\textit{Youssef M. Raad}}
- \renewcommand{\headrulewidth}{1pt}
- \cfoot{\thepage}
- \newcommand{\beginsupplement}{\setcounter{table}{0}  \renewcommand{\thetable}{S\arabic{table}}
  \setcounter{figure}{0} \renewcommand{\thefigure}{S\arabic{figure}}}
- \usepackage{float}
- \floatplacement{figure}{H}

# most figures were made by default and imported in as images using plotStationary, plot, plotPR

###
dftry <- data.frame(
  "Shark 6" = c(237620.4, 227662.8, 237611.2, 227645.7, 237558.9, 227560.9),
  "Shark 17" = c(451946.7, 429183.1, 451940.3, 429160.2, 451789.7, 428892.6),
  "Shark 30" = c(566068.7, 532996.7, 566082.1, 533023, 565888, 532846.3)
)

rownames(dftry) <-
  c(
    "2 state, intercept",
    "3 state, intercept",
    "2 state, TOD",
    "3 state, TOD",
    "2 state, TOY",
    "3 state, TOY"
  )
colnames(dftry)<- c("Shark 6", "Shark 17", "Shark 30")
mins = apply(dftry, MARGIN = 2, FUN = function(x) x == min(x, na.rm = TRUE))
mins[] = ifelse(is.na(mins), FALSE, mins)

options(knitr.kable.NA = '-')

kbl(
  dftry,
  longtable = T,
  booktabs = T,
  linesep = "\\addlinespace",
  caption = "AIC for all the fitted models"
) %>% 
  add_header_above(c( " ", "AIC" = 3 ), bold = TRUE) %>% 
  column_spec(column = 2, bold = mins[,1]) %>% 
  column_spec(column = 3, bold = mins[,2]) %>% 
  column_spec(column = 4, bold = mins[,3])%>%
  row_spec(0, bold = TRUE)%>% 
  column_spec(column = 1, bold = TRUE)
###

###
dftry2 <- data.frame(
  "Shark 6" = c("$8.8 \\cdot 10^{-7}$, $9.9 \\cdot 10^{-1}$, $5.6 \\cdot 10^{-9}$", "344.29 (343.42, 345.16)", "36.67 (36.03, 37.30)", "231.20 (228.71, 233.69)", "80.47 (78.75, 82.20)", "623.30 (616.87, 629.75)", "149.24 (145.27, 153.22)"),
  "Shark 17" = c("$1.0 \\cdot 10^0$, $4.0 \\cdot 10^{-39}$", "291.17 (290.15, 292.19)", "64.38 (63.68, 65.08)", "433.76 (433.13, 434.38)", "43.79 (43.36, 44.20)", NA, NA),
  "Shark 30" = c("$1.0 \\cdot 10^0$, $1.7 \\cdot 10^{-8}$", "221.37 (220.80, 221.93)", "55.39 (54.97, 55.82)", "597.86 (596.88, 598.84)", "60.16 (59.43, 60.90)", NA, NA)
)

rownames(dftry2) <-
  c(
    "$\\boldsymbol{\\delta}$",
    "$\\boldsymbol{\\mu_1}$",
    "$\\boldsymbol{\\sigma_1}$",
    "$\\boldsymbol{\\mu_2}$",
    "$\\boldsymbol{\\sigma_2}$",
    "$\\boldsymbol{\\mu_3}$",
    "$\\boldsymbol{\\sigma_3}$"
  )
colnames(dftry2)<- c("Shark 6", "Shark 17", "Shark 30")
options(knitr.kable.NA = '-')
kbl(
  dftry2,
  longtable = T,
  booktabs = T,
  linesep = "\\addlinespace",
  caption = "Initial distribution, state-dependent parameters and confidence intervals for chosen TOY model for every shark",
  escape = FALSE
) %>% 
  add_header_above(c( " ", "Parameters" = 3 ), bold = TRUE)%>%
  row_spec(0, bold = TRUE)
###

###
# Coloured by most likely state sequence
data_plot$state <- factor(paste0("State ", viterbi(hmm)))
ggplot(data, aes(time, Depth, col = state)) +
  geom_point() +
  scale_color_manual(values = pal, name = NULL)
###

###
# Coloured by state probability
data_plot$pr_s2 <- stateProbs(hmm)[,2]
ggplot(data, aes(time, Depth, col = pr_s2)) +
  geom_point() +
  scico::scale_color_scico(palette = "berlin",
                           name = expression("Pr("~S[t]~"= 2)"))
###

###
# Plot the density of "Depth" for each month using facet_wrap
d1<-ggplot(df6, aes(x = Depth)) +
  geom_density(fill = "blue") +
  facet_wrap(~ month(time), nrow = 3) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Shark 6")+ theme(plot.title = element_text(face = "bold")) 
# Plot the density of "Depth" for each month using facet_wrap
d2<-ggplot(df17, aes(x = Depth)) +
  geom_density(fill = "blue") +
  facet_wrap(~ month(time), nrow = 3) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Shark 17")+ theme(plot.title = element_text(face = "bold")) 
# Plot the density of "Depth" for each month using facet_wrap
d3<-ggplot(df30, aes(x = Depth)) +
  geom_density(fill = "blue") +
  facet_wrap(~ month(time), nrow = 3) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Shark 30") + theme(plot.title = element_text(face = "bold")) 
grid.arrange(d1, d2, d3, nrow = 3)
###

###
p1<- ggplot(df6, aes(x = Depth)) +
  geom_histogram(binwidth = 20, fill = "blue", color = "black") +
  xlab("Depth (m)") +
  ylab("Frequency") +
  ggtitle(" Shark 6") + 
  theme(axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5))


p2<- ggplot(df17, aes(x = Depth)) +
  geom_histogram(binwidth = 20, fill = "blue", color = "black") +
  xlab("Depth (m)") +
  ylab("Frequency") +
  ggtitle("Shark 17") + 
  theme(axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5))


p3<- ggplot(df30, aes(x = Depth)) +
  geom_histogram(binwidth = 20, fill = "blue", color = "black") +
  xlab("Depth (m)") +
  ylab("Frequency") +
  ggtitle("Shark 30") + 
  theme(axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(p1,p2,p3, ncol=3)
###