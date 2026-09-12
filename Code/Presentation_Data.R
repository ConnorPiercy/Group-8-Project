#Analysis from Quarto Document to source for Presentation

library("readxl")
library(tidyverse)
library(gt)
library(GGally)
library(cowplot)
library(MVN)
library(ggcorrplot)
library(mvtnorm)
library(knitr)
library(ggbiplot)
Table1_raw <- read_excel("../Data/Table1_raw.xlsx")

Table1 <- read_excel("../Data/Table1_clean.xlsx")
Table1$`17-Cl*` <- as.numeric(Table1$`17-Cl*`)
Table1$`15-P` <- as.numeric(Table1$`15-P`)
Table1$`29-Cu` <- as.numeric(Table1$`29-Cu`) 


### Kept this transformation in the method section.

#Non transformed table - dont use. 
# Make estimated concentrations positive
Table1 <- Table1 %>% 
  mutate(across(where(is.numeric), abs))

# Replace values recorded as zero with half of the
# smallest positive value for that element
Table1 <- Table1 %>%
  mutate(across(
    where(is.numeric),
    ~ replace(
      .x,
      is.na(.x) | .x == 0,
      min(.x[.x > 0], na.rm = TRUE) / 2
    )
  ))

#Log Table - Use as variables become comparable
Table1_log <- Table1 %>% 
  mutate(across(where(is.numeric), log10))

#Change table around to long ways for boxplots in future as current table arrangement didnt work.

#This one incase we need it for the non-transformed. 
Table1_long <- Table1 %>%
  pivot_longer(
    cols = -Elements,
    names_to = "Element_Group",     
    values_to = "Concentrations"   
  )

#Main Table we use for boxplot
Table1_long_log <- Table1_log %>%
  pivot_longer(
    cols = -Elements,
    names_to = "Element_Group",     
    values_to = "Concentrations"
  )

# Descriptive table of Raw data - Show why we need to transform
desc_pixe <- Table1 %>% 
  summarise(
    across(
      where(is.numeric),
      list(
        M = ~mean(.x, na.rm = TRUE),
        SD = ~sd(.x, na.rm = TRUE),
        Median = ~median(.x, na.rm = TRUE),
        Min = ~min(.x, na.rm =TRUE),
        Q1 = ~round(quantile(.x, probs = 0.25, na.rm = TRUE),2),
        IQR = ~round((quantile(.x, probs = 0.75, na.rm =TRUE) - quantile(.x, probs = 0.25, na.rm = TRUE)), 2),
        Q3 = ~round(quantile(.x, probs = 0.75, na.rm = TRUE),2),
        Max = ~max(.x, na.rm =TRUE)
      )
    )
  ) %>% 
  pivot_longer(
    everything(),
    names_to = c("Elements", "Statistics"),
    names_sep = "_",
    values_to = "Value"
  ) %>% 
  pivot_wider(
    names_from = Statistics,
    values_from = Value
  )

output_desc_pix <- desc_pixe %>% 
  gt() %>% 
  cols_label(
    Elements = "Element",
    M = md("Mean"),
    SD = md("Standard D."),
    Median = "Median",
    Min = "Minimum",
    Q1 = "1st Quartile",
    IQR = "Inter-Q Range",
    Q3 = "3rd Quartile",
    Max = "Maximum"
  ) %>% 
  fmt_number(
    columns = c(M, SD, Median, Min, Max),
    decimals = 2
  ) %>% 
  tab_header(
    title = "Descriptive Statistics of Elemental Concentrations",
    subtitle = "Non-Transformed"
  ) %>% 
  tab_options(
    table.font.size = 12
  )

# Log Descriptives Table
desc_pixe_log <- Table1_log %>% 
  summarise(
    across(
      where(is.numeric),
      list(
        M = ~mean(.x, na.rm = TRUE),
        SD = ~sd(.x, na.rm = TRUE),
        Median = ~median(.x, na.rm = TRUE),
        Min = ~min(.x, na.rm =TRUE),
        Q1 = ~round(quantile(.x, probs = 0.25, na.rm = TRUE),2),
        IQR = ~round((quantile(.x, probs = 0.75, na.rm =TRUE) - quantile(.x, probs = 0.25, na.rm = TRUE)), 2),
        Q3 = ~round(quantile(.x, probs = 0.75, na.rm = TRUE),2),
        Max = ~max(.x, na.rm =TRUE)
      )
    )
  ) %>% 
  pivot_longer(
    everything(),
    names_to = c("Elements", "Statistics"),
    names_sep = "_",
    values_to = "Value"
  ) %>% 
  pivot_wider(
    names_from = Statistics,
    values_from = Value
  )
output_desc_pix_log <- desc_pixe_log %>% 
  gt() %>% 
  cols_label(
    Elements = "Element",
    M = md("Mean"),
    SD = md("Standard D."),
    Median = "Median",
    Min = "Minimum",
    Q1 = "1st Quartile",
    IQR = "Inter-Q Range",
    Q3 = "3rd Quartile",
    Max = "Maximum"
  ) %>% 
  fmt_number(
    columns = c(M, SD, Median, Min, Max),
    decimals = 2
  ) %>% 
  tab_header(
    title = "Descriptive Statistics of Elemental Concentrations",
    subtitle = "Log10 Transformation"
  ) %>% 
  tab_options(
    table.font.size = 12
  )

decp_boxplot <- ggplot(data = Table1_long_log, aes(x = Element_Group, y = Concentrations)) + 
  geom_boxplot(aes(fill = Element_Group), notch = TRUE, outlier.shape = 1.5) + 
  theme_bw() +
  theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 1, vjust = 0.5), 
        legend.position = "none")

Table1_elements_log <- Table1_log[, -1]
mu_hat <- colMeans(Table1_elements_log)
Sigma_hat <- cov(Table1_elements_log)
p <- ncol(Table1_elements_log)

dM <- mahalanobis(Table1_elements_log, center=mu_hat, cov=Sigma_hat)
upper.quantiles <- qchisq(c(.9, .95, .99), df=p)
density.at.quantiles <- dchisq(x=upper.quantiles, df=p)
cut.points <- data.frame(upper.quantiles, density.at.quantiles)
dm_plot <- ggplot(data.frame(dM), aes(x=dM)) +
  geom_histogram(aes(y=after_stat(density)), bins=nclass.FD(dM),
                 fill="white", col="black") +
  geom_rug() +
  stat_function(fun=dchisq, args = list(df=p),
                col="red", linewidth=2, alpha=.7, xlim=c(0,25)) +
  geom_segment(data=cut.points,
               aes(x=upper.quantiles, xend=upper.quantiles,
                   y=rep(0,3), yend=density.at.quantiles),
               col="blue", linewidth=2) +
  xlab("Squared Mahalanobis distances and cut points") +
  ylab("Histogram and density")

Table1_elements_log$dM <- dM
Table1_elements_log$suprise <- cut(Table1_elements_log$dM,
                                   breaks = c(0, upper.quantiles, Inf),
                                   labels=c("typical", "somewhat", "Suprising", "very"))

dm_results <- data.frame(
  Sample = Table1_log[[1]],
  dM = dM,
  surprise = Table1_elements_log$suprise
)

dm_results <- dm_results %>% 
  dplyr::arrange(desc(dM)) %>% 
  head(5)
dm_plot
uq_table <- data.frame(
  "Upper Quantiles" = c("90th Quantile", "95th Quantile", "99th Quantile"),
  "Values" = upper.quantiles
)


#Had problems with the pairs plot from lectures mainly with the contours and scatter-points
#Two fix I built the two types of plots seperately so I can change the sizes and bins
#Then added them in seperately in the ggpairs command like done in the lectures
#The main problem was that they become squished when using the inbuilt command due to
#their being 17 variables. So doing them seperately helped fix that

surprise_points <- function(data, mapping, ...){
  ggplot(data = data, mapping = mapping) +
    geom_point(
      aes(colour = suprise),
      alpha = 0.5)}
upper_contour <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    stat_density_2d(
      colour = "blue",
      linewidth = 0.3,
      bins = 3)}

pairs <- ggpairs(data = Table1_elements_log,
                 columns = 1:17,
                 lower = list(continuous = surprise_points),
                 upper = list(continuous = upper_contour),
                 diag = list(continuous = wrap(
                   "densityDiag",
                   colour = "black",
                   fill = "grey"))) +
  scale_color_manual(
    values = c(
      "lightgray",
      "green",
      "blue",
      "red")) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 7))

# Legend for the pairs plot 
#Tried adding the main part to the pairs plot code itself but wouldnt show so 
#doing it seperately using cowplot. 
legend_plot <- ggplot(
  Table1_elements_log,
  aes(x = suprise, y = 1, colour = suprise)
) +
  geom_point() +
  scale_color_manual(
    values = c(
      "typical" = "lightgray",
      "somewhat" = "green",
      "Suprising" = "blue",
      "very" = "red"
    ),
    labels = c(
      "typical" = "Typical (<90%)",
      "somewhat" = "Somewhat surprising (90–95%)",
      "Suprising" = "Surprising (95–99%)",
      "very" = "Very surprising (>99%)"
    ),
    name = "Mahalanobis classification",
    drop = FALSE
  ) +
  guides(
    colour = guide_legend(
      nrow = 1,
      override.aes = list(alpha = 1, size = 3)
    )
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.box.margin = margin(5,10,5,10)
  )
#cowplot adds the legend that we made separately to the pairs plot
#Very annoying work but none the less like the result
legend <- cowplot::get_legend(legend_plot)
#Something that cowplot can use as ggpairs isnt a ggplot2 ggplot
pairs_grob <- GGally::ggmatrix_gtable(pairs)

final_pairs <- cowplot::plot_grid(
  pairs_grob,
  legend,
  ncol = 1,
  rel_heights = c(1, 0.14)
)

qq_elements <- Table1_elements_log[, -c(18, 19)]
n <- nrow(qq_elements)
p <- ncol(qq_elements)

chi_quantiles <- qchisq(
  ppoints(n),
  df = p
)

qq_data <- data.frame(
  Theoretical = qchisq(ppoints(n), df = p),
  Observed = sort(dM)
)

#Chi-square QQ-plot
normality_plot <- ggplot(qq_data,aes(x = Theoretical, y = Observed)) +
  geom_point(size = 2) +
  geom_abline(intercept = 0,
              slope = 1,
              linetype = "dashed",
              color = "red") +
  labs(x = "Theoretical chi-square(17) quantiles",
       y = "Ordered dm^2",
       title = "Multivariate Q-Q Plot"
  ) +
  theme_minimal()

#Mardia Multivariate Normal Test and individual anderson-darling tests of normality
mvn_result <- mvn(
  data = qq_elements,
  mvn_test = "mardia"
)

ggcorrplot(cor(Table1_log[,-1]),
           method = "circle",
           hc.order = TRUE,
           type = "lower")


#Look at two specific relationships too show off and talk about. 
mu <- colMeans(Table1_log[,-1])
Sigma <- var(Table1_log[,-1])
rho <- cor(Table1_log[,-1])

#Means
(mu_mg_si <- mu[c("12-Mg*", "14-Si*")])
#Covariance Matrix
(Sigma_mg_si <- Sigma[c("12-Mg*", "14-Si*"), c("12-Mg*", "14-Si*")])
#Correlation Matrix
(rho_mg_si <- rho[c("12-Mg*", "14-Si*"), c("12-Mg*", "14-Si*")])

n <- 100
x <- seq(mu_mg_si[1]-4*sqrt(Sigma_mg_si[1,1]),
         mu_mg_si[1]+4*sqrt(Sigma_mg_si[1,1]), len=n)
y <- seq(mu_mg_si[2]-4*sqrt(Sigma_mg_si[2,2]), 
         mu_mg_si[2]+4*sqrt(Sigma_mg_si[2,2]), len=n)
xy <- expand.grid(x=x, y=y)

q.samp <- cbind(xy, prob=dmvnorm(xy, mean = mu_mg_si, sigma = Sigma_mg_si))

Bi_mg_si <- ggplot(q.samp, aes(x=y, y=x, z=prob)) + 
  geom_point(data=Table1_log, aes(y=`12-Mg*`, x=`14-Si*`, z=0)) +
  geom_contour(bins=5, col="red") +
  ylab("12-Mg*") + xlab("14-Si*") + ggtitle("Bivariate Scatterplot of 12-Mg* and 14-Si*", "With Two-Dimensional Density Contours") +
  coord_fixed(
    ylim = c(mu_mg_si[1]-4*sqrt(Sigma_mg_si[1,1]), mu_mg_si[1]+4*sqrt(Sigma_mg_si[1,1])),
    xlim = c(mu_mg_si[2]-4*sqrt(Sigma_mg_si[2,2]), mu_mg_si[2]+4*sqrt(Sigma_mg_si[2,2]))
  ) +
  theme_minimal()


#Means
(mu_mg_si2 <- mu[c("22-Ti", "17-Cl*")])
#Covariance Matrix
(Sigma_mg_si2 <- Sigma[c("22-Ti", "17-Cl*"), c("22-Ti", "17-Cl*")])
#Correlation Matrix
(rho_mg_si2 <- rho[c("22-Ti", "17-Cl*"), c("22-Ti", "17-Cl*")])


n <- 100
x <- seq(mu_mg_si2[1]-4*sqrt(Sigma_mg_si2[1,1]),
         mu_mg_si2[1]+4*sqrt(Sigma_mg_si2[1,1]), len=n)
y <- seq(mu_mg_si2[2]-4*sqrt(Sigma_mg_si2[2,2]), 
         mu_mg_si2[2]+4*sqrt(Sigma_mg_si2[2,2]), len=n)
xy <- expand.grid(x=x, y=y)

q.samp <- cbind(xy, prob=dmvnorm(xy, mean = mu_mg_si2, sigma = Sigma_mg_si2))

bi_ti_cl <- ggplot(q.samp, aes(x=y, y=x, z=prob)) + 
  geom_point(data=Table1_log, aes(y=`22-Ti`, x=`17-Cl*`, z=0)) +
  geom_contour(bins=5, col="red") +
  ylab("22-Ti") + xlab("17-Cl*") + ggtitle("Bivariate Scatterplot of 22-Ti and 17-Cl*", "With Two-Dimensional Density Contours") +
  coord_fixed(
    ylim = c(mu_mg_si2[1]-4*sqrt(Sigma_mg_si2[1,1]), mu_mg_si2[1]+4*sqrt(Sigma_mg_si2[1,1])),
    xlim = c(mu_mg_si2[2]-4*sqrt(Sigma_mg_si2[2,2]), mu_mg_si2[2]+4*sqrt(Sigma_mg_si2[2,2]))
  ) +
  theme_minimal()

PCA_log <- prcomp(Table1_log[,-1], 
                  center=TRUE, 
                  scale=FALSE)
plot(PCA_log, type="l")

PCA_variance <- data.frame(
  PC = paste0("PC", 1:length(PCA_log$sdev)),
  Eigenvalue = round(PCA_log$sdev^2, 3),
  `Proportion of Variance (%)` =
    round(100 * PCA_log$sdev^2 / sum(PCA_log$sdev^2), 2),
  `Cumulative Variance (%)` =
    round(100 * cumsum(PCA_log$sdev^2 / sum(PCA_log$sdev^2)), 2)
)

PCA_table <- knitr::kable(
  PCA_variance,
  caption = "Variance explained by each principal component"
)

pca_scores <- as.data.frame(PCA_log$x[, 1:2])
pca_scores$Sample <- Table1_log[[1]]

pca_scores$Group <- case_when(
  grepl("^[Cc]ec", pca_scores$Sample)               ~ "CEC",
  grepl("^(CA|Ca|TAM|TAJ|TAC)", pca_scores$Sample)  ~ "Prehispanic",
  grepl("^(Jiu|Tot|Tez|Cua)", pca_scores$Sample)    ~ "Convents"
)

biplot <- ggbiplot::ggbiplot(PCA_log, obs.scale = 1, var.scale = 1,
                   groups = pca_scores$Group,
                   ellipse = TRUE,
                   labels = pca_scores$Sample) +
  labs(
    title   = "PCA Biplot",
    x = "PC1",
    y = "PC2")+
  theme_minimal()

Fig2_repdroduced <- ggplot(pca_scores, aes(x = PC2, y = PC1, colour = Group, label = Sample)) +
  geom_point(size = 2.5) +
  geom_text(size = 2.2, vjust = -0.7, hjust = 0.5) +
  geom_segment(
    x = -1.1, y = 1.9,
    xend = -0.65, yend = -2,
    inherit.aes = FALSE,
    colour = "black",
    linetype = "dashed",
    linewidth = 0.7
  ) +
  geom_segment(
    x = -1.1, y = 0.8,
    xend = 2.2, yend = -0.1,
    inherit.aes = FALSE,
    colour = "black",
    linetype = "dashed",
    linewidth = 0.7
  ) +
  scale_colour_manual(values = c(
    "CEC"        = "#E41A1C",
    "Prehispanic" = "#377EB8",
    "Convents"   = "#4DAF4A"
  )) +
  labs(
    title   = "Reproduction of Figure 2",
    subtitle = "PC2 vs PC1 coloured by historical group",
    x = "PC 2",
    y = "PC 1",
    colour = "Group"
  ) +
  theme_bw()