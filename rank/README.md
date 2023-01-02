# Ranking Dashboard and Algorithm

For Shiny App example, please click [here](https://kenkoonwong.shinyapps.io/opengme_rank/)

The automated ranking score ( $R_i$ ) ascribed to applicant $i$ is defined by:

$$R_i= \omega_1\frac{\bar{P_i}}{6} + \omega_2\frac{\bar{C_i}}{6} + \omega_3\frac{\bar{O_i}}{6} + \omega_4 E_i^{(I)} + \omega_5 E_i^{(II)} + \omega_6 F_i^{(6)} + \omega_7 F_i^{(7)} + \ldots + \omega_D F_i^{(D)}$$

where:

* $\bar{P_i}$ represents the mean professionalism score (across all evaluators) for applicant $i$ on a scale of 0 – 6
* $\bar{C_i}$ represents the mean communication skills score (across all evaluators) for applicant $i$ on a scale of 0 – 6
* $\bar{O_i}$ represents the mean overall impression score (across all evaluators) for applicant $i$ on a scale of 0 – 6
* $E_i^{(I)}$ represents the proxy step 1 exam score for applicant $i$, as defined below
* $E_i^{(II)}$ represents the proxy step 2 exam score for applicant $i$, as defined below
* $D\in N:D\geq5$ represents the total number of factors considered in the ranking score equation. For our demonstration, $D=5$
* $F_i^{(j)}  \forall j\in[6,D]$ represent the optionally added factors to the ranking model given that $D\geq6$
* $\omega_1,\omega_2,\omega_3,\omega_4,\omega_5$ designate the ascribed evaluation weights to the factors $\bar{P}$, $\bar{C}$, $\bar{O}$, $E^{(I)}$, and $E^{(II)}$ respectively. $\omega_j \forall j\in[6,D]$ designate the ascribed evaluation weights for $F^{(j)}  \forall j\in[6,D]$. These weights may be defined by the model user such that $\omega_j\in[0,1]  \forall j\in[1,D]$ and $$\sum_{j=1}^{D} \omega_j =1$$.

The proxy step 1 and step 2 exam scores are defined by:

$$
E_i^{(I)} =
\begin{cases} U_i^{(I)}, & \text{if}\ X_i^{(I)} \text{is unavailable}\\
X_i^{(I)}, & \text{if}\ U_i^{(I)} \text{is unavailable}\\
\max\left(U_i^{(I)},X_i^{(I)}\right), & \text{otherwise} \end{cases}
$$

and

$$
E_i^{(II)} =
\begin{cases} U_i^{(II)}, & \text{if}\ X_i^{(II)} \text{is unavailable}\\
X_i^{(II)}, & \text{if}\ U_i^{(II)} \text{is unavailable}\\
\max\left(U_i^{(II)},X_i^{(II)}\right), & \text{otherwise} \end{cases}
$$

where:
* $U_i^{(I)}$ represents the USMLE Step 1 score percentile (among the applicant pool) of applicant $i$
* $X_i^{(I)}$ represents the COMPLEX-USA Level 1 score percentile (among the applicant pool) of applicant $i$
* $U_i^{(II)}$ represents the USMLE Step 2 Clinical Skills score percentile (among the applicant pool) of applicant $i$
* $X_i^{(II)}$ represents the COMPLEX-USA Level 2 Cognitive Evaluation score percentile (among the applicant pool) of applicant $i$.
