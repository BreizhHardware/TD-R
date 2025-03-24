import pandas as pd
import scipy.stats as stats
import matplotlib.pyplot as plt
import seaborn as sns

# Charger les données
file_path = "Porjet_JO.xlsx"
data_medailles = pd.read_excel(file_path, sheet_name="Travail_medailles")

# Extraction des âges des médaillés
medailles_age = data_medailles["Age"].dropna()

# Visualisation de la distribution de l'âge des médaillés
plt.figure(figsize=(10, 5))
sns.histplot(medailles_age, kde=True, color="red", bins=20, alpha=0.7)
plt.title("Distribution de l'âge des médaillés")
plt.xlabel("Âge")
plt.ylabel("Fréquence")
plt.show()

# Test de Student (T-test) -> Comparaison avec une moyenne de référence (ex: 25 ans)
t_stat, t_pval = stats.ttest_1samp(medailles_age, 25)

# Test du Khi² (Chi²-test) -> Comparaison des âges en classes d'âge
age_bins = [0, 18, 25, 30, 35, 40, 100]  # Catégories d'âge
age_groups = pd.cut(medailles_age, bins=age_bins).value_counts()
chi2_stat, chi2_pval = stats.chisquare(f_obs=age_groups)

# Test de Kolmogorov-Smirnov (KS-test) -> Comparaison avec une distribution normale théorique
ks_stat, ks_pval = stats.kstest(medailles_age, 'norm', args=(medailles_age.mean(), medailles_age.std()))

# Affichage des résultats
print(f"Test de Student : T-stat={t_stat:.3f}, p-value={t_pval:.5f}")
print(f"Test du Khi² : Chi²-stat={chi2_stat:.3f}, p-value={chi2_pval:.5f}")
print(f"Test de Kolmogorov-Smirnov : KS-stat={ks_stat:.3f}, p-value={ks_pval:.5f}")

# Interprétation des résultats
def interpret_pvalue(pval, alpha=0.05):
    return "Différence significative" if pval < alpha else "Pas de différence significative"

print("\nInterprétation :")
print(f"Test de Student : {interpret_pvalue(t_pval)}")
print(f"Test du Khi² : {interpret_pvalue(chi2_pval)}")
print(f"Test de KS : {interpret_pvalue(ks_pval)}")
