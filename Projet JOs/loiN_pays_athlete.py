import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from scipy.stats import ttest_ind, chi2_contingency, ks_2samp
from sklearn.linear_model import LinearRegression

# Charger les données
data_athletes = pd.read_excel("Porjet_JO.xlsx", sheet_name="Travail_athletes")
data_medailles = pd.read_excel("Porjet_JO.xlsx", sheet_name="Travail_medailles")

# Calculer le nombre d'athlètes et de médailles par nationalité
athletes_par_pays = data_athletes["Nationality"].value_counts().reset_index()
athletes_par_pays.columns = ["Nationality", "Nb_Athletes"]

medailles_par_pays = data_medailles["Country_code"].value_counts().reset_index()
medailles_par_pays.columns = ["Nationality", "Nb_Medailles"]

# Fusionner les données
resultats = pd.merge(athletes_par_pays, medailles_par_pays, on="Nationality", how="outer").fillna(0)

# Régression linéaire
X = resultats[["Nb_Athletes"]]
y = resultats["Nb_Medailles"]
reg = LinearRegression().fit(X, y)

# Visualisation
plt.figure(figsize=(10, 6))
sns.scatterplot(x="Nb_Athletes", y="Nb_Medailles", data=resultats, color="blue")
plt.plot(resultats["Nb_Athletes"], reg.predict(X), color="red")
plt.xlabel("Nombre d'athlètes")
plt.ylabel("Nombre de médailles")
plt.title("Relation entre le nombre d'athlètes et le nombre de médailles")
plt.show()

# Test de Student
t_stat, p_value_t = ttest_ind(resultats["Nb_Athletes"], resultats["Nb_Medailles"], equal_var=False)

# Test du Khi²
table_contingence = pd.crosstab(resultats["Nb_Athletes"], resultats["Nb_Medailles"])
chi2_stat, p_value_chi, _, _ = chi2_contingency(table_contingence)

# Test de Kolmogorov-Smirnov
ks_stat, p_value_ks = ks_2samp(resultats["Nb_Athletes"], resultats["Nb_Medailles"])


# Fonction d'interprétation
def interpret_pvalue(pval, alpha=0.05):
    return "Différence significative" if pval < alpha else "Pas de différence significative"


# Résultats des tests
print(f"Test de Student: Stat={t_stat:.3f}, p-value={p_value_t:.5f} → {interpret_pvalue(p_value_t)}")
print(f"Test du Khi²: Stat={chi2_stat:.3f}, p-value={p_value_chi:.5f} → {interpret_pvalue(p_value_chi)}")
print(f"Test de Kolmogorov-Smirnov: Stat={ks_stat:.3f}, p-value={p_value_ks:.5f} → {interpret_pvalue(p_value_ks)}")
