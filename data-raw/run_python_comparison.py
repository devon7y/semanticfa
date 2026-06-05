import numpy as np, sys, json, os
sys.path.insert(0, '/Users/devon7y/VS_Code/LLM_Factor_Analysis')
os.chdir('/Users/devon7y/VS_Code/LLM_Factor_Analysis')

data = np.load('semanticfa/data-raw/big5_sbert.npz')
embeddings = data['embeddings'].astype(np.float64)
codes = list(data['codes'])
factors = list(data['factors'])
scoring = list(data['scoring'].astype(int))

from qwen3_efa_v2 import (
    build_similarity_matrix, compute_embedding_parallel_analysis,
    compute_kmo_from_corr_matrix, compute_tefi, compute_rmsr_and_caf,
    compute_daal, assign_items_to_extracted_factors, compute_nmi, compute_ari
)
from factor_analyzer import FactorAnalyzer
import pandas as pd

sim_matrix, emb_ar = build_similarity_matrix(embeddings, scoring, mode='atomic_reversed')
print(f"SIM shape: {sim_matrix.shape}")
print(f"SIM off-diag range: [{sim_matrix[np.triu_indices(50, k=1)].min():.6f}, {sim_matrix[np.triu_indices(50, k=1)].max():.6f}]")
print(f"SIM off-diag mean: {sim_matrix[np.triu_indices(50, k=1)].mean():.6f}")

n_factors_pa, obs_eigs, pct_eigs = compute_embedding_parallel_analysis(
    sim_matrix, emb_ar, n_iter=100, percentile=95, random_state=42)
print(f"\nPA suggested: {n_factors_pa}")
print(f"Eigenvalues[0:10]: {np.round(obs_eigs[:10], 4).tolist()}")

fa = FactorAnalyzer(n_factors=5, rotation='oblimin', method='minres',
                     is_corr_matrix=True, rotation_kwargs={'normalize': True})
fa.fit(sim_matrix)
loadings = fa.loadings_
communalities = fa.get_communalities()
variance = fa.get_factor_variance()
factor_names = [f"Factor{i+1}" for i in range(5)]
loadings_df = pd.DataFrame(loadings, index=codes, columns=factor_names)

print(f"\nEFA cumulative var: {variance[2][-1]:.4f}")
print(f"Loadings (first 10):")
print(loadings_df.head(10).round(4).to_string())

kmo_per, kmo_total = compute_kmo_from_corr_matrix(sim_matrix)
tefi = compute_tefi(sim_matrix)
rmsr, caf, _ = compute_rmsr_and_caf(sim_matrix, fa)
print(f"\nKMO: {kmo_total:.6f}")
print(f"TEFI: {tefi:.6f}")
print(f"RMSR: {rmsr:.6f}")
print(f"CAF: {caf:.6f}")
print(f"Communalities[0:10]: {np.round(communalities[:10], 4).tolist()}")

daal = compute_daal(loadings_df, factors)
print(f"\nDAAL:")
print(daal.round(3).to_string())

phi = fa.phi_ if hasattr(fa, 'phi_') and fa.phi_ is not None else np.eye(5)
print(f"\nPhi:")
print(pd.DataFrame(phi, index=factor_names, columns=factor_names).round(4).to_string())

assigned = assign_items_to_extracted_factors(loadings_df)
extracted_labels = [assigned[c] for c in codes]
nmi_val = compute_nmi(extracted_labels, factors)
ari_val = compute_ari(extracted_labels, factors)
print(f"\nNMI: {nmi_val:.6f}")
print(f"ARI: {ari_val:.6f}")

results = {
    "sim_corner_5x5": sim_matrix[:5, :5].tolist(),
    "loadings_10x5": loadings[:10, :].tolist(),
    "communalities": communalities.tolist(),
    "eigenvalues_10": obs_eigs[:10].tolist(),
    "pa_percentiles_10": pct_eigs[:10].tolist(),
    "n_factors_pa": int(n_factors_pa),
    "kmo_total": float(kmo_total),
    "tefi": float(tefi),
    "rmsr": float(rmsr),
    "caf": float(caf),
    "nmi": float(nmi_val),
    "ari": float(ari_val),
    "phi": phi.tolist(),
    "variance_cumulative": float(variance[2][-1]),
    "daal": daal.values.tolist(),
    "daal_rows": list(daal.index),
    "daal_cols": list(daal.columns),
}
with open("semanticfa/data-raw/python_results.json", "w") as f:
    json.dump(results, f, indent=2)
print("\nSaved python_results.json")
