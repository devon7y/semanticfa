"""sem-k: calibrated semantic factor retention (Python side).

Single-file vendored copy of the validated feature-extraction pipeline
from the sem-k development repository (proj_embedding_retention,
src/{features,sfa_core,granularity,partition}.py), frozen at artifact
consensus_rule_v1.0. Function bodies are verbatim so that verdicts
reproduce the published planted-truth error rates bit-for-bit; do not
"clean up" numerical code paths here without re-running the parity and
determinism suites.

Entry point: predict(X, model_path, floor=None, seed=42) -> dict.
"""

from __future__ import annotations

import numpy as np

# ------------------------------------------------------------------ encoding

REGISTER_FLOOR = 0.4783  # Qwen3-Embedding-8B register-null calibration

KGRID = [2, 3, 4, 5, 6, 8, 10, 12, 14, 17, 20, 24, 28, 32]


def mean_centered_pearson_transform(embeddings):
    X = np.asarray(embeddings, dtype=np.float64)
    Xc = X - X.mean(axis=1, keepdims=True)
    norms = np.linalg.norm(Xc, axis=1, keepdims=True)
    norms[norms == 0] = 1.0
    return Xc / norms


def similarity(embeddings, encoding="mean_centered_pearson"):
    if encoding != "mean_centered_pearson":
        raise ValueError(encoding)
    T = mean_centered_pearson_transform(embeddings)
    S = T @ T.T
    np.fill_diagonal(S, 1.0)
    return (S + S.T) / 2


def eigvals_desc(S):
    return np.sort(np.linalg.eigvalsh(S))[::-1]


# ------------------------------------------------------- battery (verbatim)

def kaiser(eigs):
    return max(1, int(np.sum(eigs > 1)))


def _horn_count(obs, thr):
    below = np.where(obs <= thr)[0]
    k = len(obs) if below.size == 0 else int(below[0])
    return max(1, k)


def parallel_isotropic(eigs, n_items, embed_dim, n_iter=100, percentile=95,
                       seed=42):
    rng = np.random.default_rng(seed)
    null_eigs = np.empty((n_iter, n_items))
    for i in range(n_iter):
        R = rng.standard_normal((n_items, embed_dim))
        R /= np.linalg.norm(R, axis=1, keepdims=True)
        G = R @ R.T
        np.fill_diagonal(G, 1.0)
        null_eigs[i] = eigvals_desc(G)
    thr = np.percentile(null_eigs, percentile, axis=0)
    return _horn_count(eigs, thr)


def ekc(eigs, n):
    J = len(eigs)
    up = (1 + np.sqrt(J / n)) ** 2
    prior = np.concatenate([[0.0], np.cumsum(eigs)[:-1]])
    refs = np.maximum((J - prior) / (J - np.arange(1, J + 1) + 1) * up, 1.0)
    below = np.where(eigs <= refs)[0]
    k = J if below.size == 0 else int(below[0])
    return max(1, k)


def velicer_map(S, max_factors=None):
    J = S.shape[0]
    if max_factors is None:
        max_factors = J - 2
    max_factors = min(max_factors, J - 2)
    w, V = np.linalg.eigh(S)
    order = np.argsort(w)[::-1]
    w, V = w[order], V[:, order]

    map_vals = np.full(max_factors, np.nan)
    for m in range(1, max_factors + 1):
        lam = V[:, :m] * np.sqrt(np.maximum(w[:m], 0))
        resid = S - lam @ lam.T
        d = np.diag(resid).copy()
        if np.any(d < 1e-10):
            break
        Rp = resid / np.sqrt(np.outer(d, d))
        map_vals[m - 1] = (np.sum(Rp ** 2)
                           - np.sum(np.diag(Rp) ** 2)) / (J * (J - 1))
    valid = np.where(~np.isnan(map_vals))[0]
    if valid.size == 0:
        return 1
    return int(valid[np.argmin(map_vals[valid])] + 1)


# ------------------------------------------- partitioning (verbatim copies)

def varimax(L, gamma=1.0, max_iter=100, tol=1e-8):
    p, k = L.shape
    R = np.eye(k)
    d = 0
    for _ in range(max_iter):
        Lr = L @ R
        u, s, vt = np.linalg.svd(
            L.T @ (Lr ** 3 - (gamma / p) * Lr @ np.diag((Lr ** 2).sum(0))))
        R = u @ vt
        if s.sum() < d * (1 + tol):
            break
        d = s.sum()
    return L @ R


def fa_partition(S, k):
    w, V = np.linalg.eigh(S)
    idx = np.argsort(w)[::-1][:k]
    L = V[:, idx] * np.sqrt(np.maximum(w[idx], 0))
    if k > 1:
        L = varimax(L)
    return np.argmax(np.abs(L), axis=1)


def ari(a, b):
    a, b = np.asarray(a), np.asarray(b)
    n = len(a)
    ua, ub = np.unique(a), np.unique(b)
    M = np.array([[np.sum((a == x) & (b == y)) for y in ub] for x in ua],
                 dtype=float)
    comb = lambda x: x * (x - 1) / 2  # noqa: E731
    sij = comb(M).sum()
    sa, sb = comb(M.sum(1)).sum(), comb(M.sum(0)).sum()
    tot = comb(n)
    exp = sa * sb / tot
    mx = (sa + sb) / 2
    return float((sij - exp) / (mx - exp)) if mx != exp else 1.0


def _silhouette(S, labels):
    n = S.shape[0]
    D = 1.0 - S
    np.fill_diagonal(D, 0.0)
    uniq = np.unique(labels)
    if len(uniq) < 2:
        return 0.0
    sil = np.zeros(n)
    for i in range(n):
        own = labels == labels[i]
        own[i] = False
        a = D[i, own].mean() if own.any() else 0.0
        b = np.inf
        for c in uniq:
            if c == labels[i]:
                continue
            mask = labels == c
            if mask.any():
                b = min(b, D[i, mask].mean())
        denom = max(a, b)
        sil[i] = 0.0 if denom == 0 or not np.isfinite(b) else (b - a) / denom
    return float(sil.mean())


# ------------------------------------------------ features (verbatim logic)

def extract_features(embeddings, n_splits=10, seed=42):
    X = np.asarray(embeddings, dtype=np.float64)
    n, p = X.shape
    S = similarity(X)
    off = ~np.eye(n, dtype=bool)
    eigs = eigvals_desc(S)
    shares = eigs / eigs.sum()

    f = {
        "n_items": n,
        "log_dim": np.log(p),
        "mean_sim": S[off].mean(),
        "sd_sim": S[off].std(),
        "floor_offset": S[off].mean() - REGISTER_FLOOR,
        "lambda1_share": shares[0],
        "spectral_entropy": float(-(shares * np.log(shares + 1e-12)).sum()),
        "participation_ratio": float(1.0 / (shares ** 2).sum() / n),
    }
    for i in range(16):
        f[f"eig_share_{i+1}"] = shares[i] if i < n else 0.0
    tail = np.maximum(eigs[1:], 1e-9)
    for i in range(14):
        f[f"ratio_defl_{i+1}"] = tail[i] / tail[i + 1] \
            if i + 1 < len(tail) else 1.0

    rng = np.random.default_rng(seed)
    for k in KGRID:
        if k >= n - 1:
            f[f"sil_{k}"] = 0.0
            f[f"con_{k}"] = 0.0
            f[f"stab_{k}"] = 0.0
            continue
        lab = fa_partition(S, k)
        f[f"sil_{k}"] = _silhouette(S, lab)
        same = (lab[:, None] == lab[None, :]) & off
        f[f"con_{k}"] = (float(S[same].mean() - S[off & ~same].mean())
                         if same.any() and (off & ~same).any() else 0.0)
        st = 0.0
        for _ in range(n_splits):
            perm = rng.permutation(p)
            Sa = similarity(X[:, perm[:p // 2]])
            Sb = similarity(X[:, perm[p // 2:]])
            st += ari(fa_partition(Sa, k), fa_partition(Sb, k))
        f[f"stab_{k}"] = st / n_splits

    f["v_kaiser"] = kaiser(eigs)
    f["v_pa_iso"] = parallel_isotropic(eigs, n, p, n_iter=50, seed=seed)
    f["v_ekc"] = ekc(eigs, p)
    f["v_map"] = velicer_map(S)
    return f


# ------------------------------------------------------------------ predict

_MODEL_CACHE = {}


def predict(X, model_path, floor=None, seed=42):
    """sem-k verdict for one item set.

    X: raw or row-centered item embeddings (n_items x embed_dim); the
    mean-centered-Pearson transform applied internally is idempotent, so
    either form is valid. model_path: consensus_rule_v1.0.joblib. floor:
    override the register floor (encoder-specific calibration); None uses
    the artifact's training floor. Returns a plain dict for reticulate.
    """
    global REGISTER_FLOOR
    import joblib

    if model_path not in _MODEL_CACHE:
        _MODEL_CACHE[model_path] = joblib.load(model_path)
    art = _MODEL_CACHE[model_path]

    old_floor = REGISTER_FLOOR
    REGISTER_FLOOR = float(floor) if floor is not None else float(art["floor"])
    try:
        f = extract_features(np.asarray(X, dtype=np.float64), n_splits=5,
                             seed=int(seed))
    finally:
        used_floor, REGISTER_FLOOR = REGISTER_FLOOR, old_floor

    cols = art["features"]
    xv = np.array([[f[c] for c in cols]])
    c = float(art["clf"].predict(xv)[0])
    khat = int(c if c <= 8 else np.round(np.exp(art["logreg"].predict(xv)[0])))
    q = float(art.get("conformal_q90_log", np.nan))
    if np.isfinite(q):
        lo = int(max(1, np.floor(np.exp(np.log(max(khat, 1)) - q))))
        hi = int(np.ceil(np.exp(np.log(max(khat, 1)) + q)))
    else:
        lo = hi = -1
    return {
        "n_factors": int(max(1, khat)),
        "lo90": lo,
        "hi90": hi,
        "floor": float(used_floor),
        "battery": {"kaiser": int(f["v_kaiser"]), "pa_iso": int(f["v_pa_iso"]),
                    "ekc": int(f["v_ekc"]), "map": int(f["v_map"])},
        "encoder_trained": str(art.get("encoder", "8B")),
        "train_versions": str(art.get("train_versions", "v1-v4")),
    }
