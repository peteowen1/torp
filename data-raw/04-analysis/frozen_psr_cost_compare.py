"""Compare the two arms of frozen_psr_cost.R: same backfill, different stat ratings.

Paired by match, so the gap is the PSR freeze alone. Reports margin MAE
(points, lower is better), Brier score (lower is better), log-loss (lower is
better) and winners picked, each with n and a paired bootstrap 95% interval
on the frozen-minus-fixed difference.
Usage: python -I data-raw/04-analysis/frozen_psr_cost_compare.py
"""
import glob
import numpy as np
import pyarrow.parquet as pq

ROOT = "data-raw/outputs/frozen-psr-cost"


def load(arm):
    rows = {}
    for f in sorted(glob.glob(f"{ROOT}/{arm}/_profile/2026/round_*.parquet")):
        t = pq.read_table(f).to_pydict()
        for i, mid in enumerate(t["match_id"]):
            rows[mid] = {k: t[k][i] for k in ("week", "pred_margin", "pred_win", "margin", "psr_diff")}
    return rows


fz, fx = load("frozen"), load("fixed")
ids = sorted(set(fz) & set(fx))
ids = [m for m in ids if fz[m]["margin"] is not None and fx[m]["margin"] is not None]
print(f"rounds: frozen {len({r['week'] for r in fz.values()})}, fixed {len({r['week'] for r in fx.values()})}; "
      f"matches in both with a result: {len(ids)}")
assert len(ids) > 100, "too few paired matches"
y = np.array([fz[m]["margin"] for m in ids], float)
assert np.allclose(y, [fx[m]["margin"] for m in ids]), "arms disagree on the actual margins"
win = (y > 0).astype(float)
keep = y != 0                      # a draw has no winner; drop it from win metrics
out = {}
for arm, d in (("frozen", fz), ("fixed", fx)):
    pm = np.array([d[m]["pred_margin"] for m in ids], float)
    pw = np.clip(np.array([d[m]["pred_win"] for m in ids], float), 1e-6, 1 - 1e-6)
    out[arm] = dict(ae=np.abs(pm - y), br=(pw - win) ** 2,
                    ll=-(win * np.log(pw) + (1 - win) * np.log(1 - pw)),
                    hit=((pw > 0.5) == (win == 1)).astype(float))
psr_gap = np.abs(np.array([fz[m]["psr_diff"] for m in ids], float) - np.array([fx[m]["psr_diff"] for m in ids], float))
print(f"mean |change in team PSR difference| between arms: {psr_gap.mean():.2f} (n {len(ids)})")

rng = np.random.default_rng(1)
B = 4000
idx = rng.integers(0, len(ids), size=(B, len(ids)))
print("\nmetric (lower is better unless noted) | frozen | fixed | frozen - fixed [95% CI] | n")
for key, label, mask in (("ae", "margin MAE, points", None), ("br", "Brier", keep),
                         ("ll", "log-loss", keep), ("hit", "winners picked (higher is better)", keep)):
    a, b = out["frozen"][key], out["fixed"][key]
    if mask is not None:
        a, b = a[mask], b[mask]
        ii = rng.integers(0, len(a), size=(B, len(a)))
    else:
        ii = idx
    d = a - b
    boot = d[ii].mean(axis=1)
    lo, hi = np.percentile(boot, [2.5, 97.5])
    if key == "hit":
        print(f"{label} | {int(a.sum())}/{len(a)} | {int(b.sum())}/{len(b)} | {int(d.sum()):+d} games [{lo*len(a):+.1f}, {hi*len(a):+.1f}] | {len(a)}")
    else:
        print(f"{label} | {a.mean():.4f} | {b.mean():.4f} | {d.mean():+.4f} [{lo:+.4f}, {hi:+.4f}] | {len(a)}")
