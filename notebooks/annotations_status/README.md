# Annotation status

Class balance figures from BIIGLE *CSV image annotation reports*, to follow annotation progress and
decide when there is enough data to train a model.

1. Download the *CSV image annotation report* of each volume from BIIGLE and put them in `reports/csv/`
   (one CSV or zip per volume).
2. Set `EXCLUDED_USERS` in the first cell if some annotators should be left out.
3. Run all cells. Figures are saved to `outputs/<date>/`:
   annotations per class, annotations per volume, and the top 40 classes per volume.

`reports/` and `outputs/` are git-ignored, so no data is committed.
