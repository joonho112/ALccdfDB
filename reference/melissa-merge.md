# Melissa Module: Merge

Merge Melissa.com geocoding coordinates into program and subsidy
household data. Programs are matched by facility_address; households by
family_address. Addresses are normalised (uppercased, punctuation
removed, whitespace collapsed) before matching to handle formatting
differences between DHR and Melissa outputs.
