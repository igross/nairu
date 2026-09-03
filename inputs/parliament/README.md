# Party composition of the Australian Parliament, 1901–2025

Data behind `docs/aus_parliament_xkcd.png` (built by
`scripts/plot_parliament_xkcd.py`), a replication of xkcd #1127 "Congress"
for the Parliament of Australia.

## Files

- `house_composition.csv` — seats won by each party in the House of
  Representatives at every federal election, 1901–2025.
- `senate_composition.csv` — party composition of the **full Senate** after
  each Senate election (newly elected plus continuing senators), 1901–2025.
  "After" means once the winners took their seats: the following 1 July for
  half-Senate elections (1 January before 1907), immediately for the 1901
  election and double dissolutions (1914, 1951, 1974, 1975, 1983, 1987, 2016).
  1929 and 1954 had no Senate contest; 1953, 1964, 1967 and 1970 were
  Senate-only.

Schema: `year,party,seats`. Parties appear under the name they ran under at
that election (e.g. the main non-Labor party appears successively as
Protectionist, Liberal (Commonwealth), Nationalist, United Australia,
Liberal). Palmer's 2013+ United Australia Party is recorded as
"UAP (Palmer)" to keep it distinct from the 1931–44 UAP.

## Conventions and caveats

- Numbers were compiled from the Wikipedia pages for each election
  ("<year> Australian federal election", "Results of the … (Senate)",
  "Members of the Australian Senate, <term>"), cross-checked against
  australianpolitics.com and Parliamentary Library material. Every year sums
  to the chamber size at the time (House: 75 → 74 in 1934 → 121 in 1949 →
  122/124/125/127 → 148 in 1984 → 147–151 since; Senate: 36 → 60 in 1949 →
  64 in 1975 → 76 in 1985).
- Constituent parties are kept as reported: the Queensland LNP (from 2010)
  and NT Country Liberals are separate rows. For the chart, LNP is folded
  into the Liberal band and the CLP into the Country/National band.
- 2019 Senate: sources cleanly report only the combined Coalition total
  (35); the Liberal 30 / National 5 rows approximate the party-room split.
- House totals exclude the limited-voting-rights NT member (from 1922) and
  ACT member (from 1949) before those seats gained full votes; the sources'
  convention differs slightly across years (1946 includes the NT member
  among the 2 independents).
- Early-year party labels are fuzzy (aisle-crossing was common before the
  party system settled). Known classification variants: 1901 and 1903 House
  (Protectionist/Free Trade/Independent splits differ by a seat or two
  across sources), 1919 House (Labor 25 + Ind 2 vs Labor 26 + Ind 1),
  1931 House (UAP 34 + Emergency Committee 6 vs UAP 33 + EC 5 + Ind 3),
  1922 and 1940 Senate (one senator classified differently across sources).
- The 1931 SA Emergency Committee and 1934 SA Liberal and Country League
  members sat with the UAP; the chart folds them into the Liberal-lineage
  band.
