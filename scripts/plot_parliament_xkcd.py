#!/usr/bin/env python3
"""Australian Parliament, xkcd-1127 style.

A replication of xkcd #1127 "Congress" (https://xkcd.com/1127/) for the
Parliament of Australia: every member of the House of Representatives and
the Senate since Federation, drawn as party bands over time. Parties are
not subdivided by ideology (yet) — each band is simply the number of
members of that party grouping after each election.

Data: inputs/parliament/house_composition.csv  (seats won at each election)
      inputs/parliament/senate_composition.csv (full-Senate composition once
      the senators elected at that election took their seats)

Output: docs/aus_parliament_xkcd.png

The hand-drawn "xkcd Script" font is optional; if fonts/xkcd-script.ttf is
missing the script falls back to matplotlib's default font (the sketchy
line style still applies). To fetch it (CC BY-NC 3.0, not committed here):
  curl -sSO https://registry.npmjs.org/xkcd-font/-/xkcd-font-1.0.1.tgz
  tar xzf xkcd-font-1.0.1.tgz package/static/xkcd-script.woff
  python3 -c "from fontTools.ttLib import TTFont; f = TTFont('package/static/xkcd-script.woff'); f.flavor = None; f.save('fonts/xkcd-script.ttf')"
"""

import os
from datetime import date

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib import font_manager
import matplotlib.dates as mdates
import matplotlib.patheffects as pe
import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
DATA = os.path.join(ROOT, "inputs", "parliament")
OUT = os.path.join(ROOT, "docs")

# ----------------------------------------------------------------------------
# Election dates and when each composition takes effect
# ----------------------------------------------------------------------------

ELECTION_DATES = {
    1901: date(1901, 3, 30), 1903: date(1903, 12, 16), 1906: date(1906, 12, 12),
    1910: date(1910, 4, 13), 1913: date(1913, 5, 31), 1914: date(1914, 9, 5),
    1917: date(1917, 5, 5), 1919: date(1919, 12, 13), 1922: date(1922, 12, 16),
    1925: date(1925, 11, 14), 1928: date(1928, 11, 17), 1929: date(1929, 10, 12),
    1931: date(1931, 12, 19), 1934: date(1934, 9, 15), 1937: date(1937, 10, 23),
    1940: date(1940, 9, 21), 1943: date(1943, 8, 21), 1946: date(1946, 9, 28),
    1949: date(1949, 12, 10), 1951: date(1951, 4, 28), 1953: date(1953, 5, 9),
    1954: date(1954, 5, 29), 1955: date(1955, 12, 10), 1958: date(1958, 11, 22),
    1961: date(1961, 12, 9), 1963: date(1963, 11, 30), 1964: date(1964, 12, 5),
    1966: date(1966, 11, 26), 1967: date(1967, 11, 25), 1969: date(1969, 10, 25),
    1970: date(1970, 11, 21), 1972: date(1972, 12, 2), 1974: date(1974, 5, 18),
    1975: date(1975, 12, 13), 1977: date(1977, 12, 10), 1980: date(1980, 10, 18),
    1983: date(1983, 3, 5), 1984: date(1984, 12, 1), 1987: date(1987, 7, 11),
    1990: date(1990, 3, 24), 1993: date(1993, 3, 13), 1996: date(1996, 3, 2),
    1998: date(1998, 10, 3), 2001: date(2001, 11, 10), 2004: date(2004, 10, 9),
    2007: date(2007, 11, 24), 2010: date(2010, 8, 21), 2013: date(2013, 9, 7),
    2016: date(2016, 7, 2), 2019: date(2019, 5, 18), 2022: date(2022, 5, 21),
    2025: date(2025, 5, 3),
}

# Double dissolutions: the whole Senate changes at the election itself.
DOUBLE_DISSOLUTIONS = {1914, 1951, 1974, 1975, 1983, 1987, 2016}

END_OF_DATA = date(2026, 8, 1)


def senate_effective_date(year):
    """When the Senate elected in `year` actually looked like the data row.

    Half-Senate winners take their seats on the following 1 July (from 1907);
    before that, terms began on 1 January; double dissolutions immediately.
    """
    d = ELECTION_DATES[year]
    if year == 1901 or year in DOUBLE_DISSOLUTIONS:
        return d
    if year < 1907:
        return date(d.year + 1, 1, 1)
    return date(d.year, 7, 1) if d.month < 7 else date(d.year + 1, 7, 1)


# ----------------------------------------------------------------------------
# Party bands, bottom (left of politics) to top (right), xkcd-1127 style
# ----------------------------------------------------------------------------

# Palette validated with the dataviz six-checks validator (light surface):
# CVD-safe adjacencies; the grey "Other" band is the designated fold-in
# neutral (always direct-labelled, bands separated by black outlines).
BANDS = [  # (key, colour, label)
    ("GRN", "#256B31", "Greens"),
    ("ALP", "#E06A5A", "Labor"),
    ("ALPX", "#9C3A50", "Labor splinters"),
    ("DEM", "#E0862C", "Democrats"),
    ("OTH", "#8E979E", "Other / Independent"),
    ("NAT", "#C7940E", "Country / National"),
    ("LIB", "#3A66A8", "Liberal lineage"),
    ("FT", "#74A2E8", "Free Trade"),
]

PARTY_TO_BAND = {
    "Greens": "GRN",
    "Labour": "ALP", "Labor": "ALP",
    # Labor splinter parties (still party-level, not ideology)
    "Lang Labor": "ALPX", "Non-Communist Labor": "ALPX",
    "Anti-Communist Labor": "ALPX", "DLP": "ALPX",
    "Democrats": "DEM",
    # Country/National lineage, incl. its Queensland/NT/WA variants
    "Country": "NAT", "National Country": "NAT", "National": "NAT",
    "CLP": "NAT", "WA National": "NAT", "Country National": "NAT",
    "Queensland Country": "NAT", "Independent Country": "NAT",
    "Country Progressive": "NAT",
    # The main non-Labor lineage: Protectionist -> Commonwealth Liberal ->
    # Nationalist -> UAP -> Liberal. Queensland LNP members are folded in
    # here (most sit in the Liberal party room); the NT CLP sits with the
    # Nationals. Palmer's 2013+ "United Australia Party" is NOT this
    # lineage — it appears in the data as "UAP (Palmer)" and falls to OTH.
    "Protectionist": "LIB", "Liberal (Commonwealth)": "LIB",
    "Nationalist": "LIB", "United Australia": "LIB", "Liberal": "LIB",
    "Liberal National": "LIB",
    # The 1931 SA Emergency Committee and the 1934 SA Liberal and Country
    # League members sat with the UAP after those elections
    "Emergency Committee": "LIB", "Liberal and Country League": "LIB",
    "Free Trade": "FT", "Anti-Socialist": "FT",
    "Greens WA": "GRN",
}
OTHER_BAND = "OTH"  # everything else: independents and minor parties


def load_chamber(csv_name, effective_date_fn):
    df = pd.read_csv(os.path.join(DATA, csv_name))
    df["band"] = df["party"].map(PARTY_TO_BAND).fillna(OTHER_BAND)
    wide = (
        df.groupby(["year", "band"])["seats"].sum().unstack(fill_value=0)
        .reindex(columns=[b[0] for b in BANDS], fill_value=0)
        .sort_index()
    )
    wide.index = [effective_date_fn(y) for y in wide.index]
    return wide


RAMP_DAYS = 400  # how long each election's flows take to play out


def smoothstep(t):
    return 3 * t ** 2 - 2 * t ** 3


def layout_row(row, keys):
    """Median-centred (bottom, top) for each band in one composition."""
    total = sum(row[k] for k in keys)
    y, L = -total / 2.0, {}
    for k in keys:
        L[k] = (y, y + row[k])
        y += row[k]
    return L


def faces_up(lo, hi):
    """Deltas ride the band edge facing the median seat, where the
    marginal members sit."""
    return (lo + hi) / 2.0 < 0


# ----------------------------------------------------------------------------
# Drawing
# ----------------------------------------------------------------------------

def draw_chamber(ax, wide, title):
    """Median-centred braided-stream chart, xkcd-1127 style.

    Between elections each band holds steady. At an election, the members
    who stay put form a "core" that slides to its new position; seats a
    band LOSES peel off its median-facing edge as a stream that tapers
    away to nothing (members leaving parliament), and seats it GAINS flow
    in as a stream that fades in from nothing and joins the band (new
    members arriving) — so chamber enlargements literally pour new
    tributaries into the river.
    """
    keys = [b[0] for b in BANDS]
    n = len(wide)
    dnum = [mdates.date2num(d) for d in wide.index] + [mdates.date2num(END_OF_DATA)]
    layouts = [layout_row(wide.iloc[i], keys) for i in range(n)]
    wedges = []  # (x0, x1, (lo, hi), colour, "in"/"out")

    for key, colour, _label in BANDS:
        xs, bo, to = [], [], []
        for i in range(n):
            bA, tA = layouts[i][key]
            x_start, x_next = dnum[i], dnum[i + 1]
            ramp = min(RAMP_DAYS, 0.55 * (x_next - x_start)) if i < n - 1 else 0
            w0 = x_next - ramp
            xs += [x_start, w0]
            bo += [bA, bA]
            to += [tA, tA]
            if not ramp:
                continue
            bB, tB = layouts[i + 1][key]
            A, B = tA - bA, tB - bB
            m = min(A, B)
            if faces_up(bA, tA):
                cA, sliceA = (bA, bA + m), (bA + m, tA)
            else:
                cA, sliceA = (tA - m, tA), (bA, tA - m)
            if faces_up(bB, tB):
                cB, sliceB = (bB, bB + m), (bB + m, tB)
            else:
                cB, sliceB = (tB - m, tB), (bB, tB - m)
            # Losing bands shed the leavers in the first quarter of the
            # window, then the core of survivors slides into place; gaining
            # bands slide first while the newcomers' stream runs alongside,
            # then absorb it in the last quarter — every confluence is a
            # tangent curve, never a cliff.
            for j in range(1, 33):
                u = j / 33.0
                if A > B:
                    if u < 0.25:
                        f, a, c = smoothstep(u / 0.25), (bA, tA), cA
                    else:
                        f, a, c = smoothstep((u - 0.25) / 0.75), cA, (bB, tB)
                else:
                    if u < 0.75:
                        f, a, c = smoothstep(u / 0.75), (bA, tA), cB
                    else:
                        f, a, c = smoothstep((u - 0.75) / 0.25), cB, (bB, tB)
                xs.append(w0 + ramp * u)
                bo.append(a[0] + (c[0] - a[0]) * f)
                to.append(a[1] + (c[1] - a[1]) * f)
            if A > B:
                wedges.append((w0, ramp, sliceA, colour, "out",
                               1 if faces_up(bA, tA) else -1))
            elif B > A:
                wedges.append((w0, ramp, sliceB, colour, "in",
                               1 if faces_up(bB, tB) else -1))
        ax.fill_between(xs, bo, to, facecolor=colour,
                        edgecolor="black", linewidth=1.1, zorder=2)

    # Flows are thick solid strokes of the band's colour, one per change:
    # line weight grows with the number of seats moving, but stays a line.
    # Leavers emerge from the median-facing edge and curl toward the median
    # as the stroke dies; arrivals sweep in from the same side and plug
    # into the band.
    for x0, ramp, (lo, hi), colour, kind, sgn in wedges:
        mid, d = (lo + hi) / 2.0, hi - lo
        lw = min(9.0, 2.2 + 0.35 * d)
        drift = sgn * (d / 2.0 + 3.0)
        px, py = [], []
        for j in range(21):
            u = j / 20.0
            if kind == "out":
                px.append(x0 + 0.55 * ramp * u)
                py.append(mid + drift * smoothstep(u))
            else:
                px.append(x0 + ramp * (0.45 + 0.55 * u))
                py.append(mid + drift * (1 - smoothstep(u)))
        ax.plot(px, py, color=colour, lw=lw, solid_capstyle="round", zorder=3)

    ax.axhline(0, color="black", lw=1.8, ls=(0, (7, 5)), zorder=4)
    ax.set_title(title, fontsize=26, pad=12)
    ax.set_xlim(mdates.date2num(date(1900, 6, 1)), mdates.date2num(END_OF_DATA))
    half = max(wide[keys].sum(axis=1)) * 1.04 / 2.0
    ax.set_ylim(-half, half)
    ax.xaxis.set_major_locator(mdates.YearLocator(10))
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y"))
    ax.yaxis.set_major_formatter(plt.FuncFormatter(lambda v, _: f"{abs(v):.0f}"))
    ax.tick_params(labelsize=13)
    ax.set_ylabel("seats from the median", fontsize=15)
    for s in ("top", "right"):
        ax.spines[s].set_visible(False)


def band_mid(wide, key, year):
    """y midpoint of a band at a given year (median-centred coordinates)."""
    d = min(wide.index, key=lambda x: abs((x - date(year, 1, 1)).days))
    row = wide.loc[d]
    keys = [b[0] for b in BANDS]
    below = sum(row[k] for k in keys[: keys.index(key)])
    return below + row[key] / 2.0 - sum(row[k] for k in keys) / 2.0


def label(ax, wide, key, year, text, colour="white", dy=0, fontsize=14, **kw):
    """Band label: white ink with a soft dark outline (replaces the xkcd
    white-halo path effect, which makes white-on-colour text illegible)."""
    x = mdates.date2num(date(year, 1, 1))
    ax.text(x, band_mid(wide, key, year) + dy, text, fontsize=fontsize,
            ha="center", va="center", color=colour, zorder=5,
            path_effects=[pe.withStroke(linewidth=2.5, foreground=(0, 0, 0, 0.45))],
            **kw)


def annotate_arrow(ax, wide, key, year, text, xytext, fontsize=11):
    x = mdates.date2num(date(year, 1, 1))
    y = band_mid(wide, key, year)
    ax.annotate(text, xy=(x, y), xytext=xytext, fontsize=fontsize,
                ha="center", va="center", zorder=6,
                arrowprops=dict(arrowstyle="->", lw=1.2, color="black"))


def main():
    font_path = os.path.join(ROOT, "fonts", "xkcd-script.ttf")
    if os.path.exists(font_path):
        font_manager.fontManager.addfont(font_path)
        plt.rcParams["font.family"] = "xkcd Script"
        os.environ.setdefault("XKCD_FONT", "loaded")

    house = load_chamber("house_composition.csv", lambda y: ELECTION_DATES[y])
    senate = load_chamber("senate_composition.csv", senate_effective_date)

    with plt.xkcd(scale=1.0, length=120, randomness=1.6):
        if os.path.exists(font_path):
            plt.rcParams["font.family"] = "xkcd Script"
        fig, (axh, axs) = plt.subplots(
            2, 1, figsize=(22, 13), sharex=False,
            gridspec_kw=dict(height_ratios=[150, 88], hspace=0.32),
        )
        fig.patch.set_facecolor("white")

        draw_chamber(axh, house, "THE HOUSE OF REPRESENTATIVES")
        draw_chamber(axs, senate, "THE SENATE")

        # ------------------------------------------------------------------
        # House labels (band, year, text)
        # ------------------------------------------------------------------
        label(axh, house, "ALP", 1946, "LABOR", fontsize=20)
        label(axh, house, "ALP", 1990, "LABOR", fontsize=22)
        label(axh, house, "LIB", 1907, "PROTECTIONIST", fontsize=11)
        label(axh, house, "FT", 1905, "FREE TRADE /\nANTI-SOCIALIST", fontsize=10)
        label(axh, house, "LIB", 1913, "LIBERAL", fontsize=11)
        label(axh, house, "LIB", 1922, "NATIONALIST", fontsize=13)
        label(axh, house, "LIB", 1936, "U.A.P.", fontsize=13)
        label(axh, house, "LIB", 1965, "LIBERAL", fontsize=22)
        label(axh, house, "LIB", 2005, "LIBERAL", fontsize=22)
        label(axh, house, "NAT", 1960, "COUNTRY PARTY", fontsize=13)
        label(axh, house, "NAT", 2004, "NATIONALS", fontsize=12)
        axh.annotate("the Fusion, 1909:\nProtectionists + Free Traders merge",
                     xy=(mdates.date2num(date(1909, 9, 1)), 22.5),
                     xytext=(mdates.date2num(date(1906, 1, 1)), 55),
                     fontsize=11, ha="center", va="center", zorder=6,
                     arrowprops=dict(arrowstyle="->", lw=1.2, color="black"))
        axh.annotate("the great Labor split, 1916:\nHughes walks out over conscription,\n"
                     "takes his followers to the Nationalists",
                     xy=(mdates.date2num(date(1916, 11, 1)), -1.5),
                     xytext=(mdates.date2num(date(1916, 1, 1)), 68),
                     fontsize=11, ha="center", va="center", zorder=6,
                     arrowprops=dict(arrowstyle="->", lw=1.2, color="black"))
        annotate_arrow(axh, house, "NAT", 1921, "Country Party arrives, 1919",
                       (mdates.date2num(date(1929, 1, 1)), 53))
        annotate_arrow(axh, house, "ALPX", 1933, "Lang Labor\n(the Labor split of 1931)",
                       (mdates.date2num(date(1943, 1, 1)), 65))
        axh.annotate("Lyons walks out too,\n1931", zorder=6,
                     xy=(mdates.date2num(date(1931, 8, 1)), 5),
                     xytext=(mdates.date2num(date(1938, 6, 1)), 27),
                     fontsize=11, ha="center", va="center",
                     arrowprops=dict(arrowstyle="->", lw=1.2, color="black"))
        annotate_arrow(axh, house, "GRN", 2023, "Greens",
                       (mdates.date2num(date(2016, 1, 1)), -63))
        annotate_arrow(axh, house, "OTH", 2023, "the teal wave etc.",
                       (mdates.date2num(date(2013, 6, 1)), 43))
        axh.annotate("the median seat -\nmajority lives here",
                     xy=(mdates.date2num(date(1960, 1, 1)), 0),
                     xytext=(mdates.date2num(date(1960, 1, 1)), 14),
                     fontsize=11, ha="center", va="center", zorder=6,
                     arrowprops=dict(arrowstyle="->", lw=1.2, color="black"))
        axh.text(mdates.date2num(date(1949, 6, 1)), 68,
                 "house enlarged\n75 to 121 seats", fontsize=11, ha="center")
        axh.text(mdates.date2num(date(1980, 1, 1)), 76,
                 "enlarged again (148)", fontsize=10, ha="center")

        # ------------------------------------------------------------------
        # Senate labels
        # ------------------------------------------------------------------
        label(axs, senate, "ALP", 1947, "LABOR", fontsize=20)
        label(axs, senate, "ALP", 2000, "LABOR", fontsize=16)
        label(axs, senate, "LIB", 1905, "PROT.", fontsize=10)
        label(axs, senate, "FT", 1904, "FREE TRADE", fontsize=10)
        label(axs, senate, "LIB", 1926, "NATIONALIST", fontsize=12)
        label(axs, senate, "LIB", 1975, "LIBERAL", fontsize=18)
        label(axs, senate, "NAT", 1965, "COUNTRY", fontsize=10)
        label(axs, senate, "GRN", 2019, "GREENS", fontsize=11)
        annotate_arrow(axs, senate, "ALP", 1921, "after 1919, Labor held\nONE Senate seat of 36",
                       (mdates.date2num(date(1932, 6, 1)), -11.5))
        annotate_arrow(axs, senate, "ALPX", 1962, "D.L.P.\n(the Labor split of 1955)",
                       (mdates.date2num(date(1952, 1, 1)), 17))
        annotate_arrow(axs, senate, "DEM", 1990, "Australian Democrats",
                       (mdates.date2num(date(1983, 1, 1)), 23))
        annotate_arrow(axs, senate, "OTH", 2018, "One Nation, Xenophon,\nLambie, Palmer, ...",
                       (mdates.date2num(date(2007, 6, 1)), 28))
        axs.annotate("the median seat -\nthe balance of power",
                     xy=(mdates.date2num(date(1996, 1, 1)), 0),
                     xytext=(mdates.date2num(date(1996, 1, 1)), 16),
                     fontsize=11, ha="center", va="center", zorder=6,
                     arrowprops=dict(arrowstyle="->", lw=1.2, color="black"))
        axs.text(mdates.date2num(date(1949, 6, 1)), 36,
                 "senate enlarged\n36 to 60 seats", fontsize=11, ha="center")
        axs.text(mdates.date2num(date(1981, 1, 1)), 40,
                 "enlarged to 76", fontsize=10, ha="center")

        fig.suptitle("EVERY MEMBER OF THE AUSTRALIAN PARLIAMENT SINCE FEDERATION",
                     fontsize=34, y=0.995)
        fig.text(0.5, 0.952,
                 "party composition after each federal election, 1901-2025 "
                 "(parties not yet subdivided by ideology)",
                 fontsize=15, ha="center")
        fig.text(0.985, 0.006,
                 "style borrowed with admiration from xkcd.com/1127 | "
                 "data: parliamentary handbook & wikipedia | igross.github.io",
                 fontsize=10, ha="right", color="#555555")

        out = os.path.join(OUT, "aus_parliament_xkcd.png")
        fig.savefig(out, dpi=160, facecolor="white", bbox_inches="tight")
        print("wrote", out)


if __name__ == "__main__":
    main()
