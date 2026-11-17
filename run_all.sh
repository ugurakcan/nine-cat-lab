#!/usr/bin/env bash
# Runs the whole pipeline: download once, then steps 2-6 for every league size.
# Usage: ./run_all.sh            (downloads fresh data first)
#        SKIP_DOWNLOAD=1 ./run_all.sh   (reuses existing game logs)
set -euo pipefail

export NBA_DATA_DIR="${NBA_DATA_DIR:-$PWD/data}"
export NBA_SITE_DIR="${NBA_SITE_DIR:-$PWD/site}"
export NBA_CURRENT_SEASON="${NBA_CURRENT_SEASON:-2027}"

TEAMS="${TEAMS:-8 10 12 14 16}"
ROSTERS="${ROSTERS:-10 11 12 13 14 15 16}"

if [ "${SKIP_DOWNLOAD:-0}" != "1" ]; then
  echo "== Step 1: download game logs"
  Rscript R/01_data_download.R > /dev/null
fi

for t in $TEAMS; do
  for r in $ROSTERS; do
    echo "== League: $t teams x $r roster spots"
    export NBA_TEAMS="$t" NBA_ROSTER="$r"
    Rscript R/02_zscore_table.R      > /dev/null
    Rscript R/03_weekly_simulation.R > /dev/null
    Rscript R/04_player_value.R      > /dev/null
    Rscript R/05_marcel_projection.R > /dev/null
    Rscript R/06_export_site_data.R
  done
done

echo "Done. Site data is in $NBA_SITE_DIR/data"
