#!/usr/bin/env bash
# Claude Code statusline.
#
# Reads the Status hook JSON on stdin and prints two lines:
#   line 1 — where you are:   project[/subdir]  ·  branch*
#   line 2 — what's running:  Model · effort   ▓▓▓░░░░ 34%   $1.24  +156/-23
#
# Schema verified against claude 2.1.220. Fields that the payload may omit
# (effort on models without it, worktree, rate_limits) are handled as optional.
# Run with --debug to dump the raw payload instead of rendering.

set -uo pipefail

# The payload uses '.' as the decimal separator; under pl_PL printf expects ','
# and mangles every float it touches. Force C for all numeric formatting.
export LC_ALL=C

input=$(cat)

if [ "${1:-}" = "--debug" ]; then
  printf '%s\n' "$input" | jq .
  exit 0
fi

j() { printf '%s' "$input" | jq -r "$1 // empty" 2>/dev/null; }

# ---- colors -------------------------------------------------------------
esc=$'\033'
R="${esc}[0m"; B="${esc}[1m"; D="${esc}[2m"
RED="${esc}[31m"; GRN="${esc}[32m"; YEL="${esc}[33m"
BLU="${esc}[34m"; MAG="${esc}[35m"; CYN="${esc}[36m"
SEP="${D}·${R}"

# ---- where we are -------------------------------------------------------
cur_dir=$(j '.workspace.current_dir'); [ -z "$cur_dir" ] && cur_dir=$(j '.cwd')
[ -z "$cur_dir" ] && cur_dir="$PWD"
proj_dir=$(j '.workspace.project_dir'); [ -z "$proj_dir" ] && proj_dir="$cur_dir"

proj=$(basename "$proj_dir")
sub=""
case "$cur_dir" in
  "$proj_dir")   ;;
  "$proj_dir"/*) sub="/${cur_dir#"$proj_dir"/}" ;;
  *)             proj=$(basename "$cur_dir") ;;
esac

loc="${B}${CYN}${proj}${R}"
[ -n "$sub" ] && loc+="${D}${sub}${R}"

# A linked worktree is worth flagging — it's easy to forget which one you're in.
wt=$(j '.worktree.name'); [ -z "$wt" ] && wt=$(j '.workspace.git_worktree')
[ -n "$wt" ] && loc+="  ${MAG}⑂ $(basename "$wt")${R}"

# ---- git ----------------------------------------------------------------
git_part=""
if branch=$(git -C "$cur_dir" rev-parse --abbrev-ref HEAD 2>/dev/null); then
  [ "$branch" = "HEAD" ] && branch=$(git -C "$cur_dir" rev-parse --short HEAD 2>/dev/null)
  # head -1 short-circuits the scan as soon as anything dirty turns up.
  if [ -n "$(git -C "$cur_dir" status --porcelain 2>/dev/null | head -1)" ]; then
    git_part="${YEL}${branch}*${R}"
  else
    git_part="${GRN}${branch}${R}"
  fi
fi

line1="  ${loc}"
[ -n "$git_part" ] && line1+="  ${SEP}  ${git_part}"

# ---- model / effort -----------------------------------------------------
model=$(j '.model.display_name'); [ -z "$model" ] && model=$(j '.model.id')
[ -z "$model" ] && model="?"
line2="  ${B}${BLU}${model}${R}"

effort=$(j '.effort.level')
[ -n "$effort" ] && line2+=" ${D}·${R} ${MAG}${effort}${R}"

# Fast mode changes how the model responds, so surface it when it's on.
[ "$(j '.fast_mode')" = "true" ] && line2+=" ${YEL}⚡${R}"

# ---- context window -----------------------------------------------------
pct=$(j '.context_window.used_percentage')
if [ -n "$pct" ]; then
  p=$(printf '%.0f' "$pct" 2>/dev/null || echo 0)
  [ "$p" -lt 0 ] && p=0; [ "$p" -gt 100 ] && p=100
  width=7
  filled=$(( (p * width + 50) / 100 ))
  bar=""
  for ((i = 0; i < width; i++)); do
    if [ "$i" -lt "$filled" ]; then bar+="▓"; else bar+="░"; fi
  done
  if   [ "$p" -ge 80 ]; then c="$RED"
  elif [ "$p" -ge 50 ]; then c="$YEL"
  else                       c="$GRN"; fi
  line2+="   ${c}${bar}${R} ${c}${p}%${R}${D} ctx${R}"
fi

# ---- cost + lines changed ----------------------------------------------
cost=$(j '.cost.total_cost_usd')
if [ -n "$cost" ]; then
  line2+="   ${D}\$$(printf '%.2f' "$cost" 2>/dev/null || echo "$cost")${R}"
fi

add=$(j '.cost.total_lines_added'); rem=$(j '.cost.total_lines_removed')
add=${add:-0}; rem=${rem:-0}
if [ "$add" -ne 0 ] 2>/dev/null || [ "$rem" -ne 0 ] 2>/dev/null; then
  line2+="  ${GRN}+${add}${R}${D}/${R}${RED}-${rem}${R}"
fi

printf '%s\n%s\n' "$line1" "$line2"
