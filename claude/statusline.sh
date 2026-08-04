#!/usr/bin/env bash
# Claude Code statusline.
#
# Reads the Status hook JSON on stdin and prints two lines, each split into a
# left half (who/where you are) and a right half (what it's costing you):
#
#   [personal]  project/sub  ·  branch* ↑2          my-session   PR #412 ✓
#   Opus 5 · high   ▓▓▓░░░░ 34% ctx        5h 42%  7d 61%   $1.24  +156/-23
#
# Schema verified against claude 2.1.221 by reading the payload builder in the
# binary — it emits more than the documented schema (fast_mode, cost.*,
# exceeds_200k_tokens, remote) and notably does NOT emit the permission mode.
# Everything except cwd/model/workspace is optional and handled as such.
# Run with --debug to dump the raw payload instead of rendering.

set -uo pipefail

# The payload uses '.' as the decimal separator; under pl_PL printf expects ','
# and mangles every float it touches. C.UTF-8 fixes that while still counting
# ${#str} in characters, which the right-alignment below depends on. If glibc
# lacks it we fall back to plain C: floats stay correct, padding drifts on the
# few non-ASCII glyphs.
export LC_ALL=C.UTF-8

input=$(cat)

if [ "${1:-}" = "--debug" ]; then
  printf '%s\n' "$input" | jq .
  exit 0
fi

# One jq for the whole payload: a jq per field cost ~250ms per redraw, which is
# a visible lag on a statusline. Fields come back \x1f-separated — a
# non-whitespace IFS so empty ones keep their slot instead of collapsing.
read -r -d '' _jq_fields <<'JQ'
[ .workspace.current_dir, .cwd, .workspace.project_dir,
  .worktree.name, .workspace.git_worktree,
  .model.display_name, .model.id, .effort.level, .fast_mode,
  .context_window.used_percentage,
  .cost.total_cost_usd, .cost.total_lines_added, .cost.total_lines_removed,
  .session_name, .pr.number, .pr.review_state, .remote.session_id,
  .rate_limits.five_hour.used_percentage, .rate_limits.five_hour.resets_at,
  .rate_limits.seven_day.used_percentage, .rate_limits.seven_day.resets_at
] | map(if . == null then "" else tostring end) | join("\u001f")
JQ

IFS=$'\x1f' read -r \
  p_cur_dir p_cwd p_proj_dir \
  p_wt_name p_git_worktree \
  p_model_name p_model_id p_effort p_fast_mode \
  p_ctx_pct \
  p_cost p_lines_added p_lines_removed \
  p_session_name p_pr_number p_pr_state p_remote_id \
  p_5h_pct p_5h_reset p_7d_pct p_7d_reset \
  <<<"$(printf '%s' "$input" | jq -r "$_jq_fields" 2>/dev/null)" || true

# ---- colors -------------------------------------------------------------
esc=$'\033'
R="${esc}[0m"; B="${esc}[1m"; D="${esc}[2m"
RED="${esc}[31m"; GRN="${esc}[32m"; YEL="${esc}[33m"
BLU="${esc}[34m"; MAG="${esc}[35m"; CYN="${esc}[36m"
SEP="${D}·${R}"

# ---- layout -------------------------------------------------------------
# Visible width: SGR sequences take no columns, so strip them before counting.
# extglob's +(...) is what keeps the match tight — a plain [0-9;]* glob is
# "one of those, then anything", which happily eats the whole line up to the
# last 'm'.
shopt -s extglob
vlen() {
  local s=${1//$esc"["+([0-9;])m/}
  # ⚡ is the one East-Asian-Wide glyph we emit: one character, two columns.
  local wide=${s//[!⚡]/}
  printf '%s' "$(( ${#s} + ${#wide} ))"
}

# Nothing in the payload carries the terminal width. COLUMNS is exported by
# some parents, /dev/tty works whenever we have a controlling terminal, and
# tput answers 80 even with no tty at all — so it goes last, as an
# underestimate that misplaces the right half rather than wrapping the line.
cols=""
case "${COLUMNS:-}" in ''|*[!0-9]*) ;; *) [ "$COLUMNS" -ge 40 ] && cols=$COLUMNS ;; esac
if [ -z "$cols" ]; then
  size=$(stty size 2>/dev/null </dev/tty)
  w=${size##* }
  case "$w" in ''|*[!0-9]*) ;; *) [ "$w" -ge 40 ] && cols=$w ;; esac
fi
if [ -z "$cols" ]; then
  w=$(tput cols 2>/dev/null)
  case "$w" in ''|*[!0-9]*) ;; *) [ "$w" -ge 40 ] && cols=$w ;; esac
fi

# Join the halves, pushing the right one to the margin when we know where the
# margin is. Two spaces is the honest fallback: never wrap on a bad guess.
join_halves() {
  local left=$1 right=$2 pad
  [ -z "$right" ] && { printf '%s' "$left"; return; }
  [ -z "$left" ] && { printf '%s' "$right"; return; }
  if [ -n "$cols" ]; then
    pad=$(( cols - 1 - $(vlen "$left") - $(vlen "$right") ))
    if [ "$pad" -ge 2 ]; then
      printf '%s%*s%s' "$left" "$pad" '' "$right"
      return
    fi
  fi
  printf '%s  %s' "$left" "$right"
}

# ---- where we are -------------------------------------------------------
cur_dir=$p_cur_dir; [ -z "$cur_dir" ] && cur_dir=$p_cwd
[ -z "$cur_dir" ] && cur_dir="$PWD"
proj_dir=$p_proj_dir; [ -z "$proj_dir" ] && proj_dir="$cur_dir"

proj=$(basename "$proj_dir")
sub=""
case "$cur_dir" in
  "$proj_dir")   ;;
  "$proj_dir"/*) sub="/${cur_dir#"$proj_dir"/}" ;;
  *)             proj=$(basename "$cur_dir") ;;
esac

# ---- which config dir ---------------------------------------------------
# Several configs live side by side (~/.claude-personal, ~/.claude-silverfin);
# CLAUDE_CONFIG_DIR is inherited from the shell that launched claude.
cfg=$(basename "${CLAUDE_CONFIG_DIR:-$HOME/.claude}")
case "$cfg" in
  .claude|"") cfg="default" ;;
  .claude-*)  cfg="${cfg#.claude-}" ;;
esac

loc="${D}[${R}${YEL}${cfg}${R}${D}]${R}  ${B}${CYN}${proj}${R}"
[ -n "$sub" ] && loc+="${D}${sub}${R}"

# A linked worktree is worth flagging — it's easy to forget which one you're in.
wt=$p_wt_name; [ -z "$wt" ] && wt=$p_git_worktree
[ -n "$wt" ] && loc+="  ${MAG}⑂ $(basename "$wt")${R}"

# ---- git ----------------------------------------------------------------
# --no-optional-locks keeps a statusline redraw from fighting a real git
# command for the index lock.
git_part=""
if branch=$(git -C "$cur_dir" --no-optional-locks rev-parse --abbrev-ref HEAD 2>/dev/null); then
  [ "$branch" = "HEAD" ] && branch=$(git -C "$cur_dir" --no-optional-locks rev-parse --short HEAD 2>/dev/null)
  # head -1 short-circuits the scan as soon as anything dirty turns up.
  if [ -n "$(git -C "$cur_dir" --no-optional-locks status --porcelain 2>/dev/null | head -1)" ]; then
    git_part="${YEL}${branch}*${R}"
  else
    git_part="${GRN}${branch}${R}"
  fi

  # "behind ahead" against the upstream, empty when the branch isn't tracking.
  if counts=$(git -C "$cur_dir" --no-optional-locks rev-list --left-right --count '@{u}...HEAD' 2>/dev/null); then
    behind=${counts%%[!0-9]*}; ahead=${counts##*[!0-9]}
    [ "${ahead:-0}" -gt 0 ] 2>/dev/null && git_part+=" ${YEL}↑${ahead}${R}"
    [ "${behind:-0}" -gt 0 ] 2>/dev/null && git_part+=" ${BLU}↓${behind}${R}"
  fi
fi

line1_l="  ${loc}"
[ -n "$git_part" ] && line1_l+="  ${SEP}  ${git_part}"

# ---- session identity (right of line 1) ---------------------------------
line1_r=""

# Set via /rename — the whole point is telling sessions apart, so lead with it.
sname=$p_session_name
[ -n "$sname" ] && line1_r+="${B}${MAG}${sname}${R}   "

# Mirrors the footer PR badge; the review state is the part worth glancing at.
pr=$p_pr_number
if [ -n "$pr" ]; then
  case "$p_pr_state" in
    approved)          pr_c="$GRN"; pr_m="✓" ;;
    changes_requested) pr_c="$RED"; pr_m="✗" ;;
    pending)           pr_c="$YEL"; pr_m="…" ;;
    draft)             pr_c="$D";   pr_m="◌" ;;
    *)                 pr_c="$CYN"; pr_m=""  ;;
  esac
  line1_r+="${pr_c}PR #${pr}${R}"
  [ -n "$pr_m" ] && line1_r+=" ${pr_c}${pr_m}${R}"
  line1_r+="   "
fi

# Only present in remote sessions; the short id is enough to match it up.
rid=$p_remote_id
[ -n "$rid" ] && line1_r+="${D}⇅ ${rid:0:8}${R}   "

line1_r=${line1_r%"${line1_r##*[![:space:]]}"}

# ---- model / effort -----------------------------------------------------
model=$p_model_name; [ -z "$model" ] && model=$p_model_id
[ -z "$model" ] && model="?"
line2_l="  ${B}${BLU}${model}${R}"

effort=$p_effort
[ -n "$effort" ] && line2_l+=" ${D}·${R} ${MAG}${effort}${R}"

# Fast mode changes how the model responds, so surface it when it's on.
[ "$p_fast_mode" = "true" ] && line2_l+=" ${YEL}⚡${R}"

# ---- context window -----------------------------------------------------
pct=$p_ctx_pct
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
  line2_l+="   ${c}${bar}${R} ${c}${p}%${R}${D} ctx${R}"
fi

# ---- rate limits + cost (right of line 2) -------------------------------
line2_r=""

# Subscription usage; absent until the first API response of the session. The
# reset clock only earns its width once the window is nearly spent.
limit_seg() {
  local label=$1 raw=$2 reset=$3 pct c
  [ -z "$raw" ] && return
  pct=$(printf '%.0f' "$raw" 2>/dev/null || echo 0)
  if   [ "$pct" -ge 90 ]; then c="$RED"
  elif [ "$pct" -ge 70 ]; then c="$YEL"
  else                         c="$GRN"; fi
  printf '%s' "${D}${label}${R} ${c}${pct}%${R}"
  if [ "$pct" -ge 80 ] && [ -n "$reset" ]; then
    reset=$(date -d "@${reset}" +%H:%M 2>/dev/null)
    [ -n "$reset" ] && printf '%s' "${D} ↻${reset}${R}"
  fi
}
five=$(limit_seg 5h "$p_5h_pct" "$p_5h_reset")
week=$(limit_seg 7d "$p_7d_pct" "$p_7d_reset")
[ -n "$five" ] && line2_r+="${five}  "
[ -n "$week" ] && line2_r+="${week}   "

cost=$p_cost
if [ -n "$cost" ]; then
  line2_r+="${D}\$$(printf '%.2f' "$cost" 2>/dev/null || echo "$cost")${R}  "
fi

add=$p_lines_added; rem=$p_lines_removed
add=${add:-0}; rem=${rem:-0}
if [ "$add" -ne 0 ] 2>/dev/null || [ "$rem" -ne 0 ] 2>/dev/null; then
  line2_r+="${GRN}+${add}${R}${D}/${R}${RED}-${rem}${R}"
fi

line2_r=${line2_r%"${line2_r##*[![:space:]]}"}

printf '%s\n%s\n' "$(join_halves "$line1_l" "$line1_r")" "$(join_halves "$line2_l" "$line2_r")"
