#!/usr/bin/env bash
#
# org-protocol-remote.sh
#
# Bridges a local `org-protocol-remote://` URL (triggered by a browser
# bookmarklet) to org-capture running in Emacs on a REMOTE machine over SSH.
#
# Flow:
#   browser bookmarklet
#     -> navigates to  org-protocol-remote://capture?template=...&url=...&title=...&body=...
#     -> the desktop handler (org-protocol-remote.desktop) runs this script with %u
#     -> we rewrite the scheme back to org-protocol:// and hand it to emacsclient
#        on the remote host via ssh.
#
# This is the remote twin of the local org-protocol.desktop handler
# (Exec=emacsclient -- %u); the only difference is the SSH hop.
#
# Requirements on the REMOTE host:
#   * An Emacs server running (emacs --daemon, or (server-start) in init.el)
#   * org-protocol loaded and the capture templates (sc, sl, ...) defined
#     (i.e. the same org-capture-templates as locally)
#   * Key-based SSH auth (this runs non-interactively, no terminal for a password)
#
set -euo pipefail

# SSH target. Add a matching `Host rapkomp` entry to your ~/.ssh/config.
REMOTE_HOST="rapkomp"

# The URL handed over by the desktop handler (%u), e.g.
#   org-protocol-remote://capture?template=sc&url=...&title=...&body=...
url="${1:?usage: org-protocol-remote.sh <org-protocol-remote-url>}"

# Rewrite the custom scheme back to the one the remote Emacs understands.
#   org-protocol-remote://capture?...  ->  org-protocol://capture?...
remote_url="org-protocol:${url#org-protocol-remote:}"

# The URL contains '&' and '=' which the remote login shell would otherwise
# interpret, so wrap it in single quotes for the remote shell. Values are
# percent-encoded by URLSearchParams (single quotes become %27), but escape any
# stray single quote anyway so the quoting can never be broken out of.
escaped=$(printf '%s' "$remote_url" | sed "s/'/'\\\\''/g")

# Trigger the capture on the remote Emacs. No -n/--no-wait, matching the local
# handler: the ssh connection stays up until the capture buffer is finalised on
# the remote (add --no-wait here if you'd rather it return immediately).
exec ssh "$REMOTE_HOST" emacsclient -- "'$escaped'"
