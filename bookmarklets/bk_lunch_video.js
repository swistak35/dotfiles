
function send_to_org_protocol(template, url, title, body) {
    url_to_open = 'org-protocol://capture?' + new URLSearchParams({ template: template, url: url, title: title, body: body});
    window.location.href = url_to_open
}

// Saves the current YouTube video to the predefined "Watch later" (WL) playlist
// using YouTube's internal InnerTube API. We use the API instead of clicking the
// "Save" button because the new save dialog (yt-list-item-view-model) only reacts
// to trusted user events, which a bookmarklet cannot synthesize.
async function save_to_watch_later() {
    let video_id = new URLSearchParams(window.location.search).get('v');
    if (!video_id || !window.ytcfg) {
        return;
    }

    let origin = 'https://www.youtube.com';
    let get_cookie = (name) => (document.cookie.match(new RegExp('(^|; )' + name + '=([^;]+)')) || [])[2];
    let sha1 = async (str) => {
        let buf = await crypto.subtle.digest('SHA-1', new TextEncoder().encode(str));
        return [...new Uint8Array(buf)].map(b => b.toString(16).padStart(2, '0')).join('');
    };
    let ts = Math.floor(Date.now() / 1000);
    let sapisid = get_cookie('SAPISID') || get_cookie('__Secure-3PAPISID');
    let authorization = `SAPISIDHASH ${ts}_${await sha1(`${ts} ${sapisid} ${origin}`)}`;

    await fetch(`${origin}/youtubei/v1/browse/edit_playlist?key=${ytcfg.get('INNERTUBE_API_KEY')}&prettyPrint=false`, {
        method: 'POST',
        credentials: 'include',
        headers: {
            'Content-Type': 'application/json',
            'Authorization': authorization,
            'X-Origin': origin,
            'X-Goog-AuthUser': '0',
        },
        body: JSON.stringify({
            context: ytcfg.get('INNERTUBE_CONTEXT'),
            playlistId: 'WL',
            actions: [{ action: 'ACTION_ADD_VIDEO', addedVideoId: video_id }],
        }),
    });
}

(async () => {
    try {
        await save_to_watch_later();
    } catch (e) {
        console.error('Failed to save video to Watch later:', e);
    }

    let template = 'wl';
    let url = window.location.href;
    let title = document.title;
    let selection = window.getSelection();
    send_to_org_protocol(template, url, title, selection)
})();
