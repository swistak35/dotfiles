
function send_to_org_protocol(template, url, title, body) {
    url_to_open = 'org-protocol://capture?' + new URLSearchParams({ template: template, url: url, title: title, body: body});
    window.location.href = url_to_open
}

let template = 'wl';
let url = window.location.href;
let title = document.title;
let selection = window.getSelection();
send_to_org_protocol(template, url, title, selection)

capture_page();