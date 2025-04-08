function is_singlefile_page() {
    return window.location.href.startsWith("moz-extension://15c35088-8d27-4349-be2a-f9c5336b947f")
}

function format_org_mode_date(date) {
    const pad = (num) => String(num).padStart(2, '0');
    const year = date.getFullYear();
    const month = pad(date.getMonth() + 1);
    const day = pad(date.getDate());
    const hours = pad(date.getHours());
    const minutes = pad(date.getMinutes());
    const seconds = pad(date.getSeconds());
    
    return `[${year}-${month}-${day} ${hours}:${minutes}:${seconds}]`;
}

function capture_singlefile_page() {
    default_template = 'wa'
    default_url = window.location.href
    default_title = document.title
    selection = window.getSelection()

    // Example single_file_comment:
    // <!-- 
    //  Page saved with SingleFile 
    //  url: https://blog.jethro.dev/posts/how_to_take_smart_notes_org/
    //  saved date: Mon Apr 07 2025 23:24:47 GMT+0200 (Central European Summer Time) 
    //  info: something something
    // URL: https://blog.jethro.dev/posts/how_to_take_smart_notes_org/
    // TITLE: How To Take Smart Notes With Org-mode · Jethro Kuan
    // DATE: 2025-04-07T21:24:47.582Z
    // LANGUAGE: 
    // DESCRIPTION: 
    // AUTHOR: 
    // CREATOR: 
    // PUBLISHER:
    //  -->
    single_file_comment = document.createTreeWalker(
        document.getElementsByTagName('html')[0], 
        NodeFilter.SHOW_COMMENT, 
        null, 
        false
    ).nextNode().nodeValue

    lines = single_file_comment.split('\n').map(line => line.trimStart())


    file_line = lines.find(line => line.startsWith('FILE:'))
    file_path = file_line.replace('FILE:', '').trim()

    url_line = lines.find(line => line.startsWith('URL:'))
    url = url_line.replace('URL:', '').trim()

    title_line = lines.find(line => line.startsWith('TITLE:'))
    title = title_line.replace('TITLE:', '').trim()

    archival_date_line = lines.find(line => line.startsWith('DATE:'))
    archival_date = new Date(archival_date_line.replace('DATE:', '').trim())

    language_line = lines.find(line => line.startsWith('LANGUAGE:'))
    language = language_line.replace('LANGUAGE:', '').trim()

    description_line = lines.find(line => line.startsWith('DESCRIPTION:'))
    description = description_line.replace('DESCRIPTION:', '').trim()

    author_line = lines.find(line => line.startsWith('AUTHOR:'))
    author = author_line.replace('AUTHOR:', '').trim()

    creator_line = lines.find(line => line.startsWith('CREATOR:'))
    creator = creator_line.replace('CREATOR:', '').trim()

    publisher_line = lines.find(line => line.startsWith('PUBLISHER:'))
    publisher = publisher_line.replace('PUBLISHER:', '').trim()

    id = crypto.randomUUID()
    date = null

    property_lines = [
        ':PROPERTIES:',
        `:ID: ${id}`,
        `:URL: [[${url}]]`,
        `:FILE: [[~/bookmarks/${file_path}]]`,
        `:TITLE: ${title}`,
        `:ARCHIVED_AT: ${format_org_mode_date(archival_date)}`,
    ]
    if (description) {
        property_lines.push(`:DESCRIPTION: ${description}`)
    }
    if (language) {
        property_lines.push(`:LANGUAGE: ${language}`)
    }
    if (author) {
        property_lines.push(`:AUTHOR: ${author}`)
    }
    if (creator) {
        property_lines.push(`:CREATOR: ${creator}`)
    }
    if (publisher) {
        property_lines.push(`:PUBLISHER: ${publisher}`)
    }


    property_lines.push(':END:')

    the_body = property_lines.map(line => `  ${line}`).join('\n')
    the_body += `\n${selection}`

    send_to_org_protocol(default_template, default_url, default_title, the_body)
}

function capture_page() {
    let template = 'wa'
    let url = window.location.href
    let title = document.title
    let selection = window.getSelection()

    id = crypto.randomUUID()
    archived_at = new Date()

    property_lines = [
        ':PROPERTIES:',
        `:ID: ${id}`,
        `:URL: [[${url}]]`,
        `:TITLE: ${title}`,
        `:ARCHIVED_AT: ${format_org_mode_date(archived_at)}`,
    ]

    property_lines.push(':END:')

    the_body = property_lines.map(line => `  ${line}`).join('\n')
    the_body += `\n${selection}`

    send_to_org_protocol(template, url, title, the_body)
}

function send_to_org_protocol(template, url, title, body) {
    url_to_open = 'org-protocol://capture?' + new URLSearchParams({ template: template, url: url, title: title, body: body});
    window.location.href = url_to_open
}

if (is_singlefile_page()) {
    capture_singlefile_page()
} else {
    capture_page()
}