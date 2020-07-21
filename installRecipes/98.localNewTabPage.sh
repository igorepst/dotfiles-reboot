#!/usr/bin/env bash

function doWork(){
    echo ${GREEN}'Installing Chrome extension LocalNewTabPage'${RESET}
    echo
    local INSTALL_DIR="${HOME}/localNewTabPage"
    rm -rf "${INSTALL_DIR}"
    mkdir -p "${INSTALL_DIR}"
    cat >"${INSTALL_DIR}/manifest.json" <<"EOF"
{
  "name": "Local new tab page",
  "description": "Override the new tab page with a local one",
  "version": "0.1",
  "incognito": "split",
  "chrome_url_overrides": {
    "newtab": "localNTP.html"
  },
  "manifest_version": 2
}
EOF

    local HTML_FILE="${INSTALL_DIR}/localNTP.html"
    cat >>"${HTML_FILE}" <<"EOF"
<html>
    <head>
        <title>Local New Tab Page</title>
        <style>
            table {
                font-family: sans-serif;
                border-collapse: collapse;
                width: 100%;
                table-layout: fixed;
            }

            td, th {
                border: 1px solid #dddddd;
                text-align: left;
                padding: 8px;
                font-size: 150%;
            }

            tr:nth-child(even) {
                background-color: #dddddd;
            }
        </style>
    </head>
<body>

<h1>Links</h1>

<table>
EOF
    
    loop "$(dirname "$0")/localNewTabPage-links.txt" "${HTML_FILE}"
    if [[ "${MY_PC_IS}" = "work" ]]; then
        local WORK_LINKS="${HOME}"/.work/localNewTabPage-work-links.txt
        [[ -f "${WORK_LINKS}" ]] && loop "${WORK_LINKS}" "${HTML_FILE}"
    fi

    cat >>"${HTML_FILE}" <<"EOF"
</table>

</body>
</html>
EOF

    echo "Done. Select 'Load unpacked extension' in Chrome and point it to ${INSTALL_DIR}"
}

function loop() {
    local OUTF="$2"
    local cnt=0
    while IFS="" read -r line || [ -n "${line}" ]; do
        IFS='=' read -r first second <<< "${line}"
        first=$(trim "${first}")
        second=$(trim "${second}")
        [[ -z "${first}" ]] || [[ -z "${second}" ]] && continue 
        (( cnt == 0 )) && echo '<tr>'>>"${OUTF}"
        echo "<td><a href=\"${second}\">${first}</a></td>">>"${OUTF}"
        if (( cnt == 3 )); then 
            cnt=0
            echo '</tr>'>>"${OUTF}"
        else 
            (( cnt += 1 ))
        fi
    done < "$1"
    (( cnt != 0 )) && echo '</tr>'>>"${OUTF}"
}

function trim() {
    shopt -s extglob
    set -- "${1##+([[:space:]])}"
    printf "%s" "${1%%+([[:space:]])}" 
}

doWork
