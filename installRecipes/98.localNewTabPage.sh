#!/usr/bin/env bash

function doWork(){
    echo ${GREEN}'Installing Chrome extension LocalNewTabPage'${RESET}
    echo
    local INSTALL_DIR="${HOME}/localNewTabPage1"
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
  <tr>
    <td><a href="http://newsru.co.il">NEWSru.co.il</a></td>
    <td><a href="https://www.reddit.com/user/igorepst/m/vim/">R/Vim</a></td>
    <td><a href="https://github.com/igorepst">Github</a></td>
  </tr>
EOF

    if [ "${MY_PC_IS}" = "work" ]; then
        cat >>"${HTML_FILE}" <<"EOF"
  <tr>
    <td><a href="https://30.30.0.151/vmanage-server/patch-governance/">Stakeholder</a></td>
    <td><a href="https://30.30.0.150/vmanage-server/patch-governance/">Bughunt</a></td>
    <td><a href="https://intigua.atlassian.net/secure/RapidBoard.jspa?rapidView=33&projectKey=CORE&quickFilter=249&assignee=5cbc4ea7bba9e20fca2733fd">Scrum Board</a></td>
    <td><a href="https://app.propertime.co.il/MonthlyTimesheet.aspx">ProperTime</a></td>
  </tr>
EOF
    fi

    cat >>"${HTML_FILE}" <<"EOF"
</table>

</body>
</html>
EOF

    echo "Done. Select 'Load unpacked extension' in Chrome and point it to ${INSTALL_DIR}"
}

doWork
