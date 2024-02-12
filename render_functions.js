function renderFullName(data, type) {
    var gym = gymData.filter(aq.escape(d => d.full_name == data));
    var full_name = gym.get('full_name', 0);
    var link = gym.get('link', 0);
    return `<a href='${link}' target='blank'>${full_name}</href>`;
}

function renderBoolean(data, type) {
    return data ? 'x' : ''
}

function getPopup(gymRow) {
    console.log(Object.keys(gymRow).filter(x => gymRow[x] === true));
    var header = `<h6>${gymRow.full_name}</h6>`;
    var table = `
        <table class="table">
            <thead>
                <tr>
                    <th scope="col">Feature</th>
                    <th scope="col">Has?</th>
                </tr>
            </thead>
            <tr>
                <td>Bouldering</td>
                <td>${gymRow.boulder}</td>
            </tr>
        </table>
    `;
    return `${header}<br>${table}`;
}