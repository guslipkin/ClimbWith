function renderFullName(data, type) {
    var gym = gymData.filter(aq.escape(d => d.full_name == data));
    var full_name = gym.get('full_name', 0);
    var link = gym.get('link', 0);
    return `<a href='${link}' target='blank'>${full_name}</href>`;
}

function renderBoolean(data, type) {
    return data ? 'x' : ''
}