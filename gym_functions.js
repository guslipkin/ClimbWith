function addGymsToTable(gymData) {
    gymTable.clear();
    for (var i = 0; i < gymData.numRows(); i++) {
        var gymRow = getGymRow(gymData, i);
        gymTable.rows.add([gymRow]).draw(false);
    }
    gymGroup.clearLayers().addLayer(getGymGroup(gymData));
    map.fitBounds(gymGroup.getBounds(), { maxZoom: 12 });
}

function getGymGroup(gymData) {
    var gyms = [];
    for (var i = 0; i < gymData.numRows(); i++) {
        var gymRow = getGymRow(gymData, i);
    
        var gym = L.marker([gymRow.lat, gymRow.lon], {alt: gymRow.full_name}).addTo(map);
        gym.bindPopup(gymRow.full_name);
        gym.on({
          mouseover: function(e) { this.openPopup(); },
          mouseout: function(e) { this.closePopup(); }
        });
        gym._id = gymRow.name;
        gyms.push(gym);
    }
    return new L.featureGroup(gyms);
}

function filterMap(selectedGyms) {
    gymGroup.clearLayers().addLayer(getGymGroup(selectedGyms));
    map.fitBounds(gymGroup.getBounds(), { maxZoom: 12 });
}

function filterCheckbox(e) {
    var id = e.target.id.substring(4);
    var boxes = [...checkboxes];
    var selectedGyms = 
        aq.table({
            'criteria': boxes.map(x => x.id.substring(4)),
            'value':    boxes.map(x => x.checked)
        })
        .derive({ value: d => open.sum(d.value) == 0 ? false : d.value })
        .filter(d => d.value)
        .pivot('criteria', 'value');
    selectedGyms = selectedGyms.numRows() > 0 ? selectedGyms.join(gymData) : gymData;
    filterMap(selectedGyms);
    addGymsToTable(selectedGyms);
}