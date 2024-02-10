function addGymsToTable(gymData) {
    gymTable.clear();
    for (var i = 0; i < gymData.numRows(); i++) {
        var gymRow = getGymRow(gymData, i);
        gymTable.rows.add([gymRow]).draw(false);
    }
    drawMap(gymData);
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

function drawMap(selectedGyms) {
    gymGroup.clearLayers().addLayer(getGymGroup(selectedGyms));
    map.fitBounds(gymGroup.getBounds(), { maxZoom: 12, padding: [15, 15] });   
}

function filterCheckbox(e) {
    var id = e.target.id.substring(4);
    var boxes = [...checkboxes];
    var regexFeatures = new RegExp('^(has_)(climbing|board|fitness)(_)(.*)');
    var regexBoards = new RegExp('^(position_)(.*)');
    var boxTable = 
        aq.table({
            'criteria': boxes.map(x => x.id),
            'value':    boxes.map(x => x.checked)
        });
    var features = 
        boxTable
        .filter(aq.escape(d => d.criteria.match(regexFeatures)))
        .derive({ 
            criteria: aq.escape(d => regexFeatures.exec(d.criteria)[4]),
            value: d => open.sum(d.value) == 0 ? false : d.value
        })
        .filter(d => d.value)
        .pivot('criteria', 'value');
    var boardPositions =
        boxTable
        .filter(aq.escape(d => d.criteria.match(regexBoards)))
        .derive({ 
            criteria: aq.escape(d => regexBoards.exec(d.criteria)[2]),
            value: d => open.sum(d.value) == 0 ? false : d.value
        })
        .filter(d => d.value);
    selectedGyms = features.numRows() > 0 ? features.join(gymData) : gymData;
    drawMap(selectedGyms);
    addGymsToTable(selectedGyms);
}