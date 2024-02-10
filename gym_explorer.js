var street_map = L.tileLayer.wms('https://ows.terrestris.de/osm/service?', {
    layers: 'OSM-WMS',
    format: 'image/png',
    transparent: true,
    attribution: 'Street Map'
});
var imagery = L.tileLayer.wms('https://basemap.nationalmap.gov:443/arcgis/services/USGSImageryTopo/MapServer/WmsServer?', {
    layers: '0',
    format: 'image/png',
    transparent: true,
    attribution: 'National Map'
});
var map = L.map('map', {
    center: [0, 0],
    zoom: 1,
    layers: [street_map]
});
var baseMaps = {
    "Street Map": street_map,
    "Imagery": imagery
};
var layerControl = L.control.layers(baseMaps, null, { collapsed: false });
layerControl.addTo(map);

// fix for map jumping in leaflet 1.8
L.Control.Measure.include({
    // set icon on the capture marker
    _setCaptureMarkerIcon: function () {
        // disable autopan
        this._captureMarker.options.autoPanOnFocus = false;
        // default function
        this._captureMarker.setIcon(
            L.divIcon({ iconSize: this._map.getSize().multiplyBy(2) })
        );
    },
});
var measureControl = new L.Control.Measure({
    position: 'bottomleft',
    primaryLengthUnit: 'miles', secondaryLengthUnit: 'kilometers',
    primaryAreaUnit: 'sqmiles', secondaryAreaUnit: 'sqkilometers',
    activeColor: '#0086E5',
    completedColor: '#002A36'
});
measureControl.addTo(map);
var gymGroup = new L.featureGroup({}).addTo(map);

var gymData = getGymData().join(getGymLink(), 'name');

const gymTable = new DataTable('#gymTable', {
    dom: 'Bfrtip',
    info: false,
    ordering: true,
    paging: false,
    scrollY: 280,
    scrollCollapse: true,
    columns: [
        { data: 'full_name',    render: renderFullName  },
        { data: 'boulder',      render: renderBoolean   },
        { data: 'toprope',     render: renderBoolean   },
        { data: 'lead',         render: renderBoolean   },
        { data: 'autobelay',   render: renderBoolean   }
    ],
    buttons: []
});

addGymsToTable(gymData);

var selectedGyms = gymTable.rows().data();
gymTable.on('click', 'tbody tr', function(e) {
    e.currentTarget.classList.toggle('selected');
    var selectedRows = gymTable.rows('.selected').data();
    selectedGyms = selectedRows.slice(0, selectedRows.length + 1).map(x => x.name);
    selectedGyms = selectedGyms.length == 0 ? gymData : gymData.semijoin(aq.table({'name': selectedGyms}), 'name');
    drawMap(selectedGyms);
});

var checkboxes = document.querySelectorAll("input[type='checkbox']");
for (var i = 0; i < checkboxes.length; i++) {
    checkboxes[i].addEventListener("click", filterCheckbox);
}
