function getGymData() {
    return aq.table({   
        'name':         ['crg_framingham',  'crg_worcester',    'metrorock_everett',    'crg_hadley'],
        'full_name':    ['CRG Framingham',  'CRG Worcester',    'MetroRock Everett',    'CRG Hadley'],
        'lat':          [42.318704,         42.2972087,         42.4076158,             42.3420669],
        'lon':          [-71.4001802,       -71.800223,         -71.0671929,            -72.5844906],
        'boulder':      [true,              true,               true,                   true],
        'toprope':      [false,             true,               true,                   true],
        'lead':         [false,             true,               true,                   true],
        'autobelay':    [false,             true,               true,                   true],
        'kilter_7x10':  [true,              false,              false,                  true],
        'tension_8x10': [false,             false,              false,                  true]
    });
}

function getGymLink() {
    return aq.from(new Map([
        ['crg_framingham', 'https://centralrockgym.com/framingham/'],
        ['crg_worcester', 'https://centralrockgym.com/worcester/'],
        ['metrorock_everett', 'https://metrorock.com/everett-home']
    ]))
    .rename(aq.names('name', 'link'));
}

function getGymBoard() {
    var gymBoard = aq.table({
        'name':     ['crg_framingham',  'crg_hadley',   'crg_hadley'],
        'board':    ['kilter_7x10',     'kilter_7x10',  'tension_8x10'],
        'position': ['adjustable',      'adjustable',        'adjustable']
    })
    .print();
    var fixedBoard = 
    gymBoard
    .filter(aq.escape(d => d.position == 'fixed'))
    .print();
    var adjustableBoard = 
    gymBoard
    .filter(aq.escape(d => d.position == 'adjustable'))
    .select(0,1)
    .print();
    return gymBoard;
}

function getGymRow(gymData, i) {
    return {
        'name':         gymData.get('name', i),
        'full_name':    gymData.get('full_name', i),
        'lat':          gymData.get('lat', i),
        'lon':          gymData.get('lon', i),
        'boulder':      gymData.get('boulder', i),
        'toprope':      gymData.get('toprope', i),
        'lead':         gymData.get('lead', i),
        'autobelay':    gymData.get('autobelay', i)
    }
}