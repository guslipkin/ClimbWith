function getGymData() {
    return aq.table({   
        'name':         ['crg_framingham',  'crg_worcester',    'metrorock_everett'],
        'full_name':    ['CRG Framingham',  'CRG Worcester',    'MetroRock Everett'],
        'lat':          [42.318704,         42.2972087,         42.4076158],
        'lon':          [-71.4001802,       -71.800223,         -71.0671929],
        'boulder':      [true,              true,               true],
        'top_rope':     [false,             true,               true],
        'lead':         [false,             true,               true],
        'auto_belay':   [false,             true,               true]
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

function getGymRow(gymData, i) {
    return {
        'name':         gymData.get('name', i),
        'full_name':    gymData.get('full_name', i),
        'lat':          gymData.get('lat', i),
        'lon':          gymData.get('lon', i),
        'boulder':      gymData.get('boulder', i),
        'top_rope':     gymData.get('top_rope', i),
        'lead':         gymData.get('lead', i),
        'auto_belay':   gymData.get('auto_belay', i)
    }
}