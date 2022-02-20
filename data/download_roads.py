import os
import time
from pathlib import Path

def run(number, bbox):
    query = f"""
    (
      // query part for: “highway=motorway”
      way["highway"="motorway"]({bbox});
      // query part for: “highway=trunk”
      way["highway"="trunk"]({bbox});
      // query part for: “highway=primary”
      way["highway"="primary"]({bbox});
    );
    // print results
    out body;
    >;
    out skel qt;
    """
    Path('cache').mkdir(exist_ok=True)
    Path('roads').mkdir(exist_ok=True)
    with open('cache/query.osm', 'w') as f:
        f.write(query)

    # https://overpass-api.de/command_line.html
    # npm install --prefix=~/.local/ -g osmtogeojson
    cmd = f'wget -O cache/{number}.osm --post-file=cache/query.osm "https://overpass-api.de/api/interpreter"'
    os.system(cmd)

    cmd = f'osmtogeojson cache/{number}.osm > roads/{number}.geojson'
    os.system(cmd)
    time.sleep(60)


# minimum latitude, minimum longitude, maximum latitude, maximum longitude
# bottom, left, top, right
bboxes = {
    1: '34.8662101999999976,-114.8887392000000034,37.3314971000000000,-111.4236393999999990',
    2: '34.6534383999999989,-111.6178409000000045,37.2452199000000022,-108.0904277000000064',
    3: '32.3823794999999990,-112.2726715000000013,34.9022404999999978,-108.4983541999999943',
    4: '32.4277468000000013,-115.9312405000000012,34.9119073000000029,-112.1154113999999993',
    5: '30.2580525000000016,-114.9271432000000033,32.7278037000000026,-111.3704866000000067',
    6: '31.2026949000000009,-112.2855679999999978,33.7264169999999979,-108.7087252999999976',
}

[run(idx, bbox) for idx, bbox in bboxes.items()]
