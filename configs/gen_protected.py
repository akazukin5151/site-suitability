"""a script to generated watson_protected_*.json"""
import json
import numpy as np

with open('watson.json', 'r') as f:
    j = json.load(f)

original = [
    (x['name'], x['weight'])
    for x in j['criteria']
]
#print(original)

original_rest = [x['weight'] for x in j['criteria'] if x['name'] != 'protected']
rest_names = [x['name'] for x in j['criteria'] if x['name'] != 'protected']

sum_of_rest = sum([
    x['weight']
    for x in j['criteria']
    if x['name'] != 'protected'
])

#print(sum_of_rest)

# proportion_of_rest relative to sum_of_rest
proportion_of_rest = [
    x['weight'] / sum_of_rest
    for x in j['criteria']
    if x['name'] != 'protected'
]

#print(proportion_of_rest)

protected_weight = [x['weight'] for x in j['criteria'] if x['name'] == 'protected'][0]

# weights for protected
#variable_weights = [0, protected_weight/4, protected_weight/2, protected_weight, 0.2]
# note: 0 might be unreliable because it doesn't remove the factor completely
#variable_weights = np.linspace(0, 0.2, 5)
#variable_weights = np.linspace(0, 0.05, 4)
#variable_weights = np.linspace(0.15, 0.2, 4)
variable_weights = [0.19]
# remaining total
remaining_weights = [1 - w for w in variable_weights]

# for every remaining total, multiply it to every proportion
# vw = original, rw=1-original, prop=0.5, xs=(1-original)*0.5  # original
# vw = 0, rw=1, prop=0.5, xs=1*0.5                             # when vw = 0
xs = [
    [x * w for x in proportion_of_rest]
    for w in remaining_weights
]
#print(xs)
# xs[3] is the weights when weight of protected = protected_weight,
# aka the original weights
#assert(xs[3] == original_rest)

# [(protected_weight, [(rest_name, rest_weight)])]
labeled_xs = [
    (vw, [(name, y) for y, name in zip(x, rest_names)])
    for x, vw in zip(xs, variable_weights)
]
print(labeled_xs)

for (pw, rest_ws) in labeled_xs:
    j1 = j.copy()
    for x in j1['criteria']:
        if x['name'] == 'protected':
            x['weight'] = pw
        else:
            x['weight'] = [w for (n, w) in rest_ws if n == x['name']][0]
    print([(x['name'], x['weight']) for x in j1['criteria']])
    out_name = f'watson_protected_{pw}.json'
    with open(out_name, 'w') as f:
        json.dump(j1, f, indent=2)
