orientation is taken from mondino, and is used in all
    - others just said "South"

slope
    - watson only uses buffer
    - sanchez minimizes slope (i use reverse range)
    - only suh uses the sigmoid
    - asakareh uses linear

# functions
- be aware that the sign of the spread arg on suhSigmoid determines its positive/negative relationship. a negative value means higher is better and vice versa

- asakareh's gaussian is invariant when multiplied, that's why i can multiply all arguments by 3600 and the final result won't change

however, suh's sigmoid is not, which is why suhSigmoid needs a 'divide' argument


# weights
- asakareh constraints use weight=1
- **sanchez don't have weights (not using AHP)**
- suh: don't have sunshine so combined insolation + sunshine weights
    0.3889+0.2682 = 0.6571
- watson: added history + wildlife to form protected: 0.069+0.065=0.134
    note that protected and residential are also constraints

# all vs mine
- don't think insolation std matters a lot but i guess i'll use asakareh's gaussian?
    - TODO: plot asakareh's vs suh's
- not using suh's temp because it's out of range
- prefer asakareh's slope
    - watson's is too crude
    - TODO: plot asakareh's vs suh's
- aspect using mondino's
- prefer asakareh's residential (more lenient) because viewshed is more accurate, plus it's not that much of a problem to see solar panels
- prefer watson's protected because it's important to be far away
- prefer watson's (and suh's?) roads as it's more lenient. but maybe use asakareh to show it better in final
- suh's and watson's power look the same, but diff says it's different

- in general i prefer asakareh's gaussian more than suh's because it seems to be "more mathematically backed"
