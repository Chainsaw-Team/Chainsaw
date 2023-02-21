import pyDigitalWaveTools

import json
import sys
# from pyDigitalWaveTools.vcd.parser import VcdParser
#
# vcd = VcdParser()
# vcd.parse("/home/ltr/IdeaProjects/Chainsaw/simWorkspace/testComplexToMagnitudeAngle_12_16/test.vcd")
# data = vcd.scope.toJson()
# # print(json.dumps(data, indent=4, sort_keys=True))

from vcdvcd import VCDVCD
vcd = VCDVCD("/home/ltr/IdeaProjects/Chainsaw/simWorkspace/testDasSignalPro/test.vcd")
print(vcd.references_to_ids)
# signal = vcd["TOP.CordicModule.flowIn_valid_delay_10"]
# print(signal.tv)

