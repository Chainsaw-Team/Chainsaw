val mhz250 = BigInt("80000000", 16)
def getFreq(mhz: Int) = (mhz250 * mhz / 250).toLong
val freq80 = (getFreq(80) + 1).toHexString
val freq100 = (getFreq(100) + 1).toHexString