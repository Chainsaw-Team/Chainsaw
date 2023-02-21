# 安装依赖项
sudo apt-get install build-essential #this already includes g++,这些已经被包含在Ubuntu 20.04发行版中
sudo apt-get install cmake
# clone源代码
git clone https://github.com/openfheorg/openfhe-development.git
cd openfhe-development/
mkdir build
cd build
# CMake流程
cmake ..
# make流程,这一过程会花费较长时间
make
# install流程
sudo make install
# 测试安装是否成功
make testall # 完整测试
# 测试输出
# -- demoData folder already exists
# [  0%] Built target third-party
# [ 30%] Built target coreobj
# [ 30%] Built target OPENFHEcore
# [ 35%] Built target binfheobj
# [ 37%] Built target OPENFHEbinfhe
# [ 40%] Built target binfhe_tests
# [ 54%] Built target core_tests
# [ 84%] Built target pkeobj
# [ 84%] Built target OPENFHEpke
# [100%] Built target pke_tests
# Scanning dependencies of target testall
# core:
# Testing Backends: 4 Native 
# ****** OpenFHE Version 1.0.2
# ****** Date 2023-02-21T15:18:38
# ****** End 160 cases 160 passed 0 failed
# pke:
# Testing Backends: 4 Native 
# ****** OpenFHE Version 1.0.2
# ****** Date 2023-02-21T15:18:45
# ****** End 1373 cases 1373 passed 0 failed
# binfhe:
# Testing Backends: 4 Native 
# ****** OpenFHE Version 1.0.2
# ****** Date 2023-02-21T15:25:05
# ****** End 33 cases 33 passed 0 failed
# [100%] Built target testall
bin/examples/pke/simple-integers # 一个简单的测试
# 测试输出
# #1 + #2 + #3: ( 5 6 9 10 15 18 21 24 27 30 33 36 ... )
# #1 * #2 * #3: ( 3 8 15 32 125 216 343 512 729 1000 1331 1728 ... )
# Left rotation of #1 by 1: ( 2 3 4 5 6 7 8 9 10 11 12 ... )
# Left rotation of #1 by 2: ( 3 4 5 6 7 8 9 10 11 12 ... )
# Right rotation of #1 by 1: ( 0 1 2 3 4 5 6 7 8 9 10 11 ... )
# Right rotation of #1 by 2: ( 0 0 1 2 3 4 5 6 7 8 9 10 ... )
make clean # 清理编译过程的中间文件