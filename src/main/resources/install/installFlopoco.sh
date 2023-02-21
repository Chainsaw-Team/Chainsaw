# # 安装依赖项
yes | sudo apt update && \
      sudo DEBIAN_FRONTEND=noninteractive apt install -y autoconf automake autotools-dev bison f2c flex git gpg g++ libblas-dev libboost-all-dev liblapack-dev liblpsolve55-dev libsollya-dev libtool lp-solve ninja-build pkg-config sollya wget && \
      wget -O - https://apt.kitware.com/keys/kitware-archive-latest.asc 2>/dev/null | gpg --dearmor - | sudo tee /usr/share/keyrings/kitware-archive-keyring.gpg >/dev/null && \
      echo "deb [signed-by=/usr/share/keyrings/kitware-archive-keyring.gpg] https://apt.kitware.com/ubuntu/ $(lsb_release -cs) main" | sudo tee /etc/apt/sources.list.d/kitware.list >/dev/null && \
      sudo apt update && \
      sudo DEBIAN_FRONTEND=noninteractive apt install -y kitware-archive-keyring cmake
# clone源代码
git clone https://gitlab.com/flopoco/flopoco
cd flopoco/
mkdir build
cd build
# cmake流程
camke ..
# make流程
make
# 建立软链接和自动补全以方便flopoco的使用
ln -s ./code/FloPoCoBin/flopoco .
# 编译文档
./flopoco BuildHTMLDoc
# 编译命令行自动补全功能
./flopoco BuildAutocomplete
mkdir ~/.bash_completion.d/
mv flopoco_autocomplete ~/.bash_completion.d/flopoco
echo ". ~/.bash_completion.d/flopoco" >> ~/.bashrc
# 根据实际路径修改
echo alias flopoco=/home/ltr/flopoco/build/flopoco >> ~/.bashrc
echo export FLOPOCO=/home/ltr/flopoco/build/flopoco >> ~/.bashrc