sudo apt-get install build-essential libboost-dev git
sudo apt-get install gnat # Ada compiler used to buid GHDL
git clone https://github.com/ghdl/ghdl.git
cd ghdl
mkdir build
cd build
../configure
make
sudo make install