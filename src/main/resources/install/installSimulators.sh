sudo apt-get install git make autoconf g++ flex bison  # First time prerequisites
git clone http://git.veripool.org/git/verilator   # Only first time
unsetenv VERILATOR_ROOT  # For csh; ignore error if on bash
unset VERILATOR_ROOT  # For bash
cd verilator
git pull        # Make sure we're up-to-date
git checkout v4.040
autoconf        # Create ./configure script
./configure
make -j$(nproc)
sudo make install
echo "DONE"

sudo apt-get install libz-dev # For verilator

sudo apt-get install gtkwave