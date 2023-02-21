sudo apt
sudo apt install python3-pip
pip install numpy
pip install scipy
pip install matplotlib

# for human user only
# jupyter notebook
# pip install notebook
# pip install jinja2  --upgrade

echo  | cat >> ~/.bashrc # empty line
echo export PYTHON=/usr/bin/python3 | cat >> ~/.bashrc
echo alias python=/usr/bin/python3 | cat >> ~/.bashrc