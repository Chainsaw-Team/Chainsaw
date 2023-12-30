import os
import shutil
import subprocess

import argparse

# parser = argparse.ArgumentParser(description='Quartus project script')
# parser.add_argument('--project_dir', required=True, help='Path to the project directory')
# parser.add_argument('--quartus_dir', required=True, help='Path to Quartus executable directory')
# args = parser.parse_args()
# project_dir = args.project_dir
# quartus_dir = args.quartus_dir  # ...

# Define the project directory path and Quartus executable path
project_dir = r"C:\Users\ltr\Documents\GitHub\Chainsaw"  # your own project directory
assert os.path.isdir(project_dir), "Project directory does not exist"
quartus_dir = r"C:\intelFPGA_lite\23.1std\quartus\bin64"
assert os.path.isdir(quartus_dir), "Quartus directory does not exist"
quartus = os.path.join(quartus_dir, "quartus.exe")
quartus_sh = os.path.join(quartus_dir, "quartus_sh.exe")
quartus_stp = os.path.join(quartus_dir, "quartus_stp.exe")

workspace = os.path.join(project_dir, "synthWorkspace", "Acq250")

source_dir = project_dir
ip_dir = os.path.join(project_dir, "src/main/resources/ips/xillybus/cycloneV")
board_dir = os.path.join(project_dir, r"src\main\resources\boards\das")

tcl_script_path = os.path.join(project_dir, r"src\main\resources\boards\das\createAcq250.tcl")

# Print the project directory
print(f"your project directory is: {project_dir}")

# Manipulate directories

os.chdir(project_dir)

# Create synthWorkspace directory if not exists
if not os.path.isdir(workspace):
    os.makedirs(workspace)
else:
    shutil.rmtree(workspace)
    os.makedirs(workspace)
os.chdir(workspace)

# TODO: run SpinalHDL to generate top module verilog file

# Copy the source files

destination_path = os.getcwd()
shutil.copy2(os.path.join(source_dir, "Acq250Top.v"), destination_path)
shutil.copytree(ip_dir, destination_path, dirs_exist_ok=True)
shutil.copy2(os.path.join(board_dir, "Acq250.sdc"), destination_path)
shutil.copy2(os.path.join(board_dir, "Acq250.stp"), destination_path)

# Run the TCL script to create a Quartus project
subprocess.run([quartus_sh, '-t', tcl_script_path])

# subprocess.run([quartus_stp, 'Acq250Top', '--enable', "--signaltap", f"--stp_file={os.path.join(board_dir, 'Acq250.stp')}"])

# Open the project created
subprocess.run([quartus, 'Acq250Top.qpf'])

# subprocess.run([quartus_sh, '--flow', 'compile', 'Acq250Top'])
