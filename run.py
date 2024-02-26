from vunit import VUnit
import os

#os.environ['PATH'] = '/opt/altera/13.1/modelsim_ase/bin:' + os.environ['PATH']
#os.environ['VUNIT_SIMULATOR'] = 'modelsim'
os.environ['VUNIT_SIMULATOR'] = 'ghdl'

# Create VUnit instance by parsing command line arguments
vu = VUnit.from_argv(compile_builtins = False)

# Optionally add VUnit's builtin HDL utilities for checking, logging, communication...
# See http://vunit.github.io/hdl_libraries.html.
vu.add_vhdl_builtins()
# or
# vu.add_verilog_builtins()

# Create library 'lib'
lib = vu.add_library("lib")

# Add all files ending in .vhd in current working directory to library
lib.add_source_files("simulation/modelsim/*.vhd")
lib.add_source_files("./firebee_utils.vhd")
lib.add_source_files("./byte_selector.vhd")
lib.add_source_files("Video/video_mod_mux_clutctr.vhd")
lib.add_source_files("Video/video_regs.vhd")

# Run vunit function
vu.main()

