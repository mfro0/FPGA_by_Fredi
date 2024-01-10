MODELSIM_PATH=/opt/altera/13.1/modelsim_ase/bin

FILES = \
	../../firebee_utils.vhd	\
	../../Video/video_mod_mux_clutctr.vhd \
	sim_mux_clutctr.vhd


GHDL = ghdl
VCOM_OPTIONS = -2008
GHDL_OPTIONS = --std=08
all: $(FILES)
	$(MODELSIM_PATH)/vcom $(VCOM_OPTIONS) $(FILES)

ghdl: work-obj08.cf 

MODULE = video_mod_mux_clutctr_tb
GHW=$(MODULE).ghw

work-obj08.cf: $(FILES)
	$(GHDL) -a $(GHDL_OPTIONS) $(FILES)
	$(GHDL) -i $(GHDL_OPTIONS) $<
	$(GHDL) -m $(GHDL_OPTIONS) $(MODULE)

sim: $(MODULE)
	ghdl -r $(GHDL_OPTIONS) $(MODULE) --wave=$(GHW)
