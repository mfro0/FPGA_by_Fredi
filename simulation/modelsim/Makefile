MODELSIM_PATH=/opt/altera/13.1/modelsim_ase/bin

FILES = \
	../../firebee_utils.vhd	\
	../../Video/video_mod_mux_clutctr.vhd \
	./sim_mux_clutctr.vhd

VCOM_OPTIONS = -2008

all: $(FILES)
	$(MODELSIM_PATH)/vcom $(VCOM_OPTIONS) $(FILES)
