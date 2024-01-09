onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /video_mod_mux_clutctr_tb/main_clk
add wave -noupdate -radix hexadecimal /video_mod_mux_clutctr_tb/cpu_status
add wave -noupdate -radix hexadecimal /video_mod_mux_clutctr_tb/fb_ad
add wave -noupdate -expand -label {Contributors: fb_ad} -group {Contributors: sim:/video_mod_mux_clutctr_tb/fb_ad} /video_mod_mux_clutctr_tb/cpu_status
add wave -noupdate /video_mod_mux_clutctr_tb/nFB_CS1
add wave -noupdate /video_mod_mux_clutctr_tb/nFB_CS2
add wave -noupdate /video_mod_mux_clutctr_tb/nFB_CS3
add wave -noupdate /video_mod_mux_clutctr_tb/nFB_OE
add wave -noupdate /video_mod_mux_clutctr_tb/nFB_WR
add wave -noupdate -divider uut
add wave -noupdate /video_mod_mux_clutctr_tb/uut/fb_b
add wave -noupdate /video_mod_mux_clutctr_tb/uut/fb_size0
add wave -noupdate /video_mod_mux_clutctr_tb/uut/fb_size1
add wave -noupdate /video_mod_mux_clutctr_tb/video_mod_ta
add wave -noupdate -expand -label {Contributors: video_mod_ta} -group {Contributors: sim:/video_mod_mux_clutctr_tb/video_mod_ta} /video_mod_mux_clutctr_tb/uut/acp_vctr_cs
add wave -noupdate -expand -label {Contributors: video_mod_ta} -group {Contributors: sim:/video_mod_mux_clutctr_tb/video_mod_ta} /video_mod_mux_clutctr_tb/uut/clut_ta
add wave -noupdate -expand -label {Contributors: video_mod_ta} -group {Contributors: sim:/video_mod_mux_clutctr_tb/video_mod_ta} /video_mod_mux_clutctr_tb/uut/sys_ctr_cs
add wave -noupdate -expand -label {Contributors: video_mod_ta} -group {Contributors: sim:/video_mod_mux_clutctr_tb/video_mod_ta} /video_mod_mux_clutctr_tb/uut/videl_cs
add wave -noupdate /video_mod_mux_clutctr_tb/uut/st_shift_mode_cs
add wave -noupdate -expand -label {Contributors: acp_vctr_cs} -group {Contributors: sim:/video_mod_mux_clutctr_tb/uut/acp_vctr_cs} -radix hexadecimal /video_mod_mux_clutctr_tb/uut/fb_adr
add wave -noupdate -expand -label {Contributors: acp_vctr_cs} -group {Contributors: sim:/video_mod_mux_clutctr_tb/uut/acp_vctr_cs} /video_mod_mux_clutctr_tb/uut/nFB_CS2
add wave -noupdate /video_mod_mux_clutctr_tb/uut/falcon_shift_mode_cs
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {196456 ps} 0}
quietly wave cursor active 1
configure wave -namecolwidth 399
configure wave -valuecolwidth 100
configure wave -justifyvalue left
configure wave -signalnamewidth 0
configure wave -snapdistance 10
configure wave -datasetprefix 0
configure wave -rowmargin 4
configure wave -childrowmargin 2
configure wave -gridoffset 0
configure wave -gridperiod 1
configure wave -griddelta 40
configure wave -timeline 0
configure wave -timelineunits ps
update
WaveRestoreZoom {2215040 ps} {3929567 ps}
