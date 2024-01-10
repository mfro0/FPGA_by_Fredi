onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /video_mod_mux_clutctr_tb/main_clk
add wave -noupdate -radix hexadecimal /video_mod_mux_clutctr_tb/cpu_status
add wave -noupdate -radix hexadecimal /video_mod_mux_clutctr_tb/fb_ad
add wave -noupdate -radix hexadecimal /video_mod_mux_clutctr_tb/fb_adr
add wave -noupdate /video_mod_mux_clutctr_tb/nFB_WR
add wave -noupdate /video_mod_mux_clutctr_tb/nFB_OE
add wave -noupdate -divider uut
add wave -noupdate /video_mod_mux_clutctr_tb/uut/fb_b
add wave -noupdate /video_mod_mux_clutctr_tb/uut/fb_size1
add wave -noupdate /video_mod_mux_clutctr_tb/video_mod_ta
add wave -noupdate /video_mod_mux_clutctr_tb/uut/st_shift_mode_cs
add wave -noupdate /video_mod_mux_clutctr_tb/uut/st_shift_mode
add wave -noupdate /video_mod_mux_clutctr_tb/uut/falcon_shift_mode_cs
add wave -noupdate /video_mod_mux_clutctr_tb/uut/falcon_shift_mode
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {3179043 ps} 0}
quietly wave cursor active 1
configure wave -namecolwidth 399
configure wave -valuecolwidth 90
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
WaveRestoreZoom {2031824 ps} {2826194 ps}
