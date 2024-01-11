onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /video_mod_mux_clutctr_tb/main_clk
add wave -noupdate -radix hexadecimal /video_mod_mux_clutctr_tb/cpu_status
add wave -noupdate -radix hexadecimal /video_mod_mux_clutctr_tb/fb_ad
add wave -noupdate -radix hexadecimal /video_mod_mux_clutctr_tb/fb_adr
add wave -noupdate -format Literal /video_mod_mux_clutctr_tb/nFB_WR
add wave -noupdate /video_mod_mux_clutctr_tb/nFB_OE
add wave -noupdate /video_mod_mux_clutctr_tb/uut/nFB_CS1
add wave -noupdate /video_mod_mux_clutctr_tb/uut/nFB_CS2
add wave -noupdate /video_mod_mux_clutctr_tb/uut/nFB_CS3
add wave -noupdate -divider uut
add wave -noupdate /video_mod_mux_clutctr_tb/uut/fb_b
add wave -noupdate /video_mod_mux_clutctr_tb/uut/fb_size1
add wave -noupdate /video_mod_mux_clutctr_tb/video_mod_ta
add wave -noupdate /video_mod_mux_clutctr_tb/uut/acp_clut_cs
add wave -noupdate /video_mod_mux_clutctr_tb/uut/acp_vctr_cs
add wave -noupdate /video_mod_mux_clutctr_tb/uut/ccr_cs
add wave -noupdate /video_mod_mux_clutctr_tb/uut/falcon_clut_cs
add wave -noupdate /video_mod_mux_clutctr_tb/uut/st_clut_cs
add wave -noupdate /video_mod_mux_clutctr_tb/uut/st_shift_mode_cs
add wave -noupdate /video_mod_mux_clutctr_tb/uut/falcon_shift_mode_cs
add wave -noupdate /video_mod_mux_clutctr_tb/uut/sys_ctr_cs
add wave -noupdate /video_mod_mux_clutctr_tb/uut/vdl_bpp_cs
add wave -noupdate /video_mod_mux_clutctr_tb/uut/vdl_hbb_cs
add wave -noupdate /video_mod_mux_clutctr_tb/uut/vdl_hbe_cs
add wave -noupdate /video_mod_mux_clutctr_tb/uut/vdl_hdb_cs
add wave -noupdate /video_mod_mux_clutctr_tb/uut/vdl_hde_cs
add wave -noupdate /video_mod_mux_clutctr_tb/uut/vdl_hht_cs
add wave -noupdate /video_mod_mux_clutctr_tb/uut/vdl_lof_cs
add wave -noupdate /video_mod_mux_clutctr_tb/uut/vdl_lwd_cs
add wave -noupdate /video_mod_mux_clutctr_tb/uut/vdl_ph_cs
add wave -noupdate /video_mod_mux_clutctr_tb/uut/vdl_pv_cs
add wave -noupdate /video_mod_mux_clutctr_tb/uut/vdl_vbb_cs
add wave -noupdate /video_mod_mux_clutctr_tb/uut/vdl_vbe_cs
add wave -noupdate /video_mod_mux_clutctr_tb/uut/vdl_vct_cs
add wave -noupdate /video_mod_mux_clutctr_tb/uut/vdl_vdb_cs
add wave -noupdate /video_mod_mux_clutctr_tb/uut/vdl_vde_cs
add wave -noupdate /video_mod_mux_clutctr_tb/uut/vdl_vft_cs
add wave -noupdate /video_mod_mux_clutctr_tb/uut/vdl_vmd_cs
add wave -noupdate /video_mod_mux_clutctr_tb/uut/vdl_vss_cs
add wave -noupdate /video_mod_mux_clutctr_tb/uut/videl_cs
add wave -noupdate /video_mod_mux_clutctr_tb/uut/video_pll_config_cs
add wave -noupdate /video_mod_mux_clutctr_tb/uut/video_pll_reconfig_cs
add wave -noupdate /video_mod_mux_clutctr_tb/uut/st_shift_mode
add wave -noupdate -radix hexadecimal /video_mod_mux_clutctr_tb/uut/falcon_shift_mode
add wave -noupdate /video_mod_mux_clutctr_tb/s
add wave -noupdate /video_mod_mux_clutctr_tb/uut/videl_cs
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {229808 ps} 0} {{Cursor 2} {4109787 ps} 0}
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
WaveRestoreZoom {145994 ps} {483579 ps}
