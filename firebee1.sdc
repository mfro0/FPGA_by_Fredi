set_time_format -unit ns -decimal_places 3

# round a floating point result (value) to (decimalplaces)
# decimal places
proc tcl::mathfunc::roundto { value decimalplaces } {
    expr {round(10.0 ** $decimalplaces * $value) / 10.0 ** $decimalplaces}
}

# calculate clock period
# a little tcl trickery to avoid warnings about too much decimal places
set period [expr roundto(1000000.0 / 33000.0, 3)]
set half_period [expr roundto($period / 2.0, 3)]

#post_message "period=$period half_period=$half_period"

#**************************************************************
# Create Clock
#**************************************************************

create_clock -name {MAIN_CLK} -period $period -waveform [list 0.000 $half_period] [get_ports {MAIN_CLK}]

# virtual clocks for MAIN_CLK input and output
create_clock -name {virt_clk_main_in} -period [get_clock_info -period [get_clocks {MAIN_CLK}]] -waveform [get_clock_info -waveform [get_clocks {MAIN_CLK}]]
create_clock -name {virt_clk_main_out} -period [get_clock_info -period [get_clocks {MAIN_CLK}]] -waveform [get_clock_info -waveform [get_clocks {MAIN_CLK}]]

derive_pll_clocks -use_net_name

#**************************************************************
# Create Generated Clock
#**************************************************************

#**************************************************************
# Set Clock Latency
#**************************************************************

#**************************************************************
# Set Clock Uncertainty
#**************************************************************

derive_clock_uncertainty

#**************************************************************
# Set Input Delay
#**************************************************************

set_input_delay -clock {virt_clk_main_in} -max 4 [remove_from_collection [all_inputs] [get_ports {MAIN_CLK}]]
set_input_delay -clock {virt_clk_main_in} -min 2 [remove_from_collection [all_inputs] [get_ports {MAIN_CLK}]]

#**************************************************************
# Set Output Delay
#**************************************************************

set_output_delay -clock {virt_clk_main_out} -min 4 [remove_from_collection [all_outputs] [get_ports {MAIN_CLK}]]
set_output_delay -clock {virt_clk_main_out} -max 2 [remove_from_collection [all_outputs] [get_ports {MAIN_CLK}]]

#**************************************************************
# Set Clock Groups
#**************************************************************

#**************************************************************
# Set False Path
#**************************************************************

set_false_path -from [get_keepers *0hh1*d_wrptr*] -to [get_keepers *0hh1*dffe13a*]
set_false_path -from [get_keepers *0hh1*rdptr_g\[*] -to [get_keepers *0hh1*dffe18*]
set_false_path -from [get_keepers *3fh1*d_wrptr*] -to [get_keepers *3fh1*dffe13a*]
set_false_path -from [get_keepers *3fh1*rdptr_g\[*] -to [get_keepers *3fh1*15\|dffe16a\[*]

set_false_path -from MAIN_CLK -to altpll4:inst22|altpll:altpll_component|altpll_r4n2:auto_generated|clk[0]

# decouple 25 MHz video clk
set_false_path -from MAIN_CLK -to altpll3:inst13|altpll:altpll_component|altpll_9j03:auto_generated|clk[0]

#**************************************************************
# Set Multicycle Path
#**************************************************************

#**************************************************************
# Set Maximum Delay
#**************************************************************

# TPD
#set_max_delay -from [all_inputs] -to [all_outputs] 1

# TSU
#set_max_delay -from [all_inputs] -to [all_registers] 1

# TCO
#set_max_delay -from [all_registers] -to [all_outputs] 1

set_max_delay -from [get_keepers FB_AD*] -to [get_keepers BA*] 5
set_max_delay -from [get_keepers FB_AD*] -to [get_keepers VA*] 5
set_max_delay -from [get_keepers FB_AD*] -to [get_keepers nVRA*] 5

#**************************************************************
# Set Minimum Delay
#**************************************************************

#**************************************************************
# Set Input Transition
#**************************************************************
