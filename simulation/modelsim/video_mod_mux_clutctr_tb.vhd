library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

library vunit_lib;
context vunit_lib.vunit_context;

use work.video_regs.all;


entity video_mod_mux_clutctr_tb is
    generic (runner_cfg : string);
end entity video_mod_mux_clutctr_tb;

architecture sim of video_mod_mux_clutctr_tb is
    -- video_mod_mux_clutctr module's inputs/outputs
    signal nRSTO            : std_logic;
    signal main_clk         : std_logic := '0';
    signal nFB_CS1          : std_logic;
    signal nFB_CS2          : std_logic;
    signal nFB_CS3          : std_logic;
    signal nFB_WR           : std_logic;
    signal nFB_OE           : std_logic;
    signal fb_size0         : std_logic;
    signal fb_size1         : std_logic;
    signal nFB_BURST        : std_logic;
    signal fb_adr           : std_logic_vector(31 downto 0);

    signal clk33m           : std_logic;
    signal clk25m           : std_logic := '0';
		
    signal blitter_run      : std_logic := '0';
    signal clk_video        : std_logic;
    signal vr_d             : std_logic_vector(8 downto 0);
    signal vr_busy          : std_logic;

    signal color8           : std_logic;
    signal acp_clut_rd      : std_logic;
    signal color1           : std_logic;

    signal falcon_clut_rdh,
           falcon_clut_rdl  : std_logic;
    signal falcon_clut_wr   : std_logic_vector(3 downto 0);
    signal st_clut_rd       : std_logic;
    signal st_clut_wr       : std_logic_vector(1 downto 0);
    signal clut_mux_adr     : std_logic_vector(3 downto 0);
		
    signal hsync,
           vsync            : std_logic;
    signal nBLANK,
           nSYNC            : std_logic;
    signal nPD_VGA          : std_logic;
    signal fifo_rde         : std_logic;
    signal color2,
           color4           : std_logic;
    signal pixel_clk        : std_logic;
    signal clut_off         : std_logic_vector(3 downto 0);
    signal blitter_on       : std_logic := '0';

    signal video_ram_ctr    : std_logic_vector(15 downto 0);
    signal video_mod_ta     : std_logic;
		
    signal ccr              : std_logic_vector(23 downto 0);
    signal ccsel            : std_logic_vector(2 downto 0);
    signal acp_clut_wr      : std_logic_vector(3 downto 0);
    signal inter_zei        : std_logic;
    signal dop_fifo_clr     : std_logic;
    signal video_reconfig   : std_logic;
    signal vr_wr,
           vr_rd            : std_logic;
    signal clr_fifo         : std_logic;
    signal dpzf_clkena      : std_logic;
    signal fb_ad            : std_logic_vector(31 downto 0);

    type cpu_state_t is (S0, S1, S2, S3);
    signal cpu_status       : cpu_state_t := S3;

    type operation_type is (R, W);
    type lane_width_type is (BYTE, WORD, LONG, LINE);
    type str is access string;
    subtype videl_reg_t is std_logic_vector(12 downto 0);
    subtype addr_t is std_logic_vector(31 downto 0);
    subtype fbcs_t is std_logic_vector(3 downto 0);
    type stim_record is record
        fbcs            : fbcs_t;
        addr            : addr_t;
        operation       : operation_type;
        data            : addr_t;
        width           : lane_width_type;
    end record;
    type reg_ptr_t is access addr_t;

    type stim_vector_t is array (positive range <>) of stim_record;

    constant sv : stim_vector_t :=
    (
        -- first do a few cycles of nothing relevant to the controller
        ("0111", x"40000000", W, 32x"bcd", LONG),       -- 1
        -- then address our module's components
        
        -- write Videl registers
        -- horizontal
        ("1101", VDL_HHT, W, 32x"123", WORD),           -- 2
        ("1101", VDL_HBB, W, 32x"234", WORD),           -- 3
        ("1101", VDL_HBE, W, 32x"345", WORD),           -- 4
        ("1101", VDL_HDB, W, 32x"567", WORD),           -- 5
        ("1101", VDL_HDE, W, 32x"678", WORD),           -- 6
        ("1101", VDL_HSS, W, 32x"789", WORD),           -- 7
        -- vertical
        ("1101", VDL_VFT, W, 32x"89A", WORD),           -- 8
        ("1101", VDL_VBB, W, 32x"20C", WORD),           -- 9
        ("1101", VDL_VBE, W, 32x"030", WORD),           -- 10
        ("1101", VDL_VDB, W, 32x"BCD", WORD),           -- 11
        ("1101", VDL_VDE, W, 32x"CDE", WORD),           -- 12
        ("1101", VDL_VSS, W, 32x"DEF", WORD),           -- 13
        ("1101", VDL_VMD, W, 32x"EF1", WORD),           -- 14

        -- read Videl registers
        -- horizontal
        ("1101", VDL_HHT, R, 32x"123", WORD),           -- 15
        ("1101", VDL_HBB, R, 32x"234", WORD),           -- 16
        ("1101", VDL_HBE, R, 32x"345", WORD),           -- 17
        ("1101", VDL_HDB, R, 32x"567", WORD),           -- 18
        ("1101", VDL_HDE, R, 32x"678", WORD),           -- 19
        ("1101", VDL_HSS, R, 32x"789", WORD),           -- 20
        -- vertical
        ("1101", VDL_VFT, R, 32x"89A", WORD),           -- 21
        ("1101", VDL_VBB, R, 32x"20C", WORD),           -- 22
        ("1101", VDL_VBE, R, 32x"030", WORD),           -- 23
        ("1101", VDL_VDB, R, 32x"BCD", WORD),           -- 24
        ("1101", VDL_VDE, R, 32x"CDE", WORD),           -- 25
        ("1101", VDL_VSS, R, 32x"DEF", WORD),           -- 26
        ("1101", VDL_VMD, R, 32x"EF1", WORD),           -- 27

        -- ACP Vctr register
        ("1011", VCTR, W, 32x"11223344", LONG),         -- 28
        ("1011", VCTR, R, 32x"11223344", LONG),         -- 29

        -- ST Shifter
        ("1101", STSHIFT, W, 32x"00000001", BYTE),      -- 30

        -- VDL_VCT
        ("1101", VDL_VCT, W, 32x"00000182", WORD),      -- 31
        ("1101", VDL_VCT, R, 32x"00000182", WORD),      -- 32

        -- try some scrspain settings (640x480)
        ("1101", VDL_HHT, W, 32x"00C6", WORD),          -- 33
        ("1101", VDL_HBB, W, 32x"008D", WORD),          -- 34
        ("1101", VDL_HBE, W, 32x"0015", WORD),          -- 35
        ("1101", VDL_HDB, W, 32x"02AB", WORD),          -- 36
        ("1101", VDL_HDE, W, 32x"0084", WORD),          -- 37
        ("1101", VDL_HSS, W, 32x"0097", WORD),          -- 38
        ("1101", VDL_VFT, W, 32x"0419", WORD),          -- 39
        ("1101", VDL_VDB, W, 32x"03FF", WORD),          -- 40
        ("1101", VDL_VBE, W, 32x"003F", WORD),          -- 41
        ("1101", VDL_VDB, W, 32x"003F", WORD),          -- 42
        ("1101", VDL_VDE, W, 32x"03FF", WORD),          -- 43
        ("1101", VDL_VSS, W, 32x"0415", WORD),          -- 44
        ("1101", STSYNC,  W, 32x"0200", WORD),          -- 45
        ("1101", VDL_VCT, W, 32x"0186", WORD),          -- 46
        ("1101", SPSHIFT, W, 32x"0000", WORD),          -- 47
        ("1101", SPSHIFT, W, 32x"0010", WORD),          -- 48
        ("1101", VDL_VMD, W, 32x"0008", WORD),          -- 49
        ("1101", VWRAP,   W, 32x"0140", WORD),          -- 50
        ("1011", VCTR, W, 32x"13223344", LONG),         -- 51 enable display
        ("1101", MONTYPE, W, 32x"FFFFFFF0", WORD),      -- 52 set monitor type (named sys_ctr in video_mod_mux_clutctr)

        -- 640x400 mono (ST)
        ("1101", VDL_HHT, W, 32x"00C0", WORD),          -- 53
        ("1101", VDL_HBB, W, 32x"008D", WORD),          -- 54
        ("1101", VDL_HBE, W, 32x"0015", WORD),          -- 55
        ("1101", VDL_HDB, W, 32x"0273", WORD),          -- 56
        ("1101", VDL_HDE, W, 32x"004C", WORD),          -- 57
        ("1101", VDL_HSS, W, 32x"0097", WORD),          -- 58
        ("1101", VDL_VFT, W, 32x"0419", WORD),          -- 59
        ("1101", VDL_VBB, W, 32x"03AF", WORD),          -- 60
        ("1101", VDL_VBE, W, 32x"008F", WORD),          -- 61
        ("1101", VDL_VDB, W, 32x"008F", WORD),          -- 62
        ("1101", VDL_VDE, W, 32x"03AF", WORD),          -- 63
        ("1101", VDL_VSS, W, 32x"0415", WORD),          -- 64
        ("1101", STSHIFT, W, 32x"0200", WORD),          -- 65
        ("1101", VDL_VCT, W, 32x"0186", WORD),          -- 64
        ("1101", SPSHIFT, W, 32x"0000", WORD),          -- 65
        ("1101", SPSHIFT, W, 32x"0400", WORD),          -- 66
        ("1101", VDL_VMD, W, 32x"0008", WORD),          -- 67
        ("1101", VWRAP,   W, 32x"0028", WORD),          -- 68
        ("1011", VCTR,    W, 32x"13223344", LONG),      -- 69
        ("1101", MONTYPE, W, 32x"FFF0", WORD)           -- 70
    );

    signal step                 : positive := 1;
    signal d                    : addr_t;
    signal pixel_clk_frq        : real;

begin
    measure_pixelclk : process
        variable t : time := now;
        variable previous_t : time := now;
    begin
        previous_t := t;
        wait until rising_edge(pixel_clk);
        t := now;
        pixel_clk_frq <= 1.0E3 / real((t - previous_t) / 1 ns);
        -- report "freq = " & to_string(1.0E3 / real((t - previous_t) / 1 ns), "%4.2f") & " MHz";
    end process measure_pixelclk;

    test_runner : process
        -- initialize index into stimulation vector and
        -- wait for the ColdFire state machine to complete a full circle state transition
        procedure prepare_test(s : natural) is
        begin
            step <= s;
            for i in S0 to S3 loop
                wait until rising_edge(main_clk);
            end loop;
        end procedure prepare_test;
    begin
        test_runner_setup(runner, runner_cfg);

        while test_suite loop
            if run("write FPGA memory") then
                prepare_test(1);
                check(true, "FPGA memory: expected to always pass");
            elsif run("write VDL_HHT") then
                prepare_test(2);
                check_equal(sv(step).data(videl_reg_t'range), <<signal uut.vdl_hht : videl_reg_t>>, "write VDL_HHT");
            elsif run("write VDL_HBB") then
                prepare_test(3);
                check(sv(step).data(videl_reg_t'range) = <<signal uut.vdl_hbb : videl_reg_t>>, "write VDL_HBB");
            elsif run("write VDL_HBE") then
                prepare_test(4);
                check(sv(step).data(videl_reg_t'range) = <<signal uut.vdl_hbe : videl_reg_t >>, "write VDL_HBE");
            elsif run("write VDL_HDB") then
                prepare_test(5);
                check(sv(step).data(videl_reg_t'range) = <<signal uut.vdl_hdb : videl_reg_t >>, "write VDL_HDB");
            elsif run("write VDL_HDE") then
                prepare_test(6);
                check(sv(step).data(videl_reg_t'range) = <<signal uut.vdl_hde : videl_reg_t >>, "write VDL_HDE");
            elsif run("write VDL_HSS") then
                prepare_test(7);
                check(sv(step).data(videl_reg_t'range) = <<signal uut.vdl_hss : videl_reg_t >>, "write VDL_HSS");
            elsif run("write VDL_VFT") then
                prepare_test(8);
                check(sv(step).data(videl_reg_t'range) = <<signal uut.vdl_vft : videl_reg_t >>, "write VDL_VFT");
            elsif run("write VDL_VBB") then
                prepare_test(9);
                check(sv(step).data(videl_reg_t'range) = <<signal uut.vdl_vbb : videl_reg_t >>, "write VDL_VBB");
            elsif run("write VDL_VBE") then
                prepare_test(10);
                check(sv(step).data(videl_reg_t'range) = <<signal uut.vdl_vbe : videl_reg_t >>, "write VDL_VBE");
            elsif run("write VDL_VDB") then
                prepare_test(11);
                check(sv(step).data(videl_reg_t'range) = <<signal uut.vdl_vdb : videl_reg_t >>, "write VDL_VDB");
            elsif run("write VDL_VDE") then
                prepare_test(12);
                check(sv(step).data(videl_reg_t'range) = <<signal uut.vdl_vde : videl_reg_t >>, "write VDL_VDE");
            elsif run("write VDL_VSS") then
                prepare_test(13);
                check(sv(step).data(videl_reg_t'range) = <<signal uut.vdl_vss : videl_reg_t >>, "write VDL_VSS");
            elsif run("write VDL_VMD") then
                prepare_test(14);
                check_equal(sv(step).data(3 downto 0), <<signal uut.vdl_vmd : std_logic_vector(3 downto 0) >>,
                            "write VDL_VMD");
            elsif run("write VDL_VCT") then
                prepare_test(31);
                check_equal(sv(step).data(videl_reg_t'range), <<signal uut.vdl_vct : videl_reg_t >>, "write VDL_VCT");

            -- now write the register, read the register and check for equality
            elsif run("write/read VDL_VCT") then
                prepare_test(31);
                prepare_test(32);
            -- now write the register, read the register and check for equality
                check_equal(d(videl_reg_t'range), sv(step).data(videl_reg_t'range), "write/read VDL_VCT");
            elsif run("write/read VDL_HHT") then
                prepare_test(2);
                prepare_test(15);
                check_equal(d(videl_reg_t'range), sv(step).data(videl_reg_t'range), "write/read VDL_HHT");
            elsif run("write/read VDL_HBB") then
                prepare_test(3);
                prepare_test(16);
                check_equal(d(videl_reg_t'range), sv(step).data(videl_reg_t'range), "write/read VDL_HBB");
            elsif run("write/read VDL_HBE") then
                prepare_test(4);
                prepare_test(17);
                check_equal(d(videl_reg_t'range), sv(step).data(videl_reg_t'range), "write/read VDL_HBE");
            elsif run("write/read VDL_HDB") then
                prepare_test(5);
                prepare_test(18);
                check_equal(d(videl_reg_t'range), sv(step).data(videl_reg_t'range), "write/read VDL_HDB");
            elsif run("write/read VDL_HDE") then
                prepare_test(6);
                prepare_test(19);
                check_equal(d(videl_reg_t'range), sv(step).data(videl_reg_t'range), "write/read VDL_HDE");
            elsif run("write/read VDL_HSS") then
                prepare_test(7);
                prepare_test(20);
                check_equal(d(videl_reg_t'range), sv(step).data(videl_reg_t'range), "write/read VDL_HSS");
            elsif run("write/read VDL_VFT") then
                prepare_test(8);
                prepare_test(21);
                check_equal(d(videl_reg_t'range), sv(step).data(videl_reg_t'range), "write/read VDL_VFT");
            elsif run("write/read VDL_VBB") then
                prepare_test(9);
                prepare_test(22);
                check_equal(d(videl_reg_t'range), sv(step).data(videl_reg_t'range), "write/read VDL_VBB");
            elsif run("write/read VDL_VBE") then
                prepare_test(10);
                prepare_test(23);
                check_equal(d(videl_reg_t'range), sv(step).data(videl_reg_t'range), "write/read VDL_VBE");
            elsif run("write/read VDL_VDB") then
                prepare_test(11);
                prepare_test(24);
                check_equal(d(videl_reg_t'range), sv(step).data(videl_reg_t'range), "write/read VDL_VDB");
            elsif run("write/read VDL_VDE") then
                prepare_test(12);
                prepare_test(25);
                check_equal(d(videl_reg_t'range), sv(step).data(videl_reg_t'range), "write/read VDL_VDE");
            elsif run("write/read VDL_VSS") then
                prepare_test(13);
                prepare_test(26);
                check_equal(d(videl_reg_t'range), sv(step).data(videl_reg_t'range), "write/read VDL_VSS");
            elsif run("write/read VDL_VMD") then
                prepare_test(14);
                prepare_test(27);
                check_equal(d(3 downto 0), sv(step).data(3 downto 0), "write/read VDL_VMD");
            elsif run("write ACP_VCTR") then
                prepare_test(28);
                -- this would fail if we would not mask out the ST/Falcon mode bits as 
                -- ACP VCTR bits 6 & 7 are read only and cannot be written (only indirectly with writing
                -- to the ST Shift or Falcon Shifter registers)
                check_equal(sv(step).data and x"FFFFFF3E",
                            <<signal uut.acp_vctr : addr_t>> and x"FFFFFF3F", "write ACP_VCTR");
            elsif run("write/read ACP_VCTR") then
                prepare_test(28);
                prepare_test(29);
                check_equal(sv(step).data and x"FFFFFF3F",
                            d and x"FFFFFF3F", "write/read ACP_VCTR");
            elsif run("write ST Shifter register") then
                prepare_test(28);       -- write ACP_VCTR first
                prepare_test(14);       -- write VDL_VMD
                prepare_test(31);       -- write VDL_VCT
                prepare_test(30);       -- write ST SHIFT MODE
                -- check if pixel_clk does something now
                check_equal(pixel_clk'quiet(32 ns), false, "check pixel clk toggling");
            elsif run("write Falcon Shifter register") then
                prepare_test(28);       -- write ACP_VCTR first
                prepare_test(14);       -- write VDL_VMD
                prepare_test(31);       -- write VDL_VCT
                prepare_test(30);       -- write ST SHIFT MODE
                prepare_test(31);
                check_equal(pixel_clk'quiet(32 ns), false, "check pixel clk toggling");
            elsif run("set screens pain init") then
                prepare_test(28);       -- write ACP_VCTR first
                prepare_test(9);        -- VDL_VBB
                prepare_test(10);       -- VDL_VBE
                prepare_test(33);       -- VDL_HHT
                prepare_test(34);       -- VDL_HBB
                prepare_test(35);       -- VDL_HBE
                prepare_test(36);       -- VDL_HDB
                prepare_test(37);       -- VDL_HDE
                prepare_test(38);       -- VDL_HSS
                prepare_test(39);       -- VDL_VFT
                prepare_test(40);       -- VDL_VDB
                prepare_test(41);       -- VDL_VBE
                prepare_test(42);       -- VDL_VDB
                prepare_test(43);       -- VDL_VDE
                prepare_test(44);       -- VDL_VSS
                prepare_test(45);       -- STSYNC,
                prepare_test(46);       -- VDL_VCT
                prepare_test(47);       -- SPSHIFT
                prepare_test(48);       -- SPSHIFT
                prepare_test(49);       -- VDL_VMD
                prepare_test(50);       -- VWRAP 
                prepare_test(51);       -- ACP_VCTR disp enable
                prepare_test(52);       -- set monitor type in sys_ctr/MONTYPE
                --
                -- then burn a few cycles (at least one screen line)
                for i in 1 to 700 loop
                    prepare_test(1);
                end loop;
                -- fast forward: inject (near) end of frame values instead of clocking
                -- through thousands of loop iterations

                wait until <<signal uut.last : std_logic>> = '1';
                <<signal uut.vvcnt : videl_reg_t>> <= 
                    -- uut.v_total is the programmed line length
                    force std_logic_vector(unsigned(<<signal uut.v_total : videl_reg_t>>) - 2);
                -- burn cycles to get this forced value into pipeline
                for i in 1 to 100 loop
                    prepare_test(1);
                end loop;
                <<signal uut.vvcnt : videl_reg_t>> <= release;
                for i in 1 to 1700 loop
                    prepare_test(1);
                end loop;

                check_equal(not pixel_clk'quiet(32 ns), true, "check if pixel clk toggles after init");
                check_equal(pixel_clk_frq, 25.00, "check if pixel clk runs at 25 MHz", max_diff => 0.1);
            elsif run("set screens pain 640x400 mono") then
                prepare_test(28);       -- write ACP_VCTR first
                prepare_test(9);        -- VDL_VBB
                prepare_test(10);       -- VDL_VBE
                prepare_test(53);       -- VDL_HHT
                prepare_test(54);       -- VDL_HBB
                prepare_test(55);       -- VDL_HBE
                prepare_test(36);       -- VDL_HDB
                prepare_test(37);       -- VDL_HDE
                prepare_test(38);       -- VDL_HSS
                prepare_test(39);       -- VDL_VFT
                prepare_test(60);       -- VDL_VDB
                prepare_test(61);       -- VDL_VBE
                prepare_test(62);       -- VDL_VDB
                prepare_test(63);       -- VDL_VDE
                prepare_test(64);       -- VDL_VSS
                prepare_test(65);       -- STSYNC,
                prepare_test(66);       -- VDL_VCT
                prepare_test(67);       -- SPSHIFT
                prepare_test(68);       -- SPSHIFT
                prepare_test(69);       -- VDL_VMD
                prepare_test(70);       -- VWRAP 
                prepare_test(51);       -- ACP_VCTR disp enable
                prepare_test(52);       -- set monitor type in sys_ctr/MONTYPE
                --
                -- then burn a few cycles (at least one screen line)
                for i in 1 to 700 loop
                    prepare_test(1);
                end loop;
                -- fast forward: inject (near) end of frame values instead of clocking
                -- through thousands of loop iterations

                wait until <<signal uut.last : std_logic>> = '1';
                <<signal uut.vvcnt : videl_reg_t>> <= 
                    -- uut.v_total is the programmed line length
                    force std_logic_vector(unsigned(<<signal uut.v_total : videl_reg_t>>) - 2);
                -- burn cycles to get this forced value into pipeline
                for i in 1 to 100 loop
                    prepare_test(1);
                end loop;
                <<signal uut.vvcnt : videl_reg_t>> <= release;
                for i in 1 to 1700 loop
                    prepare_test(1);
                end loop;

                check_equal(not pixel_clk'quiet(32 ns), true, "check if pixel clk toggles after init");
                check_equal(pixel_clk_frq, 25.00, "check if pixel clk runs at 25 MHz", max_diff => 0.1);
            end if;
        end loop;

        test_runner_cleanup(runner);
    end process test_runner;
    
    p_main_clk : process
    begin
        main_clk <= not main_clk;
        wait for (30.03 / 2.0) * 1 ns;
    end process p_main_clk;
    
    clk33m <= main_clk;

    p_clk25m : process
    begin
        clk25m <= not clk25m;
        wait for (40.0 / 2.0) * 1 ns;
    end process p_clk25m;
    
    cpu_nextstate : process(all)
    begin
        if rising_edge(main_clk) then
            case cpu_status is
                when S0 =>
                    cpu_status <= S1;
    
                when S1 =>
                    if video_mod_ta = '1' or fb_adr = x"40000000" then  -- avoid waiting for TA if uut isn't meant
                        cpu_status <= S2;
                    end if;

                when S2 =>
                    cpu_status <= S3;

                when S3 =>
                    cpu_status <= S0;
            end case;         
        end if;
    end process cpu_nextstate;

    stimulate : process(all)
    begin
        case cpu_status is
            when S0 =>
                fb_adr <= sv(step).addr;            -- set address
                fb_ad <= sv(step).addr;
            
                -- fb_ale <= '0';                   -- assert FB_ALE (not done here as we already get the address in FB_ADR)
        
                case sv(step).operation is
                    when W => 
                        nFB_WR <= '0';
                        nFB_OE <= '1';
                    when R => 
                        nFB_WR <= '1';
                        nFB_OE <= '0';
                        -- tristate bits that we want the other end of the bus writing,
                        -- leave the others still driven with the address
                        case sv(step).width is
                            when BYTE => fb_ad(31 downto 24) <= (others => 'Z');
                            when WORD => fb_ad(31 downto 16) <= (others => 'Z');
                            when LONG | LINE => fb_ad <= (others => 'Z');
                        end case;
                end case;

            when S1 =>
                -- fb_ale <= '1';                       -- negate FB_ALE (not done here because it's in one of the upstream modules)
                (nFB_CS3, nFB_CS2, nFB_CS1) <= sv(step).fbcs(3 downto 1);
            
                case sv(step).width is
                    when BYTE => (fb_size1, fb_size0) <= std_logic_vector'("01");
                    when WORD => (fb_size1, fb_size0) <= std_logic_vector'("10");
                    when LONG => (fb_size1, fb_size0) <= std_logic_vector'("00");
                    when LINE => (fb_size1, fb_size0) <= std_logic_vector'("11");
                end case;
            
                case sv(step).operation is
                    when W => 
                        nFB_WR <= '0';
                        nFB_OE <= '1';
                        case sv(step).width is
                            when BYTE => 
                                fb_ad <= (31 downto 24 => sv(step).data(7 downto 0), others => 'Z');
                            when WORD =>
                                fb_ad <= (31 downto 16 => sv(step).data(15 downto 0), others => 'Z');
                            when LONG | LINE =>
                                fb_ad <= sv(step).data;
                        end case;
                
                    when R =>
                        nFB_WR <= '1';
                        nFB_OE <= '0';
                        case sv(step).width is
                            when BYTE =>
                                d <= (7 downto 0 => fb_ad(31 downto 24), others => '0');
                            when WORD =>
                                d <= (15 downto 0 => fb_ad(31 downto 16), others => '0');
                            when LONG | LINE =>
                                d <= fb_ad;
                        end case;
                end case;

            when S2 =>
                (nFB_CS1, nFB_CS2, nFB_CS3) <= std_logic_vector'("111");
                
                nFB_OE <= '1';
                nFB_WR <= '1';
        
            when S3 =>
                -- invalidate address
                -- invalidate FB_WRn
        end case;
    end process stimulate;

    uut : entity work.video_mod_mux_clutctr
        port map
        (
            nRSTO           => nRSTO,
            main_clk        => main_clk,
            nFB_CS1         => nFB_CS1,
            nFB_CS2         => nFB_CS2,
            nFB_CS3         => nFB_CS3,
            nFB_WR          => nFB_WR,
            nFB_OE          => nFB_OE,
            fb_size0        => fb_size0,
            fb_size1        => fb_size1,
            nFB_BURST       => nFB_BURST,
            fb_adr          => fb_adr,
    
            clk33m          => clk33m,
            clk25m          => clk25m,
		    
            blitter_run     => blitter_run,
            clk_video       => clk_video,
            vr_d            => vr_d,
            vr_busy         => vr_busy,
    
            color8          => color8,
            acp_clut_rd     => acp_clut_rd,
            color1          => color1,
    
            falcon_clut_rdh => falcon_clut_rdh,
            falcon_clut_rdl => falcon_clut_rdl,
            falcon_clut_wr  => falcon_clut_wr,
            st_clut_rd      => st_clut_rd,
            st_clut_wr      => st_clut_wr,
            clut_mux_adr    => clut_mux_adr,
		    
            hsync           => hsync,
            vsync           => vsync,
            nBLANK          => nBLANK,
            nSYNC           => nSYNC,
            nPD_VGA         => nPD_VGA,
            fifo_rde        => fifo_rde,
            color2          => color2,
            color4          => color4,
            pixel_clk       => pixel_clk,
            clut_off        => clut_off,
            blitter_on      => blitter_on,
    
            video_ram_ctr   => video_ram_ctr,
            video_mod_ta    => video_mod_ta,
		    
            ccr             => ccr,
            ccsel           => ccsel,
            acp_clut_wr     => acp_clut_wr,
            inter_zei       => inter_zei,
            dop_fifo_clr    => dop_fifo_clr,
            video_reconfig  => video_reconfig,
            vr_wr           => vr_wr,
            vr_rd           => vr_rd,
            clr_fifo        => clr_fifo,
            dpzf_clkena     => dpzf_clkena,
            fb_ad           => fb_ad
        );
end architecture sim;

