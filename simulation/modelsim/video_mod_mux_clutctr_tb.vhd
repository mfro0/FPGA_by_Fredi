-- vunit: run_all_in_same_sim


library ieee;
use ieee.std_logic_1164.all;
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
		
    signal blitter_run      : std_logic;
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
    signal blitter_on       : std_logic;

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
        ("0111", x"40000000", W, 32x"bcd", LONG),
        -- then address our module's components
        --("1101", VDL_HHC, W, 32x"ef0", WORD), -- does not exist
        ("1101", VDL_HHT, W, 32x"123", WORD),
        ("1101", VDL_HBB, W, 32x"456", WORD),
        ("1101", VDL_HBE, W, 32x"789", WORD),
        ("1101", VDL_HDB, W, 32x"abc", WORD),
        ("1101", VDL_HDE, W, 32x"def", WORD),
        ("1101", VDL_HSS, W, 32x"123", WORD),
        --("1101", VDL_HFS, W, 32x"bcd", WORD), -- these do not exist either
        --("1101", VDL_HEE, W, 32x"bcd", WORD),

        --("1101", VDL_HHC, R, 32x"bcd", WORD),
        ("1101", VDL_HHT, R, 32x"123", WORD),
        ("1101", VDL_HBB, R, 32x"456", WORD),
        ("1101", VDL_HBE, R, 32x"789", WORD),
        ("1101", VDL_HDB, R, 32x"abc", WORD),
        ("1101", VDL_HDE, R, 32x"def", WORD),
        ("1101", VDL_HSS, R, 32x"123", WORD)
        --("1101", VDL_HFS, R, 32x"bcd", WORD),
        --("1101", VDL_HEE, R, 32x"bcd", WORD)
    );

    signal step         : positive := 1;
    signal d            : addr_t;

begin    
    test_runner : process
        -- initialize index into stimulation vector and
        -- wait for the ColdFire state machine to complete a full circle state transition
        procedure prepare_test(s : natural) is
        begin
            step <= s;
            for i in S0 to S3 loop
                wait until rising_edge(main_clk);
            end loop;
        end;
    begin
        test_runner_setup(runner, runner_cfg);

        while test_suite loop
            if run("write FPGA memory") then
                prepare_test(1);
                check(true, "expected to always pass");
            elsif run("write VDL_HHT") then
                prepare_test(2);
                report to_hstring(sv(step).data(videl_reg_t'range)) & " vdl_hht=" &
                       to_hstring(<<signal uut.vdl_hht : videl_reg_t>>);
                check(sv(step).data(videl_reg_t'range) = <<signal uut.vdl_hht : videl_reg_t >>, "VDL_HHT");
            elsif run("write VDL_HBB") then
                prepare_test(3);
                report to_hstring(sv(step).data(videl_reg_t'range)) & " vdl_hbb=" &
                       to_hstring(<<signal uut.vdl_hbb : videl_reg_t>>);
                check(sv(step).data(videl_reg_t'range) = <<signal uut.vdl_hbb : videl_reg_t>>, "VDL_HBB");
            elsif run("write VDL_HBE") then
                prepare_test(4);
                report to_hstring(sv(step).data(videl_reg_t'range)) & " vdl_hbe=" &
                       to_hstring(<<signal uut.vdl_hbe : videl_reg_t>>);
                check(sv(step).data(videl_reg_t'range) = <<signal uut.vdl_hbe : videl_reg_t >>, "VDL_HBE");
            elsif run("write VDL_HDB") then
                prepare_test(5);
                report to_hstring(sv(step).data(videl_reg_t'range)) & " vdl_hdb=" &
                       to_hstring(<<signal uut.vdl_hdb : videl_reg_t>>);
                check(sv(step).data(videl_reg_t'range) = <<signal uut.vdl_hdb : videl_reg_t >>, "VDL_HDB");
            elsif run("write VDL_HDE") then
                prepare_test(6);
                report to_hstring(sv(step).data(videl_reg_t'range)) & " vdl_hde=" &
                       to_hstring(<<signal uut.vdl_hde : videl_reg_t>>);
                check(sv(step).data(videl_reg_t'range) = <<signal uut.vdl_hde : videl_reg_t >>, "VDL_HDE");
            elsif run("write VDL_HSS") then
                prepare_test(7);
                report to_hstring(sv(step).data(videl_reg_t'range)) & " vdl_hss=" &
                       to_hstring(<<signal uut.vdl_hss : videl_reg_t>>);
                check(sv(step).data(videl_reg_t'range) = <<signal uut.vdl_hss : videl_reg_t >>, "VDL_HSS");

            -- now write the register, read the register and check for equality
            elsif run("write/read VDL_HHT") then
                prepare_test(2);
                prepare_test(8);
                check(d(videl_reg_t'range) = <<signal uut.vdl_hht : videl_reg_t >>, "VDL_HHT");
            elsif run("write/read VDL_HBB") then
                prepare_test(3);
                prepare_test(9);
                check(d(videl_reg_t'range) = <<signal uut.vdl_hbb : videl_reg_t >>, "VDL_HBB");
            elsif run("write/read VDL_HBE") then
                prepare_test(4);
                prepare_test(10);
                check(d(videl_reg_t'range) = <<signal uut.vdl_hbe : videl_reg_t >>, "VDL_HBE");
            elsif run("write/read VDL_HDB") then
                prepare_test(5);
                prepare_test(11);
                check(d(videl_reg_t'range) = <<signal uut.vdl_hdb : videl_reg_t >>, "VDL_HDB");
            elsif run("write/read VDL_HDE") then
                prepare_test(6);
                prepare_test(12);
                check(d(videl_reg_t'range) = <<signal uut.vdl_hde : videl_reg_t >>, "VDL_HDE");
            elsif run("write/read VDL_HSS") then
                prepare_test(7);
                prepare_test(13);
                check(d(videl_reg_t'range) = <<signal uut.vdl_hss : videl_reg_t >>, "VDL_HSS");
            end if;
        end loop;

        test_runner_cleanup(runner);
    end process test_runner;
    
    p_main_clk : process
    begin
        main_clk <= not main_clk;
        wait for 30.03 ns;
    end process p_main_clk;
    
    clk33m <= main_clk;

    p_clk25m : process
    begin
        clk25m <= not clk25m;
        wait for 40 ns;
    end process p_clk25m;
    
    cpu_nextstate : process(all)
    begin
        if rising_edge(main_clk) then
            report "change cpu_status to " & cpu_state_t'image(cpu_status);
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
        -- s(1 to sv(step).desc.all'length) <= sv(step).desc.all;
        -- s(sv(step).desc.all'length + 1 to s'length) <= (others => ' ');
             
        -- fb_ad <= (others => 'Z');
        
        report "Videl registers read/write";

        -- report "step = " & integer'image(step) & " cpu_status=" & cpu_state_t'image(cpu_status);
        case cpu_status is
            when S0 =>
                report "S0 handling";
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
                report "S1 handling";
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
                --report "video_mod_ta=" & to_string(video_mod_ta);

            when S2 =>
                report "S2 handling";

                if sv(step).addr /= x"40000000" and sv(step).operation = R then
                    report "vdl_hht=" & to_hstring(<<signal uut.vdl_hht : videl_reg_t>>);
                    report "sv(step).data=" & to_hstring(sv(step).data);
                    -- check(sv(step).data = <<signal uut.vdl_hht : videl_reg_t>>, "check failed");
                end if;

                (nFB_CS1, nFB_CS2, nFB_CS3) <= std_logic_vector'("111");
                
                nFB_OE <= '1';
                nFB_WR <= '1';
        
            when S3 =>
                report "S3 handling";
                if sv(step).operation = R then
                    --report "received " & to_hstring(d) &
                        -- " from uut (stim #" & to_string(step) & ")." severity note;
                else
                    --report "sent (" & lane_width_type'image(sv(step).width) & ") " &
                        -- to_hstring(sv(step).data) & " to uut (stim #" & to_string(step) & ")."
                        -- severity note;
                end if;
                
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

