library ieee;
use ieee.std_logic_1164.all;

entity video_mod_mux_clutctr_tb is
end entity video_mod_mux_clutctr_tb;

architecture sim of video_mod_mux_clutctr_tb is
    -- module inputs/outputs
    signal nRSTO            : std_ulogic;
    signal main_clk         : std_ulogic := '0';
    signal nFB_CS1          : std_ulogic;
    signal nFB_CS2          : std_ulogic;
    signal nFB_CS3          : std_ulogic;
    signal nFB_WR           : std_ulogic;
    signal nFB_OE           : std_ulogic;
    signal fb_size0         : std_ulogic;
    signal fb_size1         : std_ulogic;
    signal nFB_BURST        : std_ulogic;
    signal fb_adr           : std_ulogic_vector(31 downto 0);

    signal clk33m           : std_ulogic;
    signal clk25m           : std_ulogic;
		
    signal blitter_run      : std_ulogic;
    signal clk_video        : std_ulogic;
    signal vr_d             : std_ulogic_vector(8 downto 0);
    signal vr_busy          : std_ulogic;

    signal color8           : std_ulogic;
    signal acp_clut_rd      : std_ulogic;
    signal color1           : std_ulogic;

    signal falcon_clut_rdh,
           falcon_clut_rdl  : std_ulogic;
    signal falcon_clut_wr   : std_ulogic_vector(3 downto 0);
    signal st_clut_rd       : std_ulogic;
    signal st_clut_wr       : std_ulogic_vector(1 downto 0);
    signal clut_mux_adr     : std_ulogic_vector(3 downto 0);
		
    signal hsync,
           vsync            : std_ulogic;
    signal nBLANK,
           nSYNC            : std_ulogic;
    signal nPD_VGA          : std_ulogic;
    signal fifo_rde         : std_ulogic;
    signal color2,
           color4           : std_ulogic;
    signal pixel_clk        : std_ulogic;
    signal clut_off         : std_ulogic_vector(3 downto 0);
    signal blitter_on       : std_ulogic;

    signal video_ram_ctr    : std_ulogic_vector(15 downto 0);
    signal video_mod_ta     : std_ulogic;
		
    signal ccr              : std_ulogic_vector(23 downto 0);
    signal ccsel            : std_ulogic_vector(2 downto 0);
    signal acp_clut_wr      : std_ulogic_vector(3 downto 0);
    signal inter_zei        : std_ulogic;
    signal dop_fifo_clr     : std_ulogic;
    signal video_reconfig   : std_ulogic;
    signal vr_wr,
           vr_rd            : std_ulogic;
    signal clr_fifo         : std_ulogic;
    signal dpzf_clkena      : std_ulogic;
    signal fb_ad            : std_logic_vector(31 downto 0);

    type state is (S0, S1, S2, S3);
    signal cpu_status       : state := S0;

    type operation_type is (R, W);
    type lane_width_type is (BYTE, WORD, LONG, LINE);
        
    type stim_record is record
        fbcs            : std_ulogic_vector(3 downto 0);
        addr            : std_ulogic_vector(31 downto 0);
        operation       : operation_type;
        data            : std_ulogic_vector(31 downto 0);
        width           : lane_width_type;
    end record;

    type stim_vector_type is array (positive range <>) of stim_record;
    constant stim_vector  : stim_vector_type :=
    (
        ("1101", x"ffff8240", W, 32x"ab", BYTE),
        ("1101", x"ffff8240", R, 32x"ab", BYTE),
        ("1101", x"ffff8260", W, 32x"ab", BYTE),
        ("1101", x"ffff8260", W, 32x"ab", BYTE),
        ("1101", x"ffff8266", W, 32x"ab", WORD),
        ("1101", x"ffff8266", R, 32x"ab", WORD),
        ("1101", x"ffff8210", W, 32x"ab", WORD),
        ("1101", x"ffff8210", R, 32x"ab", WORD),
        ("1101", x"ffff8212", W, 32x"ab", WORD),
        ("1101", x"ffff8212", R, 32x"ab", WORD),
        ("1101", x"ffff8214", W, 32x"ab", BYTE),
        ("1101", x"ffff8214", R, 32x"ab", BYTE),
        ("1101", x"ffff8260", W, 32x"ab", BYTE),
        ("1101", x"ffff8260", R, 32x"ab", BYTE),
        ("1101", x"ffff8260", W, 32x"ab", BYTE),
        ("1101", x"ffff8260", R, 32x"ab", BYTE)
    );

    signal step : positive := 1;
    signal d : std_ulogic_vector(31 downto 0);
begin
    p_main_clk : process
    begin
        main_clk <= not main_clk;
        wait for 30.03 ns;
    end process p_main_clk;

    cpu_statemachine : process(all)
    begin
        if rising_edge(main_clk) then
            case cpu_status is
                when S0 => cpu_status <= S1;
                when S1 => 
                    if video_mod_ta = '1' then
                        cpu_status <= S2;
                    end if;
                when S2 => cpu_status <= S3;
                when S3 =>
                    cpu_status <= S0;
                    step <= step + 1;
            end case;
        end if;
    end process cpu_statemachine;

    cpu_statemachine_worker : process(all)
        -- variable d : std_ulogic_vector(31 downto 0);
    begin
        if step > stim_vector'high then
            std.env.stop(0);
        end if;
        
        case cpu_status is
            when S0 =>
                fb_adr <= stim_vector(step).addr;          -- set address
                
                -- fb_ale <= '0';
                
                case stim_vector(step).operation is
                    when W => nFB_WR <= '0';
                              nFB_OE <= '1';
                    when R => nFB_WR <= '1';
                              nFB_OE <= '0';
                end case;

            when S1 =>
                -- fb_ale <= '1';
                nFB_CS1 <= stim_vector(step).fbcs(1);
                nFB_CS2 <= stim_vector(step).fbcs(2);
                nFB_CS3 <= stim_vector(step).fbcs(3);
                
                case stim_vector(step).width is
                    when BYTE => (fb_size1, fb_size0) <= std_ulogic_vector'("01");
                    when WORD => (fb_size1, fb_size0) <= std_ulogic_vector'("10");
                    when LONG => (fb_size1, fb_size0) <= std_ulogic_vector'("00");
                    when LINE => (fb_size1, fb_size0) <= std_ulogic_vector'("11");
                end case;
                
                case stim_vector(step).operation is
                    when W => nFB_WR <= '0';
                              nFB_OE <= '1';
                    when R => nFB_WR <= '1';
                              nFB_OE <= '0';
                end case;
                
            when S2 =>
                (nFB_CS1, nFB_CS2, nFB_CS3) <= std_ulogic_vector'("111");
                
                fb_ad <= (others => 'Z');
                nFB_OE <= '1';
                nFB_WR <= '1';

            when S3 =>
                if stim_vector(step).operation = R then
                    report "received " & to_hstring(d) & " from uut (stim #" & to_string(step) & ")." severity note;
                else
                    report "sent " & to_hstring(stim_vector(step).data) & " to uut (stim #" & ")." severity note;
                end if;
                
                -- invalidate address
                -- invalidate FB_WRn
        end case;
    end process cpu_statemachine_worker;

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
