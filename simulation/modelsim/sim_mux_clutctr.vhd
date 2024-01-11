library ieee;
use ieee.std_logic_1164.all;
use std.textio.all;

entity video_mod_mux_clutctr_tb is
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
    signal clk25m           : std_logic;
		
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

    type state is (S0, S1, S2, S3);
    signal cpu_status       : state := S3;

    type operation_type is (R, W);
    type lane_width_type is (BYTE, WORD, LONG, LINE);
    type str is access string;
    type stim_record is record
        fbcs            : std_logic_vector(3 downto 0);
        addr            : std_logic_vector(31 downto 0);
        operation       : operation_type;
        data            : std_logic_vector(31 downto 0);
        width           : lane_width_type;
        desc            : str;
    end record;

    type stim_vector_type is array (positive range <>) of stim_record;
    -- signal stim_vector  : stim_vector_type(1 to 18);
    signal step : positive := 1;
    signal d : std_logic_vector(31 downto 0);
    signal s  : string(1 to 40);
        
begin    
    p_main_clk : process
    begin
        main_clk <= not main_clk;
        wait for 30.03 ns;
    end process p_main_clk;

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
                    step <= step + 1;
            end case;         
        end if;
    end process cpu_nextstate;
    
    cpu_statemachine : process(all)
        -- variable d : std_logic_vector(31 downto 0);
        variable sv : stim_vector_type(1 to 20);
    begin
        sv :=
        (
            -- first do a few cycles of nothing relevant to the controller
            ("0111", x"40000000", W, 32x"ff", WORD, new string'("DUMMY")),
            ("0111", x"40000000", R, 32x"ff", WORD, new string'("DUMMY")),
            -- then address our module's components
            ("1011", x"f0000100", W, 32x"12345678", LONG, new string'("ACP vctr")),
            ("1011", x"f0000100", R, 32x"12345678", LONG, new string'("ACP vctr")),
            ("1101", x"ffff8260", W, 32x"ab", WORD, new string'("ST Shift Mode")),
            ("1101", x"ffff8260", R, 32x"ab", WORD, new string'("ST Shift Mode")),
            ("1101", x"ffff8266", W, 32x"ef", WORD, new string'("Falcon Shift MODE")),
            ("1101", x"ffff8266", R, 32x"ef", WORD, new string'("Falcon Shift MODE")),
            ("1101", x"ffff8210", W, 32x"01", WORD, new string'("Falcon Videl LWD")),
            ("1101", x"ffff8210", R, 32x"01", WORD, new string'("Falcon Videl LWD")),
            ("1101", x"ffff82C2", W, 32x"cdef", WORD, new string'("vdl_vmd")),
            ("1101", x"ffff82C2", R, 32x"cd", WORD, new string'("vdl_vmd")),
            ("1101", x"ffff828a", W, 32x"23", WORD, new string'("vdl_hde")),
            ("1101", x"ffff828a", R, 32x"23", WORD, new string'("vdl_hde")),
            ("1101", x"ffff8282", W, 32x"34", WORD, new string'("vdl_hht")),
            ("1101", x"ffff8282", R, 32x"34", WORD, new string'("vdl_hht")),
            ("1101", x"ffff8286", W, 32x"56", WORD, new string'("vdl_hbe")),
            ("1101", x"ffff8286", R, 32x"56", WORD, new string'("vdl_hbe")),
            ("1101", x"ffff8288", W, 32x"78", WORD, new string'("vdl_hdb")),
            ("1101", x"ffff8288", R, 32x"78", WORD, new string'("vdl_hdb"))
        );

        if step > sv'high then                      -- stop if stim vector exhausted
            std.env.stop(0);
        end if;

        s(1 to sv(step).desc.all'length) <= sv(step).desc.all;
        s(sv(step).desc.all'length + 1 to s'length) <= (others => ' ');
             
        -- fb_ad <= (others => 'Z');
        
        case cpu_status is
            when S0 =>
                fb_adr <= sv(step).addr;            -- set address
                
                -- fb_ale <= '0';                   -- assert FB_ALE (not done here as we already get the address in FB_ADR)
               
                case sv(step).operation is
                    when W => 
                        nFB_WR <= '0';
                        nFB_OE <= '1';
                    when R => 
                        nFB_WR <= '1';
                        nFB_OE <= '0';
                        -- tristate bits that we want the other end of the bus writing
                        case sv(step).width is
                            when BYTE => fb_ad(31 downto 24) <= (others => 'Z');
                            when WORD => fb_ad(31 downto 16) <= (others => 'Z');
                            when LONG | LINE => fb_ad <= (others => 'Z');
                        end case;
                end case;

            when S1 =>
                -- fb_ale <= '1';                       -- negate FB_ALE (not done here because it's in one of the upstream modules)
                nFB_CS1 <= sv(step).fbcs(1);
                nFB_CS2 <= sv(step).fbcs(2);
                nFB_CS3 <= sv(step).fbcs(3);
              
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
                                d <= (7 downto 0 => fb_ad(31 downto 24), others => '0');
                            when WORD =>
                                fb_ad <= (31 downto 16 => sv(step).data(15 downto 0), others => 'Z');
                                d <= (15 downto 0 => fb_ad(31 downto 16), others => '0');
                            when LONG | LINE =>
                                fb_ad <= sv(step).data;
                                d <= fb_ad;
                        end case;
                   
                    when R =>
                        nFB_WR <= '1';
                        nFB_OE <= '0';
                end case;
                    

            when S2 =>
                (nFB_CS1, nFB_CS2, nFB_CS3) <= std_logic_vector'("111");
                
                nFB_OE <= '1';
                nFB_WR <= '1';
            
            when S3 =>
                if sv(step).operation = R then
                    report "received " & sv(step).desc.all & ": " & to_hstring(d) & " from uut (stim #" & to_string(step) & ")." severity note;
                else
                    report "sent (" & lane_width_type'image(sv(step).width) & ") " & sv(step).desc.all & ": " &
                           to_hstring(sv(step).data) & " to uut (stim #" & to_string(step) & ")." severity note;
                end if;
                
                -- invalidate address
                -- invalidate FB_WRn
        end case;
    end process cpu_statemachine;

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
