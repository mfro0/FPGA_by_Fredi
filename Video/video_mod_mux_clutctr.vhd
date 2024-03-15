library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.video_regs.all;

use work.firebee_utils.all;

entity video_mod_mux_clutctr is
    port
    (
        nRSTO               : in std_logic;
        main_clk            : in std_logic;
        nFB_CS1             : in std_logic;
        nFB_CS2             : in std_logic;
        nFB_CS3             : in std_logic;
        nFB_WR              : in std_logic;
        nFB_OE              : in std_logic;
        fb_size0            : in std_logic;
        fb_size1            : in std_logic;
        nFB_BURST           : in std_logic;
        fb_adr              : in std_logic_vector(31 downto 0);

        clk33m              : in std_logic;
        clk25m              : in std_logic;
		
        blitter_run         : in std_logic;
        clk_video           : in std_logic;
        vr_d                : in std_logic_vector(8 downto 0);
        vr_busy             : in std_logic;

        acp_clut_rd         : out std_logic;
        color1              : out std_logic;

        falcon_clut_rdh,
        falcon_clut_rdl     : out std_logic;
        falcon_clut_wr      : out std_logic_vector(3 downto 0);
        st_clut_rd          : out std_logic;
        st_clut_wr          : out std_logic_vector(1 downto 0);
        clut_mux_adr        : out std_logic_vector(3 downto 0);
		
        hsync,
        vsync               : out std_logic;
        nBLANK,
        nSYNC               : out std_logic;
        nPD_VGA             : out std_logic;
        fifo_rde            : out std_logic;
        color2,
        color4,
        color8              : out std_logic;
        pixel_clk           : out std_logic;
        clut_off            : out std_logic_vector(3 downto 0);
        blitter_on          : out std_logic;

        video_ram_ctr       : out std_logic_vector(15 downto 0);
        video_mod_ta        : out std_logic;
		
        ccr                 : out std_logic_vector(23 downto 0);
        ccsel               : out std_logic_vector(2 downto 0);
        acp_clut_wr         : out std_logic_vector(3 downto 0);
        inter_zei           : out std_logic;
        dop_fifo_clr        : out std_logic;
        video_reconfig      : out std_logic;
        vr_wr,
        vr_rd               : out std_logic;
        clr_fifo            : out std_logic;
        dpzf_clkena         : out std_logic;
        fb_ad               : inout std_logic_vector(31 downto 0)
    );
end entity video_mod_mux_clutctr;

architecture rtl of video_mod_mux_clutctr is
    signal clk17m                   : std_logic;
    signal clk13m                   : std_logic;
    signal acp_clut_cs              : boolean;
    signal acp_clut                 : std_logic;
    signal video_pll_config_cs      : boolean;
    signal vr_dout                  : std_logic_vector(8 downto 0);
    signal vr_frq                   : std_logic_vector(7 downto 0);
    signal video_pll_reconfig_cs    : boolean;
    signal falcon_clut_cs           : boolean;
    signal falcon_clut              : std_logic;
    signal st_clut_cs               : boolean;
    signal st_clut                  : std_logic;
    signal st_shift_mode            : std_logic_vector(1 downto 0);
    signal st_shift_mode_cs         : boolean;
    
    signal fb_b                     : std_logic_vector(3 downto 0);
    signal fb_16b                   : std_logic_vector(1 downto 0);
    
    signal falcon_shift_mode        : std_logic_vector(10 downto 0);
    signal falcon_shift_mode_cs     : boolean;
    
    type clut_mux_av_t is array(1 downto 0) of std_logic_vector(3 downto 0);
    signal clut_mux_av              : clut_mux_av_t;
    
    signal acp_vctr_cs              : boolean;
    signal acp_vctr                 : std_logic_vector(31 downto 0);
    alias acp_video                 : std_logic is acp_vctr(0);
    alias dac_on                    : std_logic is acp_vctr(1);
    alias acp_24                    : std_logic is acp_vctr(2);
    alias acp_16                    : std_logic is acp_vctr(3);
    alias acp_8                     : std_logic is acp_vctr(4);
    alias acp_1                     : std_logic is acp_vctr(5);
    alias st_video                  : std_logic is acp_vctr(6);
    alias falcon_video              : std_logic is acp_vctr(7);
    alias acp_vclk                  : std_logic_vector(1 downto 0) is acp_vctr(9 downto 8);
    alias sync                      : std_logic is acp_vctr(15);
    alias rand_ena                  : std_logic is acp_vctr(25);

    signal ccr_cs                   : boolean;
    
    signal sys_ctr                  : std_logic_vector(6 downto 0);
    signal sys_ctr_cs               : boolean;
    
    signal vdl_lof                  : std_logic_vector(15 downto 0);
    signal vdl_lof_cs               : boolean;
    signal vdl_lwd                  : std_logic_vector(15 downto 0);
    signal vdl_lwd_cs               : boolean;
    
    -- control registers
    signal clut_ta                  : std_logic;
    signal hsync_i                  : std_logic_vector(7 downto 0);
    signal hsy_len                  : std_logic_vector(7 downto 0);
    signal hsync_start              : std_logic;
    signal last                     : std_logic;
    signal vsync_i                  : std_logic;
    signal disp_on                  : std_logic := '0';
    signal dpo_zl                   : std_logic := '0';
    signal dpo_on                   : std_logic := '0';
    signal dpo_off                  : std_logic := '0';
    signal vdtron                   : std_logic := '0';
    signal vdo_zl                   : std_logic;
    signal vdo_on                   : std_logic;
    signal vdo_off                  : std_logic;
    signal vhcnt                    : unsigned(12 downto 0);
    signal sub_pixel_cnt            : std_logic_vector(6 downto 0);
    signal vvcnt                    : std_logic_vector(12 downto 0) := (others => '0');
    
    type verz_t is array(10 downto 0) of std_logic_vector(2 downto 0);
    signal verz                     : verz_t := (others => (others => '0'));
    signal rand                     : std_logic_vector(6 downto 0);
    signal rand_on                  : std_logic;
    signal start_zeile              : std_logic;
    signal sync_pix                 : std_logic;
    signal sync_pix1                : std_logic;
    signal sync_pix2                : std_logic;
    signal color16                  : std_logic;
    signal color24                  : std_logic;
    signal te                       : std_logic;
    
    -- horizontal
    signal rand_links               : std_logic_vector(12 downto 0);
    signal rand_rechts              : std_logic_vector(12 downto 0);

    signal hdis_start               : std_logic_vector(12 downto 0);
    signal startp                   : std_logic_vector(12 downto 0);
    signal hdis_end                 : std_logic_vector(12 downto 0);
    signal mulf                     : std_logic_vector(12 downto 0);
    signal hs_start                 : unsigned(12 downto 0);
    signal h_total                  : unsigned(12 downto 0);
    signal hdis_len                 : std_logic_vector(12 downto 0);
    signal wpl                      : std_logic_vector(15 downto 0);
    signal vdl_hht                  : std_logic_vector(12 downto 0);
    signal vdl_hht_cs               : boolean;
    signal vdl_hbe                  : std_logic_vector(12 downto 0);
    signal vdl_hbe_cs               : boolean;
    signal vdl_hdb                  : std_logic_vector(12 downto 0);
    signal vdl_hdb_cs               : boolean;
    signal vdl_hde                  : std_logic_vector(12 downto 0);
    signal vdl_hde_cs               : boolean;
    signal vdl_hbb                  : std_logic_vector(12 downto 0);
    signal vdl_hbb_cs               : boolean;
    signal vdl_hss                  : std_logic_vector(12 downto 0);
    signal vdl_hss_cs               : boolean;

    -- vertical
    signal rand_oben                : std_logic_vector(12 downto 0);
    signal vdis_start               : std_logic_vector(12 downto 0);
    signal vdis_end                 : std_logic_vector(12 downto 0);
    signal rand_unten               : std_logic_vector(12 downto 0);
    signal vs_start                 : std_logic_vector(12 downto 0);
    signal v_total                  : std_logic_vector(12 downto 0);
    
    signal videl_cs                 : boolean;
    signal vdl_vbe                  : std_logic_vector(12 downto 0);
    signal vdl_vbe_cs               : boolean;
    signal vdl_vdb                  : std_logic_vector(12 downto 0);
    signal vdl_vdb_cs               : boolean;
    signal vdl_vde                  : std_logic_vector(12 downto 0);
    signal vdl_vde_cs               : boolean;
    signal vdl_vbb                  : std_logic_vector(12 downto 0);
    signal vdl_vbb_cs               : boolean;
    signal vdl_vss                  : std_logic_vector(12 downto 0);
    signal vdl_vss_cs               : boolean;
    signal vdl_vft                  : std_logic_vector(12 downto 0);
    signal vdl_vft_cs               : boolean;
    signal vdl_vct                  : std_logic_vector(12 downto 0);
    signal vdl_vct_cs               : boolean;
    signal vdl_vmd                  : std_logic_vector(3 downto 0);
    signal vdl_vmd_cs               : boolean;

    signal vdl_bpp_cs               : boolean;
    signal vdl_ph_cs                : boolean;
    signal vdl_pv_cs                : boolean;
    
    signal vdl_hbep                 : std_logic;
    
    signal pixel_clk_i              : std_logic;
    signal tsize                    : std_logic_vector(1 downto 0);
    
    signal w                        : width_t;
    signal fbcs                     : std_logic_vector(0 to 5);
begin
    fbcs <= '1' & nFB_CS1 & nFB_CS2 & nFB_CS3 & '1' & '1';

    byte_selector : entity work.byte_selector
        port map
        (
            fb_adr      => fb_adr,
            tsize       => fb_size1 & fb_size0,
            fb_b        => fb_b,
            fb_16b      => fb_16b
        );

    -- VIDEL cs
    videl_cs <= adr_match(fb_adr, x"FFFF8200", fbcs, 1, 16#FF#);
    acp_clut_cs <=  adr_match(fb_adr, work.video_regs.ACP_CLUT, fbcs, 2, 16#400#);

    acp_clut_rd <= '1' when acp_clut_cs and nFB_OE = '0' else '0';
    acp_clut_wr <= fb_b when acp_clut_cs and nFB_WR = '0' else (others => '0');
    
    -- Falcon clut
    falcon_clut_cs <= adr_match(fb_adr, VDL_CLUT, fbcs, 1, 16#400#);
    falcon_clut_rdh <= '1' when falcon_clut_cs and nFB_OE = '0' and fb_adr(1) = '0' else '0';
    falcon_clut_rdl <= '1' when falcon_clut_cs and nFB_OE = '0' and fb_adr(1) = '1' else '0';
    falcon_clut_wr(1 downto 0) <= fb_16b when fb_adr(1) = '0' and falcon_clut_cs and nFB_WR = '0' else (others => '0');
    falcon_clut_wr(3 downto 2) <= fb_16b when fb_adr(1) = '1' and falcon_clut_cs and nFB_WR = '0'else (others => '0');
    
    -- ST clut
    st_clut_cs <= adr_match(fb_adr, STE_PAL, fbcs, 1, 16#20#);
    st_clut_rd <= '1' when st_clut_cs and nFB_OE = '0' else '0';
    st_clut_wr <= fb_16b when st_clut_cs and nFB_WR = '0' else (others => '0');
    
    st_shift_mode_cs <= adr_match(fb_adr, STSHIFT, fbcs, 1, 2);
    falcon_shift_mode_cs <= adr_match(fb_adr, SPSHIFT, fbcs, 1, 2);
    clut_off <= falcon_shift_mode(3 downto 0) when color4 else (others => '0');
    
    acp_vctr_cs <= adr_match(fb_adr, VCTR, fbcs, 2, 4);
    nPD_VGA <= dac_on;
    
    -- video PLL configuration
    video_pll_config_cs <= adr_match(fb_adr, ACP_PLL_CFG, fbcs, 2, 16#200#) and fb_b(0) = '1' and fb_b(1) = '1'; 
    
    vr_rd <= '1' when video_pll_config_cs and nFB_WR = '1' and vr_busy = '0' else '0';
    
    -- video PLL reconfig
    video_pll_reconfig_cs <= adr_match(fb_adr, ACP_PLL_RECFG, fbcs, 2, 4) and fb_b(0) = '1';
    
    video_ram_ctr <= acp_vctr(31 downto 16);
    
    acp_clut <= (acp_video and (color1 or color8)) or (st_video and color1);
    
    falcon_clut <= falcon_video and not acp_video and not color16;
    st_clut <= st_video and not acp_video and not color1;
    
    pixel_clk <= pixel_clk_i;
    
    -- border colour
    ccr_cs <= adr_match(fb_adr, work.video_regs.CCR, fbcs, 2, 4);
    sys_ctr_cs <= adr_match(fb_adr, MONTYPE, fbcs, 1, 2);

    
    blitter_on <= sys_ctr(3);
    
    vdl_lof_cs <= adr_match(fb_adr, LIN_OFS, fbcs, 1, 2);
    vdl_lwd_cs <= adr_match(fb_adr, VWRAP, fbcs, 1, 2);
    -- FireBee specific R/O register: Bits per Plane
    vdl_bpp_cs <= adr_match(fb_adr, VBPP, fbcs, 1, 2);
    -- FireBee specific R/O register: width in pixels. Doesn't seem to be used anywhere
    vdl_ph_cs <= adr_match(fb_adr, VWPXL, fbcs, 1, 2);
    -- FireBee specific R/O register: height in pixels. Doesn't seem to be used anywhere
    vdl_pv_cs <= adr_match(fb_adr, VHPXL, fbcs, 1, 2);
    
    -- here we are back to original Falcon registers
    vdl_hht_cs <= adr_match(fb_adr, work.video_regs.VDL_HHT, fbcs, 1, 2);
    vdl_hbb_cs <= adr_match(fb_adr, work.video_regs.VDL_HBB, fbcs, 1, 2);
    vdl_hbe_cs <= adr_match(fb_adr, work.video_regs.VDL_HBE, fbcs, 1, 2);
    vdl_hdb_cs <= adr_match(fb_adr, work.video_regs.VDL_HDB, fbcs, 1, 2);
    vdl_hde_cs <= adr_match(fb_adr, work.video_regs.VDL_HDE, fbcs, 1, 2);
    vdl_hss_cs <= adr_match(fb_adr, work.video_regs.VDL_HSS, fbcs, 1, 2);
    
    -- vertical
    
    vdl_vbe_cs <= adr_match(fb_adr, work.video_regs.VDL_VBE, fbcs, 1, 2);
    vdl_vdb_cs <= adr_match(fb_adr, work.video_regs.VDL_VDB, fbcs, 1, 2);
    vdl_vde_cs <= adr_match(fb_adr, work.video_regs.VDL_VDE, fbcs, 1, 2);
    vdl_vbb_cs <= adr_match(fb_adr, work.video_regs.VDL_VBB, fbcs, 1, 2);
    vdl_vss_cs <= adr_match(fb_adr, work.video_regs.VDL_VSS, fbcs, 1, 2);
    vdl_vft_cs <= adr_match(fb_adr, work.video_regs.VDL_VFT, fbcs, 1, 2);
    vdl_vct_cs <= adr_match(fb_adr, work.video_regs.VDL_VCT, fbcs, 1, 2);
    vdl_vmd_cs <= adr_match(fb_adr, work.video_regs.VDL_VMD, fbcs, 1, 2);
    
    -- multiplication factor (not really, anymore)
    mulf <= 13d"1" when not st_video and (te or vdl_vct(0)) else
            13d"2" when not st_video and (not te and not vdl_vct(0)) else
            13d"4" when st_video and te and vdl_vct(0) else
            13d"8" when st_video and ((te and not vdl_vct(0)) or (not te and vdl_vct(0))) else
            13d"16" when st_video and not te and not vdl_vct(0) else
            (others => '0');

    -- width in pixel
    hdis_len <= 13d"320" when te and not acp_video else
                13d"640" when not te and not acp_video else
                std_logic_vector(unsigned(hdis_end) - unsigned(hdis_start) + 1) when acp_video else
                (others => '0');

    wpl <= vdl_lwd when not acp_video else
           (7d"0" & hdis_len(12 downto 4)) when color1 and acp_video else
           (4d"0" & hdis_len(12 downto 1)) when color8 and acp_video else
           (3d"0" & hdis_len) when color16 and acp_video else
           (2d"0" & hdis_len & "0") when color24 and acp_video else
           (others => '0');

    
            
    -- register out
    register_out_p : process(all)
    begin
        -- assert false report "FB_AD = " & to_hstring(fb_ad) severity note;
        if (acp_vctr_cs or ccr_cs or video_pll_config_cs or videl_cs or sys_ctr_cs) and nFB_OE = '0' then
            if st_shift_mode_cs then
                fb_ad(31 downto 16) <= 6d"0" & st_shift_mode & 8x"ff";
            elsif falcon_shift_mode_cs then
                fb_ad(31 downto 16) <= 5d"0" & falcon_shift_mode;
            elsif sys_ctr_cs then
                fb_ad(31 downto 16) <= "100000000" & sys_ctr(6 downto 4) & blitter_run & sys_ctr(2 downto 0);
            elsif vdl_lof_cs then
                fb_ad(31 downto 16) <= vdl_lof;
            elsif vdl_lwd_cs then
                fb_ad(31 downto 16) <= wpl;
            elsif vdl_bpp_cs then
                fb_ad(31 downto 16) <= 11d"0" & color24 & color16 & color8 & color4 & color1;
            elsif vdl_ph_cs then
                fb_ad(31 downto 16) <= 3d"0" & hdis_len;
            elsif vdl_pv_cs then
                fb_ad(31 downto 16) <= 3d"0" & std_logic_vector(unsigned(vdis_end) - unsigned(vdis_start) + 1);
            elsif vdl_hbe_cs then
                fb_ad(31 downto 16) <= 3d"0" & vdl_hbe;
            elsif vdl_hdb_cs then
                fb_ad(31 downto 16) <= 3d"0" & vdl_hdb;
            elsif vdl_hde_cs then
                fb_ad(31 downto 16) <= 3d"0" & vdl_hde;
            elsif vdl_hbb_cs then
                fb_ad(31 downto 16) <= 3d"0" & vdl_hbb;
            elsif vdl_hss_cs then
                fb_ad(31 downto 16) <= 3d"0" & vdl_hss;
            elsif vdl_hht_cs then
                fb_ad(31 downto 16) <= 3d"0" & vdl_hht;
            elsif vdl_vbe_cs then
                fb_ad(31 downto 16) <= 3d"0" & vdl_vbe;
            elsif vdl_vdb_cs then
                fb_ad(31 downto 16) <= 3d"0" & vdl_vdb;
            elsif vdl_vde_cs then
                fb_ad(31 downto 16) <= 3d"0" & vdl_vde;
            elsif vdl_vbb_cs then
                fb_ad(31 downto 16) <= 3d"0" & vdl_vbb;
            elsif vdl_vss_cs then
                fb_ad(31 downto 16) <= 3d"0" & vdl_vss;
            elsif vdl_vft_cs then
                fb_ad(31 downto 16) <= 3d"0" & vdl_vft;
            elsif vdl_vct_cs then
                fb_ad(31 downto 16) <= 3d"0" & vdl_vct;
            elsif vdl_vmd_cs then
                fb_ad(31 downto 16) <= 12d"0" & vdl_vmd;
            elsif acp_vctr_cs then
                fb_ad(31 downto 16) <= acp_vctr(31 downto 16);
            elsif ccr_cs then
                fb_ad(31 downto 16) <= 8d"0" & ccr(23 downto 16);
            elsif video_pll_config_cs then
                fb_ad(31 downto 16) <= 7d"0" & vr_dout;
            elsif video_pll_reconfig_cs then
                fb_ad(31 downto 16) <= vr_busy & "0000" & vr_wr & vr_rd & video_reconfig & x"fa";
            else
                fb_ad(31 downto 16) <= (others => 'Z');
            end if;
        else
            fb_ad(31 downto 16) <= (others => 'Z');
        end if;
        
        if nFB_OE = '0' and (acp_vctr_cs or ccr_cs) then
            if acp_vctr_cs then
                fb_ad(15 downto 0) <= acp_vctr(15 downto 0);
            elsif ccr_cs then
                fb_ad(15 downto 0) <= ccr(15 downto 0);
            else
                fb_ad(15 downto 0) <= (others => 'Z');
            end if;
        else
            fb_ad(15 downto 0) <= (others => 'Z');
        end if;
    end process register_out_p;
    
    video_mod_ta <= '1' when clut_ta = '1' or acp_vctr_cs or sys_ctr_cs or videl_cs else '0';
    
    -- set video output
    
    p_vclk17 : process(all)
    begin
        if rising_edge(clk33m) then
            clk17m <= not clk17m;
        end if; -- rising_edge(clk33m)
    end process p_vclk17;
    
    p_vclk13 : process(all)
    begin
        if rising_edge(clk25m) then
            clk13m <= not clk13m;
        end if; -- rising_edge(clk25m)
    end process p_vclk13;
    
    te <= (vdl_vmd(2) and not vdl_vct(0)) or (not vdl_vmd(2) and vdl_vct(0));
    
    pixel_clk_i <= clk13m    when acp_video = '0' and (falcon_video = '1' or st_video = '1') and vdl_vct(2) = '1' and te = '1' else
                   clk17m    when acp_video = '0' and (falcon_video = '1' or st_video = '1') and vdl_vct(2) = '0' and te = '1' else
                   clk25m    when acp_video = '0' and (falcon_video = '1' or st_video = '1') and vdl_vct(2) = '1' and te = '0' else
                   clk33m    when acp_video = '0' and (falcon_video = '1' or st_video = '1') and vdl_vct(2) = '0' and te = '0' else 
                   clk25m    when acp_video = '1' and acp_vclk = "00" else
                   clk33m    when acp_video = '1' and acp_vclk = "01" else
                   clk_video when acp_video = '1' and acp_vclk = "10" else
                   '0';

    p_shiftmode : process(all)
    begin
        (color1, color2, color4, color8, color16, color24) <= std_logic_vector'("000000");
        if st_video then
            case st_shift_mode is
                when "10" => color1 <= '1'; color2 <= '0'; color4 <= '0';
                when "01" => color2 <= '1'; color1 <= '0'; color4 <= '0';
                when "00" => color4 <= '1'; color1 <= '0'; color2 <= '0';
                when others => null;
            end case;
        end if;
        if falcon_video then
            case falcon_shift_mode is
                when 11x"400" => color1 <= '1'; color4 <= '0'; color8 <= '0'; color16 <= '0';
                when 11x"000" => color4 <= '1'; color1 <= '0'; color8 <= '0'; color16 <= '0';
                when 11x"010" => color8 <= '1'; color1 <= '0'; color4 <= '0'; color16 <= '0';
                when 11x"100" => color16 <= '1'; color1 <= '0'; color4 <= '0'; color8 <= '0';
                when others => null;
            end case;
        end if;
        if acp_video then
            (color1, color8, color16, color24) <= acp_vctr(5 downto 2);
        end if;
    end process p_shiftmode;
    
    p : process(all)
    begin
        if rising_edge(main_clk) then
            clut_ta <= '0';
            
            if st_shift_mode_cs and nFB_WR = '1' and fb_b(0) = '1' then
                st_shift_mode <= fb_ad(25 downto 24);
            end if;
            
            if falcon_shift_mode_cs then
                if not nFB_WR then
                    if fb_b(2) then falcon_shift_mode(10 downto 8) <= fb_ad(26 downto 24); end if;
                    if fb_b(3) then falcon_shift_mode(7 downto 0) <= fb_ad(23 downto 16); end if;
                end if;
            end if;
                
            if (acp_clut_cs or falcon_clut_cs or st_clut_cs) and video_mod_ta = '0' then
                clut_ta <= '1';
            else
                clut_ta <= '0';
            end if;

            if acp_vctr_cs then
                if not nFB_WR then
                    if fb_b(0) then acp_vctr(31 downto 24) <= fb_ad(31 downto 24); end if;
                    if fb_b(1) then acp_vctr(23 downto 16) <= fb_ad(23 downto 16); end if;
                    if fb_b(2) then acp_vctr(15 downto 8) <= fb_ad(15 downto 8); end if;
                    if fb_b(3) then acp_vctr(5 downto 0) <= fb_ad(5 downto 0); end if;
                end if;
            end if;

            -- set ST or Falcon shift mode when write x..shift registers
            if (falcon_shift_mode_cs or st_shift_mode_cs) and nFB_WR = '0' then
                if falcon_shift_mode_cs and nFB_WR = '0' and acp_video = '0' then
                    falcon_video <= '1';
                else falcon_video <= '0';
                end if;
                if st_shift_mode_cs and nFB_WR = '0' and acp_video = '0' then
                    st_video <= '1';
                else
                    st_video <= '0';
                end if;
            end if;
            
            if not vr_busy then
                vr_dout <= vr_d;
            end if;
            
            if vr_wr = '1' and fb_adr(8 downto 0) = 9x"04" then
                vr_frq <= fb_ad(23 downto 16);
            end if;
            
            if video_pll_reconfig_cs and nFB_WR = '0' and vr_busy = '0' and video_reconfig = '0' then
                video_reconfig <= '1';
            else
                video_reconfig <= '0';
            end if;

            
            if ccr_cs and fb_b(1) = '1' and nFB_WR = '0' then
                if ccr_cs and fb_b(1) = '1' and nFB_WR = '0' then
                    ccr(23 downto 16) <= fb_ad(23 downto 16);
                else
                    ccr(23 downto 16) <= (others => '0');
                end if;
                if ccr_cs and fb_b(2) = '1' and nFB_WR = '0' then
                    ccr(15 downto 8) <= fb_ad(15 downto 8);
                else
                    ccr(15 downto 8) <= (others => '0');
                end if;
                if ccr_cs and fb_b(3) = '1' and nFB_WR = '0' then
                    ccr(7 downto 0) <= fb_ad(7 downto 0);
                else
                    ccr(7 downto 0) <= (others => '0');
                end if;                    
            end if;
            
            if sys_ctr_cs and nFB_WR = '0' and fb_b(3) = '1' then
                sys_ctr(6 downto 0) <= fb_ad(22 downto 16);
            end if;
            
            if vdl_lof_cs and nFB_WR = '0' then
                if fb_b(2) then vdl_lof(15 downto 8) <= fb_ad(31 downto 24); end if;
                if fb_b(3) then vdl_lof(7 downto 0) <= fb_ad(23 downto 16); end if;
            end if;
            
            if vdl_lwd_cs and nFB_WR = '0' then
                if fb_b(0) then vdl_lwd(15 downto 8) <= fb_ad(31 downto 24); end if;
                if fb_b(1) then vdl_lwd(7 downto 0) <= fb_ad(23 downto 16); end if;
            end if;
            
            if vdl_hht_cs and nFB_WR = '0' then
                if fb_b(2) then vdl_hht(12 downto 8) <= fb_ad(28 downto 24); end if;
                if fb_b(3) then vdl_hht(7 downto 0) <= fb_ad(23 downto 16); end if;
            end if;
            
            if vdl_hbe_cs and nFB_WR = '0' then
                if fb_b(2) then vdl_hbe(12 downto 8) <= fb_ad(28 downto 24); end if;
                if fb_b(3) then vdl_hbe(7 downto 0) <= fb_ad(23 downto 16); end if;
            end if;
            
            if vdl_hdb_cs and nFB_WR = '0' then
                if fb_b(0) then vdl_hdb(12 downto 8) <= fb_ad(28 downto 24); end if;
                if fb_b(1) then vdl_hdb(7 downto 0) <= fb_ad(23 downto 16); end if;
            end if;
            
            if vdl_hde_cs and nFB_WR = '0' then
                if fb_b(2) then vdl_hde(12 downto 8) <= fb_ad(28 downto 24); end if;
                if fb_b(3) then vdl_hde(7 downto 0) <= fb_ad(23 downto 16); end if;
            end if;
            
            if vdl_hbb_cs and nFB_WR = '0' then
                if fb_b(0) then vdl_hbb(12 downto 8) <= fb_ad(28 downto 24); end if;
                if fb_b(1) then vdl_hbb(7 downto 0) <= fb_ad(23 downto 16); end if;
            end if;
            
            if vdl_hss_cs and nFB_WR = '0' then
                if fb_b(0) then vdl_hss(12 downto 8) <= fb_ad(28 downto 24); end if;
                if fb_b(1) then vdl_hss(7 downto 0) <= fb_ad(23 downto 16); end if;
            end if;
            
            -- vertical
            
            if vdl_vbe_cs and nFB_WR = '0' then
                if fb_b(2) then vdl_vbe(12 downto 8) <= fb_ad(28 downto 24); end if;
                if fb_b(3) then vdl_vbe(7 downto 0) <= fb_ad(23 downto 16); end if;
            end if;
            
            if vdl_vdb_cs and nFB_WR = '0' then
                if fb_b(0) then vdl_vdb(12 downto 8) <= fb_ad(28 downto 24); end if;
                if fb_b(1) then vdl_vdb(7 downto 0) <= fb_ad(23 downto 16); end if;
            end if;
            
            if vdl_vde_cs and nFB_WR = '0' then
                if fb_b(2) then vdl_vde(12 downto 8) <= fb_ad(28 downto 24); end if;
                if fb_b(3) then vdl_vde(7 downto 0) <= fb_ad(23 downto 16); end if;
            end if;
            
            if vdl_vbb_cs and nFB_WR = '0' then
                if fb_b(0) then vdl_vbb(12 downto 8) <= fb_ad(28 downto 24); end if;
                if fb_b(1) then vdl_vbb(7 downto 0) <= fb_ad(23 downto 16); end if;
            end if;
            
            if vdl_vss_cs and nFB_WR = '0' then
                if fb_b(0) then vdl_vss(12 downto 8) <= fb_ad(28 downto 24); end if;
                if fb_b(1) then vdl_vss(7 downto 0) <= fb_ad(23 downto 16); end if;
            end if;
            
            if vdl_vft_cs and nFB_WR = '0' then
                if fb_b(2) then vdl_vft(12 downto 8) <= fb_ad(28 downto 24); end if;
                if fb_b(3) then vdl_vft(7 downto 0) <= fb_ad(23 downto 16); end if;
            end if;
            
            if vdl_vct_cs and nFB_WR = '0' then
                if fb_b(0) then vdl_vct(12 downto 8) <= fb_ad(28 downto 24); end if;
                if fb_b(1) then vdl_vct(7 downto 0) <= fb_ad(23 downto 16); end if;
            end if;
            
            if vdl_vmd_cs and nFB_WR = '0' then
                if fb_b(3) then vdl_vmd <= fb_ad(19 downto 16); end if;
            end if;

            
            -- horizontal sync length in pixel clk
            if acp_video = '0' and (falcon_video = '1' or st_video = '1') then
                case std_logic_vector'(vdl_vct(2) & te) is
                    when "11" => hsy_len <= 8d"19";
                    when "01" => hsy_len <= 8d"25";
                    when "10" => hsy_len <= 8d"38";
                    when "00" => hsy_len <= 8d"50";
                    when others => null;
                end case;
            elsif acp_video = '1' then
                case acp_vclk is
                    when "00" => hsy_len <= 8d"38";
                    when "01" => hsy_len <= 8d"50";
                    when "10" => hsy_len <= vr_frq;
                    when others => null;
                end case;
            end if;
            
            if video_pll_config_cs and nFB_WR = '0' and vr_busy = '0' and vr_wr = '0' then
                vr_wr <= '1';
            else
                vr_wr <= '0';
            end if;
        end if;  -- if rising_edge()
    end process p;
    
    p_pxl: process(all)
    begin
        if rising_edge(pixel_clk_i) then
            if st_clut then ccsel <= "000";
            elsif falcon_clut then ccsel <= "001";
            elsif acp_clut then ccsel <= "100";
            elsif color16 then ccsel <= "101";
            elsif color24 then ccsel <= "110";
            elsif rand_on then ccsel <= "111";
            end if;
            
            if (vvcnt(0) /= vdis_start(0)) and dpzf_clkena = '1' then
                inter_zei <= vdl_vmd(0) and (falcon_video or st_video);
            else
                inter_zei <= '0';
            end if;
            if vhcnt = hs_start and inter_zei = '1' then dop_fifo_clr <= '1'; else dop_fifo_clr <= '0';  end if; -- delete fifo at end of odd lines
            
            -- counters
            if vhcnt = h_total - 1 then
                last <= '1';
            else
                last <= '0';
            end if;

            if not(last) then
                vhcnt <= vhcnt + 1;
            else
                vhcnt <= (others => '0');
            end if;
            
            if last then
                if vvcnt = v_total then
                    vvcnt <= (others => '0');
                else
                    vvcnt <= std_logic_vector(unsigned(vvcnt) + 1);
                end if;
            end if;
            
            -- display on/off
            if (unsigned(vvcnt) > unsigned(rand_oben) and unsigned(vvcnt) <  unsigned(rand_unten)) then
                dpo_zl <= '1';
            else
                dpo_zl <= '0';
            end if;
            
            if vhcnt = unsigned(rand_links) - 1 then
                dpo_on <= '1';
            else
                dpo_on <= '0';
            end if;
            
            if vhcnt = unsigned(rand_rechts) - 2 then
                dpo_off <= '1';
            else
                dpo_off <= '0';
            end if;
            
            disp_on <= (disp_on and not(dpo_off)) or (dpo_on and dpo_zl);
            
            -- data transfer on/off
            if vhcnt = unsigned(hdis_start) - 2 then 
                vdo_on <= '1';
            else
                vdo_on <= '0';
            end if;

            if vhcnt = unsigned(hdis_end) - 1 then
                vdo_off <= '1';
            else
                vdo_off <= '0';
            end if;
            
            if unsigned(vvcnt) > unsigned(vdis_start) and unsigned(vvcnt) <= unsigned(vdis_end) then
                vdo_zl <= '1';
            else
                vdo_zl <= '0';
            end if;
            
            vdtron <= (vdtron and not(vdo_off)) or (vdo_on and vdo_zl);
            
            -- delay and sync
            if vhcnt = hs_start - 2 then
                hsync_start <= '1';
            else
                hsync_start <= '0';
            end if;
            
            if hsync_start then
                hsync_i <= hsy_len;
            else
                if hsync_i /= 8x"0" then
                    hsync_i <= std_logic_vector(unsigned(hsync_i) - 1);
                end if;
            end if;
            
            if vvcnt(12 downto 1) = vs_start(12 downto 1) then
                vsync_i <= '1';
            else
                vsync_i <= '0';
            end if;
            
            for i in 0 to 8 loop
                verz(i + 1) <= verz(i);
            end loop;

            verz(0)(0) <= disp_on;
        
            -- set hsync according to the requested polarity (vdl_vct(6) = '1': positive)
            if (vdl_vct(6) = '0' and hsync_i /= 8d"0") or (vdl_vct(6) = '1' and hsync_i = 8d"0") then
                verz(0)(1) <= '1';
            else
                verz(0)(1) <= '0';
            end if;
            -- set vsync according to the requested polarity (vdl_vct(5) = '1': positive)
            if (vdl_vct(5) = '0' and vsync_i /= '0') or (vdl_vct(5) = '1' and vsync_i = '0') then
                verz(0)(2) <= '1';
            else
                verz(0)(2) <= '0';
            end if;
            nBLANK <= verz(8)(0);
            hsync <= verz(9)(1);
            vsync <= verz(9)(2);
            
            -- fifo clr
            if last then
                if vvcnt = std_logic_vector(unsigned(vdis_end) + 2) then
                    clr_fifo <= '1';
                else
                    clr_fifo <= '0';
                end if;
            end if;
            
            if last then
                if vvcnt = 13d"1" then
                    start_zeile <= '1';
                else
                    start_zeile <= '0';
                end if;
            end if;
            
            if start_zeile then
                if vhcnt = 13d"3" then sync_pix <= '1'; else sync_pix <= '0'; end if;
                if vhcnt = 13d"5" then sync_pix1 <= '1'; else sync_pix1 <= '0'; end if;
                if vhcnt = 13d"7" then sync_pix2 <= '1'; else sync_pix2 <= '0'; end if;
            end if;
            
            if vdtron or sync_pix then
                if not sync_pix then
                    sub_pixel_cnt <= std_logic_vector(unsigned(sub_pixel_cnt) + 1);
                else
                    sub_pixel_cnt <= (others => '0');
                end if;
            end if;
            
            if vdtron then
                if (sub_pixel_cnt(6 downto 0) = 7d"1" and color1 = '1') or
                    (sub_pixel_cnt(5 downto 0) = 6d"1" and color2 = '1') or
                    (sub_pixel_cnt(4 downto 0) = 5d"1" and color4 = '1') or
                    (sub_pixel_cnt(3 downto 0) = 4d"1" and color8 = '1') or
                    (sub_pixel_cnt(2 downto 0) = 3d"1" and color16 = '1') or
                    (sub_pixel_cnt(1 downto 0) = 2d"1" and color24 = '1') then
                    fifo_rde <= '1';
                else
                    fifo_rde <= '0';
                end if;
            elsif sync_pix or sync_pix1 or sync_pix2 then
                fifo_rde <= '1';
            else
                fifo_rde <= '0';
            end if;

            clut_mux_av(0) <= sub_pixel_cnt(3 downto 0);
            clut_mux_av(1) <= clut_mux_av(0);
            clut_mux_adr <= clut_mux_av(1);
            
            -- make border color
            rand(0) <= disp_on and not(vdtron) and acp_vctr(25);
            for i in 0 to 5 loop
                rand(i + 1) <= rand(i);
            end loop;
        end if; -- rising_edge(pixel_clk_i)
    end process p_pxl;

    rand_on <= rand(6);
    nSYNC <= '0';
    dpzf_clkena <= '1' when unsigned(vvcnt) > d"4" else '0';
    
    -- timing horizontal
    startp <= std_logic_vector(unsigned(rand_links) + unsigned(rand_rechts) - unsigned(hdis_len));
    
    rand_links <= vdl_hbe                      when                   acp_video = '1' else
                  vdl_hbe                      when mulf = 13d"1" and acp_video = '0' else
                  vdl_hbe(11 downto 0) & "0"   when mulf = 13d"2" and acp_video = '0' else
                  vdl_hbe(10 downto 0) & "00"  when mulf = 13d"4" and acp_video = '0' else
                  vdl_hbe(9 downto 0) & "000"  when mulf = 13d"8" and acp_video = '0' else
                  vdl_hbe(8 downto 0) & "0000" when mulf = 13d"16" and acp_video = '0' else
                  (others => '0');
                  
    hdis_start <= vdl_hdb when acp_video else
                  std_logic_vector((unsigned(rand_links) + 1)) when not vdl_vct(0) and not acp_video else
                  "0" & std_logic_vector(unsigned(startp(12 downto 1)) + 1) when vdl_vct(0) and not acp_video else
                  (others => '0');
    hdis_end <= vdl_hde when acp_video else
                std_logic_vector(unsigned(hdis_start) + unsigned(hdis_len) - 1) when not acp_video else
                (others => '0');
    rand_rechts <= vdl_hbb when acp_video else
                std_logic_vector(resize((unsigned(vdl_hht) + 2 + unsigned(vdl_hbb)) * unsigned(mulf) + 1, rand_rechts'length)) when not acp_video else
                (others => '0');
    hs_start <= unsigned(vdl_hss) when acp_video else
                resize((unsigned(vdl_hht) + 2 + unsigned(vdl_hss)) * unsigned(mulf) + 1, hs_start'length) when not acp_video else
                (others => '0');
    h_total <= unsigned(vdl_hht) when acp_video else
               resize((unsigned(vdl_hht) + 2) * 2 * unsigned(mulf), h_total'length) when not acp_video else
               (others => '0');
               
    -- timing vertical
    rand_oben <= vdl_vbe                    when acp_video = '1' else
                 "0" & vdl_vbe(12 downto 1) when vdl_vct(0) = '0' and acp_video = '0' else
                 vdl_vbe                    when vdl_vct(0) = '1' and acp_video = '0' else
                 (others => '0');
                 
    vdis_start <= vdl_vdb                                                    when                      acp_video = '1' else
                  "0" & std_logic_vector(unsigned(vdl_vdb(12 downto 1)) + 1) when vdl_vct(0) = '0' and acp_video = '0' else
                  std_logic_vector(unsigned(vdl_vdb) + 1)                    when vdl_vct(0) = '1' and acp_video = '0' else
                  (others => '0');
                  
    vdis_end <= vdl_vde                    when                      acp_video = '1' else
                "0" & vdl_vde(12 downto 1) when vdl_vct(0) = '0' and acp_video = '0' else
                vdl_vde                    when vdl_vct(0) = '1' and acp_video = '0' else
                (others => '0');
                
    rand_unten <= vdl_vbb                                                                         when acp_video = '1' else
                  "0" & std_logic_vector(unsigned(vdl_vbb(12 downto 1)) + 1) when vdl_vct(0) = '0' and acp_video = '0' else
                  std_logic_vector(unsigned(vdl_vbb) + 1)                    when vdl_vct(0) = '1' and acp_video = '0' else
                  (others => '0');
    vs_start <= vdl_vss                    when                      acp_video = '1' else
                "0" & vdl_vss(12 downto 1) when vdl_vct(0) = '0' and acp_video = '0' else
                vdl_vss                    when vdl_vct(0) = '1' and acp_video = '0' else
                (others => '0');
                
    v_total <= vdl_vft                                                    when                      acp_video = '1' else
               "0" & std_logic_vector(unsigned(vdl_vft(12 downto 1)) + 1) when vdl_vct(0) = '0' and acp_video = '0' else
               std_logic_vector(unsigned(vdl_vft) + 1)                    when vdl_vct(0) = '1' and acp_video = '0' else
               (others => '0');
end architecture rtl;
		
