library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ddr_ctr is
    port
    (
        FB_ADR      : in std_logic_vector(31 downto 0);
        nFB_CS1,
        nFB_CS2,
        nFB_CS3     : in std_logic;
        nFB_OE      : in std_logic;
        FB_SIZE0,
        FB_SIZE2    : in std_logic;
        nRSTO       : in std_logic;
        MAIN_CLK    : in std_logic;
        FB_ALE      : in std_logic;
        nFB_WR      : in std_logic;
        DDR_SYNC_66M    : in std_logic;
        CLR_FIFO        : in std_logic;
        VIDEO_RAM_CTR   : in std_logic_vector(15 downto 0);
        BLITTER_ADR     : in std_logic_vector(15 downto 0);
        BLITTER_SIG     : in std_logic;
        BLITTER_WR      : in std_logic;
        DDRCLK0         : in std_logic;
        CLK33M          : in std_logic;
        FIFO_MW         : in std_logic;
        VA              : out std_logic_vector(12 downto 0);
        nVWE            : out std_logic;
        nVRAS           : out std_logic;
        nVCS            : out std_logic;
        VCKE            : out std_logic;
        nVCAS           : out std_logic;
        FB_LE           : out std_logic_vector(3 downto 0);
        FB_VDOE         : out std_logic_vector(3 downto 0);
        SR_FIFO_WRE     : out std_logic;
        SR_DDR_FB       : out std_logic;
        SR_DDR_WR       : out std_logic;
        SR_DDRWR_D_SEL  : out std_logic;
        SR_VDMP         : out std_logic_vector(7 downto 0);
        VIDEO_DDR_TA    : out std_logic;
        SR_BLITTER_DTACK    : out std_logic;
        BA              : out std_logic_vector(1 downto 0);
        DDRWR_D_SEL1    : out std_logic;
        VDM_SEL         : out std_logic_vector(3 downto 0);
        FB_AD           : inout std_logic_vector(31 downto 0)
    );
end entity DDR_CTR;

architecture rtl of DDR_CTR is
    type fb_regddr is (FR_WAIT, FR_S0, FR_S1, FR_S2, FR_S3);
    type ddr_sm is (DS_T1, DS_T2A, DS_T2B, DS_T3, DS_N5, DS_N6, DS_N7, DS_N8,                       -- START (NORMAL 8 CYCLES TOTAL = 60ns)
                           DS_C2, DS_C3, DS_C4, DS_C5, DS_C6, DS_C7,                                -- CONFIG
                                         DS_T4R, DS_T5R,                                            -- READ CPU UND BLITTER,
                                         DS_T4W, DS_T5W, DS_T6W, DS_T7W, DS_T8W, DS_T9W,            -- WRITE CPU UND BLITTER
                                         DS_T4F, DS_T5F, DS_T6F, DS_T7F, DS_T8F, DS_T9F, DS_T10F,   -- READ FIFO
                                                         DS_CB6,       DS_CB8,                      -- CLOSE FIFO BANK
                           DS_R2, DS_R3, DS_R4, DS_R5, DS_R6);

    signal fb_b                         : std_ulogic_vector(3 downto 0);
    signal vcas,
           vras                         : std_ulogic;
    signal vwe                          : std_ulogic;
    signal va_p                         : std_ulogic_vector(12 downto 0);
    signal ba_p                         : std_ulogic_vector(1 downto 0);
    signal va_s                         : std_ulogic_vector(12 downto 0);
    signal ba_s                         : std_ulogic_vector(1 downto 0);
    signal mcs                          : std_ulogic_vector(1 downto 0);
    signal cpu_ddr_sync                 : std_ulogic;
    signal ddr_sel                      : std_ulogic;
    signal ddr_cs                       : std_ulogic;
    signal ddr_config                   : std_ulogic;
    signal sr_ddr_wr                    : std_ulogic;
    signal sr_ddrwr_d_sel               : std_ulogic;
    signal sr_vdmp                      : std_ulogic_vector(7 downto 0);
    signal cpu_row_adr                  : std_ulogic_vector(12 downto 0);
    signal cpu_ba                       : std_ulogic_vector(1 downto 0);
    signal cpu_col_adr                  : std_ulogic_vector(9 downto 0);
    signal cpu_sig                      : std_ulogic;
    signal cpu_reg                      : std_ulogic;
    signal cpu_ac                       : std_ulogic;
    signal bus_cyc                      : std_ulogic;
    signal bus_cyc_end                  : std_ulogic;
    signal blitter_req                  : std_ulogic;
    signal blitter_ac                   : std_ulogic;
    signal blitter_row_adr              : std_ulogic_vector(12 downto 0);
    signal blitter_ba                   : std_ulogic_vector(1 downto 0);
    signal blitter_col_adr              : std_ulogic_vector(9 downto 0);
    signal fifo_req                     : std_ulogic;
    signal fifo_ac                      : std_ulogic;
    signal fifo_row_adr                 : std_ulogic_vector(12 downto 0);
    signal fifo_ba                      : std_ulogic_vector(1 downto 0);
    signal fifo_col_adr                 : std_ulogic_vector(9 downto 0);
    signal fifo_active                  : std_ulogic;
    signal clr_fifo_sync,
           clear_fifo_cnt               : std_ulogic;
    signal stop                         : std_ulogic;
    signal sr_fifo_wre                  : std_ulogic;
    signal fifo_bank_ok,
           fifo_bank_not_ok             : std_ulogic;
    signal ddr_refresh_on               : std_ulogic;
    signal ddr_refresh_cnt              : std_ulogic_vector(10 downto 0);
    signal ddr_refresh_req              : std_ulogic;
    signal ddr_refresh_sig              : std_ulogic_vector(3 downto 0);
    signal refresh_time                 : std_ulogic;
    signal video_base_l_d               : std_ulogic_vector(7 downto 0);
    signal video_base_l                 : std_ulogic;
    signal video_base_m_d               : std_ulogic_vector(7 downto 0);
    signal video_base_m                 : std_ulogic;
    signal video_base_m_d               : std_ulogic_vector(7 downto 0);
    signal video_base_m                 : std_ulogic;
    signal video_base_h_d               : std_ulogic_vector(7 downto 0);
    signal video_base_h                 : std_ulogic;
    signal video_base_x_d               : std_ulogic_vector(2 downto 0);
    signal video_adr_cnt                : std_ulogic_vector(22 downto 0);
    signal video_cnt_l,
           video_cnt_m,
           video_cnt_h                  : std_ulogic;
    signal video_base_adr               : std_ulogic_vector(22 downto 0);
    signal video_act_adr                : std_ulogic_vector(26 downto 0);
    
    signal fb_siz                       : std_ulogic_vector(1 downto 0);
    
    constant siz_long : std_ulogic_vector(1 downto 0) := "00";
    constant siz_byte : std_ulogic_vector(1 downto 0) := "01";
    constant siz_word : std_ulogic_vector(1 downto 0) := "10";
    constant siz_line : std_ulogic_vector(1 downto 0) := "11";
begin
    fb_siz <= fb_size1 & fb_size0;
    fline <= fb_size0 and fb_size1;
    
    -- Byte selects
    fb_b0 <= '1' when fb_adr(1 downto 0) = "00" else
             '1' when fb_siz = SIZ_LONG else
             '1' when fb_siz = SIZ_LINE else
             '0';
    fb_b1 <= '1' when fb_adr(1 downto 0) = "01" else
             '1' when fb_siz = SIZ_WORD and fb_adr(1) = '0' else
             '1' when fb_siz = SIZ_LONG else
             '1' when fb_siz = SIZ_LINE else
             '0';
    fb_b2 <= '1' when fb_adr(1 downto 0) = "10" else
             '1' when fb_siz = SIZ_LONG else
             '1' when fb_siz = SIZ_LINE else
             '0';
    fb_b3 <= '1' when fb_adr(1 downto 0) = "11" else
             '1' when fb_siz = SIZ_WORD and fb_adr(1) = '1' else
             '1' when fb_siz = SIZ_LONG else
             '0';

    p_cpu_rw : process
    begin
        wait until rising_edge(MAIN_CLK);

        case fb_regddr is
            when FR_WAIT =>
                fb_le(0) <= not nFB_WR;
                if bus_cyc = '1' or ddr_sel = '1' or fline = '1' or nFB_WR = '0' then
                    fb_regddr <= FR_S0;
                end if;
            
            when FR_S0 =>
                if ddr_cs = '1' then
                    fb_le(0) <= not nFB_WR;
                    video_ddr_ta <= '1';
                    if fline = '1' then
                        vb_vdoe(0) <= not nFB_OE or not ddr_config = '1';
                        fb_regddr <= FR_S1;
                    end if;
                else
                    fb_regddr <= FR_WAIT;
                end if;
            
            when FR_S1 =>
                if ddr_cs = '1' then
                    fb_vdoe(1) <= not nFB_OE and not ddr_config = '1';
                    fb_le(1) <= not nFB_WR;
                    video_ddr_ta <= '1';
                    fb_regddr <= FR_S2;
                else
                    fb_regddr <= FR_WAIT;
                end if;
            
            when FR_S2 =>
                if ddr_cs = '1' then
                    fb_vdoe(2) <= not nFB_OE and not ddr_config;
                    fb_le(2) <= not nFB_WR;
                    if bus_cyc = '0' and fline = '1' and nFB_WR = '0' then
                        fb_regddr <= FR_S2;
                    else
                        video_ddr_ta <= '1';
                        fb_regddr <= FR_S3;
                    end if;
                else
                    fb_regddr <= FR_WAIT;
                end if;

            when FR_S3 =>
                if ddr_cs = '1' then
                    fb_vdoe(3) <= not nFB_OE and not MAIN_CLK and not ddr_config;
                    fb_le(3) <= not nFB_WR;
                    video_ddr_ta <= '1';
                    bus_cyc_end <= '1';
                    fb_regddr <= FR_WAIT;
                else
                    fb_regddr <= FR_WAIT;
                end if;
        end case;
    end process p_cpu_rw;
    
    vcke <= VIDEO_RAM_CTR(0);
    nVCS <= not VIDEO_RAM_CTR(1);
    ddr_refresh_on <= VIDEO_RAM_CTR(2);
    ddr_config <= VIDEO_RAM_CTR(3);
    fifo_active <= VIDEO_RAM_CTR(8);

    -- CPU
    cpu_row_adr <= FB_ADR(26 downto 14);
    cpu_ba <= FB_ADR(13 downto 12);
    cpu_col_adr <= FB_ADR(11 downto 2);

    p_cpu_ac : process
    begin
        wait until rising_edge(ddrclk(0));
        
        bus_cyc <= bus_cyc and not bus_cyc_end;
    end process p_cpu_ac;
    
    cpu_sig <= '1' when ddr_sel = '1' and (nFB_WR = '0' and fline = '0') and ddr_config = '0' else
               '1' when ddr_sel = '1' and ddr_config = '0' else
               '1' when fb_regddr = FR_S1 and nFB_WR = '0' else
               '0';

    p_cpu_req : process
    begin
        wait until rising_edge(DDR_SYNC_66M);
        cpu_req <= '0';
        if cpu_sig = '1' then cpu_req <= '1'; else cpu_sig <= '0'; end if;
        if cpu_req = '1' and fb_regddr /= FR_S1 and fb_regddr /= FR_S3 and not bus_cyc_end and not bus_cyc then
            cpu_req <= '1';
        else
            cpu_req <= '0';
        end if;
    end process p_cpu_req;
    
    ddr_sel <= '1' when fb_ale = '1' and fb_ad(31 downto 30) = "01" else
               '0';

    p_ddr_cs : process
    begin
        wait until rising_edge(MAIN_CLK);
        if fb_ale then
        end if;
    end process p_ddr_cs;
    
end architecture rtl;
