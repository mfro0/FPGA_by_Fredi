library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library altera_mf;
use altera_mf.altera_mf_components.all;

entity blitter is
    port
    (
        nRSTO           : in std_logic;
        MAIN_CLK        : in std_logic;
        FB_ALE          : in std_logic;
        nFB_WR,
        nFB_OE,
        FB_SIZE0,
        FB_SIZE1        : in std_logic;
        VIDEO_RAM_CTR   : in std_logic_vector(15 downto 0);
        BLITTER_ON      : in std_logic;
        FB_ADR          : in std_logic_vector(31 downto 0);
        nFB_CS1,
        nFB_CS2,
        nFB_CS3         : in std_logic;
        DDRCLK0         : in std_logic;
        VDP_IN          : in std_logic_vector(63 downto 0);
        BLITTER_DACK    : in std_logic_vector(4 downto 0);
        SR_BLITTER_DACK : in std_logic;
        BLITTER_RUN     : out std_logic;
        BLITTER_INT     : out std_logic;
        BLITTER_DOUT    : out std_logic_vector(127 downto 0);
        BLITTER_ADR     : out std_logic_vector(31 downto 0);
        BLITTER_SIG     : out std_logic;
        BLITTER_WR      : out std_logic;
        BLITTER_TA      : out std_logic;
        FB_AD           : inout std_logic_vector(31 downto 0)
    );
end entity blitter;

architecture rtl of blitter is
    signal byt              : std_logic;
    signal fb_16b           : std_logic_vector(1 downto 0);
    signal blitter_cs,
           bl_hram_cs,
           dp_ram_cs        : std_logic;
    signal bl_hram_be       : std_logic_vector(1 downto 0);
    signal bl_hram_out      : std_logic_vector(15 downto 0);
    signal bl_dpram_out     : std_logic_vector(15 downto 0);
    signal bl_src_x_inc_cs  : std_logic;
    signal bl_src_x_inc     : std_logic_vector(15 downto 0);
    signal src_adr_inc      : std_logic_vector(31 downto 0);
    signal src_xinc32       : std_logic_vector(31 downto 0);
    
    signal bl_src_y_inc_cs  : std_logic;
    signal bl_src_y_inc     : std_logic_vector(31 downto 0);
    signal src_yinc32       : std_logic_vector(31 downto 0);
    
    signal bl_endmask1_cs   : std_logic;
    signal bl_endmask1      : std_logic_vector(15 downto 0);
    signal bl_endmask2_cs   : std_logic;
    signal bl_endmask2      : std_logic_vector(15 downto 0);
    signal bl_endmask3_cs   : std_logic;
    signal bl_endmask3,
           bl_endmask0,
           bl_endmaskf,
           bl_endmaskl,
           bl_endmaskr      : std_logic_vector(15 downto 0);
    
    signal bl_src_adrh_cs,
           bl_src_adrl_cs   : std_logic;
    signal bl_src_adr       : std_logic_vector(31 downto 0);
    signal src_old          : std_logic_vector(27 downto 0);
    signal iaddrh_cs,
           iaddrl_cs        : std_logic;
    signal src_iadr         : std_logic_vector(31 downto 0);
    signal src_iadr_clr     : std_logic;
    signal src_adr32        : std_logic_vector(31 downto 0);
    
    signal bl_dst_x_inc_cs  : std_logic;
    signal bl_dst_x_inc     : std_logic_vector(15 downto 0);
    
    signal dst_adr_inc      : std_logic_vector(31 downto 0);
    signal dst_xinc32       : std_logic_vector(31 downto 0);
    
    signal bl_dst_y_inc_cs  : std_logic;
    signal bl_dst_y_inc     : std_logic_vector(15 downto 0);
    
    signal dst_yinc32       : std_logic_vector(31 downto 0);
    
    signal bl_dst_adrh_cs,
           bl_dst_adrl_cs   : std_logic;
    signal bl_dst_adr       : std_logic_vector(31 downto 0);
    
    signal dst_iadrh_cs     : std_logic;
    signal dsb_iadrl_cs     : std_logic;
    signal dst_iadr         : std_logic_vector(31 downto 0);
    signal dst_iadr_clr     : std_logic;
    signal dst_adr32        : std_logic_vector(31 downto 0);
    
    signal bl_x_cnt_cs      : std_logic;
    signal bl_x_cnt         : std_logic_vector(15 downto 0);
    signal x_cnt16          : std_logic_vector(15 downto 0);
    signal bl_y_cnt_cs      : std_logic;
    signal bl_y_cnt         : std_logic_vector(15 downto 0);
    
    signal bl_hop_cs        : std_logic;
    signal bl_hop           : std_logic_vector(7 downto 0);
    signal bl_op            : std_logic_vector(7 downto 0);
    
    signal bl_ln_cs         : std_logic;
    signal bl_ln_wr         : std_logic;
    signal ln7_clr          : std_logic;
    signal bl_ln            : unsigned(7 downto 0);
    signal bl_skew          : std_logic_vector(7 downto 0);

    -- barrel shifter
    signal dist_right       : std_logic_vector(8 downto 0);
    signal bs_skew          : std_logic_vector(7 downto 0);
    signal bl_bsin          : std_logic_vector(383 downto 0);
    signal bl_bsout         : std_logic_vector(383 downto 0);
    signal shift_dir        : std_logic;
        
    signal bl_src_buf1      : std_logic_vector(127 downto 0);
    signal bl_src_buf2      : std_logic_vector(127 downto 0);
    signal bl_src_buf3      : std_logic_vector(127 downto 0);
    signal bl_dst_bufrd     : std_logic_vector(127 downto 0);

    signal bl_read_dst      : std_logic;            -- LATCH SIGNAL DST BUF RD
    signal bl_read_src      : std_logic;            -- LATCH SIGNAL SRC BUF
    signal src_read         : std_logic;            -- FREIGABE LATCH SIGNAL
    signal not_dst_read     : std_logic;
    signal wren_b           : std_logic;            -- WR ENA HALFTONE RAM
    signal x_index_cs       : std_logic;
    signal x_index          : std_logic_vector(15 downto 0);    -- LAUFZEIGER X COUNT
    signal x_index_clr      : std_logic;            -- X INDEX L�SCHEN CPU WRITE
    signal x_index_clr_dir  : std_logic;            -- X INDEX L�SCHEN STATE MACHINE
    signal dst_x_inc        : std_logic_vector(15 downto 0);    -- ANZAHL WORTE PRO DURCHLAUF
    signal x_cnt_t          : std_logic_vector(15 downto 0);
    signal y_index_cs       : std_logic;
    signal y_index          : unsigned(15 downto 0);    -- LAUFZEIGER Y COUNT
    signal y_index_clr      : std_logic;
    signal line_nr          : unsigned(3 downto 0);
    signal sdxinc           : std_logic;            -- INC INDEX SPALTE
    signal yiinc            : std_logic;            -- INC INDEX ZEILE
    signal zainc            : std_logic;            -- INC ADRESSEN ZEILENUMBRUCH
    signal hop_out          : std_logic_vector(127 downto 0);
    signal op_out           : std_logic_vector(127 downto 0);
    signal endmask1_shift,
           endmask2_shift   : std_logic_vector(7 downto 0);
    signal endmask12_in,
           endmask12_out,
           endmask23_in,
           endmask23_out    : std_logic_vector(143 downto 0);
    signal endmaskm_in,
           endmaskm_out     : std_logic_vector(127 downto 0);

    signal ror_cnt          : std_logic_vector(8 downto 0);
    signal endmask123       : std_logic_vector(127 downto 0);
    signal endmaskend       : std_logic_vector(31 downto 0);
    -- signal blitter_sig      : std_logic;
    signal blitter_req      : std_logic;
    signal bl_start         : std_logic;
    signal bl_notrun        : std_logic;

-- MAIN STATE MACHINE
    type blitter_state_type is (START, NEW_LINE, RDSRC3, RDSRC2, RDSRC1, RDDST, WRDSTW, WRDST, TESTZEILENENDE, TESTFERTIG, FERTIG);
    signal bl_sm            : blitter_state_type := START;
begin
    -- byte and word select 16 bits
    byt <= not fb_size1 and fb_size0;
    fb_16b(0) <= '1' when fb_adr(0) = '0' else '0';
    fb_16b(1) <= '1' when fb_adr(0) = '1' or byt = '0' else '0';
    
    -- blitter cs
    blitter_cs <= '1' when nFB_CS1 = '0' and fb_adr(19 downto 7) = x"1f1f" else '0';    -- x"ff8a00" - x"ff8a7f"
    blitter_ta <= blitter_cs;
    
    -- registers
    -- halftone RAM
    bl_hram_cs <= '1' when nFB_CS1 = '0' and fb_adr(19 downto 5) = x"7c50" else '0';    -- x"ff8a00" - xf"f8a1f"
    bl_hram_be(1) <= bl_hram_cs and fb_16b(0);
    bl_hram_be(0) <= bl_hram_cs and fb_16b(1);
    wren_b <= '0';

    line_nr <= resize(bl_ln + y_index(3 downto 0), line_nr'length) when bl_dst_x_inc < d"0" else
               resize(bl_ln - y_index(3 downto 0), line_nr'length);

    i_altsyncram : altsyncram
        generic map
        (
            ADDRESS_REG_B => "CLOCK1",
            BYTE_SIZE => 8,
            CLOCK_ENABLE_INPUT_A => "BYPASS",
            CLOCK_ENABLE_INPUT_B => "BYPASS",
            CLOCK_ENABLE_OUTPUT_A => "BYPASS",
            CLOCK_ENABLE_OUTPUT_B => "BYPASS",
            INDATA_REG_B => "CLOCK1",
            INTENDED_DEVICE_FAMILY => "Cyclone III",
            LPM_TYPE => "altsyncram",
            NUMWORDS_A => 16,
            NUMWORDS_B => 16,
            OPERATION_MODE => "BIDIR_DUAL_PORT",
            OUTDATA_ACLR_A => "NONE",
            OUTDATA_ACLR_B => "NONE",
            OUTDATA_REG_A => "UNREGISTERED",
            OUTDATA_REG_B => "UNREGISTERED",
            POWER_UP_UNINITIALIZED => "FALSE",
            READ_DURING_WRITE_MODE_PORT_A => "NEW_DATA_WITH_NBE_READ",
            READ_DURING_WRITE_MODE_PORT_B => "NEW_DATA_WITH_NBE_READ",
            WIDTHAD_A => 4,
            WIDTHAD_B => 4,
            WIDTH_A => 16,
            WIDTH_B => 16,
            WIDTH_BYTEENA_A => 2,
            WIDTH_BYTEENA_B => 1,
            WRCONTROL_WRADDRESS_REG_B => "CLOCK1"
        )
        port map
        (
            address_a => fb_adr(4 downto 1),
            address_b => std_logic_vector(line_nr),
            byteena_a => bl_hram_be,
            clock0 => MAIN_CLK,
            clock1 => ddrclk0,
            data_a => fb_ad(31 downto 16),
            wren_a => bl_hram_cs and not nFB_WR,
            wren_b => wren_b,
            q_a => bl_dpram_out,
            q_b => bl_hram_out            
        );
    -- FIXME: (bl_dpram_out, bl_hram_out) <= altsyncram(fb_adr(4 downto 1), line_nr, bl_hram_be, main_clk, ddrclk0, fb_ad(31 downto 16), bl_hram_cs and not nFB_WR, wren_b);
    
    -- until we have something more reasonable:
    blitter_run <= '0';
    blitter_int <= '0';
    blitter_dout <= (others => '0');
    blitter_adr <= (others => '0');
    blitter_sig <= '0';
    blitter_wr <= '0';
    blitter_ta <= '0';
    
    
end architecture rtl;
