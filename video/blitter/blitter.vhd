library ieee;
use ieee.std_logic_1164.all;

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
           bl_endmaskf.
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
    signal bl_hop           : std_logic(7 downto 0);
    signal bl_op            : std_logic(7 downto 0);
    
    signal bl_ln_cs         : std_logic;
    signal bl_ln_wr         : std_logic;
    signal ln7_clr          : std_logic;
    signal bl_ln            : std_logic_vector(7 downto 0);
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
    signal y_index          : std_logic_vector(15 downto 0);    -- LAUFZEIGER Y COUNT
    signal y_index_clr      : std_logic;
    signal line_nr          : std_logic_vector(3 downto 0);
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
    signal blitter_sig      : std_logic;
    signal blitter_req      : std_logic;
    signal bl_start         : std_logic;
    signal bl_notrun        : std_logic;

-- MAIN STATE MACHINE
    type blitter_state_type is (START, NEW_LINE, RDSRC3, RDSRC2, RDSRC1, RDDST, WRDSTW, WRDST, TESTZEILENENDE, TESTFERTIG, FERTIG);
    signal bl_sm            : blitter_state_type := START;
begin
end architecture rtl;
