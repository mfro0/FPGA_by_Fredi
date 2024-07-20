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
    signal bl_endmask3      : std_logic_vector(15 downto 0);
    
begin
end architecture rtl;
