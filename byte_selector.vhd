library ieee;
use ieee.std_logic_1164.all;
use work.firebee_utils.all;


entity byte_selector is
    port
    (
        fb_adr          : in adr_t;
        tsize           : in std_logic_vector(1 downto 0);
        fb_b            : out std_logic_vector(3 downto 0);
        fb_16b          : out std_logic_vector(1 downto 0)
    );
end entity byte_selector;

architecture rtl of byte_selector is
    signal w            : width_t;
begin
   p_fb_b : process(all)
    begin
        case tsize is
            when "00" => w <= LONG;
            when "01" => w <= BYTE;
            when "10" => w <= WORD;
            when "11" => w <= LINE;
            when others => null;
        end case;
    end process p_fb_b;
    -- byte select 32 bit
    fb_b(0) <= '1' when fb_adr(1 downto 0) = "00" else '0';                         -- adr = 0              
    fb_b(1) <= '1' when fb_adr(1 downto 0) = "01" else                              -- adr = 1
               '1' when w = WORD and fb_adr(1) = '0' else                           -- high word
               '1' when w = LINE or w = LONG else
               '0';                                                                 -- long and line
    fb_b(2) <= '1' when fb_adr(1 downto 0) = "10" else                              -- adr = 2
               '1' when w = LONG or w = LINE else        -- long and line
               '0';
    fb_b(3) <= '1' when fb_adr(1 downto 0) = "11" else                              -- adr = 3
               '1' when w = WORD and fb_adr(1) = '1' else                           -- low word
               '1' when w = LONG or w = LINE else        -- long and line
               '0';

        -- byte select 16 bit
    fb_16b(0) <= '1' when fb_adr(0) = '0' else '0';
    fb_16b(1) <= '1' when fb_adr(0) = '1' else
                 '1' when w /= BYTE else
                 '0';
end architecture rtl;
    
