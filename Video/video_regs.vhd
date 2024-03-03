library ieee;
use ieee.std_logic_1164.all;
use ieee.math_real.all;

package video_regs is
    subtype addr_t is std_logic_vector(31 downto 0);

    constant MONTYPE    : addr_t := x"FFFF8006";   -- monitor type
    constant STSYNC     : addr_t := x"FFFF820A";   -- ST Sync mode
    constant LIN_OFS    : addr_t := x"FFFF820E";   -- offset to next line
    constant VWRAP      : addr_t := x"FFFF8210";   -- linewidth in words
    constant VBPP       : addr_t := x"FFFF8210";   -- Firebee only: bits per plane, R/O
    constant VWPXL      : addr_t := x"FFFF8212";   -- Firebee only: width in pixels, R/O
    constant VHPXL      : addr_t := x"FFFF8214";   -- Firebee only: height in pixels, R/O

    constant STE_PAL    : addr_t := x"FFFF8240";   -- STE compatible palette registers 0-15
                                                   -- ends at  x"FFFF825E" (64 bytes)
    constant STSHIFT    : addr_t := x"FFFF8260";   -- ST compatible shifter control register

    constant HSCROLL    : addr_t := x"FFFF8265";   -- STE compatible horizontal scroll register

    constant SPSHIFT    : addr_t := x"FFFF8266";   -- Falcon shift register

    constant VDL_HHC    : addr_t := x"FFFF8280";   -- horizontal hold counterA (read only) -- FIXME: not implemented in HDL
    constant VDL_HHT    : addr_t := x"FFFF8282";   -- horizontal half line total
    constant VDL_HBB    : addr_t := x"FFFF8284";   -- horizontal blank begin
    constant VDL_HBE    : addr_t := x"FFFF8286";   -- horizontal blank end
    constant VDL_HDB    : addr_t := x"FFFF8288";   -- horizontal display begin
    constant VDL_HDE    : addr_t := x"FFFF828A";   -- horizontal display end
    constant VDL_HSS    : addr_t := x"FFFF828C";   -- horizontal sync start
    constant VDL_HFS    : addr_t := x"FFFF828E";   -- horizontal field sync (FIXME: not implemented in HDL)
    constant VDL_HEE    : addr_t := x"FFFF8290";   -- horizontal equalization end (FIXME: not implemented in HDL)

    constant VDL_VFC    : addr_t := x"FFFF82A0";   -- vertical frequency counter (read only) -- FIXME: not implemented in HDL
    constant VDL_VFT    : addr_t := x"FFFF82A2";   -- vertical field total
    constant VDL_VBB    : addr_t := x"FFFF82A4";   -- vertical blank begin
    constant VDL_VBE    : addr_t := x"FFFF82A6";   -- vertical blank end
    constant VDL_VDB    : addr_t := x"FFFF82A8";   -- vertical display begin 0 and 1
    constant VDL_VDE    : addr_t := x"FFFF82AA";   -- vertical display end 0 and 1
    constant VDL_VSS    : addr_t := x"FFFF82AC";   -- vertical sync start

    constant VDL_VCT    : addr_t := x"FFFF82C0";   -- Video Master Control
    constant VDL_VMD    : addr_t := x"FFFF82C2";   -- Falcon Video Control

    constant VDL_CLUT   : addr_t := x"FFFF8900";   -- Falcon palette (1024 bytes)

    -- ACP registers
    constant ACP_CLUT           : addr_t := x"F0000000";        -- ACP color lookup table. 1024 bytes
    constant VCTR               : addr_t := x"F0000400";
    constant CCR                : addr_t := x"F0000404";
    constant ACP_PLL_CFG        : addr_t := x"F0000600";        -- PLL config x"200" bytes
    constant ACP_PLL_RECFG      : addr_t := x"F0000800";        -- bit 31 = '1' = busy

    --
    subtype fbcs_t is std_logic_vector(5 downto 0);

    type fb_cs_rec_t is record
        base_address            : addr_t;
        address_mask            : addr_t;
        control_reg             : addr_t;
    end record;
    type fb_cs_array_t is array (natural range <>) of fb_cs_rec_t;

    -- Bit definitions and macros for MCF_FBCS_CSMR 
    --
    -- adapted from MCF5475_FBCS.h

    -- #define MCF_FBCS_CSMR_BAM(x)                 (((x)&0xFFFF)<<0x10)
    constant CSMR_BAM_4G        : addr_t := x"FFFF0000";
    constant CSMR_BAM_2G        : addr_t := x"7FFF0000";
    constant CSMR_BAM_1G        : addr_t := x"3FFF0000";
    constant CSMR_BAM_1024M     : addr_t := x"3FFF0000";
    constant CSMR_BAM_512M      : addr_t := x"1FFF0000";
    constant CSMR_BAM_256M      : addr_t := x"0FFF0000";
    constant CSMR_BAM_128M      : addr_t := x"07FF0000";
    constant CSMR_BAM_64M       : addr_t := x"03FF0000";
    constant CSMR_BAM_32M       : addr_t := x"01FF0000";
    constant CSMR_BAM_16M       : addr_t := x"00FF0000";
    constant CSMR_BAM_8M        : addr_t := x"007F0000";
    constant CSMR_BAM_4M        : addr_t := x"003F0000";
    constant CSMR_BAM_2M        : addr_t := x"001F0000";
    constant CSMR_BAM_1M        : addr_t := x"000F0000";
    constant CSMR_BAM_1024K     : addr_t := x"000F0000";
    constant CSMR_BAM_512K      : addr_t := x"00070000";
    constant CSMR_BAM_256K      : addr_t := x"00030000";
    constant CSMR_BAM_128K      : addr_t := x"00010000";
    constant CSMR_BAM_64K       : addr_t := x"00000000";


    -- these must match settings in sysinit.c (FireBee ColdFire firmware)
    --
    constant fb_cs_fb           : fb_cs_array_t :=
    (
        (base_address => x"E0000000", address_mask => CSMR_BAM_1G, control_reg => (others => '0')),
        (base_address => x"FFF80000", address_mask => CSMR_BAM_512K, control_reg => (others => '0')),
        (base_address => x"F0000000", address_mask => CSMR_BAM_128M, control_reg => (others => '0')),
        (base_address => x"FFF00000", address_mask => CSMR_BAM_512K, control_reg => (others => '0')),
        (base_address => x"40000000", address_mask => CSMR_BAM_1G, control_reg => (others => '0')),
        (base_address => x"00000000", address_mask => CSMR_BAM_1G, control_reg => (others => '0'))
    );


    function adr_match(reg : addr_t; check : addr_t; fbcs : std_logic_vector; fbc : natural;
                       width : natural) return boolean; 
end package video_regs;


--
-- find if bus address matches with address range of a register.
-- reg  address from the bus
-- check the address to check against
-- fbc   the index into the fb_cs_fb array to check for
--
-- result: true when check is a match, false otherwise
--
package body video_regs is
    function adr_match(reg : addr_t; check : addr_t; fbcs : std_logic_vector; fbc : natural;
                       width : natural) return boolean is
        variable zero_count : natural := 0;
        variable upper      : natural := 0;
        variable lower      : natural := width - 1;
    begin
        if fbcs(fbc) = '0' then
            -- count the number of '0' bits in the fbc address mask
            for i in fb_cs_fb(fbc).address_mask'high downto fb_cs_fb(fbc).address_mask'low loop
                if fb_cs_fb(fbc).address_mask(i) = '0' then
                    zero_count := zero_count + 1;
                else
                    exit;
                end if;
            end loop;

            upper := reg'high - zero_count + 1;
            lower := natural(ceil(log2(real(width))));

            -- report "slice: " & natural'image(upper) & " downto " & natural'image(lower);
    
            -- synthesis translate off
            report "reg(" & natural'image(upper) & " downto " & natural'image(lower) & " ) = " &
                to_hstring(reg(upper downto lower)) & ", " &
                "check(" & natural'image(upper) & " downto " & natural'image(lower) & ") = " &
                to_hstring(check(upper downto lower));
            -- synthesis translate on
            if reg(upper downto lower) = check(upper downto lower) then
                report "slice matches";
                return true;
            end if;
        end if;
        -- report "slice does not match";
        return false;
    end function adr_match;
end package body video_regs;
