library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package firebee_utils is
    function vext(e : std_ulogic; size : integer) return std_ulogic_vector;
    function "and" (v : std_ulogic_vector; s : std_ulogic) return std_ulogic_vector;
end package firebee_utils;

package body firebee_utils is
    function vext(e : std_ulogic; size : integer) return std_ulogic_vector is
        variable ev : std_ulogic_vector(size - 1 downto 0) := (others => e);
    begin
        return ev;
    end function vext;

    function "and" (v : std_ulogic_vector; s : std_ulogic) return std_ulogic_vector is
        variable vi : std_ulogic_vector(v'range) := v;
    begin
        for i in vi'range loop
            vi(i) := vi(i) and s;
        end loop;
        return vi;
    end function "and";
end package body firebee_utils;