library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package firebee_utils is
    function vext(e : std_logic; size : integer) return std_logic_vector;
    -- function "and" (v : std_logic_vector; s : std_logic) return std_logic_vector;
    function tr(b : boolean) return std_logic;
end package firebee_utils;

package body firebee_utils is
    function vext(e : std_logic; size : integer) return std_logic_vector is
        variable ev : std_logic_vector(size - 1 downto 0) := (others => e);
    begin
        return ev;
    end function vext;

    function "and" (v : std_logic_vector; s : std_logic) return std_logic_vector is
        variable vi : std_logic_vector(v'range) := v;
    begin
        for i in vi'range loop
            vi(i) := vi(i) and s;
        end loop;
        return vi;
    end function "and";
    
    function tr(b : boolean) return std_logic is
    begin
        -- assert false report "b=" & to_string(b) severity note;
        if b then return '1'; else return '0'; end if;
    end function tr;
    
end package body firebee_utils;
