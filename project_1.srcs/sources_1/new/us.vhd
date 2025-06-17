library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity us is
    Port (
        Clock100MHz : in  STD_LOGIC;
        TRIG        : out STD_LOGIC;
        ECHO        : in  STD_LOGIC;
        DIST_OUT    : out unsigned(19 downto 0)
    );
end us;

architecture Behavioral of us is

    -------------------------------------------------------------------------
    --  Sygna�y pomocnicze: licznik, flaga pomiaru, wynik pomiaru
    -------------------------------------------------------------------------
    signal counter        : unsigned(19 downto 0) := (others => '0');
    signal measure_start  : boolean := false;

    -------------------------------------------------------------------------
    --  Sygna�y do generacji impulsu TRIG (10us co 60ms)
    -------------------------------------------------------------------------
    signal trig_cnt       : integer range 0 to 6000000 := 0;
    signal trig_signal    : std_logic := '0';

begin

    -------------------------------------------------------------------------
    --  Generowanie impulsu TRIG (10us impuls co 60ms)
    -------------------------------------------------------------------------
    process(Clock100MHz)
    begin
        if rising_edge(Clock100MHz) then
            if trig_cnt < 100 then
                trig_signal <= '1';  -- 10us = 100 takt�w @100MHz
                trig_cnt <= trig_cnt + 1;
            elsif trig_cnt < 6_000_000 then
                trig_signal <= '0';
                trig_cnt <= trig_cnt + 1;
            else
                trig_cnt <= 0;
            end if;
        end if;
    end process;

    TRIG <= trig_signal;

    
    process(Clock100MHz)
    begin
        if rising_edge(Clock100MHz) then
            if ECHO = '1' then
                measure_start <= true;
                counter <= counter + 1;
            elsif measure_start = true then
                DIST_OUT <= counter;
                counter <= (others => '0');
                measure_start <= false;
            end if;
        end if;
    end process;

end Behavioral;
