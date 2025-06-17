library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity debouncer is
    port (
        clk           : in std_logic;         -- Clock input
        button_in     : in std_logic;         -- Raw button input
        pulse_out     : out std_logic;        -- Single pulse output
        hold_out      : out std_logic
    );
end debouncer;

architecture Behavioral of debouncer is
    signal delay: unsigned (32 downto 0) := (others => '0');
    signal state: unsigned(1 downto 0) := "00";
    signal next_state: unsigned(1 downto 0) := "00";
begin

    process(clk)
    begin
        if rising_edge(clk) then
            
            -- State transitions
            if (state = "00" and button_in = '1') then
                next_state <= "01";
            end if;
            
            if (state = "01" and delay > 2000000) then
                if button_in = '1' then
                    next_state <= "11";
                else
                    next_state <= "00";
                end if;
            end if;
            
            if (state = "11") then
                if delay > 100000000 then
                    next_state <= "00";
                else
                    next_state <= "11";
                end if;
            end if;

            -- Generate single pulse on transition to "11"
            if (state = "01" and next_state = "11") then
                pulse_out <= '1';  -- Output single clock cycle pulse
            else
                pulse_out <= '0';
            end if;
            
            if (state = "11") then
                hold_out <= '1';
            else
                hold_out <= '0';
            end if;

            -- Update delay
            if state = next_state then
                delay <= delay + 1;
            else
                delay <= (others => '0');
            end if;
            
            -- Update state
            state <= next_state;
            
        end if;
    end process;

end Behavioral;
