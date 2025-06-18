
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.numeric_std.ALL;

ENTITY main IS
    PORT (

        LCD_RS : OUT STD_LOGIC;
        LCD_E : OUT STD_LOGIC;
        LCD_DATA : OUT STD_LOGIC_VECTOR (3 DOWNTO 0);

        Clock100MHz : IN STD_LOGIC;
        SPI_SCK : OUT STD_LOGIC;
        SPI_MISO : IN STD_LOGIC;
        SPI_SS : OUT STD_LOGIC
    );
END main;

ARCHITECTURE Behavioral OF main IS
    SIGNAL lcd_reset : STD_LOGIC := '0';

    COMPONENT lcd
        PORT (
            reset : IN STD_LOGIC;
            Clock100MHz : IN STD_LOGIC;
            LCD_RS : OUT STD_LOGIC;
            LCD_E : OUT STD_LOGIC;
            LCD_DATA : OUT STD_LOGIC_VECTOR (3 DOWNTO 0);
            spi_miso_data : IN STD_LOGIC_VECTOR(11 DOWNTO 0)
        );
    END COMPONENT;

    COMPONENT spi_master IS
        PORT (
            clk : IN STD_LOGIC;
            start : IN STD_LOGIC;
            sck : OUT STD_LOGIC;
            -- mosi : OUT STD_LOGIC;
            miso : IN STD_LOGIC; -- Added MISO input to component declaration
            ss : OUT STD_LOGIC;
            busy : OUT STD_LOGIC;
            miso_data_out : OUT STD_LOGIC_VECTOR(11 DOWNTO 0)
        );
    END COMPONENT;

    SIGNAL spi_busy : STD_LOGIC;
    SIGNAL spi_miso_data : STD_LOGIC_VECTOR(11 DOWNTO 0); -- Signal to store 12-bit MISO data
    SIGNAL spi_start_pulse : STD_LOGIC := '0'; -- New signal for SPI start pulse

    SIGNAL lcd_clk_cnt : INTEGER RANGE 0 TO 200000000 := 0;

    SIGNAL counter : INTEGER RANGE 0 TO 10000002 := 0; -- Counter for triggering SPI (adjust range as needed)
    SIGNAL counter_limit : INTEGER := 10000000; -- Send every 100ms (100MHz clock / 10000000)

    SIGNAL ref_command : STD_LOGIC_VECTOR(23 DOWNTO 0) := x"204000"; -- REF command: Internal 2.5V
    SIGNAL power_command : STD_LOGIC_VECTOR(23 DOWNTO 0) := x"400000"; -- POWER command: Normal operation
    SIGNAL voltage_command : STD_LOGIC_VECTOR(23 DOWNTO 0) := x"123456"; -- POWER command: Normal operation

    TYPE state_type IS (IDLE, END_COMM, RECEIVE_DATA); -- Added RECEIVE_DATA state
    SIGNAL current_state : state_type := IDLE;
    SIGNAL next_state : state_type := IDLE;

BEGIN
    U2 : lcd
    PORT MAP(
        reset => lcd_reset,
        Clock100MHz => Clock100MHz,
        LCD_RS => LCD_RS,
        LCD_E => LCD_E,
        LCD_DATA => LCD_DATA,
        spi_miso_data => spi_miso_data
    );
    -- Instantiate SPI Master
    spi_master_inst : spi_master
    PORT MAP(
        clk => Clock100MHz,
        start => spi_start_pulse, -- Connect to the new pulse signal
        sck => SPI_SCK,
        ss => SPI_SS,
        busy => spi_busy,
        miso => SPI_MISO,
        miso_data_out => spi_miso_data -- Connect the 12-bit MISO data
    );

    lcd_refresh : PROCESS (Clock100MHz)
    BEGIN
        IF rising_edge(Clock100MHz) THEN
            IF lcd_clk_cnt > 100000000 THEN
                lcd_reset <= '1';
                lcd_clk_cnt <= 0;
            ELSE
                lcd_reset <= '0';
            END IF;
            lcd_clk_cnt <= lcd_clk_cnt + 1;
        END IF;
    END PROCESS; -- lcd_refresh

    -- State machine to send configuration commands and data
    PROCESS (Clock100MHz)
    BEGIN
        IF rising_edge(Clock100MHz) THEN
            spi_start_pulse <= '0'; -- Default to low

            CASE current_state IS
                WHEN IDLE =>
                    counter <= counter + 1;
                    IF counter >= counter_limit THEN -- Trigger when counter reaches limit
                        next_state <= RECEIVE_DATA;
                        spi_start_pulse <= '1'; -- Default to low
                        counter <= 0;
                    END IF;
                WHEN RECEIVE_DATA => -- New state to handle received data
                    IF spi_busy = '0' THEN
                        -- Data is now available in data_received signal
                        -- Add logic here to process the received data if needed
                        next_state <= END_COMM; -- Transition to END_COMM after receiving
                    END IF;
                WHEN END_COMM =>
                    IF spi_busy = '0' THEN
                        next_state <= IDLE; -- Go back to IDLE to restart the sequence
                    END IF;

            END CASE;
            current_state <= next_state;
        END IF;
    END PROCESS;
END Behavioral;