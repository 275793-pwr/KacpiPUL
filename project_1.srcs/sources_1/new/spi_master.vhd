LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.numeric_std.ALL;

ENTITY spi_master IS
    PORT (
        clk : IN STD_LOGIC;
        start : IN STD_LOGIC;
        sck : OUT STD_LOGIC;
        miso : IN STD_LOGIC;  -- Added MISO input
        ss : OUT STD_LOGIC;
        busy : OUT STD_LOGIC;
        miso_data_out : OUT STD_LOGIC_VECTOR(11 DOWNTO 0)
    );
END spi_master;

ARCHITECTURE Behavioral OF spi_master IS

    TYPE state_type IS (IDLE, START_TX, SEND_BIT, END_TX);
    SIGNAL current_state, next_state : state_type := IDLE;
    SIGNAL bit_count : INTEGER RANGE 0 TO 16 := 0;
    SIGNAL spi_sck_int : STD_LOGIC := '0';
    SIGNAL spi_ss_int : STD_LOGIC := '1';
    SIGNAL busy_int : STD_LOGIC := '0';
    SIGNAL miso_buffer : STD_LOGIC_VECTOR(15 DOWNTO 0) := (OTHERS => '0');
    SIGNAL clk_div_counter : INTEGER RANGE 0 TO 250 := 0; -- Counter for clock division

BEGIN

    sck <= spi_sck_int;
    ss <= spi_ss_int;
    busy <= busy_int;
    miso_data_out <= miso_buffer(12 downto 1);

    PROCESS (clk)
    BEGIN
        IF rising_edge(clk) THEN
            current_state <= next_state;
            CASE current_state IS
                WHEN IDLE =>
                    IF start = '1' THEN
                        next_state <= START_TX;
                        busy_int <= '1';
                    ELSE
                        clk_div_counter <= 0;
                        next_state <= IDLE;
                        busy_int <= '0';
                    END IF;
                    spi_ss_int <= '1'; -- Ensure SS is high in IDLE
                    spi_sck_int <= '0'; -- Ensure SCK is low in IDLE
                    clk_div_counter <= 0;

                WHEN START_TX =>
                    spi_ss_int <= '0'; -- Assert SS low
                    spi_sck_int <= '0'; -- Start with SCK low
                    bit_count <= 0;
                    clk_div_counter <= 0;
                    next_state <= SEND_BIT;

                WHEN SEND_BIT =>
                    IF bit_count < 12 THEN
                        IF clk_div_counter = 250 THEN -- Toggle SCK
                            clk_div_counter <= 0;
                            spi_sck_int <= NOT spi_sck_int;
                            -- IF spi_sck_int = '1' THEN -- Sample MISO on falling edge
                            IF spi_sck_int = '0' THEN -- Sample MISO on rising edge
                                miso_buffer(15 - bit_count) <= miso;
                                bit_count <= bit_count + 1;
                            END IF;
                        END IF;
                        clk_div_counter <= clk_div_counter + 1;
                        next_state <= SEND_BIT;
                    ELSE
                        next_state <= END_TX;
                        clk_div_counter <= 0;
                    END IF;

                WHEN END_TX =>
                    spi_ss_int <= '1'; -- Deassert SS high
                    spi_sck_int <= '0'; -- Ensure SCK is low
                    IF clk_div_counter = 250 THEN
                         next_state <= IDLE;
                    END IF;
                    clk_div_counter <= clk_div_counter + 1;

            END CASE;
        END IF;
    END PROCESS;

END Behavioral;
