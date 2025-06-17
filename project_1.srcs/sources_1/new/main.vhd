
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.numeric_std.ALL;

ENTITY main IS
    PORT (
        SW : IN STD_LOGIC_VECTOR(3 DOWNTO 0);

        LCD_RS : OUT STD_LOGIC;
        LCD_E : OUT STD_LOGIC;
        LCD_DATA   : out STD_LOGIC_VECTOR (3 downto 0);
      
        Clock100MHz : IN STD_LOGIC;
        DEBUG : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
        SPI_SCK : OUT STD_LOGIC;
        SPI_MOSI : OUT STD_LOGIC;
        SPI_SS : OUT STD_LOGIC;
        LDAC : OUT STD_LOGIC
    );
END main;

ARCHITECTURE Behavioral OF main IS


    signal distance_val : unsigned(19 downto 0);
    signal distance_cm  : STD_LOGIC_VECTOR(7 downto 0);
    signal lcd_reset    : std_logic := '0';
    
    component lcd
        Port (
            reset       : in  STD_LOGIC;
            Clock100MHz  : in  STD_LOGIC;
            LCD_RS      : out STD_LOGIC;
            LCD_E       : out STD_LOGIC;
            LCD_DATA    : out STD_LOGIC_VECTOR (3 downto 0);
            spi_miso_data : in  STD_LOGIC_VECTOR(11 downto 0);
            DEBUG: out STD_LOGIC_VECTOR(7 downto 0)
        );
    end component;
    
     COMPONENT spi_master IS
        PORT (
            clk : IN STD_LOGIC;
            start : IN STD_LOGIC;
            data_in : IN STD_LOGIC_VECTOR(23 DOWNTO 0);
            sck : OUT STD_LOGIC;
            -- mosi : OUT STD_LOGIC;
            miso : IN STD_LOGIC;  -- Added MISO input to component declaration
            ss : OUT STD_LOGIC;
            busy : OUT STD_LOGIC;
            data_out : OUT STD_LOGIC_VECTOR(23 DOWNTO 0); -- Added data_out to component declaration
            miso_data_out : OUT STD_LOGIC_VECTOR(11 DOWNTO 0)
        );
    END COMPONENT;

    SIGNAL spi_busy : STD_LOGIC;
    SIGNAL spi_miso_data : STD_LOGIC_VECTOR(11 DOWNTO 0); -- Signal to store 12-bit MISO data
    SIGNAL spi_miso_in : STD_LOGIC; -- Signal to store MISO input
    SIGNAL data_received : STD_LOGIC_VECTOR(23 DOWNTO 0) := (OTHERS => '0');
    SIGNAL spi_data_out : STD_LOGIC_VECTOR(23 DOWNTO 0) := (OTHERS => '0');
    SIGNAL spi_start_pulse : STD_LOGIC := '0'; -- New signal for SPI start pulse

    SIGNAL counter : INTEGER RANGE 0 TO 99999999 := 0; -- Counter for triggering SPI (adjust range as needed)
    SIGNAL counter_limit : INTEGER := 10000000; -- Send every 100ms (100MHz clock / 10000000)

    SIGNAL ref_command : STD_LOGIC_VECTOR(23 DOWNTO 0) := x"204000"; -- REF command: Internal 2.5V
    SIGNAL power_command : STD_LOGIC_VECTOR(23 DOWNTO 0) := x"400000"; -- POWER command: Normal operation
    SIGNAL voltage_command : STD_LOGIC_VECTOR(23 DOWNTO 0) := x"123456"; -- POWER command: Normal operation

    TYPE state_type IS (IDLE, SEND_REF, SEND_POWER, RUN, END_COMM, RECEIVE_DATA); -- Added RECEIVE_DATA state
    SIGNAL current_state : state_type := IDLE;
    SIGNAL next_state : state_type := IDLE;

BEGIN


    U2: lcd
    port map (
        reset       => lcd_reset,
        Clock100MHz  => Clock100MHz,
        LCD_RS      => LCD_RS,
        LCD_E       => LCD_E,
        LCD_DATA    => LCD_DATA,
        spi_miso_data => spi_miso_data,
        DEBUG => DEBUG
    );


 -- Instantiate SPI Master
    spi_master_inst : spi_master
    PORT MAP(
        clk => Clock100MHz,
        start => spi_start_pulse, -- Connect to the new pulse signal
        data_in => spi_data_out, -- Connect data to be transmitted
        sck => SPI_SCK,
        ss => SPI_SS,
        busy => spi_busy,
        miso => spi_miso_in,
        data_out => data_received, -- Connect the data_out from spi_master
        miso_data_out => spi_miso_data -- Connect the 12-bit MISO data
    );

    
    -- LDAC control process
    PROCESS (Clock100MHz)
    BEGIN
        IF rising_edge(Clock100MHz) THEN
            IF current_state = IDLE THEN
                LDAC <= '0';
            ELSE
                LDAC <= '1';
            END IF;
        END IF;
    END PROCESS;

    -- State machine to send configuration commands and data
    PROCESS (Clock100MHz)
    BEGIN
        IF rising_edge(Clock100MHz) THEN
            spi_start_pulse <= '0'; -- Default to low

            -- Counter logic
            IF counter < counter_limit - 1 THEN
                counter <= counter + 1;
            ELSE
                counter <= 0;
            END IF;

            CASE current_state IS
                WHEN IDLE =>
                    IF counter = 0 THEN -- Trigger when counter reaches limit
                        next_state <= SEND_REF;
                    END IF;
                WHEN SEND_REF =>
                    IF spi_busy = '0' THEN
                        spi_data_out <= ref_command;
                        spi_start_pulse <= '1'; -- Start REF command
                        next_state <= RECEIVE_DATA; -- Transition to RECEIVE_DATA after sending
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
                WHEN SEND_POWER => -- Added missing state
                    -- Add logic for SEND_POWER state if needed
                    next_state <= RUN; -- Example transition
                WHEN RUN => -- Added missing state
                    -- Add logic for RUN state if needed
                    next_state <= IDLE; -- Example transition

            END CASE;
            current_state <= next_state;
        END IF;
    END PROCESS;


END Behavioral;
