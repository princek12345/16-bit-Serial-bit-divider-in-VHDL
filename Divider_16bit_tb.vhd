----------------------------------------------------------------------------------
-- Name : Princekumar B. Kothadiya
-- UID  : 2209688
-- EE119a : HW-(8)
-- Test bench for Serial Divider Design
-- 
-----------------------------------------------------------------------------------
-- Description :
--
--	It is testbench for Serial Divider design with UUT as Divider_16bit. It uses seperate Muxcounter and digitcounter and using them to synchronize with VHDL code.
--  It uses few already noted down test cases in terms of Digitbits array. 
--  It takes dividend and divisor from Digitbits, performing calculation using UUT (Divider_16bit) and checking the resulted quotient bit with digitbits.
--------------------------------------------------------------------------------------

library ieee;                  
use ieee.std_logic_1164.all;  
use ieee.numeric_std.all;  


entity Divider_16bit_tb is 
end entity;


architecture dataflow_tb of Divider_16bit_tb is

    constant CLOCK_PERIOD  :    time := 1 us;   -- System clock period 
    
    constant total_cases : integer:= 12;    -- total cases used for check
    
    -- An integer array for storing the count sequence of the muxed digit
    type int_array is array (0 to 11) of  integer range 0 to 11;
    
    -- Digit count of testbench for display and sync with UUT
    constant output_mux_count : int_array :=  (3, 2, 1, 0, 7, 6, 5, 4, 11, 10, 9, 8);

    -- A test array for storing the pre-selected test cases
    type test_array is array (0 to total_cases-1) of std_logic_vector(47 downto 0);                 
    
    ------------------------------------------------------
    signal Clock        :  std_logic;       -- the system clock (1 MHz)
    signal nCalculate   :  std_logic;       -- calculate the quotient (active low signal)
    signal Divisor      :  std_logic;       -- input the divisor (not the dividend) - sliding switch : '0' for dividend and '1' for divisor
    signal KeypadRdy    :  std_logic;       -- there is a key available : providing just after the key input
    signal KeypadVal    :  std_logic_vector(3 downto 0);    -- keypad input
    
    signal HexDigit    : std_logic_vector(3 downto 0);      -- hex digit to display
    signal DecoderBits     : std_logic_vector(3 downto 0);  -- digit to display (to 4:12 decoder)
    signal DecoderEn    : std_logic;                        -- enable signal for the 4:12 digit decoder
    
    -- Test signal
    signal END_SIM  :       boolean := FALSE;       -- Flag
   
   -- Expected mux count : 0 to 1023 : 10 bit as per UUT
    signal ExpectedMuxCnt : integer range 0 to 1023;
    
    -- Expected digit count : initializing with 3
    signal ExpectedDigit :  integer range 0 to 11 := 3; 
    
    -- Test_Digitbits for displaying current checking case 
    signal TestDigitbits : std_logic_vector(47 downto 0);

    --- Pre-entered test cases as digitbits check array:  (feel free to change and check !!)
    --    bits 15-0    Dividend
    --    bits 31-16   Divisor
    --    bits 47-32   Quotient
    constant DigitBits_check: test_array := (           -- Test cases  
        x"123400000000",
        x"0000FFFF0000",
        x"12340000FFFF",
        x"123400000237",
        x"0000FFFF0789",
        x"0001FFFFFFFF",
        x"000101110111",
        x"0000ABCD0000",
        x"04C50024ABCD",
        x"002B0473BFAC",
        x"0002721BEB56",
        x"006C0198AD12"
    );
    
--- UUT : component declaration    
component Divider_16bit is
    port (
        nCalculate   :  in   std_logic;             -- calculate the quotient (active low signal)
        Divisor      :  in   std_logic;             -- input the divisor (not the dividend) - sliding switch : '0' for dividend and '1' for divisor
        KeypadRdy    :  in   std_logic;             -- there is a key available (from keypad controller)
        Keypad       :  in   std_logic_vector(3 downto 0);  -- keypad input (from keypad controller)
        HexDigit     :  out  std_logic_vector(3 downto 0);  -- hex digit to display (to segment decoder)
        DecoderEn    :  out  std_logic;             -- enable signal for the 4:12 digit decoder
        DecoderBits  :  out  std_logic_vector(3 downto 0);  -- digit to display (to 4:12 decoder)
        CLK          :  in   std_logic              -- the clock (1 MHz)
    );
end component;
    
begin  
   --- UUT : port map
    UUT : Divider_16bit
        port map  (
            nCalculate => nCalculate,
            Divisor    => Divisor,
            KeypadRdy  => KeypadRdy,
            Keypad     => KeypadVal,
            HexDigit   => HexDigit,
            DecoderEn  => DecoderEn,
            DecoderBits => DecoderBits,
            CLK        => Clock
        );
 
        
Test: process 
        -- variable to check quotient bit 
        variable ExpectedQuotient  : std_logic_vector(3 downto 0);
begin 
    
    -- Initializing with no calculation, dividend and no input
    nCalculate  <= '1';        
    Divisor    <= '0';          
    KeypadRdy  <= '0';        
    
    -- loop over Digitbits check array 
    for j in 0 to total_cases-1 loop
        
        TestDigitbits <= DigitBits_check(j);    -- taking current test case into test digitbits for display
        
        -- no input and dividend selected
        Divisor    <= '0';
        KeypadRdy  <= '0';
        
        -- loop for dividend input from test array
        for i in 3 downto 0 loop
            wait until ExpectedMuxCnt = 0;
            wait until ExpectedDigit = 3;
            wait until falling_edge(Clock);
            KeypadVal  <= DigitBits_check(j)((4*i) + 3 downto (4*i));
            KeypadRdy  <= '1';
            wait until falling_edge(Clock);
            KeypadRdy  <= '0';
        end loop;
        
        -- waiting for next mux counter cycle and settign divisor to be '1'
        wait until ExpectedMuxCnt = 0;
        divisor    <= '1';
        
         -- loop for divisor input from test array
        for i in 7 downto 4 loop
            wait until ExpectedMuxCnt = 0;
            wait until ExpectedDigit = 7;
            wait until falling_edge(Clock);
            KeypadVal  <= DigitBits_check(j)((4*i) + 3 downto (4*i));
            KeypadRdy  <= '1';
            wait until falling_edge(Clock);
            KeypadRdy  <= '0';
        end loop;
       
        -- waiting for next mux counter cycle 
        wait until ExpectedMuxCnt = 0;
        
        -- Pressing calculate button for sufficient time (few cycles)
        nCalculate <= '0';
        
        wait until ExpectedDigit = output_mux_count(11);
        wait until ExpectedDigit = output_mux_count(0);
        wait until ExpectedDigit = output_mux_count(11);
        wait until ExpectedDigit = output_mux_count(0);
        wait until ExpectedDigit = output_mux_count(11);
        wait until ExpectedDigit = output_mux_count(0);
        
        nCalculate <= '1'; 
        
        -- Checking quotient with pre-entered output from digitbits check array
        for i in 8 to 11 loop
            wait until ExpectedDigit = output_mux_count(i);
            wait until falling_edge(Clock);
            ExpectedQuotient := DigitBits_check(j)((4*i) + 3 downto (4*i));
            assert(std_match(HexDigit, ExpectedQuotient))   -- comparing hexdigit output from UUT with expected output from array
                report "Division Error! Calculated value: " &   -- printing msg in case of error 
                       integer'image(to_integer(unsigned(
                            HexDigit))) &
                       " Expected value: " &
                       integer'image(to_integer(unsigned(
                            ExpectedQuotient)))
                severity ERROR;
        end loop;
    end loop;
    
    END_SIM <= TRUE;    -- End of the simulation
    wait;
end process;
  
Mux_counter: process(Clock)
begin
    if rising_edge(Clock) then
        if ExpectedMuxCnt = 1023 then   -- restart the counter after 1023
            ExpectedMuxCnt <= 0;
        else
            ExpectedMuxCnt <= ExpectedMuxCnt + 1;   -- increment by 1 in counter value
        end if;
    end if;
end process;
    
    
Digit_counter: process(Clock)
begin
    if rising_edge(Clock) then
        if ExpectedMuxCnt = 1023 then   -- update the counter at mux counter of 1023
          for i in 0 to 11 loop   
            if (ExpectedDigit = 8) then    -- restart from 3 after the last value 
                 ExpectedDigit <= output_mux_count(0);
            elsif ExpectedDigit = output_mux_count(i) then  -- update the counter to next value
                 ExpectedDigit <= output_mux_count(i + 1);
            end if;
          end loop;
        end if;
    end if;
end process;

CLOCK_CLK : process
begin
    if END_SIM = FALSE then     -- Run the clock while simulation is going on....
        Clock <= '0';
        wait for CLOCK_PERIOD/2;    -- clock = '0' for half period
    else
        wait;
    end if;
    if END_SIM = FALSE then
        Clock <= '1';
        wait for CLOCK_PERIOD/2;    -- clock = '1' for half period
    else
        wait;
    end if;
end process;
 
end architecture;