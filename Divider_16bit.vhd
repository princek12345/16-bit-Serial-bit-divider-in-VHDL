----------------------------------------------------------------------------------
-- Name : Princekumar B. Kothadiya
-- UID  : 2209688
-- EE119a : HW-(8)
-- Serial Divider Design
-- 
-----------------------------------------------------------------------------------
-- Description :
--
--	It is the serial divider VHDL code which uses non-restoring algorithm to divide two 16-bit number and gives the quotient as 16-bit output.
--  The entity takes input from the keypad and displays it on the 7-segment LEDs. 
--  When the calculate button is pressed, 16-bit quotient is calculated of the two input 16-bit values dividend and divisor taken as input from user depending on the position of the Divisor switch).  
--  The dividend, divisor and quotinet are displayed in hexadecimal.
--  Also, for both input values only the last 4 hex digits (16-bits) are used and previous values are discarded. 
--
--	Provided info (directly using):
--			Keypad controller 
--			Segment Decoder
--			Digit Decoder

--  Extra Credit :
--      Preserve Dividend and Divisor
--      Preserve Display while Calculating
--      Divide-by-Zero : displaying x"1234" as quotient
---------------------------------------------------------------------------------------------------
-- Logic :
--    According to the non-restoring algorithm of divison, MSB of shifted dividend is taken each time and addition/subtraction logic is applied based on the previous time borrow/carry.
--    First time, MSB of dividend is taken as LSB of temporary dividend register with 16-zeros and 1 LSB as MSB of dividend. The process start with subtraction and remainder is calculated,
--    The same remaninder's MSB is discareded and LSB is taken as next bit of dividend and operation is performed as per the final borrow/carry bit. 
--    The quotient bit is also decided based on the borrow/ carry bit as follows:
--    If borrow = '1' -> append '0' in the quotient bit from LSB and -> change to addition
--              = '0' -> append '1' into quotient bit from LSB and -> continue doing subtraction
--         
--    If carry  = '1' -> append '1' in the quotient bit from LSB and -> change to addition
--              = '0' -> append '0' into quotient bit from LSB and -> continue doing subtraction
--  
--   Since serial implementation is required, need to shift the 17-bit temporary dividend each cycle to perform operations and the final 17th cycle decide the final borroe/carry.
--   After each 17th cycle, one bit from dividend is appended into temporary dividend and the same operation continues...
--   Now, total of 16-bit from dividend needs to be used and for each 17 cycles are required ->  total minimum of 16*17 cycles are required.
--   BUT to reduce the logic, Implementation is done as follows as far as timing is concern:
--
--      cycle : 0 to 15  ->  MSB of dividend used              cycle : 17 -> update the quotient bit as per final borrow/carry 
--      cycle : 32 to 47 -> next bit of dividend used          cycle : 49 -> update the next quotient bit as per final borrow/carry 
--                  :                                                               :
--                  :                                                               :
--
--   like this, 16 times needs to be done and at the end at cycle : 497 -> calculation is done !!
------------------------------------------------------------------------------------------------------------------
--Port Description :
--  
--  Inputs :
--     nCalculate              - calculate the quotient (active low signal)
--     Divisor                 - input the divisor (not the dividend) - sliding switch : '0' for dividend and '1' for divisor
--     KeypadRdy               - there is a key available (from keypad controller)
--     Keypad(3 downto 0)      - keypad input (from keypad controller)
--     CLK                     - the clock (1 MHz)

--  Outputs :
--     HexDigit(3 downto 0)    - hex digit to display (to segment decoder)
--     DecoderEn               - enable signal for the 4:12 digit decoder
--     DecoderBits(3 downto 0) - digit to display (to 4:12 decoder)
--------------------------------------------------------------------------------------------------------
-- Note :- change the parameter for fitting this on CPLD : ispMACH4128ZE ::
--- Collapsing Mode           : Area
--- Fmax logic level          : 1
--- Max Pterm collapse        : 24
--- Max Fan In                : 36
--- Max Fan In limit          : 36
--- Logic Optimization Effort : 7
--------------------------------------------------------------------------------------------------------

-- libraries
library  ieee;
use  ieee.std_logic_1164.all;
use  ieee.numeric_std.all;

entity  Divider_16bit  is       -- Entity declaration

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

end  Divider_16bit;


architecture  Dataflow  of  Divider_16bit  is

    -- keypad signals
    signal  HaveKey     :  std_logic;           -- have a key from the keypad
    signal  KeypadRdyS  :  std_logic_vector(2 downto 0); -- keypad ready synchronization

    -- LED multiplexing signals
    signal  MuxCntr     :  unsigned(9 downto 0):= "00" & x"00";   -- multiplex counter (to divide 1 MHz to 1 KHz)
    
    signal  DigitClkEn  :  std_logic;           -- enable for the digit clock (to enable calculations)
    signal  CurDigit    :  std_logic_vector(3 downto 0):= "0011"; -- current mux digit

    --  signals to select shift register operation
    --     ShiftOp = 0  ==>  hold
    --     ShiftOp = 1  ==>  calculate shift
    --     ShiftOp = 2  ==>  keypad input shift
    --     ShiftOp = 3  ==>  display shift
    signal    ShiftOp       :  std_logic_vector(1 downto 0);
    constant  ShiftOpHOLD   :  std_logic_vector(1 downto 0) := "00";
    constant  ShiftOpCALC   :  std_logic_vector(1 downto 0) := "01";
    constant  ShiftOpKEYIN  :  std_logic_vector(1 downto 0) := "10";
    constant  ShiftOpSHIFT  :  std_logic_vector(1 downto 0) := "11";

    --  12 stored hex digits (48 bits) in a shift register 
    --    bits 15-0    Dividend
    --    bits 31-16   Divisor
    --    bits 47-32   Quotient
    signal  DigitBits  :  std_logic_vector(47 downto 0);

    --   adder/subtracter signals
    signal  CalcResultBit  :  std_logic;        -- sum/difference output
    signal  CalcCarryOut   :  std_logic_vector(1 downto 0);        -- carry/borrow out
    signal  CarryFlag      :  std_logic;        -- stored carry flag

    
    ----------------------------------------------------------------------------------------
    -- temporary dividend and divisor of 17 bit (extra '0' + 16 bit for divisor)
    signal temp_dividend : std_logic_vector(16 downto 0);
    signal divisor_reg : std_logic_vector(16 downto 0);
    
    -- signal to indicate addition/subtraction 
    --  '1' for subtraction
    --  '0' for addition
    signal add_sub   : std_logic;
   
    signal calc_done : std_logic;   -- to indicate calculation is completed 
    
    -- Synchronization signal for Calculate and divisor two user input
	signal nCalculateS : std_logic_vector(1 downto 0);
	signal DivisorS    : std_logic_vector(1 downto 0);

	signal divisor_reg_0 : std_logic;  -- to indicate divide by zero case 

	signal next_calc_req : std_logic;  -- to indicate that new inputs are here and next calculation is required to be done

begin
    
    -- result and carry/borrow bit as per addition or subtraction
    -- always using LSB of both registers because of serial implementation 
    CalcResultBit    <= temp_dividend(0) xor add_sub xor divisor_reg(0) xor CarryFlag;
	CalcCarryOut(0)  <= (temp_dividend(0) and CarryFlag) or
                          ((divisor_reg(0) xor add_sub) and (temp_dividend(0) or CarryFlag));

Sync: process(clk)
    
begin
    if (rising_edge(clk)) then
		
		-- Synchronizing user input on rising edge of the clock
        nCalculateS <= nCalculateS(0) & nCalculate;     
        DivisorS <= DivisorS(0) & Divisor;
        
        -- storing previous value for use 
        CalcCarryOut(1) <= CalcCarryOut(0);
	end if;
	
end process;
	
Calc_operation: process (clk)
   
begin
    if (rising_edge(clk)) then
    
       if (MuxCntr = "1111111111" and next_calc_req = '1') then   -- initializing temporary dividend and divisor reg one cycle before the start of the calculation
            
            divisor_reg <= '0' & DigitBits(35 downto 20);       -- '0' & 16-bit of divisor from user input
            temp_dividend <= x"0000" & DigitBits(19);           -- 16 zeros & MSB of dividend
            add_sub <= '1';                                     -- starting with subtraction
            CarryFlag <= '1';                                   -- initialing carryFlag with '1'
            calc_done <= '0';                                   -- calculation is about to start in next cycle so -> calculation done signal is set to '0'
            divisor_reg_0 <= '0';                               -- divide by zero case : don't know yet but intializing with '0'
        
        elsif (ShiftoP = ShiftOpCALC and calc_done = '0') then  -- Calculating if in calculation state and calculation is not over yet
            
                if (std_match(MuxCntr,"0----0----")) then       -- calculating for 16 cycles each time
            
                    if (std_match(divisor_reg,'0' & x"0000")) then  -- divide by zero case 
                        calc_done <= '1';
                        divisor_reg_0 <= '1';
                    end if;
                 
                    -- rotating both divisor and temporary dividend by one bit for serial operation 
                    -- Appending result bit (of remainder) each time in temporary dividend for use in next cycle
                    divisor_reg <= divisor_reg(0) & divisor_reg(16 downto 1);   
                    temp_dividend <= CalcResultBit & temp_dividend(16 downto 1);
                    
                    CarryFlag <= CalcCarryOut(0);           -- updating carryFlag as per current value of carry
        
                elsif (std_match(MuxCntr,"0----10001")) then    -- condition for 17th cycle 
                    
                    divisor_reg <= divisor_reg(0) & divisor_reg(16 downto 1);        -- final rotation to get back same divisor for next cycle 
                    temp_dividend <= temp_dividend(16 downto 1) & DigitBits(15);     -- appending next bit of dividend
                    CarryFlag <= CalcCarryOut(0);                           -- updating carryFlag as per current value of carry
                    add_sub <= CalcCarryOut(1);                             -- updating addition or subtraction as per the previous carry bit stored (MSB of remainder is anyway discared)
       
                    if (std_match(MuxCntr,"-1111-----")) then   -- contion on 497th cycle 
                            calc_done <= '1';               -- division is over -> update the calc_done signal to '1'
                    end if;
                end if;
        
         elsif (ShiftoP = ShiftOpCALC) then         -- If still in calculation state -> update the next calculation requrired signal as per cal_done 
              next_calc_req <= not calc_done;       
        
         elsif (ShiftoP = ShiftOpKEYIN)	then        -- If in the key input state -> new input arrived -> next calculation reqired -> update the signal
              next_calc_req <= '1';
         end if;
    end if;
    
end process;
   
Key_input: process(CLK)
begin
    if rising_edge(CLK) then

        -- shift the keypad ready signal to synchronize and edge detect
        KeypadRdyS  <=  KeypadRdyS(1 downto 0) & KeypadRdy;

        -- have a key if have one already that hasn't been processed or a
        -- new one is coming in (rising edge of KeypadRdy), reset if on
        -- the last clock of Digit 3 or Digit 7 (depending on position of
        -- Divisor switch) and held otherwise
        if  (std_match(KeypadRdyS, "01-")) then
            -- set HaveKey on rising edge of synchronized KeypadRdy
            HaveKey <=  '1';
        elsif ((DigitClkEn = '1') and (CurDigit = "0011") and (DivisorS(1) = '0')) then
            -- reset HaveKey if on Dividend and at end of digit 3
            HaveKey <=  '0';
        elsif ((DigitClkEn = '1') and (CurDigit = "0111") and (DivisorS(1) = '1')) then
            -- reset HaveKey if on Divisor and at end of digit 7
            HaveKey <=  '0';
        else
            -- otherwise hold the value
            HaveKey <=  HaveKey;
        end if;
    end if;
end process;

    -- counter for mux rate of approximately 1 KHz (1 MHz / 1024)
Mux_counter: process(CLK)
begin
    -- count on the rising edge (clear on reset)
    if rising_edge(CLK) then
            MuxCntr <= MuxCntr + 1;
    end if;

end process;

    -- the multiplex counter is also used for controlling the operation of the circuit - DigitClkEn signals the end of a multiplexed digit (MuxCntr = 3FF) 
    DigitClkEn  <=  '1'  when (MuxCntr = "1111111111")  else
                    '0';

    -- create the counter for output the current digit - order is 3, 2, 1, 0, 7, 6, 5, 4, 11, 10, 9, then 8
Digit_counter: process (CLK)
begin

    if (rising_edge(CLK)) then

         -- create the appropriate count sequence
        if (DigitClkEn = '1') then
            CurDigit(0) <= not CurDigit(0);
            CurDigit(1) <= CurDigit(1) xor not CurDigit(0);
            if (std_match(CurDigit, "0-00")) then
                CurDigit(2) <= not CurDigit(2);
            end if;
            if (std_match(CurDigit, "-100") or std_match(CurDigit, "1-00")) then
                CurDigit(3) <= not CurDigit(3);
            end if;

        -- otherwise hold the current value
        else
            CurDigit <= CurDigit;

        end if;
    end if;

end process;

    -- always enable the digit decoder
    DecoderEn  <=  '1';

    -- output the current digit to the digit decoder
    DecoderBits  <=  CurDigit;

    -- the hex digit to output is just the low nibble of the shift register
    HexDigit  <=  DigitBits(3 downto 0);

   -- shift register commands
    ShiftOp  <=  ShiftOpSHIFT  when (DigitClkEn = '1')  else        -- Shifting state for display 
                 ShiftOpKEYIN  when ((nCalculateS(1) = '1') and     -- taking input state
                                     (HaveKey = '1') and
                                     (std_match(MuxCntr, "1111111110") and      -- checking for 3FE condition just before the DigitClkEn = '1' (3FF) case
                                      (((CurDigit = "0011") and (DivisorS(1) = '0')) or
                                       ((CurDigit = "0111") and (DivisorS(1) = '1')))))  else
                 ShiftOpCALC   when ((nCalculateS(1) = '0') and     -- calculation state : updated with next calculation required signal
                                     (CurDigit = "0011") and
					                   (next_calc_req = '1'))  else
                 ShiftOpHOLD;                                       -- Hold state

    -- the shift register
    --    bits 15-0    Dividend
    --    bits 31-16   Divisor
    --    bits 47-32   Quotient
    --
    -- operations
    --    ShiftHold:    DigitBits(47..0) = DigitBits(47..0)
    --    ShiftCalc:    DigitBits(47..0) = CalcResultBit | DigitBits(47..1)
    --    ShiftKeyIn:   DigitBits(47..0) = DigitBits(47..16) | DigitBits(11..0) | Keypad(3..0)
    --    ShiftDisplay: DigitBits(47..0) = DigitBits(3..0) | DigitBits(47..4)
    
 Shift_Op: process(CLK)
begin
    -- shift on the rising edge
    if rising_edge(CLK) then
        case  ShiftOp  is
            when ShiftOpHOLD =>     -- Hold the Digitbits reg values
                DigitBits <= DigitBits;
            when ShiftOpCALC =>     -- Calculation State 
                      if (divisor_reg_0 = '1') then     -- Divide by zero case
                            DigitBits(47 downto 32) <=  x"1234";    -- storing x"1234" in quotient bit 
                            
                      elsif(std_match(MuxCntr, "0----10000") and calc_done = '0') then  -- After each 16 cycle : rotate the dividend by one bit
                            DigitBits(15 downto 0) <= DigitBits(14 downto 0) & DigitBits(15);		
                            
                      elsif(std_match(MuxCntr, "0----10001") and calc_done = '0') then  -- next of each 16th cycle : update the quotient bit in digitbits reg
                            DigitBits(47 downto 32) <=  DigitBits(46 downto 32) & (CalcCarryOut(1));
                      else 
                            DigitBits <= DigitBits;     -- otherwise : hold the same value 
                end if;
            when ShiftOpKEYIN =>    -- Key-input State 
                DigitBits <= DigitBits(47 downto 16) & DigitBits(11 downto 0) & Keypad;     -- taking input from keypad (4 digit - 1 hex digit)
            when ShiftOpSHIFT =>    -- Shift state
                DigitBits <= DigitBits(3 downto 0) & DigitBits(47 downto 4);    -- rotating 4-bits each time for display 
            when others =>
                DigitBits <= DigitBits;     -- otherwise : hold the same value
        end case;
    end if;

end process;

end  Dataflow;

