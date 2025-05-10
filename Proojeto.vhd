--==============================================================================
-- Entity: Proojeto
-- Description: ALU with operations in 2's complement and status indication on LEDs.
--              Improved synchronous design to avoid gated clocks.
-- Operations: ADD(000), SUB(001), INC(010), OR(011), AND(100), XOR(101), NEG(110), SHL(111)
-- Target Board: Xilinx Spartan-3
--==============================================================================

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

-- Entity declaration for the ALU project
entity Proojeto is -- "Proojeto" translates to "Project"
    Port (
        clk          : in  STD_LOGIC;                     -- System clock input
        btn_confirm  : in  STD_LOGIC;                     -- Confirmation button input
        sw           : in  STD_LOGIC_VECTOR (3 downto 0); -- 4-bit switch input for opcode/operands
        led_flags    : out STD_LOGIC_VECTOR (3 downto 0); -- 4-bit LED output for status flags (Z,N,C,V)
        led_result   : out STD_LOGIC_VECTOR (3 downto 0)  -- 4-bit LED output for ALU result or state indication
    );
end Proojeto;

--==============================================================================
-- Architecture
--==============================================================================
architecture rtl of Proojeto is

    -- --- Types ---
    -- Define states for the Finite State Machine (FSM)
    type state_t is (
        S_WAIT_OPCODE,             -- Waiting for opcode input
        S_WAIT_OPCODE_CONFIRM,     -- Opcode entered, waiting for confirmation to load it
        S_WAIT_OPERAND_A,          -- Waiting for operand A input
        S_WAIT_OPERAND_A_CONFIRM,  -- Operand A entered, waiting for confirmation to load it
        S_CHECK_OPCODE,            -- Check opcode to decide next step (need operand B or calculate)
        S_WAIT_OPERAND_B_OR_N,     -- Waiting for operand B or shift amount (N) input
        S_WAIT_OPERAND_B_OR_N_CONFIRM, -- Operand B/N entered, waiting for confirmation to load it
        S_CALCULATE_DISPLAY        -- Calculate result and display it
    );

    -- --- Constants ---
    -- Debounce counter limit (number of clock cycles to wait for button signal to stabilize)
    constant DEBOUNCE_LIMIT : integer := 35000000; -- Adjust this value based on the clock frequency and desired debounce time

    -- --- Internal Signals ---
    -- FSM state signals
    signal current_state, next_state : state_t := S_WAIT_OPCODE; -- Current and next state of the FSM

    -- Debounce signals for btn_confirm
    signal btn_first           : std_logic := '0';         -- Flag for first press logic in debounce
    signal btn_confirm_debounced : std_logic := '0';       -- Debounced button signal (high for one cycle after press)
    signal btn_counter         : integer range 0 to DEBOUNCE_LIMIT := 0; -- Counter for debounce timing
    signal btn_pressed_edge    : std_logic := '0';         -- Single clock cycle pulse indicating a debounced button press

    -- State for the button debouncer's own simple FSM
    type btn_confirm_state is (not_on, is_on);
    signal state_b : btn_confirm_state := not_on; -- State of the debounce logic

    -- Registers to store opcode and operands
    signal reg_opcode    : std_logic_vector(2 downto 0) := "000";       -- 3-bit register for operation code
    signal reg_operand_a : std_logic_vector(3 downto 0) := (others => '0'); -- 4-bit register for operand A
    signal reg_operand_b : std_logic_vector(3 downto 0) := (others => '0'); -- 4-bit register for operand B or shift amount

    -- Calculated output signals (combinational, based on registered inputs)
    signal calc_result : std_logic_vector(3 downto 0); -- Result of the ALU operation
    signal calc_flag_z : std_logic;                    -- Zero flag
    signal calc_flag_n : std_logic;                    -- Negative flag
    signal calc_flag_c : std_logic;                    -- Carry flag
    signal calc_flag_v : std_logic;                    -- Overflow flag

    -- NEW SIGNALS: Enable signals for registers (Clock Enables)
    -- These signals control when the respective registers are loaded, preventing gated clocks.
    signal load_opcode_en : std_logic := '0'; -- Enable for loading reg_opcode
    signal load_op_a_en   : std_logic := '0'; -- Enable for loading reg_operand_a
    signal load_op_b_en   : std_logic := '0'; -- Enable for loading reg_operand_b

begin

    -- =========================================================================
    -- Button Debounce Logic
    -- This process debounces the 'btn_confirm' input.
    -- It generates a single clock pulse 'btn_pressed_edge' when a stable button press is detected.
    -- =========================================================================
    debounce_proc: process(clk)
    begin
        if rising_edge(clk) then
            case state_b is
                when not_on => -- State: Button is not currently considered pressed or is stable released
                    if btn_confirm /= btn_pressed_edge then  -- Detects an initial change on btn_confirm (potential press)
                                                             -- btn_pressed_edge is '0' unless a pulse was just generated
                        if btn_first = '0' then -- First time check or after full release cycle
                            btn_counter <= 0;   -- Reset debounce counter
                            state_b <= is_on;   -- Transition to 'is_on' state to start debounce timing
                        else
                            btn_counter <= 0;   -- Reset debounce counter
                            state_b <= is_on;   -- Transition to 'is_on' state
                        end if;
                    end if;

                when is_on => -- State: Button press detected, timing for debounce
                    if btn_counter < DEBOUNCE_LIMIT then
                        btn_counter <= btn_counter + 1; -- Increment counter until DEBOUNCE_LIMIT is reached
                    else
                        -- Debounce period has passed
                        if btn_first = '0' then -- Special handling for the very first interaction (initialization)
                            btn_first <= '1';   -- Mark that the first interaction has occurred
                            state_b <= not_on;  -- Return to 'not_on' state
                        else
                            if btn_confirm_debounced = '0' then -- If the debounced pulse has not yet been generated for this press
                                if btn_confirm = '1' then       -- Check if button is still pressed (confirms it wasn't a glitch)
                                    btn_pressed_edge <= '1';    -- Generate the single-cycle pulse
                                    btn_confirm_debounced <= '1'; -- Flag that the pulse has been generated
                                else -- Button was released before debounce limit while in is_on after counter maxed
                                    state_b <= not_on;          -- Go back, it was a glitch or too short press
                                    btn_confirm_debounced <= '0'; -- Reset for next potential press
                                end if;
                            else -- Debounced pulse was generated (btn_confirm_debounced = '1')
                                btn_pressed_edge <= '0';         -- De-assert the pulse
                                if btn_confirm = '1' then        -- User is holding the button down
                                    -- Do nothing, wait for release
                                else                             -- User has released the button
                                    btn_confirm_debounced <= '0'; -- Reset flag, ready for next press
                                    state_b <= not_on;           -- Return to 'not_on' state
                                end if;
                            end if;
                        end if;
                    end if;
            end case;
        end if;
    end process debounce_proc;


    -- =========================================================================
    -- Main Synchronous Process: Updates State and Registers with Clock Enables
    -- It updates the current state of the FSM and loads data into registers
    -- only when their respective enable signals are active.
    -- =========================================================================
    sync_proc: process(clk)
    begin
        if rising_edge(clk) then
            -- Update the current state of the FSM
            current_state <= next_state;

            -- Update registers ONLY when enabled (Clock Enable is inferred)
            if load_opcode_en = '1' then
                reg_opcode <= sw(2 downto 0); -- Load 3 LSBs of switches into opcode register
            end if;

            if load_op_a_en = '1' then
                reg_operand_a <= sw;          -- Load all 4 bits of switches into operand A register
            end if;

            if load_op_b_en = '1' then
                reg_operand_b <= sw;          -- Load all 4 bits of switches into operand B register
            end if;
        end if;
    end process sync_proc;


    -- =========================================================================
    -- Combinational Logic: Calculates Next State and Register Enable Signals
    -- This process determines the next state of the FSM and asserts the
    -- clock enable signals for the registers based on the current state,
    -- the debounced button press, and the current input from switches/opcode.
    -- =========================================================================
    fsm_comb_logic_proc: process(current_state, btn_pressed_edge, sw, reg_opcode)
    begin
        -- Default assignments: Stay in the current state, do not load registers
        next_state <= current_state;
        load_opcode_en <= '0';
        load_op_a_en   <= '0';
        load_op_b_en   <= '0';

        case current_state is
            when S_WAIT_OPCODE =>
                -- In this state, waiting for the user to input an opcode and press confirm.
                if btn_pressed_edge = '1' then
                    next_state <= S_WAIT_OPCODE_CONFIRM; -- Move to confirm state on button press
                    -- The opcode will be loaded in the S_WAIT_OPCODE_CONFIRM state's handling.
                end if;

            when S_WAIT_OPCODE_CONFIRM =>
                -- Opcode is ready on switches, this state loads it.
                load_opcode_en <= '1';                   -- Enable loading of the opcode register in the next clock cycle.
                next_state <= S_WAIT_OPERAND_A;          -- Transition to wait for operand A.

            when S_WAIT_OPERAND_A =>
                -- Waiting for the user to input operand A and press confirm.
                if btn_pressed_edge = '1' then
                    next_state <= S_WAIT_OPERAND_A_CONFIRM; -- Move to confirm state on button press
                end if;

            when S_WAIT_OPERAND_A_CONFIRM =>
                -- Operand A is ready on switches, this state loads it.
                load_op_a_en <= '1';                     -- Enable loading of the operand A register.
                next_state <= S_CHECK_OPCODE;            -- Transition to check the loaded opcode.

            when S_CHECK_OPCODE =>
                -- Combinational decision based on the ALREADY loaded opcode (reg_opcode).
                -- Determine if operand B is needed or if we can proceed to calculation.
                case reg_opcode is
                    when "000" | "001" | "011" | "100" | "101" | "111" => -- ADD, SUB, OR, AND, XOR, SHL (need two operands)
                        next_state <= S_WAIT_OPERAND_B_OR_N;
                    when "010" | "110" =>                                -- INC, NEG (need one operand, A)
                        next_state <= S_CALCULATE_DISPLAY;
                    when others =>                                        -- Should not happen with 3-bit opcode
                        next_state <= S_CALCULATE_DISPLAY; -- Or an error state
                end case;

            when S_WAIT_OPERAND_B_OR_N =>
                -- Waiting for user to input operand B (or N for shift) and press confirm.
                 if btn_pressed_edge = '1' then
                    next_state <= S_WAIT_OPERAND_B_OR_N_CONFIRM; -- Move to confirm state
                 end if;

            when S_WAIT_OPERAND_B_OR_N_CONFIRM =>
                 -- Operand B/N is ready on switches, this state loads it.
                 load_op_b_en <= '1';                    -- Enable loading of the operand B register.
                 next_state <= S_CALCULATE_DISPLAY;      -- Transition to calculate and display.

            when S_CALCULATE_DISPLAY =>
                -- Result is being displayed. Wait for button press to start a new operation.
                 if btn_pressed_edge = '1' then
                    next_state <= S_WAIT_OPCODE;         -- Go back to wait for a new opcode.
                 end if;

            when others => -- Should not be reached if FSM is designed correctly
                next_state <= S_WAIT_OPCODE;
        end case;
    end process fsm_comb_logic_proc;


    -- =========================================================================
    -- Combinational Logic - ALU Operations and Flag Calculation
    -- This process is purely combinational and performs the arithmetic/logic
    -- operations based on the values stored in reg_opcode, reg_operand_a, and reg_operand_b.
    -- The results (calc_*) are updated whenever the input registers change.
    -- =========================================================================
    calculation_proc: process(reg_opcode, reg_operand_a, reg_operand_b)
        -- Variables for intermediate calculations. Using 5 bits for operands (op_a_ext, op_b_ext, res_ext)
        -- allows easy detection of carry-out from the 4th bit.
        variable op_a_ext     : unsigned(4 downto 0); -- Extended operand A (sign-extended or zero-extended for calculation)
        variable op_b_ext     : unsigned(4 downto 0); -- Extended operand B
        variable res_ext      : unsigned(4 downto 0); -- Extended result
        variable op_a_usign   : unsigned(3 downto 0); -- Unsigned version of operand A
        variable op_b_usign   : unsigned(3 downto 0); -- Unsigned version of operand B
        variable res_usign    : unsigned(3 downto 0); -- Unsigned result (4 bits)
        variable shift_amount : integer range 0 to 15; -- For SHL operation
        variable result_flags : std_logic_vector(3 downto 0); -- Temporary storage for flags [Z, N, C, V]
        alias A3 : std_logic is reg_operand_a(3); -- Alias for MSB of operand A
        alias B3 : std_logic is reg_operand_b(3); -- Alias for MSB of operand B
        variable R3 : std_logic;                 -- Variable for MSB of the 4-bit result
    begin
        -- Initialize default values
        result_flags := "0000";          -- Default: Z=0, N=0, C=0, V=0
        res_usign := (others => '0');     -- Default result is zero
        R3 := '0';                        -- Default MSB of result

        -- Convert std_logic_vector operands to unsigned type for arithmetic operations
        op_a_usign := unsigned(reg_operand_a);
        op_b_usign := unsigned(reg_operand_b);

        -- Perform operation based on the opcode
        case reg_opcode is
            when "000" => -- ADD: reg_operand_a + reg_operand_b
                op_a_ext := unsigned('0' & reg_operand_a); -- Zero-extend operands to 5 bits
                op_b_ext := unsigned('0' & reg_operand_b);
                res_ext  := op_a_ext + op_b_ext;            -- Perform 5-bit addition
                res_usign := res_ext(3 downto 0);          -- Truncate result to 4 bits
                R3 := res_usign(3);                        -- MSB of the 4-bit result

                if res_usign = 0 then result_flags(3) := '1'; else result_flags(3) := '0'; end if; -- Z (Zero flag)
                result_flags(2) := R3;                                                           -- N (Negative flag, MSB of result)
                result_flags(1) := res_ext(4);                                                   -- C (Carry flag, 5th bit of extended result)
                if (A3 = B3) and (A3 /= R3) then result_flags(0) := '1'; else result_flags(0) := '0'; end if; -- V (Overflow flag for signed addition)

            when "001" => -- SUB: reg_operand_a - reg_operand_b (implemented as A + (-B), where -B is 2's complement of B)
                op_a_ext := unsigned('0' & reg_operand_a);
                op_b_ext := unsigned('0' & (not reg_operand_b)); -- 1's complement of B
                res_ext  := op_a_ext + op_b_ext + 1;              -- Add 1 for 2's complement: A + (not B) + 1
                res_usign := res_ext(3 downto 0);
                R3 := res_usign(3);

                if res_usign = 0 then result_flags(3) := '1'; else result_flags(3) := '0'; end if; -- Z
                result_flags(2) := R3;                                                           -- N
                result_flags(1) := res_ext(4); -- C (Borrow is inverted carry for subtraction: C=1 means no borrow)
                if (A3 /= B3) and (B3 = R3) then result_flags(0) := '1'; else result_flags(0) := '0'; end if; -- V (Overflow for subtraction)

            when "010" => -- INC: reg_operand_a + 1
                op_a_ext := unsigned('0' & reg_operand_a);
                res_ext  := op_a_ext + 1;
                res_usign := res_ext(3 downto 0);
                R3 := res_usign(3);

                if res_usign = 0 then result_flags(3) := '1'; else result_flags(3) := '0'; end if; -- Z
                result_flags(2) := R3;                                                           -- N
                result_flags(1) := res_ext(4);                                                   -- C
                if reg_operand_a = "0111" then result_flags(0) := '1'; else result_flags(0) := '0'; end if; -- V (Overflow if 0111 -> 1000)

            when "011" => -- OR: reg_operand_a OR reg_operand_b
                res_usign := op_a_usign or op_b_usign;
                R3 := res_usign(3);

                if res_usign = 0 then result_flags(3) := '1'; else result_flags(3) := '0'; end if; -- Z
                result_flags(2) := R3;                                                           -- N
                result_flags(1) := '0';                                                          -- C (Carry is not applicable for OR)
                result_flags(0) := '0';                                                          -- V (Overflow is not applicable for OR)

            when "100" => -- AND: reg_operand_a AND reg_operand_b
                res_usign := op_a_usign and op_b_usign;
                R3 := res_usign(3);

                if res_usign = 0 then result_flags(3) := '1'; else result_flags(3) := '0'; end if; -- Z
                result_flags(2) := R3;                                                           -- N
                result_flags(1) := '0';                                                          -- C (Not applicable)
                result_flags(0) := '0';                                                          -- V (Not applicable)

            when "101" => -- XOR: reg_operand_a XOR reg_operand_b
                res_usign := op_a_usign xor op_b_usign;
                R3 := res_usign(3);

                if res_usign = 0 then result_flags(3) := '1'; else result_flags(3) := '0'; end if; -- Z
                result_flags(2) := R3;                                                           -- N
                result_flags(1) := '0';                                                          -- C (Not applicable)
                result_flags(0) := '0';                                                          -- V (Not applicable)

            when "110" => -- NEG: -reg_operand_a (2's complement of A)
                op_a_ext := unsigned('0' & (not reg_operand_a)); -- 1's complement of A
                res_ext  := op_a_ext + 1;                        -- Add 1 for 2's complement
                res_usign := res_ext(3 downto 0);
                R3 := res_usign(3);

                if res_usign = 0 then result_flags(3) := '1'; else result_flags(3) := '0'; end if; -- Z
                result_flags(2) := R3;                                                           -- N
                result_flags(1) := res_ext(4); -- C (Carry from negation, relevant if checking for -0)
                if reg_operand_a = "1000" then result_flags(0) := '1'; else result_flags(0) := '0'; end if; -- V (Overflow if negating the most negative number, e.g. -8 for 4-bit)

            when "111" => -- SHL: reg_operand_a << reg_operand_b (logical shift left)
                shift_amount := to_integer(op_b_usign); -- Convert shift amount from unsigned to integer
                res_usign := shift_left(op_a_usign, shift_amount);
                R3 := res_usign(3);
                -- Note: For SHL, carry flag could represent the last bit shifted out.
                -- Here, it's simplified. If a specific carry behavior is needed, it should be implemented.
                -- Example: if shift_amount > 0 and op_a_usign(op_a_usign'left - (shift_amount-1)) = '1' then C <= '1'

                if res_usign = 0 then result_flags(3) := '1'; else result_flags(3) := '0'; end if; -- Z
                result_flags(2) := R3;                                                           -- N
                result_flags(1) := '0'; -- C (Carry for SHL often means last bit shifted out, not implemented here simply)
                result_flags(0) := '0'; -- V (Overflow for logical shift left is usually not defined in this simple way)

            when others => -- Default case for any undefined opcodes
                res_usign := (others => 'X'); -- Undefined result for unknown opcode
                result_flags := "XXXX";       -- Undefined flags
        end case;

        -- Assign calculated values to the output signals of this process
        calc_result <= std_logic_vector(res_usign);
        calc_flag_z <= result_flags(3); -- Z is at index 3
        calc_flag_n <= result_flags(2); -- N is at index 2
        calc_flag_c <= result_flags(1); -- C is at index 1
        calc_flag_v <= result_flags(0); -- V is at index 0

    end process calculation_proc;


    -- =========================================================================
    -- Combinational Process to control the LEDs 
    -- This process drives the LED outputs based on the current FSM state
    -- and the calculated ALU results/flags.
    -- =========================================================================
    led_driver_proc: process(current_state, calc_flag_z, calc_flag_n, calc_flag_c, calc_flag_v, calc_result)
    begin
        -- Default LED states (all off)
        led_flags  <= "0000";
        led_result <= "0000";

        case current_state is
            -- Indicate current FSM state using 'led_result'
            when S_WAIT_OPCODE =>
                led_result(0) <= '1'; -- e.g., LED0 on indicates waiting for opcode
            when S_WAIT_OPCODE_CONFIRM =>
                led_result(1) <= '1'; -- e.g., LED1 on
            when S_WAIT_OPERAND_A =>
                led_result(2) <= '1'; -- e.g., LED2 on
            when S_WAIT_OPERAND_A_CONFIRM =>
                led_result(3) <= '1'; -- e.g., LED3 on
            when S_CHECK_OPCODE =>
                led_flags(0) <= '1';  -- e.g., FlagLED0 on
            when S_WAIT_OPERAND_B_OR_N =>
                led_flags(1) <= '1';  -- e.g., FlagLED1 on
            when S_WAIT_OPERAND_B_OR_N_CONFIRM =>
                led_flags(2) <= '1';  -- e.g., FlagLED2 on

            when S_CALCULATE_DISPLAY =>
                -- Display actual ALU flags and result
                led_flags(3) <= calc_flag_z; 
                led_flags(2) <= calc_flag_n;
                led_flags(1) <= calc_flag_c;
                led_flags(0) <= calc_flag_v;
                led_result   <= calc_result; -- Display the 4-bit ALU result

            when others =>
                -- Error or undefined state: Turn all LEDs on as an indicator
                led_flags  <= "1111";
                led_result <= "1111";
        end case;
    end process led_driver_proc;

end rtl;
