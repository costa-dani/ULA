--==============================================================================
-- Entidade: ula_final_sync
-- Descrição: ULA com operações em C2 e indicação de estado nos LEDs.
--            Design síncrono aprimorado para evitar gated clocks.
-- Operações: ADD(000), SUB(001), INC(010), OR(011), AND(100), XOR(101), NEG(110), SHL(111)
-- Placa Alvo: Xilinx Spartan-3
--==============================================================================
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity Proojeto is
    Port ( clk          : in  STD_LOGIC;
           btn_confirm  : in  STD_LOGIC;
           sw           : in  STD_LOGIC_VECTOR (3 downto 0);
           led_flags    : out STD_LOGIC_VECTOR (3 downto 0);
           led_result   : out STD_LOGIC_VECTOR (3 downto 0)
          );
end Proojeto;

--==============================================================================
-- Arquitetura: rtl
--==============================================================================
architecture rtl of Proojeto is

    -- --- Tipos ---
    type state_t is ( S_WAIT_OPCODE, S_WAIT_OPCODE_CONFIRM,
                      S_WAIT_OPERAND_A, S_WAIT_OPERAND_A_CONFIRM,
                      S_CHECK_OPCODE,
                      S_WAIT_OPERAND_B_OR_N, S_WAIT_OPERAND_B_OR_N_CONFIRM,
                      S_CALCULATE_DISPLAY );

    -- --- Constantes ---
    constant DEBOUNCE_LIMIT : integer := 35000000;

    -- --- Sinais Internos ---
    -- Estado da máquina
    signal current_state, next_state : state_t := S_WAIT_OPCODE;

    -- Debounce
    signal btn_confirm_delayed : std_logic := '0';
    signal btn_confirm_sync    : std_logic_vector(1 downto 0) := (others => '0');
    signal btn_first: std_logic := '0';
signal btn_confirm_debounced: std_logic := '0';
    signal btn_counter         : integer range 0 to DEBOUNCE_LIMIT := 0;
    signal btn_pressed_edge    : std_logic := '0';
type btn_confirm_state is (not_on, is_on);
    signal state_b : btn_confirm_state := not_on;

    -- Registradores
    signal reg_opcode    : std_logic_vector(2 downto 0) := "000";
    signal reg_operand_a : std_logic_vector(3 downto 0) := (others => '0');
    signal reg_operand_b : std_logic_vector(3 downto 0) := (others => '0');

    -- Sinais de saída calculados (combinacionais baseados nos regs)
    signal calc_result : std_logic_vector(3 downto 0);
    signal calc_flag_z : std_logic;
    signal calc_flag_n : std_logic;
    signal calc_flag_c : std_logic;
    signal calc_flag_v : std_logic;

    -- NOVOS SINAIS: Habilitação para os registradores (Clock Enables)
    signal load_opcode_en : std_logic := '0';
    signal load_op_a_en   : std_logic := '0';
    signal load_op_b_en   : std_logic := '0';

begin

    -- =========================================================================
    -- Lógica de Debounce do Botão
    -- =========================================================================
    process(clk)
    begin
        if rising_edge(clk) then
            case state_b is
   
                when not_on =>
                    if btn_confirm /= btn_pressed_edge then  -- Click no botão
                        if btn_first = '0' then
                            btn_counter <= 0;                -- Reseta o contador
                            state_b <= is_on;                -- Lógica é acionada
                        else  
                            btn_counter <= 0;                -- Reseta o contador
                            state_b <= is_on;                -- Lógica é acionada
                        end if;
                    end if;
   
                when is_on =>
                    if btn_counter < DEBOUNCE_LIMIT then
                        btn_counter <= btn_counter + 1;
                    else
                        if btn_first = '0' then
                            btn_first <= '1';
                            state_b <= not_on;               -- Muda o estado do botão na lógica do debounce
                        else
                            if btn_confirm_debounced = '0' then  -- Se o pulso ainda não foi feito
                                btn_pressed_edge <= '1';         -- Pulsa o botão no programa
                                btn_confirm_debounced <= '1';    -- Diz que o pulso foi autorizado
                            else
                                btn_pressed_edge <= '0';         -- Desaciona o pulso
                                if btn_confirm = '1' then        -- O usuário está segurando o botão
                                    -- Não faz nada
                                else                             -- O usuário soltou o botão
                                    btn_confirm_debounced <= '0';
                                    state_b <= not_on;           -- Muda o estado do botão na lógica do debounce
                                end if;
                            end if;
                        end if;
                    end if;
   
            end case;
        end if;
    end process;
   
    -- =========================================================================
    -- Processo Síncrono Principal: Atualiza Estado e Registradores com Habilitação
    -- =========================================================================
    sync_proc: process(clk)
    begin
        if rising_edge(clk) then
            -- Atualiza o estado atual
            current_state <= next_state;

            -- Atualiza registradores APENAS quando habilitado (Clock Enable inferido)
            if load_opcode_en = '1' then
                reg_opcode <= sw(2 downto 0);
            end if;

            if load_op_a_en = '1' then
                reg_operand_a <= sw;
            end if;

            if load_op_b_en = '1' then
                reg_operand_b <= sw;
            end if;
        end if;
    end process sync_proc;


    -- =========================================================================
    -- Lógica Combinacional: Calcula Próximo Estado e Sinais de Habilitação
    -- =========================================================================
    fsm_comb_logic_proc: process(current_state, btn_pressed_edge, sw, reg_opcode)
    begin
        -- Padrões: Manter estado, não habilitar cargas
        next_state <= current_state;
        load_opcode_en <= '0';
        load_op_a_en   <= '0';
        load_op_b_en   <= '0';

        case current_state is
            when S_WAIT_OPCODE =>
                if btn_pressed_edge = '1' then
                    next_state <= S_WAIT_OPCODE_CONFIRM;
                    -- Prepara para carregar opcode no *próximo* ciclo, quando entrar em S_WAIT_OPCODE_CONFIRM
                end if;

            when S_WAIT_OPCODE_CONFIRM =>
                -- Habilita carga do opcode *neste* ciclo (será efetivado na prox subida do clk)
                load_opcode_en <= '1';
                next_state <= S_WAIT_OPERAND_A;

            when S_WAIT_OPERAND_A =>
                if btn_pressed_edge = '1' then
                    next_state <= S_WAIT_OPERAND_A_CONFIRM;
                     -- Prepara para carregar op A no *próximo* ciclo
                end if;

            when S_WAIT_OPERAND_A_CONFIRM =>
                -- Habilita carga do operando A *neste* ciclo
                load_op_a_en <= '1';
                next_state <= S_CHECK_OPCODE;

            when S_CHECK_OPCODE =>
                -- Lógica de decisão (combinacional) baseada no opcode JÁ carregado
                case reg_opcode is
                    when "000" | "001" | "011" | "100" | "101" | "111" =>
                        next_state <= S_WAIT_OPERAND_B_OR_N;
                    when "010" | "110" =>
                         next_state <= S_CALCULATE_DISPLAY;
                    when others =>
                         next_state <= S_CALCULATE_DISPLAY;
                end case;

            when S_WAIT_OPERAND_B_OR_N =>
                 if btn_pressed_edge = '1' then
                    next_state <= S_WAIT_OPERAND_B_OR_N_CONFIRM;
                     -- Prepara para carregar op B no *próximo* ciclo
                 end if;

            when S_WAIT_OPERAND_B_OR_N_CONFIRM =>
                 -- Habilita carga do operando B / N *neste* ciclo
                 load_op_b_en <= '1';
                 next_state <= S_CALCULATE_DISPLAY;

            when S_CALCULATE_DISPLAY =>
                 if btn_pressed_edge = '1' then
                    next_state <= S_WAIT_OPCODE;
                 end if;

            when others =>
                next_state <= S_WAIT_OPCODE;
        end case;
    end process fsm_comb_logic_proc;


    -- =========================================================================
    -- Lógica Combinacional - Cálculo das Operações e Flags (Inalterada em sua lógica interna)
    -- =========================================================================
    -- Este processo agora depende dos *registradores* (reg_*) que são atualizados sincronamente.
    -- Os resultados (calc_*) são combinacionais baseados nos valores estáveis dos registradores.
    calculation_proc: process(reg_opcode, reg_operand_a, reg_operand_b) -- Não depende mais do current_state diretamente aqui
      variable op_a_ext     : unsigned(4 downto 0);
      variable op_b_ext     : unsigned(4 downto 0);
      variable res_ext      : unsigned(4 downto 0);
      variable op_a_usign   : unsigned(3 downto 0);
      variable op_b_usign   : unsigned(3 downto 0);
      variable res_usign    : unsigned(3 downto 0);
      variable shift_amount : integer range 0 to 15;
      variable result_flags : std_logic_vector(3 downto 0);
      alias A3 : std_logic is reg_operand_a(3);
      alias B3 : std_logic is reg_operand_b(3);
      variable R3 : std_logic;
    begin
        -- Calcula sempre baseado nos regs, mas a exibição final só usa no estado certo
        result_flags := "0000";
        res_usign := (others => '0'); -- Inicializa resultado temporário
        R3 := '0';
        op_a_usign := unsigned(reg_operand_a);
        op_b_usign := unsigned(reg_operand_b);

        case reg_opcode is
             when "000" => -- ADD
                 op_a_ext := unsigned('0' & reg_operand_a); op_b_ext := unsigned('0' & reg_operand_b);
                 res_ext  := op_a_ext + op_b_ext; res_usign := res_ext(3 downto 0); R3 := res_usign(3);
                 if res_usign = 0 then result_flags(3) := '1'; else result_flags(3) := '0'; end if; -- Z
                 result_flags(2) := R3; result_flags(1) := res_ext(4); -- N, C
                 if (A3 = B3) and (A3 /= R3) then result_flags(0) := '1'; else result_flags(0) := '0'; end if; -- V
             when "001" => -- SUB
                 op_a_ext := unsigned('0' & reg_operand_a); op_b_ext := unsigned('0' & (not reg_operand_b));
                 res_ext  := op_a_ext + op_b_ext + 1; res_usign := res_ext(3 downto 0); R3 := res_usign(3);
                 if res_usign = 0 then result_flags(3) := '1'; else result_flags(3) := '0'; end if; -- Z
                 result_flags(2) := R3; result_flags(1) := res_ext(4); -- N, C
                 if (A3 /= B3) and (B3 = R3) then result_flags(0) := '1'; else result_flags(0) := '0'; end if; -- V
            when "010" => -- INC
                 op_a_ext := unsigned('0' & reg_operand_a); res_ext  := op_a_ext + 1; res_usign := res_ext(3 downto 0); R3 := res_usign(3);
                 if res_usign = 0 then result_flags(3) := '1'; else result_flags(3) := '0'; end if; -- Z
                 result_flags(2) := R3; result_flags(1) := res_ext(4); -- N, C
                 if reg_operand_a = "0111" then result_flags(0) := '1'; else result_flags(0) := '0'; end if; -- V
             when "011" => -- OR
                 res_usign := op_a_usign or op_b_usign; R3 := res_usign(3);
                 if res_usign = 0 then result_flags(3) := '1'; else result_flags(3) := '0'; end if; -- Z
                 result_flags(2) := R3; result_flags(1) := '0'; result_flags(0) := '0'; -- N, C, V
             when "100" => -- AND
                 res_usign := op_a_usign and op_b_usign; R3 := res_usign(3);
                 if res_usign = 0 then result_flags(3) := '1'; else result_flags(3) := '0'; end if; -- Z
                 result_flags(2) := R3; result_flags(1) := '0'; result_flags(0) := '0'; -- N, C, V
             when "101" => -- XOR
                 res_usign := op_a_usign xor op_b_usign; R3 := res_usign(3);
                 if res_usign = 0 then result_flags(3) := '1'; else result_flags(3) := '0'; end if; -- Z
                 result_flags(2) := R3; result_flags(1) := '0'; result_flags(0) := '0'; -- N, C, V
            when "110" => -- NEG
                 op_a_ext := unsigned('0' & (not reg_operand_a)); res_ext  := op_a_ext + 1; res_usign := res_ext(3 downto 0); R3 := res_usign(3);
                 if res_usign = 0 then result_flags(3) := '1'; else result_flags(3) := '0'; end if; -- Z
                 result_flags(2) := R3; result_flags(1) := res_ext(4); -- N, C
                 if reg_operand_a = "1000" then result_flags(0) := '1'; else result_flags(0) := '0'; end if; -- V
             when "111" => -- SHL
                 shift_amount := to_integer(op_b_usign); res_usign := shift_left(op_a_usign, shift_amount); R3 := res_usign(3);
                 if res_usign = 0 then result_flags(3) := '1'; else result_flags(3) := '0'; end if; -- Z
                 result_flags(2) := R3; result_flags(1) := '0'; result_flags(0) := '0'; -- N, C, V
             when others =>
                 res_usign := (others => '0'); result_flags := "0000";
         end case;

        -- Atribui aos sinais de saída calculados
        calc_result <= std_logic_vector(res_usign);
        calc_flag_z <= result_flags(3);
        calc_flag_n <= result_flags(2);
        calc_flag_c <= result_flags(1);
        calc_flag_v <= result_flags(0);

    end process calculation_proc;


    -- =========================================================================
    -- Processo Combinacional para controlar os LEDs (Inalterado)
    -- =========================================================================
    led_driver_proc: process(current_state, calc_flag_z, calc_flag_n, calc_flag_c, calc_flag_v, calc_result)
    begin
        led_flags  <= "0000"; led_result <= "0000"; -- Padrão
        case current_state is
            when S_WAIT_OPCODE => led_result(0) <= '1';
            when S_WAIT_OPCODE_CONFIRM => led_result(1) <= '1';
            when S_WAIT_OPERAND_A => led_result(2) <= '1';
            when S_WAIT_OPERAND_A_CONFIRM => led_result(3) <= '1';
            when S_CHECK_OPCODE => led_flags(0) <= '1';
            when S_WAIT_OPERAND_B_OR_N => led_flags(1) <= '1';
            when S_WAIT_OPERAND_B_OR_N_CONFIRM => led_flags(2) <= '1';
            when S_CALCULATE_DISPLAY =>
                led_flags(3) <= calc_flag_z; led_flags(2) <= calc_flag_n;
                led_flags(1) <= calc_flag_c; led_flags(0) <= calc_flag_v;
                led_result   <= calc_result;
            when others => led_flags  <= "1111"; led_result <= "1111";
        end case;
    end process led_driver_proc;

end rtl;
