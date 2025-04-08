library ieee;
use ieee.std_logic_1164.all;
 
entity ula is
  port (
    a    : in std_logic_vector (7 downto 4);         --primeira entrada
    b    : in std_logic_vector (3 downto 0);         --segunda entrada
    op : IN std_logic_vector (2 downto 0);           --operação
    flags : IN std_logic_vector (3 downto 0);        --vetor pras flags
    R : out std_logic                                --resultado
    );
end ula;
 
architecture behavioral of ula is
  signal and_gate : std_logic;

begin
    
  and_gate   <= a and b;
  R <= and_gate;

end behavioral;