package body Observatory_Format_Budget with SPARK_Mode is
   function Current (Item : State) return Row_Index is (Item.Next);
   procedure Start (Item : in out State; Rows : Row_Count) is
   begin Item := (Next => 0, Limit => Rows); end Start;
   procedure Advance (Item : in out State) is
   begin Item.Next := Item.Next + 1; end Advance;
   procedure Cancel (Item : out State) is
   begin Item := (others => 0); end Cancel;
end Observatory_Format_Budget;
