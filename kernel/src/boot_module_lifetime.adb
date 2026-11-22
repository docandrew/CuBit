pragma Ada_2022;

package body Boot_Module_Lifetime with SPARK_Mode is
   procedure Advance (State : in out Phase; To : Phase) is
   begin
      State := To;
   end Advance;

   procedure Share (Table : out Reference_Table) is
   begin
      Table := [others => 1];
   end Share;

   procedure Acquire (Table : in out Reference_Table; First : Page_Offset;
                      Count : Page_Count; Granted : out Boolean)
   is
      Last : constant Page_Offset := First + (Count - 1);
   begin
      Granted := True;
      for P in First .. Last loop
         pragma Loop_Invariant
           (Granted = (for all Q in First .. P - 1 =>
                         Table (Q) in 1 .. Maximum_References - 1));
         if Table (P) not in 1 .. Maximum_References - 1 then
            Granted := False;
         end if;
      end loop;
      if not Granted then
         return;
      end if;
      for P in First .. Last loop
         pragma Loop_Invariant
           (for all Q in Table'Range =>
              (if Q in First .. P - 1 then Table (Q) = Table'Loop_Entry (Q) + 1
               else Table (Q) = Table'Loop_Entry (Q)));
         Table (P) := Table (P) + 1;
      end loop;
   end Acquire;

   procedure Drop (Table : in out Reference_Table; Page : Page_Offset;
                   Released : out Boolean) is
   begin
      Table (Page) := Table (Page) - 1;
      Released := Table (Page) = 0;
   end Drop;
end Boot_Module_Lifetime;
