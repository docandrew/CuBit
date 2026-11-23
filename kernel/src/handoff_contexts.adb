pragma Ada_2022;

package body Handoff_Contexts with SPARK_Mode is

   function Empty return Table is
     (Is_Sealed => False, Phases => [others => Untracked]);

   procedure Admit (T : in out Table; Who : Requester; Admitted : out Boolean) is
   begin
      Admitted := T.Phases (Who) = Untracked;
      if Admitted then
         T.Phases (Who) := Passing_Through;
      end if;
   end Admit;

   procedure Seal (T : in out Table) is
   begin
      T.Is_Sealed := True;
   end Seal;

   procedure Switch (T : in out Table; Who : Requester; Was_Passing : out Boolean) is
   begin
      Was_Passing := T.Phases (Who) = Passing_Through;
      if Was_Passing then
         T.Phases (Who) := Translated;
      end if;
   end Switch;

end Handoff_Contexts;
