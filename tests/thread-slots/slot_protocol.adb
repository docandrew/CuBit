pragma Ada_2022;

package body Slot_Protocol with SPARK_Mode => On is

   function Initial return State is
     ((Slots => Thread_Slots.Main_Only,
       Live  => [Main => True, others => False],
       Owner => [others => RP.Main_Thread_Slot]));

   procedure Create (S : in out State; T : Thread; Created : out Boolean) is
      Slot : Thread_Slots.Slot;
   begin
      Thread_Slots.Take (S.Slots, Slot, Created);
      if Created then
         S.Live (T) := True;
         S.Owner (T) := Slot;
      end if;
   end Create;

   procedure Finish (S : in out State; T : Thread) is
   begin
      Thread_Slots.Give_Back (S.Slots, S.Owner (T));
      S.Live (T) := False;
   end Finish;

   procedure Prove_One_Writer (S : State; T, U : Thread) is null;
end Slot_Protocol;
