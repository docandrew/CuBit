------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  CuBit.Handles.Lines - implementation
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Handles.Kernel;

package body CuBit.Handles.Lines is

   package K renames CuBit.Handles.Kernel;

   function Route
     (Vector    : LA.Line_Vector;
      Owner     : Process_ID;
      CPU       : LA.Route_CPU;
      Trigger   : LA.Trigger_Mode;
      Polarity  : LA.Pin_Polarity;
      Authority : Process;
      Lines     : Interrupt) return LA.Route_Outcome
   is
      Answer : constant Unsigned_64 :=
        K.Call (K.Enable_IRQ, Unsigned_64 (Vector), CuBit.Process_IDs.To_Word (Owner),
                LA.Route_Word (CPU, Trigger, Polarity), K.Word (Authority), K.Word (Lines));
   begin
      --  The kernel answers with an outcome word; anything else is no route.
      return (if LA.Is_Outcome_Word (Answer) then LA.To_Outcome (Answer)
              else LA.No_Line_Authority);
   end Route;

   function Acknowledge (Line : Interrupt; Vector : LA.Line_Vector) return Acknowledgement is
      Answer : constant Unsigned_64 :=
        K.Call (K.Enable_IRQ, Unsigned_64 (Vector), LA.No_Owner_Word,
                LA.Route_Acknowledge, LA.No_Authority_Word, K.Word (Line));
   begin
      if LA.Is_Acknowledged (Answer) then
         return (Accepted => True, Delivered => LA.Count_Of (Answer));
      end if;
      --  A refusal is an outcome word; anything else is no line authority.
      return (Accepted => False,
              Why => (if LA.Is_Outcome_Word (Answer) then LA.To_Outcome (Answer)
                      else LA.No_Line_Authority));
   end Acknowledge;

end CuBit.Handles.Lines;
