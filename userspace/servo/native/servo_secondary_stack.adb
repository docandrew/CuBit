with System.Parameters;
with System.Storage_Elements; use System.Storage_Elements;

package body Servo_Secondary_Stack is
   package SS renames System.Secondary_Stack;
   use type SS.Mark_Id;
   Scratch : aliased SS.SS_Stack
     (System.Parameters.Runtime_Default_Sec_Stack_Size);
   for Scratch'Alignment use 16;
   Initialized : Boolean := False;

   function Get return SS.SS_Stack_Ptr is
   begin
      -- SS_Init only writes the runtime's counters; it does not allocate.
      -- Every allocating/marking runtime entry asks this wrapped getter first,
      -- including any call made during standalone library elaboration.
      if not Initialized then
         SS.SS_Init (Scratch'Access);
         Initialized := True;
      end if;
      return Scratch'Access;
   end Get;

   function Check return Interfaces.Unsigned_32 is
      Saved : constant SS.Mark_Id := SS.SS_Mark;
      Address : System.Address;
      Valid : Boolean;
   begin
      SS.SS_Allocate (Address, 64);
      Valid := To_Integer (Address) mod 16 = 0 and then
        To_Integer (Address) >= To_Integer (Scratch'Address) and then
        To_Integer (Address) <=
          To_Integer (Scratch'Address) + Scratch'Size / 8 - 64;
      SS.SS_Release (Saved);
      return (if Valid and then SS.SS_Mark = Saved then 1 else 0);
   end Check;
end Servo_Secondary_Stack;
