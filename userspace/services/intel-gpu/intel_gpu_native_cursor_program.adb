with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
package body Intel_GPU_Native_Cursor_Program is
   package CP renames Intel_GPU_Cursor_Program;
   Count : Natural := 0;

   procedure Write (Field : CP.Field; Value : Unsigned_32) is
      Register : Unsigned_32 with Import, Volatile_Full_Access,
        Address => Register_Page + Storage_Offset (CP.Page_Offset (Field));
   begin
      Register := Value;
      if Count < Natural'Last then Count := Count + 1; end if;
   end Write;

   function Apply
     (Owner : Boolean; Values : CP.Register_Values; Order : CP.Write_Sequence)
      return Outcome is
   begin
      if not Owner then return Rejected; end if;
      if not Power_Held then return Power_Unavailable; end if;
      for Field of Order loop
         Write (Field, CP.Value (Values, Field));
      end loop;
      return Written;
   end Apply;

   function Program (Owner : Boolean; Values : CP.Register_Values)
      return Outcome is (Apply (Owner, Values, CP.Full_Update));
   function Move (Owner : Boolean; Values : CP.Register_Values)
      return Outcome is (Apply (Owner, Values, CP.Move_Update));
   function Writes return Natural is (Count);
end Intel_GPU_Native_Cursor_Program;
