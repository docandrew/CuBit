with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Cursor_Collect;
package body Intel_GPU_Native_Cursor is
   use Interfaces;
   use Intel_GPU_Display_Topology;
   Active, Authorized : Boolean := False;
   -- Linux v6.16 intel_cursor_regs.h and TGL_CURSOR_OFFSETS:
   -- CURCNTR, CURBASE, CURSURFLIVE, CUR_FBC_CTL. B is A + 0x1000.
   Offsets : constant array (0 .. 3) of Unsigned_32 :=
     [16#70080#, 16#70084#, 16#700AC#, 16#700A0#];
   procedure Begin_Access (Success : out Boolean) is
   begin
      Success := Authorized and then not Active and then Power_Held;
      if Success then Active := True; end if;
   end Begin_Access;
   procedure End_Access (Success : out Boolean) is
   begin
      Success := Active and then Power_Held; Active := False;
   end End_Access;
   procedure Read_Field (Index : Natural; Value : out Unsigned_32; Success : out Boolean) is
   begin
      Value := Unsigned_32'Last; Success := False;
      if not Active or else not Authorized or else not Power_Held
        or else Index not in Offsets'Range
      then return; end if;
      declare
         Address : constant Integer_Address := 16#6000_0000# +
           Integer_Address (Offsets (Index)) + 16#1000# * Pipe'Pos (Item);
         Register_Value : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Address);
      begin Value := Register_Value; end;
      Success := Value /= Unsigned_32'Last;
   end Read_Field;
   package Collector is new Intel_GPU_Cursor_Collect (Begin_Access, End_Access, Read_Field);
   function Diagnostic (Value : Observation) return String is
   begin
      case Value.Status is
         when Rejected => return "rejected prerequisites";
         when Power_Unavailable => return "power unavailable; no register sample";
         when Read_Failed =>
            return "register read failed after" & Natural'Image (Value.Reads) & " reads";
         when Access_End_Failed => return "power lost at end of sample";
         when Complete =>
            case Value.Decoded.State is
               when Intel_GPU_Cursor_Decode.Invalid_Read => return "invalid read";
               when Intel_GPU_Cursor_Decode.Changing => return "changing";
               when Intel_GPU_Cursor_Decode.Disabled => return "disabled";
               when Intel_GPU_Cursor_Decode.Unsupported => return "unsupported";
               when Intel_GPU_Cursor_Decode.Invalid_Geometry => return "invalid geometry";
               when Intel_GPU_Cursor_Decode.Ready => return "ready";
            end case;
      end case;
   end Diagnostic;
   function Inspect (Owner : Boolean; Table_Bytes : Unsigned_64) return Observation is
      Result : Collector.Observation;
      use type Collector.Outcome;
   begin
      if Active or else not Owner or else Table_Bytes not in 2_097_152 | 4_194_304 | 8_388_608 then
         return (others => <>);
      end if;
      Authorized := True;
      Collector.Inspect (Table_Bytes, Result);
      Authorized := False;
      return (Status => (case Result.State is
                 when Collector.Access_Unavailable => Power_Unavailable,
                 when Collector.Read_Failed => Read_Failed,
                 when Collector.Access_End_Failed => Access_End_Failed,
                 when Collector.Collected => Complete),
              Collected => Result.State = Collector.Collected, Reads => Result.Reads,
              Before => Result.Before, After => Result.After, Decoded => Result.Decoded);
   end Inspect;
end Intel_GPU_Native_Cursor;
