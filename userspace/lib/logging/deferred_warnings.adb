pragma Ada_2022;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Logging;
with CuBit.Log_Records;

package body Deferred_Warnings is
   package Logs renames CuBit.Log_Records;

   --  The driver budget xhci and intel-gpu publish under too.
   Driver_Budget : constant CuBit.Log_Protocol.Budget_Id := 15;
   CAP_ENDPOINT  : constant Unsigned_64 := 1;
   RIGHT_READ_WRITE : constant Unsigned_64 := 3;

   Capacity   : constant := 32;
   Line_Bytes : constant := 240;
   type Line is record
      Text   : String (1 .. Line_Bytes) := [others => ' '];
      Length : Natural range 0 .. Line_Bytes := 0;
      Level  : Logs.Severity := Logs.Warning;
   end record;
   Kept  : array (1 .. Capacity) of Line;
   Count : Natural range 0 .. Capacity := 0;
   Lost  : Unsigned_64 := 0;

   type Phase is (Unbound, Bound, Failed);
   State  : Phase := Unbound;
   Writer : CuBit.Logging.Publisher;

   Hex_Digits : constant String (1 .. 16) := "0123456789ABCDEF";
   function Hex (Value : Unsigned_64) return String is
      Text  : String (1 .. 16) := [others => '0'];
      V     : Unsigned_64 := Value;
      First : Positive range 1 .. 16 := 16;
   begin
      for I in reverse Text'Range loop
         Text (I) := Hex_Digits (Natural (V and 16#F#) + 1);
         V := Shift_Right (V, 4);
         if Text (I) /= '0' then
            First := I;
         end if;
      end loop;
      return "0x" & Text (First .. 16);
   end Hex;

   procedure Pump_Now;

   procedure Emit (Text : String; Level : Logs.Severity := Logs.Warning) is
      Made : constant Logs.Decoded := Logs.Make (Text, Level);
      Accepted : Boolean;
   begin
      if Made.Success then
         CuBit.Logging.Emit (Writer, Made.Value, Accepted);
      end if;
   end Emit;

   procedure Write (Text : String; Level : Logs.Severity);
   procedure Warn (Text : String) is
   begin
      Write (Text, Logs.Warning);
   end Warn;
   procedure Inform (Text : String) is
   begin
      Write (Text, Logs.Information);
   end Inform;

   procedure Write (Text : String; Level : Logs.Severity) is
      Length : constant Natural := Natural'Min (Text'Length, Line_Bytes);
   begin
      debugPrint (Text & ASCII.LF);
      Pump_Now;
      if State = Bound then
         Emit (Text, Level);
      elsif State = Unbound then
         if Count < Capacity then
            Count := Count + 1;
            Kept (Count).Text (1 .. Length) := Text (Text'First .. Text'First + Length - 1);
            Kept (Count).Length := Length;
            Kept (Count).Level := Level;
         elsif Lost < Unsigned_64'Last then
            Lost := Lost + 1;
         end if;
      end if;
   end Write;

   function Pending return Boolean is (State = Unbound and then (Count /= 0 or else Lost /= 0));

   procedure Pump is
   begin
      Pump_Now;
   end Pump;

   procedure Pump_Now is
      Collector : Process_ID;
   begin
      if State /= Unbound then
         return;
      end if;
      Collector := Registered_Driver (DRIVER_LOGSTORE);
      if Collector = No_Process then
         return;
      end if;
      if syscall (SYSCALL_POLICY_MINT_CAPABILITY, syscall (SYSCALL_GETPID),
                  CAP_ENDPOINT, To_Word (Collector),
                  CuBit.Log_Protocol.Publisher_Tag (Driver_Budget, Issuance),
                  RIGHT_READ_WRITE, CuBit.Log_Protocol.Publisher_Slot) = Unsigned_64'Last
      then
         State := Failed;
         debugPrint ("deferred warnings: no logstore binding" & ASCII.LF);
         return;
      end if;
      State := Bound;
      for I in 1 .. Count loop
         Emit (Kept (I).Text (1 .. Kept (I).Length), Kept (I).Level);
      end loop;
      Count := 0;
      if Lost /= 0 then
         Emit ("deferred warnings: lost" & Unsigned_64'Image (Lost) & " before logstore started");
      end if;
   end Pump_Now;
end Deferred_Warnings;
