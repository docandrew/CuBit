package body HID_Feature_Commands with SPARK_Mode is
   use type D.Byte;
   use type D.Register_Number;
   function None (Index : Positive) return Command is
      pragma Unreferenced (Index);
   begin return (others => <>); end None;
   function Encode
     (Command_Register, Data_Register : D.Register_Number; ID : D.Byte;
      Payload : D.Bytes; Mode, Surface, Buttons : Boolean) return Command
   is
      R : Command := (ID => ID, Mode => Mode, Surface => Surface, Buttons => Buttons, others => <>);
      N : Natural := 0;
      Size : constant Natural := 2 + (if ID = 0 then 0 else 1) + Payload'Length;
      procedure Put (Value : D.Byte) with Pre => N < Maximum_Wire, Post => N = N'Old + 1 is
      begin N := N + 1; R.Data (N) := Value; end Put;
   begin
      Put (D.Byte (Command_Register mod 256)); Put (D.Byte (Command_Register / 256));
      Put (16#30# + (if ID < 15 then ID else 15)); Put (3);
      if ID >= 15 then Put (ID); end if;
      Put (D.Byte (Data_Register mod 256)); Put (D.Byte (Data_Register / 256));
      Put (D.Byte (Size)); Put (0);
      if ID /= 0 then Put (ID); end if;
      declare Header : constant Natural := N; begin
         pragma Assert (Header in 8 .. 10);
         for Index in Payload'Range loop
            pragma Loop_Invariant (N = Header + (Index - Payload'First));
            pragma Loop_Invariant (N >= 8);
            Put (Payload (Index));
         end loop;
      end;
      R.Length := N; return R;
   end Encode;
end HID_Feature_Commands;
