with AML_Delays;
with Ada.Text_IO;
with AML_Namespace;
with AML_Decode;
with AML_Objects;
procedure Static_Buffer_Count_Tests is
   use AML_Decode;
   package NS is new AML_Namespace (64, AML_Delays.Unavailable_Provider);
   use NS;
   use type AML_Objects.Object_Kind;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
   function B (S : String) return Bytes is
      R : Bytes (1 .. S'Length);
   begin
      for I in R'Range loop R (I) := Character'Pos (S (S'First + I - 1)); end loop;
      return R;
   end B;
   function Name (S : String; V : Bytes) return Bytes is
     (Bytes'(1 => 16#08#) & B (S) & V);
   function Buffer_Data (Size : Bytes; Raw : Bytes := []) return Bytes is
     (Bytes'(16#11#, Byte (1 + Size'Length + Raw'Length)) & Size & Raw);
   function Device (Data : Bytes) return Bytes is
     (Bytes'(16#5B#, 16#82#, Byte (5 + Data'Length)) & B ("DEV0") & Data);
   procedure Good (Width : Integer_Width; Data : Bytes; Expected : Natural; Nested : Boolean := False) is
      Tree : State := Empty;
      Status : Load_Status;
      Scope, Node : Node_ID;
      High : Bytes (Positive'Last - Data'Length + 1 .. Positive'Last) := Data;
   begin
      Load_Names (Tree, High, Width, Status);
      Check (Status = Loaded);
      Scope := (if Nested then Child (Tree, Root, "DEV0") else Root);
      Node := Child (Tree, Scope, "BUF0");
      Check (Node /= Root and then Kind (Tree, Node) = Buffer_Object);
      Check (AML_Objects.Length (Value_Store (Tree), Data_Object (Tree, Node)) = Expected);
      Check (AML_Objects.Kind (Value_Store (Tree), Data_Object (Tree, Node)) = AML_Objects.Buffer_Object);
      High := [others => 0];
      Check (AML_Objects.Length (Value_Store (Tree), Data_Object (Tree, Node)) = Expected);
   end Good;
   procedure Bad (Width : Integer_Width; Data : Bytes; Expected : Load_Status) is
      Tree : State := Empty;
      Status : Load_Status;
   begin
      Load_Names (Tree, Name ("KEEP", Bytes'(16#0A#, 7)), Width, Status);
      Check (Status = Loaded);
      declare Prior : constant State := Tree; begin
         Load_Names (Tree, Data, Width, Status);
         Check (Status = Expected); Check (Tree = Prior);
      end;
   end Bad;
   Three : constant Bytes := Name ("CNT0", Bytes'(16#0A#, 3));
   Named_Buffer : constant Bytes := Name ("BUF0", Buffer_Data (B ("CNT0")));
   Five : constant Bytes := Name ("CNT0", Bytes'(16#0A#, 5));
begin
   for Width in Integer_Width loop
      Good (Width, Three & Named_Buffer, 3);
      Good (Width, Name ("CNT0", Bytes'(1 => 16#0D#) & B ("12") & Bytes'(1 => 0)) & Named_Buffer, 18);
      Good (Width, Name ("CNT0", Bytes'(1 => 16#0D#) & B ("0x12") & Bytes'(1 => 0)) & Named_Buffer, 18);
      Good (Width, Name ("CNT0", Bytes'(1 => 16#0D#) & B ("0X12") & Bytes'(1 => 0)) & Named_Buffer, 18);
      Good (Width, Name ("CNT0", Bytes'(1 => 16#0D#) & B (" 12") & Bytes'(1 => 0)) & Named_Buffer, 18);
      Good (Width, Name ("CNT0", Bytes'(1 => 16#0D#) & B ("+3") & Bytes'(1 => 0)) & Named_Buffer, 0);
      Good (Width, Name ("CNT0", Bytes'(1 => 16#0D#) & B ("-1") & Bytes'(1 => 0)) & Named_Buffer, 0);
      Good (Width, Name ("CNT0", Bytes'(16#0D#, 0)) & Named_Buffer, 0);
      Good (Width, Name ("CNT0", Bytes'(1 => 16#0D#) & B ("Z3") & Bytes'(1 => 0)) & Named_Buffer, 0);
      Good (Width, Name ("CNT0", Bytes'(1 => 16#0D#) & B ("3Z") & Bytes'(1 => 0)) & Named_Buffer, 3);
      Good (Width, Name ("CNT0", Buffer_Data (Bytes'(16#0A#, 16), Bytes'(3, 0, 0, 0, 1, 0, 0, 0, 255))) & Named_Buffer, 3);
      Good (Width, Name ("CNT0", Bytes'(16#0D#, 16#33#, 0)) & Device (Named_Buffer), 3, True);
      Good (Width, Three & Device (Name ("CNT0", Buffer_Data (Bytes'(1 => 1), Bytes'(1 => 5))) & Named_Buffer), 5, True);
      Good (Width, Name ("CNT0", Bytes'(16#0D#, 16#33#, 0)) & Device (Five & Name ("BUF0", Buffer_Data (Bytes'(1 => 16#5C#) & B ("CNT0")))), 3, True);
      Good (Width, Name ("CNT0", Buffer_Data (Bytes'(1 => 1), Bytes'(1 => 3))) & Device (Five & Name ("BUF0", Buffer_Data (Bytes'(1 => 16#5E#) & B ("CNT0")))), 3, True);
      Good (Width, Name ("CNT0", Bytes'(16#0D#, 16#31#, 0)) &
        Name ("BUF0", Buffer_Data (B ("CNT0"), Bytes'(16#08#, 16#11#, 16#FF#, 0))) &
        Name ("NEXT", Bytes'(1 => 1)), 4);
      Bad (Width, Name ("CNT0", Buffer_Data (Bytes'(1 => 0))) & Named_Buffer, Unsupported_Opcode);
      Bad (Width, Name ("BUF0", Buffer_Data (B ("BUF0"))), Unsupported_Opcode);
      Bad (Width, Bytes'(16#5B#, 16#80#) & B ("REG0") & Bytes'(0, 0, 16#0A#, 1) &
        Bytes'(16#5B#, 16#81#, 11) & B ("REG0") & Bytes'(1 => 0) & B ("CNT0") & Bytes'(1 => 8) &
        Named_Buffer, Unsupported_Opcode);

      Bad (Width, Bytes'(16#14#, 6) & B ("CNT0") & Bytes'(1 => 0) & Named_Buffer, Unsupported_Opcode);
      Bad (Width, Name ("CNT0", Bytes'(16#0D#, 16#33#, 0)) & Named_Buffer & Bytes'(1 => 16#FF#), Unsupported_Opcode);
      Bad (Width, Name ("CNT0", Bytes'(16#0D#, 16#33#, 0)) &
        Name ("PKG0", Bytes'(16#13#, 5) & B ("CNT0")), Bad_Package);
      if Width = Bits_32 then
         Bad (Width, Name ("CNT0", Bytes'(1 => 16#0D#) & B ("100000003") & Bytes'(1 => 0)) & Named_Buffer, Value_Limit);
      else
         Good (Width, Name ("CNT0", Bytes'(1 => 16#0D#) & B ("100000003") & Bytes'(1 => 0)) & Named_Buffer, 3);
      end if;

      Good (Width, Three & Device (Named_Buffer), 3, True);
      Good (Width, Three & Device (Five & Named_Buffer), 5, True);
      Good (Width, Three & Device (Five & Name ("BUF0", Buffer_Data (Bytes'(1 => 16#5C#) & B ("CNT0")))), 3, True);
      Good (Width, Three & Device (Five & Name ("BUF0", Buffer_Data (Bytes'(1 => 16#5E#) & B ("CNT0")))), 3, True);
      Good (Width, Name ("BUF0", Buffer_Data (Bytes'(16#0A#, 2), Bytes'(1, 2, 3, 4))), 4);
      Good (Width, Name ("CNT0", Bytes'(16#0E#, 3, 0, 0, 0, 1, 0, 0, 0)) & Named_Buffer, 3);
      Bad (Width, Named_Buffer, Unsupported_Opcode);
      Bad (Width, Named_Buffer & Three, Unsupported_Opcode);
      Bad (Width, Name ("CNT0", Bytes'(16#12#, 2, 0)) & Named_Buffer, Unsupported_Opcode);
      Bad (Width, Name ("CNT0", Bytes'(1 => 16#FF#)) & Named_Buffer, Value_Limit);
      Bad (Width, Name ("CNT0", Bytes'(16#0B#, 1, 4)) & Named_Buffer, Value_Limit);
      Bad (Width, Three & Name ("BUF0", Bytes'(16#11#, 5, 16#43#, 16#4E#)), Bad_Buffer);
      Bad (Width, Three & Named_Buffer & Bytes'(1 => 16#FF#), Unsupported_Opcode);
   end loop;
   Ada.Text_IO.Put_Line ("STATIC-BUFFER-COUNT PASS" & Checks'Image);
end Static_Buffer_Count_Tests;
