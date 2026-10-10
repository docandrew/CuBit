with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Resource_Templates;
procedure Resource_Tests is
   use type Byte;
   package P is new AML_Resource_Templates (256);
   package Tiny is new AML_Resource_Templates (1);
   package Exact is new AML_Resource_Templates (5);
   use type P.Scan_Status;
   use type P.Build_Status;
   use type Tiny.Build_Status;
   use type Exact.Build_Status;
   Checks : Natural := 0;
   End_Data : constant Bytes := [16#79#, 0];
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then
         raise Program_Error with Checks'Image;
      end if;
   end Check;
   procedure Scan (Data : Bytes; Status : P.Scan_Status; Prefix : Natural := 0) is
      Result : constant P.Location := P.Locate_End (Data);
   begin
      Check (Result.Status = Status);
      if Status = P.Located then
         Check (Result.Prefix_Length = Prefix);
      end if;
   end Scan;
   procedure Compare (Left, Right : Bytes; Status : P.Build_Status; Expected : Bytes := [1 .. 0 => 0]) is
      Result : constant P.Build_Result := P.Build (Left, Right);
   begin
      Check (Result.Status = Status);
      if Status = P.Built then
         Check (Result.Length = Expected'Length);
         Check (Result.Data (1 .. Result.Length) = Expected);
         for I in Result.Length + 1 .. Result.Data'Last loop
            Check (Result.Data (I) = 0);
         end loop;
      end if;
   end Compare;
   function Large_Min (Name : Natural) return Natural is
     (case Name is when 1 | 6 | 14 | 19 => 9, when 2 => 12, when 4 => 0,
       when 5 | 15 | 18 => 17, when 7 => 23, when 8 => 13, when 9 => 6,
       when 10 => 43, when 11 => 53, when 12 => 20, when 13 => 15,
       when 16 => 11, when 17 => 14, when others => 0);
   Data : Bytes (1 .. 128) := [others => 0];
   Expected_Status : P.Scan_Status;
begin
   Scan ([1 .. 0 => 0], P.Located);
   Scan ([1 => 16#79#], P.No_End_Tag);
   Scan ([16#70#,16#79#,0], P.Located,1);
   Scan ([16#70#,16#70#], P.Buffer_Length);
   Scan ([16#70#,16#70#,16#79#], P.Buffer_Length);
   for Tag in Natural range 0 .. 127 loop
      declare
         Name : constant Natural := Tag / 8;
         Len : constant Natural := Tag mod 8;
      begin
         Data := [others => 0];
         Data (1) := Byte (Tag);
         Data (Len + 2) := 16#79#;
         Data (Len + 3) := 0;
         Expected_Status :=
           (if Name in 0 .. 3 | 11 .. 13 then P.Invalid_Resource_Type
            elsif (Name=4 and Len in 2..3) or (Name=5 and Len=2) or (Name=6 and Len<=1)
              or (Name=7 and Len=0) or (Name=8 and Len=7) or (Name=9 and Len=3)
              or (Name=10 and Len=5) or Name=14 or (Name=15 and Len=1)
            then P.Located else P.Bad_Resource_Length);
         Scan (Data (1 .. Len+3), Expected_Status, (if Name=15 then 0 else Len+1));
      end;
   end loop;
   for Name in Natural range 0 .. 127 loop
      for Len in Natural range 0 .. 60 loop
         Data := [others => 0];
         Data (1) := Byte (128+Name);
         Data (2) := Byte (Len);
         Data (6) := 1; -- legal common SerialBus type, if this descriptor uses it
         Data (Len+4) := 16#79#;
         Data (Len+5) := 0;
         Expected_Status :=
           (if Name in 0 | 3 | 20 .. 127 then P.Invalid_Resource_Type
            elsif Len < Large_Min (Name) or else (Name in 1 | 2 | 5 | 6 | 11 and Len /= Large_Min (Name))
            then P.Bad_Resource_Length else P.Located);
         Scan (Data, Expected_Status, Len+3);
      end loop;
   end loop;
   for Kind in Natural range 0 .. 255 loop
      Data := [others => 0];
      Data (1) := 16#8E#;
      Data (2) := 9;
      Data (6) := Byte (Kind);
      Data (13) := 16#79#;
      Scan (Data (1..14), (if Kind in 1..4 then P.Located else P.Invalid_Resource_Type),12);
   end loop;
   for Len in Natural range 2 .. 5 loop
      Data := [others=>0];
      Data(1):=16#84#;
      Scan(Data(1..Len),P.Buffer_Length);
   end loop;
   Scan ([16#84#,0,0,16#79#,0,0],P.Located,3);
   Scan ([16#84#,255,255,0,0,0],P.Buffer_Length);
   Scan ([16#82#,0,0,0,0,0],P.Bad_Resource_Length);
   Scan ([16#77#,0,0,0],P.Buffer_Length);
   Compare ([],[],P.Built,End_Data);
   Compare ([16#72#,16#79#,16#AA#,16#79#,16#FF#],[16#71#,16#42#,16#79#,0],P.Built,
     [16#72#,16#79#,16#AA#,16#71#,16#42#,16#79#,0]);
   Compare ([16#79#,0,255],[],P.Built,End_Data);
   Compare ([16#78#,0],[0,0],P.Bad_Resource_Length);
   Compare ([],[0,0],P.Invalid_Resource_Type);
   declare
      R : constant Tiny.Build_Result := Tiny.Build ([],[]);
   begin
      Check(R.Status=Tiny.Output_Limit);
   end;
   declare
      R : constant Tiny.Build_Result := Tiny.Build ([0,0],[]);
   begin
      Check(R.Status=Tiny.Invalid_Resource_Type);
   end;
   declare
      R : constant Exact.Build_Result := Exact.Build ([16#72#,1,2,16#79#,0],[]);
   begin
      Check(R.Status=Exact.Built);
   end;
   declare
      R : constant Exact.Build_Result := Exact.Build ([16#73#,1,2,3,16#79#,0],[]);
   begin
      Check(R.Status=Exact.Output_Limit);
   end;
   declare High : constant Bytes (Positive'Last-5..Positive'Last) := [16#84#,0,0,16#79#,0,0];
   begin
      Scan(High,P.Located,3);
      Compare(High,[],P.Built,[16#84#,0,0,16#79#,0]);
   end;
   declare Wide : Bytes(1..65_537):=[others=>255];
   begin Wide(1):=16#79#;Wide(2):=0;Scan(Wide,P.Located);
   end;
   Compare ([], [], P.Built, [121,0]); -- 1-EMPTY
   Compare ([121,90], [121,0], P.Built, [121,0]); -- 1-CHECK
   Compare ([121,0,255,255], [121,0], P.Built, [121,0]); -- 1-TRAIL
   Compare ([114,121,170,121,0], [113,66,121,0], P.Built, [114,121,170,113,66,121,0]); -- 1-PAYLOAD
   Compare ([121], [121,0], P.No_End_Tag); -- 1-SINGLE
   Compare ([113,66], [121,0], P.No_End_Tag); -- 1-MISSING
   Compare ([120,0], [121,0], P.Bad_Resource_Length); -- 1-ENDZERO
   Compare ([122,0,0], [121,0], P.Bad_Resource_Length); -- 1-ENDTWO
   Compare ([0,0,121,0], [121,0], P.Invalid_Resource_Type); -- 1-RESERVED
   Compare ([42,0,0,121,0], [121,0], P.Built, [42,0,0,121,0]); -- 1-FIXED
   Compare ([41,0,121,0], [121,0], P.Bad_Resource_Length); -- 1-BADFIX
   Compare ([119,0,121,0], [121,0], P.Buffer_Length); -- 1-OVERRUN
   Compare ([132,0,0,121,0], [121,0], P.Buffer_Length); -- 1-LARGE
   Compare ([132,1,0,66,121,0], [121,0], P.Built, [132,1,0,66,121,0]); -- 1-LARGEFULL
   Compare ([132,0], [121,0], P.Buffer_Length); -- 1-HEADER
   Compare ([131,1,0,0,121,0], [121,0], P.Invalid_Resource_Type); -- 1-LARGERES
   Compare ([121,0,0,0], [121,0], P.Built, [121,0]); -- 1-INTEGER
   Compare ([121,0], [121,0], P.Built, [121,0]); -- 1-STRING
   Compare ([0,0,0,0], [121,0], P.Invalid_Resource_Type); -- 1-BADINTEGER
   Compare ([120,0], [0,0], P.Bad_Resource_Length); -- 1-ERROR
   Compare ([], [], P.Built, [121,0]); -- 2-EMPTY
   Compare ([121,90], [121,0], P.Built, [121,0]); -- 2-CHECK
   Compare ([121,0,255,255], [121,0], P.Built, [121,0]); -- 2-TRAIL
   Compare ([114,121,170,121,0], [113,66,121,0], P.Built, [114,121,170,113,66,121,0]); -- 2-PAYLOAD
   Compare ([121], [121,0], P.No_End_Tag); -- 2-SINGLE
   Compare ([113,66], [121,0], P.No_End_Tag); -- 2-MISSING
   Compare ([120,0], [121,0], P.Bad_Resource_Length); -- 2-ENDZERO
   Compare ([122,0,0], [121,0], P.Bad_Resource_Length); -- 2-ENDTWO
   Compare ([0,0,121,0], [121,0], P.Invalid_Resource_Type); -- 2-RESERVED
   Compare ([42,0,0,121,0], [121,0], P.Built, [42,0,0,121,0]); -- 2-FIXED
   Compare ([41,0,121,0], [121,0], P.Bad_Resource_Length); -- 2-BADFIX
   Compare ([119,0,121,0], [121,0], P.Buffer_Length); -- 2-OVERRUN
   Compare ([132,0,0,121,0], [121,0], P.Buffer_Length); -- 2-LARGE
   Compare ([132,1,0,66,121,0], [121,0], P.Built, [132,1,0,66,121,0]); -- 2-LARGEFULL
   Compare ([132,0], [121,0], P.Buffer_Length); -- 2-HEADER
   Compare ([131,1,0,0,121,0], [121,0], P.Invalid_Resource_Type); -- 2-LARGERES
   Compare ([121,0,0,0,0,0,0,0], [121,0], P.Built, [121,0]); -- 2-INTEGER
   Compare ([121,0], [121,0], P.Built, [121,0]); -- 2-STRING
   Compare ([0,0,0,0,0,0,0,0], [121,0], P.Invalid_Resource_Type); -- 2-BADINTEGER
   Compare ([120,0], [0,0], P.Bad_Resource_Length); -- 2-ERROR
   Compare ([132,0,0,121,0], [121,0], P.Buffer_Length); -- 1-LONG5
   Compare ([132,0,0,121,0,0], [121,0], P.Built, [132,0,0,121,0]); -- 1-LONG6
   Compare ([112,121,0], [121,0], P.Built, [112,121,0]); -- 1-SMALL0
   Compare ([132,0,0,121,0], [121,0], P.Buffer_Length); -- 2-LONG5
   Compare ([132,0,0,121,0,0], [121,0], P.Built, [132,0,0,121,0]); -- 2-LONG6
   Compare ([112,121,0], [121,0], P.Built, [112,121,0]); -- 2-SMALL0
   declare
      Vendor : Bytes (1 .. 261) := [others => 0];
   begin
      Vendor (1) := 16#84#;
      Vendor (3) := 1;
      Vendor (260) := 16#79#;
      Scan (Vendor, P.Located, 259);
      Compare (Vendor, End_Data, P.Output_Limit);
      Check (Tiny.Build (End_Data, [0, 0]).Status = Tiny.Invalid_Resource_Type);
   end;
   Ada.Text_IO.Put_Line ("Resource template checks" & Checks'Image);
end Resource_Tests;
