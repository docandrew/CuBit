with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
with AML_Coercions;
with AML_Objects;
procedure Runtime_Buffer_Tests is
   use type AML_Decode.Bytes;
   package NS is new AML_Namespace (16, AML_Delays.Unavailable_Provider);
   use NS; use NS.Owned;
   use type Integer_Value;
   use type Byte;
   use type AML_Coercions.Conversion_Status;
   A : Arena;
   Input : aliased AML_Table_Backing.State (1,1);
   OK : Boolean;
   Loaded_Status : Load_Status;
   R : Execution_Result;
   Before : NS.State;
   Checks : Natural := 0;
   Temp : constant Bytes := [84,69,77,80];
   Mark : constant Bytes := [77,65,82,75];
   Helper_Name : constant Bytes := [67,78,84,48];
   function Buffer_Data (Count : Bytes; Initial : Bytes := [1=>7]) return Bytes is
     (Bytes'(16#11#,Byte(1+Count'Length+Initial'Length)) & Count & Initial);
   function Declare_Buffer (Count : Bytes; Initial : Bytes := [1=>7]) return Bytes is
     (Bytes'(1=>16#08#) & Temp & Buffer_Data(Count,Initial));
   function Return_Size return Bytes is (Bytes'(16#A4#,16#87#)&Temp);
   function Method_Data (Name, Code : Bytes; Flags : Byte := 0) return Bytes is
     (Bytes'(16#14#,Byte(6+Code'Length)) & Name & Bytes'(1=>Flags) & Code);
   procedure Check(B : Boolean) is
   begin Checks:=Checks+1; if not B then raise Program_Error with Checks'Image & " " & R.Status'Image; end if; end Check;
   procedure Run(Code : Bytes; W : Integer_Width; Expected : Execution_Status;
                 Value : Integer_Value := 0; Arg : Integer_Value := 4;
                 Mark_Value : Integer_Value := 0; Budget : Natural := 200;
                 Helper : Bytes := [16#A4#,16#0A#,4]) is
   begin
      Reset(A,OK); Check(OK);
      Load(A,Bytes'(1=>16#08#)&Mark&Bytes'(1=>0)&Method_Data(Helper_Name,Helper)&
        Method_Data([84,69,83,84],Code,1),W,Loaded_Status); Check(Loaded_Status=Loaded);
      Before:=Snapshot(A);
      Invoke(A,Input,3,[others=>(Integer_Datum,Arg,Ordinary_Integer)],1,Budget,R);
      Check(R.Status=Expected); if Expected=Returned then Check(R.Value=Value); end if;
      Check(Node_Count(A)=3 and Integer_Data(Snapshot(A),1)=Mark_Value);
      if Mark_Value=0 then pragma Assert(Allocating_Cleanup_Frame(Snapshot(A),Before)); end if;
      Check(R.Charged<=Budget);
   end Run;
   function Static_Buffer_Code (Count, Initializer : Bytes) return Bytes is
      Payload : constant Bytes := Count & Initializer;
      Encoding : Positive := 1;
      Extent, Remaining : Natural;
      Header : Bytes (1 .. 4) := [others => 0];
   begin
      if Payload'Length + 1 > 63 then Encoding := 2; end if;
      if Payload'Length + 2 > 4095 then Encoding := 3; end if;
      if Payload'Length + 3 > 1_048_575 then Encoding := 4; end if;
      Extent := Payload'Length + Encoding;
      if Encoding = 1 then Header (1) := Byte (Extent);
      else
         Header (1) := Byte ((Encoding - 1) * 64 + Extent mod 16);
         Remaining := Extent / 16;
         for I in 2 .. Encoding loop
            Header (I) := Byte (Remaining mod 256); Remaining := Remaining / 256;
         end loop;
      end if;
      return Bytes'[16#11#] & Header (1 .. Encoding) & Payload;
   end Static_Buffer_Code;
   procedure Static_Cases (Width : Integer_Width) is
      Count_Name : constant Bytes := [67,78,84,48];
      Empty_Data : constant Bytes (1 .. 0) := [others => 0];
      Long_Initializer : Bytes (7 .. 1031) := [others => 16#A5#];
      procedure Load_And_Check (Count, Initializer : Bytes; Expected_Length : Natural;
                                Named_Count : Boolean := False) is
         Prefix : constant Bytes := (if Named_Count then
           Bytes'[16#08#] & Count_Name & [16#0B#, 1, 4] else Empty_Data);
         Node : Node_ID;
      begin
         Reset (A, OK); Check (OK);
         Load (A, Prefix & Bytes'[16#08#] & Temp & Static_Buffer_Code (Count, Initializer),
           Width, Loaded_Status); Check (Loaded_Status = Loaded);
         Node := Child (Snapshot (A), Root, "TEMP");
         Check (Node /= Root and then Kind (Snapshot (A), Node) = Buffer_Object);
         declare
            Actual : constant Bytes := NS.Buffer_Data (Snapshot (A), Node);
            Correct : Boolean := Actual'Length = Expected_Length;
         begin
            for I in 1 .. Actual'Length loop
               Correct := Correct and then Actual (Actual'First + I - 1) =
                 (if I <= Initializer'Length then Initializer (Initializer'First + I - 1) else 0);
            end loop;
            Check (Correct);
         end;
      end Load_And_Check;
   begin
      Long_Initializer (Long_Initializer'First) := 16#D3#;
      Long_Initializer (Long_Initializer'Last) := 16#7E#;
      Load_And_Check ([16#0B#, 1, 4], [16#42#], 1025);
      Load_And_Check (Count_Name, [16#52#], 1025, Named_Count => True);
      Load_And_Check ([0], Empty_Data, 0);
      Load_And_Check ([0], Long_Initializer, Long_Initializer'Length);
      Load_And_Check ([16#0B#, 0, 32], Long_Initializer, 8192);
      -- Byte quota failure must undo the new count binding and candidate buffer,
      -- retaining the preexisting buffer's exact bytes and object state.
      Before := Snapshot (A);
      Load (A, Bytes'[16#08#] & Count_Name & [16#0C#, 1, 0, 1, 0]
        & Bytes'[16#08#, 66,65,68,48] & Static_Buffer_Code (Count_Name, [16#CC#]),
        Width, Loaded_Status);
      Check (Loaded_Status = Value_Limit and then Snapshot (A) = Before);
      Load (A, Bytes'[16#08#, 66,65,68,48]
        & Static_Buffer_Code ([16#0C#, 1, 0, 1, 0], Empty_Data), Width, Loaded_Status);
      Check (Loaded_Status = Value_Limit and then Snapshot (A) = Before);
   end Static_Cases;
   Parsed : Buffer_Result;
   Conversion : AML_Coercions.Result;
begin
   for W in Integer_Width loop
      Static_Cases (W);
      Run(Declare_Buffer([1=>16#68#])&Return_Size,W,Returned,4);
      Run(Declare_Buffer([1=>16#68#],[1,2,3])&Return_Size,W,Returned,3,Arg=>0);
      Run(Declare_Buffer([1=>16#68#])&Return_Size,W,Returned,2,Arg=>16#1_0000_0002#);
      Run(Declare_Buffer([1=>16#68#])&Return_Size,W,Returned,1025,Arg=>1025);
      Run(Declare_Buffer([1=>16#68#])&Return_Size,W,Returned,8192,Arg=>8192);
      Run(Declare_Buffer([1=>16#68#])&Return_Size,W,Value_Limit,
        Arg=>AML_Objects.Max_Bytes+1);
      Run(Declare_Buffer(Helper_Name)&Return_Size,W,Value_Limit,Mark_Value=>1,
        Helper=>Bytes'(16#70#,1)&Mark&Bytes'(16#A4#,16#0C#,1,0,1,0));
      Run(Bytes'(16#70#,16#0A#,3,16#60#)&Declare_Buffer([1=>16#60#])&Return_Size,W,Returned,3);
      Run(Declare_Buffer(Helper_Name)&Return_Size,W,Returned,4);
      Run(Declare_Buffer(Helper_Name)&Return_Size,W,Returned,3,
        Helper=>Bytes'(16#08#,73,78,78,82)&Buffer_Data([16#72#,1,16#0A#,2,0])&Bytes'(16#A4#,16#87#,73,78,78,82));
      Run(Declare_Buffer(Bytes'(16#7B#,16#5B#,16#12#)&Temp&Bytes'(16#60#,1,0))&Return_Size,W,Returned,1);
      Run(Declare_Buffer(Bytes'(16#70#,16#0A#,2)&Temp)&Bytes'(1=>16#A4#)&Temp,W,Returned,2);
      Run(Bytes'(16#70#,16#0D#,52,0,16#60#)&Declare_Buffer([1=>16#60#])&Return_Size,W,Returned,4);
      Run(Bytes'(1=>16#70#)&Buffer_Data([16#0A#,8],[2,0,0,0,1,0,0,0])&Bytes'(1=>16#60#)&Declare_Buffer([1=>16#60#])&Return_Size,W,Returned,2);
      -- Reference-valued Local count through actual RefOf expression stored by Store.
      Run(Bytes'(16#70#,16#0A#,3,16#60#,16#70#,16#71#,16#60#,16#61#)&Declare_Buffer([1=>16#61#])&Return_Size,W,Returned,3);
      Run(Bytes'(1=>16#70#)&Buffer_Data([1=>0],[])&Bytes'(1=>16#60#)&Declare_Buffer([1=>16#60#])&Return_Size,W,Empty_Buffer);
      Run(Declare_Buffer([16#72#,1],[])&Return_Size,W,Truncated);
      Run(Bytes'(1=>16#08#)&Temp&Bytes'(1=>0)&Declare_Buffer([16#72#,1],[])&Return_Size,W,Duplicate_Name);
      Run(Declare_Buffer([16#72#,1,1,0])&Return_Size,W,Budget_Exceeded,Budget=>2);
      Run(Declare_Buffer(Helper_Name)&Return_Size,W,Division_By_Zero,Mark_Value=>1,
        Helper=>Bytes'(16#70#,1)&Mark&Bytes'(16#78#,1,0,16#60#,16#61#,16#A4#,1));
      Parsed:=Read_Buffer(Buffer_Data([16#0E#,2,0,0,0,1,0,0,0]),W);
      Check(Parsed.Kind=Accepted and then Parsed.Length=2);
      Conversion:=AML_Coercions.From_String([49,48,48,48,48,48,48,48,50],W);
      Check(Conversion.Status=AML_Coercions.Converted and then Conversion.Value=
        (if W=Bits_32 then 16#1000_0000# else 16#1_0000_0002#));
   end loop;
   Ada.Text_IO.Put_Line("Runtime Buffer checks"&Checks'Image);
end Runtime_Buffer_Tests;
