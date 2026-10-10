with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Objects;
with AML_References;
with AML_Table_Backing;
procedure Region_Metadata_Tests is
   package NS is new AML_Namespace (32, AML_Delays.Unavailable_Provider);
   use NS; use NS.Owned;
   use type AML_Objects.State;
   use type Byte;
   use type Integer_Value;
   A, Foreign : Arena;
   OK : Boolean;
   Status : Load_Status;
   Checks : Natural := 0;
   Input : aliased AML_Table_Backing.State (1,36);
   Result : Execution_Result;
   Ref : AML_References.Reference;
   Meta : Reference_Metadata;
   Value : Datum;
   Exec_Status : Execution_Status;
   function Region (Address : Bytes := [0]; Length : Bytes := [1]; Space : Byte := 0) return Bytes is
     (Bytes'[16#5B#,16#80#,82,69,71,48,Space] & Address & Length);
   function Pack (Payload : Bytes) return Bytes is
      N : constant Natural := Payload'Length + (if Payload'Length < 63 then 1 else 2);
   begin
      if N <= 63 then return Bytes'[Byte (N)] & Payload;
      else return Bytes'[16#40# + Byte (N mod 16),Byte (N / 16)] & Payload; end if;
   end Pack;
   function Field (Entries : Bytes := [70,76,68,48,8]; Flags : Byte := 1) return Bytes is
     (Bytes'[16#5B#,16#81#] & Pack (Bytes'[82,69,71,48,Flags] & Entries));
   function Method (Code : Bytes) return Bytes is
     (Bytes'[16#14#] & Pack (Bytes'[84,69,83,84,0] & Code));
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Checks'Image & Status'Image; end if; end Check;
   procedure Reject (Code : Bytes; Width : Integer_Width) is
      Before : constant NS.State := Snapshot (A);
   begin
      Load (A, Code, Width, Status); Check (Status /= Loaded and then Snapshot (A) = Before);
   end Reject;
   procedure Metadata_Check (Width : Integer_Width) is
      Before : constant NS.State := Snapshot (A);
   begin
      Load (A, Region & Field, Width, Status); Check (Status = Loaded);
      Check (Value_Store (Snapshot (A)) = Value_Store (Before));
      Check (Method_Usage (Snapshot (A)) = Method_Usage (Before));
      Check (Node_Count (A) = 2 and then Kind (A,1) = Operation_Region_Object and then Kind (A,2) = Region_Field_Object);
      Check (Operation_Region_Data (A,1) = (0,0,1,Width,No_Access));
      Check (Region_Field_Data (A,2).Region = 1 and then Region_Field_Data (A,2).Offset = 0
        and then Region_Field_Data (A,2).Bits = 8 and then Region_Field_Data (A,2).Access_State = No_Access);
      for Node in Node_ID range 1 .. 2 loop
         Make_Named_Identity (A, Node, Ref, OK); Check (OK);
         Describe_Named_Identity (A, Ref, Meta);
         Check (Meta.Kind = Metadata_Only and then Meta.Object_Type = (if Node = 1 then Region_Metadata else Field_Metadata));
         Resolve_Value (A, Ref, Value, Exec_Status); Check (Exec_Status = Unsupported_Value);
         declare Prior : constant NS.State := Snapshot (A); begin
            Store_Reference_Value (A,Ref,Width,(Integer_Datum,9,Ordinary_Integer),Exec_Status);
            Check (Exec_Status = Unsupported_Value and then Snapshot (A) = Prior);
         end;
         Reset (Foreign,OK); Check (OK); Describe_Named_Identity (Foreign,Ref,Meta); Check (Meta.Kind = Invalid_Reference);
      end loop;
      Reset (A,OK); Check (OK); Describe_Named_Identity (A,Ref,Meta); Check (Meta.Kind = Invalid_Reference);
   end Metadata_Check;
begin
   for Width in Integer_Width loop
      Reset (A,OK); Check (OK); Metadata_Check (Width);
      for Space of Bytes'[0,16#0B#,16#7F#,16#80#,16#FF#] loop
         Reset (A,OK); Check (OK);
         Load (A,Region ([16#FF#],[16#0E#,255,255,255,255,255,255,255,255],Space),Width,Status);
         Check (Status = Loaded and then Operation_Region_Data (A,1).Space = Region_Space_ID (Space));
         Check (Operation_Region_Data (A,1).Address = (if Width = Bits_32 then 16#FFFF_FFFF# else Integer_Value'Last));
         Check (Operation_Region_Data (A,1).Length = Operation_Region_Data (A,1).Address);
      end loop;
      Reset (A,OK); Check (OK);
      Load (A,Region & Field ([0,4,70,76,68,48,8,1,3,16#A5#,70,76,68,49,0,3,5,16#CC#,7,70,76,68,50,1,1,2,16#44#,70,76,68,51,1],16#FF#),Width,Status);
      Check (Status = Loaded and then Node_Count (A) = 5);
      Check (Region_Field_Data (A,2).Offset = 4 and then Region_Field_Data (A,2).Raw_Flags = 255 and then Region_Field_Data (A,2).Access_Type = 15);
      Check (Region_Field_Data (A,3).Offset = 12 and then Region_Field_Data (A,3).Bits = 0 and then Region_Field_Data (A,3).Access_Type = 3 and then Region_Field_Data (A,3).Attribute = 16#A5#);
      Check (Region_Field_Data (A,4).Offset = 12 and then Region_Field_Data (A,4).Access_Length = 7 and then Region_Field_Data (A,4).Attribute = 16#CC#);
      Check (Region_Field_Data (A,5).Access_Length = 0 and then Region_Field_Data (A,5).Attribute = 16#44# and then Region_Field_Data (A,5).Access_Type = 2);
      Reset (A,OK); Check (OK);
      declare Data : constant Bytes := Region & Field; High : constant Bytes (Positive'Last - Data'Length + 1 .. Positive'Last) := Data; begin
         Load (A,High,Width,Status); Check (Status = Loaded);
      end;
      for N in 1 .. Region'Length - 1 loop
         Reset (A,OK); Check (OK); declare Data : constant Bytes := Region; begin Reject (Data (1 .. N), Width); end;
      end loop;
      Reset (A,OK); Check (OK); Load (A,Region,Width,Status); Check (Status = Loaded);
      for N in 1 .. Field'Length - 1 loop declare Data : constant Bytes := Field; begin Reject (Data (1 .. N),Width); end; end loop;
      Reset (A,OK); Check (OK);
      Reject (Region & Field & Bytes'[16#08#,66],Width);
      Reject (Region & Field & Field,Width);
      Reject (Region & Field ([2,67,79,78,48]),Width);
      Reject (Region ([65,66,67,68]),Width); Check (Status = Unsupported_Opcode);
      Reject (Region ([66,65,83,69]),Width); Check (Status = Unsupported_Opcode);
      -- A declared method is still deferred AML, not a literal initializer.
      Reject (Bytes'[16#14#] & Pack (Bytes'[66,65,83,69,0,16#A4#,0])
        & Region ([66,65,83,69]),Width); Check (Status = Unsupported_Opcode);
      Reject (Region ([16#0E#,1]),Width); Check (Status = Bad_Integer);
      Reject (Field,Width);
      -- Cumulative UINT32 boundary, each reserved length is a real 28-bit PkgLength.
      declare Entries : Bytes (1 .. 85) := [others => 0]; begin
         for J in 0 .. 15 loop Entries (J * 5 + 1 .. J * 5 + 5) := [0,16#CF#,255,255,255]; end loop;
         Entries (81 .. 85) := [70,76,68,48,8];
         Load (A,Region & Field (Entries),Width,Status); Check (Status = Loaded);
         Check (Region_Field_Data (A,2).Offset = 16#FFFF_FFF0#);
         Reset (A,OK); Check (OK); Entries (85) := 16; Reject (Region & Field (Entries),Width);
      end;
      Reset (A,OK); Check (OK);
      Load (A,Method ([16#A4#,1]) & Bytes'[16#08#,86,65,76,48,16#0D#,97,0],Width,Status); Check (Status = Loaded);
      declare Before : constant NS.State := Snapshot (A); begin
         Load (A,Region & Field,Width,Status); Check (Status = Loaded);
         Check (Value_Store (Snapshot (A)) = Value_Store (Before));
         Check (Method_Usage (Snapshot (A)) = Method_Usage (Before) and then Method_Data (Snapshot (A),1) = Method_Data (Before,1));
      end;
      for Target of Bytes'[82,70] loop
         for Indirect in Boolean loop
            Reset (A,OK); Check (OK);
            declare Name : constant Bytes := (if Target = 82 then Bytes'[82,69,71,48] else Bytes'[70,76,68,48]); Code : constant Bytes :=
               Bytes'[16#A4#,16#8E#] & (if Indirect then Bytes'[16#71#] else Bytes'[1 .. 0 => 0]) & Name; begin
               Load (A,Region & Field & Method (Code),Width,Status); Check (Status = Loaded);
               Invoke (A,Input,3,[others => <>],0,100,Result);
               Check (Result.Status = Returned and then Result.Value = (if Target = 82 then 10 else 5));
            end;
         end loop;
      end loop;
      Reset (A,OK); Check (OK); Load (A,Region & Field & Method ([16#A4#,70,76,68,48]),Width,Status); Check (Status = Loaded);
      Invoke (A,Input,3,[others => <>],0,100,Result); Check (Result.Status = Unsupported_Value);
      Result := NS.Invoke (Snapshot (A),3,[others => 0],0,100);
      Check (Result.Status = Unsupported_Value);
      Reset (A,OK); Check (OK); Load (A,Region & Method ([16#A4#,82,69,71,48]),Width,Status); Check (Status = Loaded);
      Result := NS.Invoke (Snapshot (A),2,[others => 0],0,100); Check (Result.Status = Unsupported_Value);
      Invoke (A,Input,2,[others => <>],0,100,Result); Check (Result.Status = Unsupported_Value);
   end loop;
   declare package Small is new AML_Namespace (8, AML_Delays.Unavailable_Provider, Max_Node_Incarnation => 2); S : Small.State := Small.Empty; R : Small.Load_Status; use type Small.Load_Status; begin
      Small.Load_Names (S,Region & Field & Bytes'[16#08#,78,69,88,84,0],Bits_64,R);
      Check (R = Small.Storage_Full and then Small.Count (S) = 0);
   end;
   declare package Tiny is new AML_Namespace (1, AML_Delays.Unavailable_Provider); S : Tiny.State := Tiny.Empty; R : Tiny.Load_Status; use type Tiny.Load_Status; begin
      Tiny.Load_Names (S,Region & Field,Bits_64,R);
      Check (R = Tiny.Storage_Full and then Tiny.Count (S) = 0);
   end;
   Ada.Text_IO.Put_Line ("Region metadata checks" & Checks'Image);
end Region_Metadata_Tests;
