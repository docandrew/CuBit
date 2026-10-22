pragma Ada_2022;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Namespace;
with AML_Objects;
with AML_Table_Backing;
procedure Table_Field_Alias_GC_Tests is
   use type AML_Decode.Bytes;
   package N is new AML_Namespace (64, Perform_Delay => AML_Delays.Unavailable_Provider);
   package V is new N.Owned.Collecting;
   use type Byte;
   use type Integer_Value;
   use type N.Bind_Status;
   use type N.Load_Status;
   use type N.Lookup_Status;
   use type V.Access_Status;
   use type V.Value_Kind;
   use type V.Collection_Count;
   use type AML_Execute.Execution_Status;
   Checks : Natural := 0;
   Octet_Bits : constant Positive := 8;
   Field_Shift : constant Natural := 3;
   Sizes : constant array (Positive range <>) of Positive := [1_025, 8_192];
   procedure Check (Good : Boolean; Message : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Message; end if;
   end Check;
   function Enc (Name : String) return Bytes is
      Data : Bytes (1 .. Name'Length);
   begin
      for I in Data'Range loop Data (I) := Character'Pos (Name (Name'First + I - 1)); end loop;
      return Data;
   end Enc;
   function Block (Opcode : Byte; Payload : Bytes) return Bytes is
      Size : Natural := Payload'Length + 1;
   begin
      if Size <= 63 then return Bytes'[Opcode, Byte (Size)] & Payload; end if;
      Size := Payload'Length + 2;
      if Size > 4_095 then raise Program_Error with "fixture package extent"; end if;
      return Bytes'[Opcode, Byte (16#40# + Size mod 16), Byte (Size / 16)] & Payload;
   end Block;
   function Method (Name : String; Body_Code : Bytes) return Bytes is
     (Block (16#14#, Enc (Name) & [0] & Body_Code));
   function Pressure (Count : Positive) return Bytes is
      One_Read : constant Bytes := Bytes'[16#70#] & Enc ("ALS2") & [16#61#];
      Body_Code : Bytes (1 .. Count * One_Read'Length + 2);
   begin
      for I in 0 .. Count - 1 loop
         Body_Code (I * One_Read'Length + 1 .. (I + 1) * One_Read'Length) := One_Read;
      end loop;
      Body_Code (Body_Code'Last - 1 .. Body_Code'Last) := [16#A4#, 0];
      return Body_Code;
   end Pressure;
begin
   for Width in Integer_Width loop
      for Field_Bytes of Sizes loop
         declare
            A : V.Arena;
            Input : aliased AML_Table_Backing.State (1, Field_Bytes + 1);
            Access_Result : V.Access_Status;
            Bind_Result : N.Bind_Status;
            Loaded : N.Load_Status;
            Region, Field : N.Node_ID;
            Init : N.Initialization_Report;
            Result : V.Result;
            Retained, Caller_Result : V.Value_Handle;
            Before, After : V.Collection_Statistics;
            Expected : Bytes (1 .. Field_Bytes) := [others => 0];
            Pressure_Reads : constant Positive := AML_Objects.Max_Bytes / Field_Bytes + 4;
            Code : constant Bytes :=
              Bytes'[16#06#] & Enc ("FLD0") & Enc ("ALS1") &
              [16#06#] & Enc ("ALS1") & Enc ("ALS2") &
              Method ("TYPE", Bytes'[16#A4#, 16#8E#] & Enc ("ALS2")) &
              Method ("READ", Bytes'[16#A4#] & Enc ("ALS2")) &
              Method ("PRES", Pressure (Pressure_Reads)) &
              Method ("NEST", Bytes'[16#70#] & Enc ("ALS2") & [16#60#] &
                Enc ("PRES") & [16#A4#, 16#60#]);
            procedure Invoke (Name : String; Status : AML_Execute.Execution_Status) is
               Found : N.Lookup_Result;
            begin
               V.Find_Object (A, N.Root, Name, Found, Access_Result);
               Check (Access_Result = V.Available and then Found.Status = N.Found, "method found " & Name);
               V.Invoke (A, Input, Found.Node, [others => (V.Immediate_Argument, 0)], 0,
                 20_000, Result, Access_Result);
               Check (Access_Result = V.Available and then Result.Status = Status, "invoke " & Name);
            end Invoke;
            procedure Full_Bytes (Handle : V.Value_Handle) is
               Description : V.Value_Description;
               Actual : Bytes (7 .. Field_Bytes + 6);
               Copied : Natural;
            begin
               V.Describe (A, Handle, Description, Access_Result);
               Check (Access_Result = V.Available and then Description.Kind = V.Buffer_Description
                 and then Description.Length = Field_Bytes, "owned field buffer description");
               V.Read_Bytes (A, Handle, 0, Actual, Copied, Access_Result);
               Check (Access_Result = V.Available and then Copied = Field_Bytes
                 and then Actual = Expected, "all shifted field bytes preserved");
            end Full_Bytes;
         begin
            Input.Count := 1;
            Input.Tables (1) := (Offset => 0, Extent => Field_Bytes + 1);
            for I in Input.Data.Bytes'Range loop Input.Data.Bytes (I) := Byte ((I * 37 + 17) mod 256); end loop;
            for Bit in 0 .. Field_Bytes * Octet_Bits - 1 loop
               if (Natural (Input.Data.Bytes (Input.Data.Bytes'First + (Bit + Field_Shift) / Octet_Bits)) /
                 2 ** ((Bit + Field_Shift) mod Octet_Bits)) mod 2 = 1 then
                  Expected (1 + Bit / Octet_Bits) := Expected (1 + Bit / Octet_Bits)
                    + Byte (2 ** (Bit mod Octet_Bits));
               end if;
            end loop;
            V.Initialize (A, Access_Result); Check (Access_Result = V.Available, "initialize");
            V.Bind_Table_Region (A, N.Root, "REG0", (Table => 1, Extent => Field_Bytes + 1),
              Region, Bind_Result, Access_Result);
            Check (Access_Result = V.Available and then Bind_Result = N.Bound, "bind real table region");
            V.Bind_Table_Field (A, N.Root, "FLD0", Region, Field_Shift, Field_Bytes * Octet_Bits,
              Field, Bind_Result, Access_Result);
            Check (Access_Result = V.Available and then Bind_Result = N.Bound and then Field > N.Root,
              "bind real field");
            V.Load (A, Code, Width, Loaded, Access_Result);
            Check (Access_Result = V.Available and then Loaded = N.Loaded, "load static alias chain and methods");
            V.Seal (A, Init, Access_Result);
            Check (Access_Result = V.Available and then Init.Missing = 0 and then Init.Unsupported = 0,
              "complete initialization");
            for I in 1 .. 3 loop
               Invoke ("TYPE", AML_Execute.Returned);
               Check (Result.Number = 5 and then V.Observe_Usage (A).Values.Objects = 0
                 and then V.Observe_Usage (A).Values.Bytes = 0 and then V.Retained_Count (A) = 0,
                 "alias type query does not materialize field value");
            end loop;
            Invoke ("READ", AML_Execute.Object_Returned);
            Retained := Result.Handle;
            Check (V.Retained_Count (A) = 1, "first retained pin");
            Full_Bytes (Retained);
            Before := V.Reclamation_Metrics (A);
            Invoke ("NEST", AML_Execute.Object_Returned);
            Caller_Result := Result.Handle;
            After := V.Reclamation_Metrics (A);
            Check (After.Completed > Before.Completed and then After.Freed_Objects > Before.Freed_Objects
              and then After.Freed_Bytes > Before.Freed_Bytes,
              "nested materializing reads actually collect unreachable buffers");
            Check (V.Retained_Count (A) = 2, "retained and caller result pins");
            Full_Bytes (Retained);
            Full_Bytes (Caller_Result);
            V.Release (A, Caller_Result, Access_Result); Check (Access_Result = V.Available, "release caller result");
            V.Release (A, Retained, Access_Result);
            Check (Access_Result = V.Available and then V.Retained_Count (A) = 0, "release retained result");
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Table field alias GC checks" & Checks'Image);
end Table_Field_Alias_GC_Tests;
