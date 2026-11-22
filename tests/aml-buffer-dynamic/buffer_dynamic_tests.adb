with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
with AML_Objects;
procedure Buffer_Dynamic_Tests is
   use type AML_Decode.Bytes;
   package E renames AML_Execute;
   package N is new AML_Namespace (64, Perform_Delay => AML_Delays.Unavailable_Provider);
   package V is new N.Owned.Collecting;
   use type Byte;
   use type Integer_Value;
   use type V.Access_Status;
   use type V.Collection_Count;
   use type V.Value_Kind;
   use type E.Named_Metadata_Type;
   use type N.Load_Status;
   use type N.Lookup_Status;
   use type E.Execution_Status;
   A : V.Arena;
   Input : aliased AML_Table_Backing.State (1, 1);
   Access_Result : V.Access_Status;
   Loaded : N.Load_Status;
   Report : N.Initialization_Report;
   Result : V.Result;
   Before, After : V.Collection_Statistics;
   Checks : Natural := 0;
   function Enc (Name : String) return Bytes is
      Data : Bytes (1 .. Name'Length);
   begin
      for I in Data'Range loop Data (I) := Character'Pos (Name (Name'First + I - 1)); end loop;
      return Data;
   end Enc;
   function Block (Op, Payload : Bytes) return Bytes is
      Size : Natural := Payload'Length + 1;
   begin
      if Size <= 63 then return Op & [Byte (Size)] & Payload; end if;
      Size := Payload'Length + 2;
      if Size > 4095 then raise Program_Error with "fixture package bound"; end if;
      return Op & [Byte (16#40# + Size mod 16), Byte (Size / 16)] & Payload;
   end Block;
   function Buffer_Code (Count : Integer_Value; Initializer : Bytes) return Bytes is
      Count_Code : Bytes (1 .. 9) := [others => 0];
   begin
      Count_Code (1) := 16#0E#;
      for I in 0 .. 7 loop Count_Code (I + 2) := Byte ((Count / 2 ** (8 * I)) mod 256); end loop;
      return Block ([16#11#], Count_Code & Initializer);
   end Buffer_Code;
   function Method (Name : String; Body_Code : Bytes) return Bytes is
     (Block ([16#14#], Enc (Name) & [0] & Body_Code));
   Large_Initializer : constant Bytes (1 .. 1025) := [others => 16#A7#];
   function Pressure return Bytes is
      First : constant Bytes := Bytes'[16#70#] & Buffer_Code (8192, [16#A5#, 16#5A#]) & [16#60#];
      One : constant Bytes := Bytes'[16#70#] & Buffer_Code (8192, [16#19#]) & [16#61#];
      Count : constant := 20;
      Code : Bytes (1 .. First'Length + Count * One'Length + 6);
   begin
      Code (1 .. First'Length) := First;
      for I in 0 .. Count - 1 loop
         Code (First'Length + I * One'Length + 1 .. First'Length + (I + 1) * One'Length) := One;
      end loop;
      Code (Code'Last - 5 .. Code'Last) := [16#A4#, 16#83#, 16#88#, 16#60#, 1, 0];
      return Code;
   end Pressure;
   function Dynamic_Buffer (Count_Code : Bytes; Initializer : Bytes := [16#A5#, 16#5A#]) return Bytes is
     (Block ([16#11#], Count_Code & Initializer));
   function Deep_Count (Levels : Positive) return Bytes is
     (if Levels = 1 then Dynamic_Buffer ([16#72#,1,1,0], [1])
      else Dynamic_Buffer (Deep_Count (Levels - 1), [1]));
   function Collecting_Count return Bytes is
      Work : constant Bytes := Pressure;
   begin
      -- Reuse the pressure body, replace its terminal Return(Index(...)).
      return Bytes'[16#75#] & Enc ("MARK") & Work (Work'First .. Work'Last - 6)
        & [16#A4#,16#0B#,0,16#20#];
   end Collecting_Count;
   Code : constant Bytes :=
     Bytes'[16#08#] & Enc ("MARK") & [0]
     & Block ([16#5B#,16#82#], Enc ("DEV0"))
     & Method ("CLRM", Bytes'[16#70#,0] & Enc ("MARK") & [16#A4#,0])
     & Method ("CAPM", Bytes'[16#9D#] & Enc ("DEV0") & [16#60#,16#A4#,16#60#])
     & Method ("GCT0", Collecting_Count)
     & Method ("DGC0", Bytes'[16#70#] & Buffer_Code (8192, [16#A5#,16#5A#])
       & [16#60#,16#70#] & Dynamic_Buffer (Enc ("GCT0"))
       & [16#61#,16#A4#,16#83#,16#88#,16#60#,1,0])
     & Block ([16#14#], Enc ("ARG0") & [1,16#70#] & Dynamic_Buffer ([16#68#])
       & [16#60#,16#A4#,16#87#,16#60#])
     & Method ("DARG", Bytes'[16#A4#] & Enc ("ARG0") & [16#0B#,0,16#20#])
     & Block ([16#14#], Enc ("ASIZ") & [1,16#A4#,16#87#,16#68#])
     & Method ("DACT", Bytes'[16#70#,16#0B#,0,16#20#,16#60#,16#A4#]
       & Enc ("ASIZ") & Dynamic_Buffer ([16#60#]))
     & Method ("INIT", Bytes'[16#70#,0] & Enc ("MARK") & [16#70#,1,16#60#,16#70#]
       & Dynamic_Buffer ([16#60#], Bytes'[16#70#,1] & Enc ("MARK") & [16#A4#,1])
       & [16#61#,16#A4#,16#87#,16#61#])
     -- Malformed AML is a local framing rejection fixture, not oracle conformance.
     & Method ("BAD0", Bytes'[16#70#,0] & Enc ("MARK")
       & [16#A4#,16#11#,3,16#72#,1,16#70#,16#0A#,99] & Enc ("MARK"))
     & Method ("CERR", Bytes'[16#75#] & Enc ("MARK") & [16#A4#,16#78#,1,0,0,0])
     & Method ("DERR", Bytes'[16#70#,0] & Enc ("MARK") & [16#A4#]
       & Dynamic_Buffer (Enc ("CERR")))
     & Method ("CBIG", Bytes'[16#75#] & Enc ("MARK") & [16#A4#,16#0C#,1,0,1,0])
     & Method ("DQUO", Bytes'[16#70#,0] & Enc ("MARK") & [16#A4#]
       & Dynamic_Buffer (Enc ("CBIG")))
     & Method ("SST0", Buffer_Code (1, [1]) & [16#A4#,1])
     & Method ("SST1", Dynamic_Buffer (Enc ("CNT0")) & [16#A4#,1])
     & Method ("EDGE", Bytes'[16#A4#] & Deep_Count (63))
     & Method ("CLIV", Bytes'[16#A4#] & Dynamic_Buffer (Enc ("DEV0")))
     & Method ("CCAP", Bytes'[16#9D#] & Enc ("DEV0") & [16#60#,16#A4#]
       & Dynamic_Buffer ([16#60#]))
     & Method ("CCYC", Bytes'[16#70#,16#71#,16#60#,16#60#,16#A4#]
       & Dynamic_Buffer ([16#60#]))
     & Method ("CBUD", Bytes'[16#75#] & Enc ("MARK")
       & Block ([16#A2#], [1,16#A3#]) & [16#A4#,1])
     & Method ("DBUD", Bytes'[16#A4#] & Dynamic_Buffer (Enc ("CBUD")))
     & Method ("BDBG", Bytes'[16#A4#]
       & Dynamic_Buffer ([16#72#,1,1,16#5B#], Bytes'(1 .. 0 => 0))
       & [16#31#,16#70#,16#0A#,99] & Enc ("MARK"))
     & Method ("BNAM", Bytes'[16#A4#]
       & Dynamic_Buffer (Bytes'[16#72#,1,1] & Enc ("MA"), Bytes'(1 .. 0 => 0))
       & Enc ("RK") & [16#70#,16#0A#,99] & Enc ("MARK"))
     & Method ("BINS", Bytes'[16#A4#]
       & Dynamic_Buffer (Bytes'[16#87#] & Enc ("MA"), Bytes'(1 .. 0 => 0))
       & Enc ("RK") & [16#70#,16#0A#,99] & Enc ("MARK"))
     & Method ("BACT", Bytes'[16#A4#]
       & Dynamic_Buffer (Enc ("ASIZ"), Bytes'(1 .. 0 => 0))
       & [1,16#70#,16#0A#,99] & Enc ("MARK"))
     & Method ("DEEP", Bytes'[16#A4#] & Deep_Count (65))
     & Method ("CNT0", Bytes'[16#75#] & Enc ("MARK") & [16#A4#,16#0B#,0,16#20#])
     & Method ("DYN0", Bytes'[16#70#,16#0B#,0,16#20#,16#60#,16#70#]
       & Dynamic_Buffer ([16#60#]) & [16#61#,16#A4#,16#87#,16#61#])
     & Method ("DYN1", Bytes'[16#70#]
       & Dynamic_Buffer ([16#72#,1,16#0B#,16#FF#,16#1F#,0]) & [16#60#,16#A4#,16#87#,16#60#])
     & Method ("DYN2", Bytes'[16#70#] & Dynamic_Buffer (Enc ("CNT0"))
       & [16#60#,16#A4#,16#87#,16#60#])
     & Method ("POST", Bytes'[16#A4#] & Enc ("MARK"))
     & Method ("DPFX", Bytes'[16#A4#,16#72#]
       & Dynamic_Buffer ([16#72#,1,1,0], [7]) & [0,0])
     & Method ("DNST", Bytes'[16#70#]
       & Dynamic_Buffer (Dynamic_Buffer ([16#72#,1,1,0], [3]))
       & [16#60#,16#A4#,16#87#,16#60#])
     & Method ("CAPT", Bytes'[16#9D#] & Enc ("DEV0") & [16#60#, 16#A4#, 16#60#])
     & Method ("SIZE", Bytes'[16#70#] & Buffer_Code (8192, [16#A5#, 16#5A#]) & [16#60#, 16#A4#, 16#87#, 16#60#])
     & Method ("RETN", Bytes'[16#A4#] & Buffer_Code (8192, [16#A5#, 16#5A#]))
     & Method ("LONG", Bytes'[16#70#] & Buffer_Code (0, Large_Initializer) & [16#60#, 16#A4#, 16#87#, 16#60#])
     & Method ("TAIL", Bytes'[16#70#] & Buffer_Code (8192, [16#A5#, 16#5A#])
       & [16#60#, 16#A4#, 16#83#, 16#88#, 16#60#, 16#0B#, 16#FF#, 16#1F#, 0])
     & Method ("PREF", Bytes'[16#A4#, 16#72#]
       & Buffer_Code (16#FFFF_FFFF#, [1, 2, 3, 4, 5, 6, 7, 8, 9]) & [0, 0])
     & Method ("ZERO", Bytes'[16#A4#, 16#72#] & Buffer_Code (0, Bytes'(1 .. 0 => 0)) & [0, 0])
     & Method ("FAIL", Bytes'[16#70#] & Buffer_Code (AML_Objects.Max_Bytes + 1, [16#A5#]) & [16#60#, 16#A4#, 1])
     & Method ("LIVE", Pressure);
   procedure Check (Good : Boolean; Message : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Message; end if;
   end Check;
   procedure Invoke (Name : String; Expected : E.Execution_Status; Number : Integer_Value := 0; Expected_Pins : Natural := 0; Budget : Natural := 20_000) is
      Found : N.Lookup_Result;
   begin
      V.Find_Object (A, N.Root, Name, Found, Access_Result);
      Check (Access_Result = V.Available and then Found.Status = N.Found, "method lookup " & Name);
      V.Invoke (A, Input, Found.Node, [others => (V.Immediate_Argument, 0)], 0, Budget, Result, Access_Result);
      Check (Access_Result = V.Available and then Result.Status = Expected, "execution status " & Name);
      if Expected = E.Returned then Check (Result.Number = Number, "integer result " & Name); end if;
      Check (V.Retained_Count (A) = Expected_Pins, "result pin count " & Name);
   end Invoke;
   procedure Retained_Buffer (Base_Pins : Natural := 0) is
      Handle : V.Value_Handle;
      Description : V.Value_Description;
      Prefix : Bytes (7 .. 9);
      Last : Bytes (1 .. 1);
      Copied : Natural;
   begin
      Invoke ("RETN", E.Object_Returned, Expected_Pins => Base_Pins + 1);
      Handle := Result.Handle;
      V.Describe (A, Handle, Description, Access_Result);
      Check (Access_Result = V.Available and then Description.Kind = V.Buffer_Description
        and then Description.Length = 8192, "returned literal retained as real owner buffer");
      V.Read_Bytes (A, Handle, 0, Prefix, Copied, Access_Result);
      Check (Access_Result = V.Available and then Copied = 3
        and then Prefix = Bytes'(16#A5#, 16#5A#, 0), "retained initializer and zero prefix");
      Before := V.Reclamation_Metrics (A);
      Invoke ("LIVE", E.Returned, 16#5A#, Expected_Pins => Base_Pins + 1);
      After := V.Reclamation_Metrics (A);
      Check (After.Completed > Before.Completed and then After.Freed_Objects > Before.Freed_Objects,
        "separate invocation actually reclaims while retained buffer remains rooted");
      V.Read_Bytes (A, Handle, 0, Prefix, Copied, Access_Result);
      Check (Access_Result = V.Available and then Copied = 3
        and then Prefix = Bytes'(16#A5#, 16#5A#, 0), "retained bytes survive collection");
      V.Read_Bytes (A, Handle, 8191, Last, Copied, Access_Result);
      Check (Access_Result = V.Available and then Copied = 1 and then Last (1) = 0,
        "retained last byte zero-filled after collection");
      V.Release (A, Handle, Access_Result);
      Check (Access_Result = V.Available and then V.Retained_Count (A) = Base_Pins, "release returned literal pin");
   end Retained_Buffer;
   procedure Mixed_Retained_Roots is
      Handle : V.Value_Handle;
      Description : V.Value_Description;
   begin
      Invoke ("CAPT", E.Metadata_Returned, Expected_Pins => 1);
      Handle := Result.Handle;
      Retained_Buffer (Base_Pins => 1);
      V.Describe (A, Handle, Description, Access_Result);
      Check (Access_Result = V.Available and then Description.Kind = V.Metadata_Description
        and then Description.Object_Type = E.Device_Metadata,
        "captured metadata survives alongside retained large buffer through collection");
      V.Release (A, Handle, Access_Result);
      Check (Access_Result = V.Available and then V.Retained_Count (A) = 0, "release metadata pin");
   end Mixed_Retained_Roots;
   procedure Mixed_Count_GC is
      Metadata_Handle, Buffer_Handle : V.Value_Handle;
      Description : V.Value_Description;
      Data : Bytes (1 .. 3);
      Copied : Natural;
   begin
      Invoke ("CLRM", E.Returned, 0);
      Invoke ("CAPM", E.Metadata_Returned, Expected_Pins => 1);
      Metadata_Handle := Result.Handle;
      Invoke ("RETN", E.Object_Returned, Expected_Pins => 2);
      Buffer_Handle := Result.Handle;
      Before := V.Reclamation_Metrics (A);
      Invoke ("DGC0", E.Returned, 16#5A#, Expected_Pins => 2);
      After := V.Reclamation_Metrics (A);
      Check (After.Completed > Before.Completed, "count method triggers actual collection");
      Invoke ("POST", E.Returned, 1, Expected_Pins => 2);
      V.Describe (A, Metadata_Handle, Description, Access_Result);
      Check (Access_Result = V.Available and then Description.Kind = V.Metadata_Description
        and then Description.Object_Type = E.Device_Metadata, "retained captured metadata survives count collection");
      V.Read_Bytes (A, Buffer_Handle, 0, Data, Copied, Access_Result);
      Check (Access_Result = V.Available and then Copied = 3
        and then Data = Bytes'(16#A5#,16#5A#,0), "earlier retained buffer survives count collection");
      V.Release (A, Metadata_Handle, Access_Result);
      Check (Access_Result = V.Available, "release metadata after count collection");
      V.Release (A, Buffer_Handle, Access_Result);
      Check (Access_Result = V.Available and then V.Retained_Count (A) = 0, "release buffer after count collection");
   end Mixed_Count_GC;
begin
   V.Initialize (A, Access_Result); Check (Access_Result = V.Available, "initialize");
   for Width in Integer_Width loop
      V.Load (A, Code, Width, Loaded, Access_Result);
      Check (Access_Result = V.Available and then Loaded = N.Loaded, "load real owner methods");
      V.Seal (A, Report, Access_Result); Check (Access_Result = V.Available and then Report.Missing = 0 and then Report.Unsupported = 0, "seal complete");
      Invoke ("DYN0", E.Returned, 8192);
      Invoke ("DYN1", E.Returned, 8192);
      Invoke ("DYN2", E.Returned, 8192);
      Invoke ("POST", E.Returned, 1);
      Invoke ("DPFX", E.Returned, 7);
      Invoke ("DNST", E.Returned, 3);
      Invoke ("DARG", E.Returned, 8192);
      Invoke ("DACT", E.Returned, 8192);
      Invoke ("INIT", E.Returned, 8);
      Invoke ("POST", E.Returned, 0);
      Invoke ("BAD0", E.Truncated);
      Invoke ("POST", E.Returned, 0);
      Invoke ("DERR", E.Division_By_Zero);
      Invoke ("POST", E.Returned, 1);
      Invoke ("DQUO", E.Value_Limit);
      Invoke ("POST", E.Returned, 1);
      Invoke ("CLRM", E.Returned, 0);
      Invoke ("SST0", E.Unsupported);
      Invoke ("POST", E.Returned, 0);
      Invoke ("SST1", E.Unsupported);
      Invoke ("POST", E.Returned, 0);
      Invoke ("EDGE", E.Object_Returned, Expected_Pins => 1);
      V.Release (A, Result.Handle, Access_Result);
      Check (Access_Result = V.Available and then V.Retained_Count (A) = 0,
        "release near-depth-boundary result");
      Invoke ("CLIV", E.Unsupported_Value);
      Invoke ("CCAP", E.Unsupported_Value);
      Invoke ("CCYC", E.Unsupported_Value);
      Invoke ("CLRM", E.Returned, 0);
      Invoke ("DBUD", E.Budget_Exceeded, Budget => 100);
      Invoke ("POST", E.Returned, 1);
      Invoke ("CLRM", E.Returned, 0);
      Invoke ("BDBG", E.Truncated);
      Invoke ("POST", E.Returned, 0);
      Invoke ("BNAM", E.Truncated);
      Invoke ("POST", E.Returned, 0);
      Invoke ("BINS", E.Truncated);
      Invoke ("POST", E.Returned, 0);
      Invoke ("BACT", E.Truncated);
      Invoke ("POST", E.Returned, 0);
      Invoke ("DEEP", E.Expression_Limit);
      Invoke ("DYN0", E.Budget_Exceeded, Budget => 1);
      Invoke ("CLRM", E.Returned, 0);
      Invoke ("DYN2", E.Budget_Exceeded, Budget => 1);
      Invoke ("POST", E.Returned, 0);
      Mixed_Count_GC;
      Invoke ("SIZE", E.Returned, 8192);
      Retained_Buffer;
      Mixed_Retained_Roots;
      Invoke ("LONG", E.Returned, 1025);
      Invoke ("TAIL", E.Returned, 0);
      -- A huge declared buffer used only for integer coercion needs no backing allocation.
      Invoke ("PREF", E.Returned, (if Width = Bits_32 then 16#0403_0201# else 16#0807_0605_0403_0201#));
      Invoke ("ZERO", E.Empty_Buffer);
      Invoke ("FAIL", E.Value_Limit);
      Before := V.Reclamation_Metrics (A);
      Invoke ("LIVE", E.Returned, 16#5A#);
      After := V.Reclamation_Metrics (A);
      Check (After.Completed > Before.Completed and then After.Freed_Objects > Before.Freed_Objects,
        "actual collection preserves earlier local buffer through new owner callback");
      V.Reset (A, Access_Result); Check (Access_Result = V.Available, "reset");
   end loop;
   Ada.Text_IO.Put_Line ("Buffer execute owner checks" & Checks'Image);
end Buffer_Dynamic_Tests;
