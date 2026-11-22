pragma Ada_2022;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
procedure Name_Reference_Oracle_Tests is
   package N is new AML_Namespace (32, AML_Delays.Unavailable_Provider);
   package V is new N.Owned.Collecting;
   use type AML_Decode.Integer_Value;
   use type N.Load_Status;
   use type N.Lookup_Status;
   use type V.Access_Status;
   use type V.Value_Kind;
   use type V.Value_Handle;
   use type AML_Execute.Execution_Status;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Label : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Label; end if; end Check;
   procedure Run_Case (Key : String; Width : Integer_Width; Base, Code : Bytes;
                       Expected_Rejection : Boolean; Expected : Integer_Value) is
      A : V.Arena;
      Input : aliased AML_Table_Backing.State (1, 1);
      Access_Result : V.Access_Status;
      Load_Result : N.Load_Status;
      Init : N.Initialization_Report;
      Outcome : V.Result;
      Node : N.Lookup_Result;
      procedure Observe (Name : String; Reject : Boolean; Number : Integer_Value) is
         Handle, Next : V.Value_Handle := V.No_Value;
         Description : V.Value_Description;
      begin
         V.Find_Object (A, N.Root, Name, Node, Access_Result);
         Check (Access_Result = V.Available and then Node.Status = N.Found, Key & " find " & Name);
         V.Invoke (A, Input, Node.Node, [others => (V.Immediate_Argument, 0)], 0, 20_000, Outcome, Access_Result);
         Check (Access_Result = V.Available, Key & " invocation access");
         if Reject then
            -- ACPICA reports AE_AML_INTERNAL for its expired namespace pointer.
            -- CuBit's explicit stale-reference rejection has a different status.
            Check (Outcome.Status = AML_Execute.Unsupported_Value, Key & " stale NAME rejection " & Outcome.Status'Image);
         elsif Outcome.Status = AML_Execute.Returned then
            Check (Outcome.Number = Number, Key & " integer " & Name);
         else
            Check (Outcome.Status = AML_Execute.Reference_Returned, Key & " scalar observation kind " & Outcome.Status'Image);
            Handle := Outcome.Handle;
            -- API-only observation bridge: debugger scalar export dereferences
            -- returned references; each CuBit handle is released exactly once.
            for Depth in 1 .. 8 loop
               V.Describe (A, Handle, Description, Access_Result);
               Check (Access_Result = V.Available, Key & " describe");
               exit when Description.Kind = V.Integer_Description;
               Check (Description.Kind = V.Reference_Description, Key & " reference bridge kind");
               V.Dereference (A, Handle, Next, Access_Result);
               Check (Access_Result = V.Available and then Next /= V.No_Value, Key & " dereference");
               V.Release (A, Handle, Access_Result); Check (Access_Result = V.Available, Key & " release bridge");
               Handle := Next; Next := V.No_Value;
            end loop;
            V.Describe (A, Handle, Description, Access_Result);
            Check (Access_Result = V.Available and then Description.Kind = V.Integer_Description
              and then Description.Number = Number, Key & " bridged scalar");
            V.Release (A, Handle, Access_Result); Check (Access_Result = V.Available, Key & " release scalar");
         end if;
         Check (V.Retained_Count (A) = 0, Key & " no leaked result pins");
         pragma Assert (V.Module_Transient_Free (A));
      end Observe;
   begin
      V.Initialize (A, Access_Result); Check (Access_Result = V.Available, Key & " initialize");
      V.Load (A, Base, Width, Load_Result, Access_Result);
      Check (Access_Result = V.Available and then Load_Result = N.Loaded, Key & " base load");
      V.Load (A, Code, Width, Load_Result, Access_Result);
      Check (Access_Result = V.Available and then Load_Result = N.Loaded, Key & " source load");
      V.Seal (A, Init, Access_Result); Check (Access_Result = V.Available, Key & " seal");
      for Pass in 1 .. 2 loop Observe ("INIT", False, 0); Observe ("TEST", Expected_Rejection, Expected); end loop;
   end Run_Case;
begin
   Run_Case ("r1-runtime-self-Store", Bits_32,
     [16#08#,16#42#,16#41#,16#53#,16#45#,16#00#],
     [16#14#,16#08#,16#49#,16#4E#,16#49#,16#54#,16#00#,16#A4#,16#00#,16#14#,16#2B#,16#54#,16#45#,16#53#,16#54#,16#00#,16#08#,16#54#,16#45#,16#4D#,16#50#,16#12#,16#06#,16#01#,16#54#,16#45#,16#4D#,16#50#,16#70#,16#54#,16#45#,16#4D#,16#50#,16#60#,16#70#,16#12#,16#06#,16#02#,16#0A#,16#22#,16#0A#,16#33#,16#54#,16#45#,16#4D#,16#50#,16#A4#,16#87#,16#83#,16#88#,16#60#,16#00#,16#00#], False, 2);
   Run_Case ("r1-static-data-Store", Bits_32,
     [16#08#,16#42#,16#41#,16#53#,16#45#,16#00#],
     [16#08#,16#53#,16#52#,16#43#,16#30#,16#0A#,16#11#,16#08#,16#50#,16#4B#,16#47#,16#30#,16#12#,16#06#,16#01#,16#53#,16#52#,16#43#,16#30#,16#14#,16#0F#,16#49#,16#4E#,16#49#,16#54#,16#00#,16#9D#,16#0A#,16#11#,16#53#,16#52#,16#43#,16#30#,16#A4#,16#00#,16#14#,16#16#,16#54#,16#45#,16#53#,16#54#,16#00#,16#70#,16#0A#,16#22#,16#53#,16#52#,16#43#,16#30#,16#A4#,16#83#,16#88#,16#50#,16#4B#,16#47#,16#30#,16#00#,16#00#], False, 17);
   Run_Case ("r1-runtime-self-CopyObject", Bits_32,
     [16#08#,16#42#,16#41#,16#53#,16#45#,16#00#],
     [16#14#,16#08#,16#49#,16#4E#,16#49#,16#54#,16#00#,16#A4#,16#00#,16#14#,16#2B#,16#54#,16#45#,16#53#,16#54#,16#00#,16#08#,16#54#,16#45#,16#4D#,16#50#,16#12#,16#06#,16#01#,16#54#,16#45#,16#4D#,16#50#,16#70#,16#54#,16#45#,16#4D#,16#50#,16#60#,16#9D#,16#12#,16#06#,16#02#,16#0A#,16#22#,16#0A#,16#33#,16#54#,16#45#,16#4D#,16#50#,16#A4#,16#87#,16#83#,16#88#,16#60#,16#00#,16#00#], False, 2);
   Run_Case ("r1-static-data-CopyObject", Bits_32,
     [16#08#,16#42#,16#41#,16#53#,16#45#,16#00#],
     [16#08#,16#53#,16#52#,16#43#,16#30#,16#0A#,16#11#,16#08#,16#50#,16#4B#,16#47#,16#30#,16#12#,16#06#,16#01#,16#53#,16#52#,16#43#,16#30#,16#14#,16#0F#,16#49#,16#4E#,16#49#,16#54#,16#00#,16#9D#,16#0A#,16#11#,16#53#,16#52#,16#43#,16#30#,16#A4#,16#00#,16#14#,16#16#,16#54#,16#45#,16#53#,16#54#,16#00#,16#9D#,16#0A#,16#22#,16#53#,16#52#,16#43#,16#30#,16#A4#,16#83#,16#88#,16#50#,16#4B#,16#47#,16#30#,16#00#,16#00#], False, 17);
   Run_Case ("r1-escaped-runtime-self", Bits_32,
     [16#08#,16#42#,16#41#,16#53#,16#45#,16#00#],
     [16#08#,16#53#,16#41#,16#56#,16#45#,16#12#,16#02#,16#00#,16#14#,16#1D#,16#4D#,16#41#,16#4B#,16#45#,16#00#,16#08#,16#54#,16#45#,16#4D#,16#50#,16#12#,16#06#,16#01#,16#54#,16#45#,16#4D#,16#50#,16#70#,16#54#,16#45#,16#4D#,16#50#,16#53#,16#41#,16#56#,16#45#,16#A4#,16#00#,16#14#,16#08#,16#49#,16#4E#,16#49#,16#54#,16#00#,16#A4#,16#00#,16#14#,16#14#,16#54#,16#45#,16#53#,16#54#,16#00#,16#4D#,16#41#,16#4B#,16#45#,16#A4#,16#87#,16#83#,16#88#,16#53#,16#41#,16#56#,16#45#,16#00#,16#00#], True, 0);
   Run_Case ("r1-detached-runtime-cycle", Bits_32,
     [16#08#,16#42#,16#41#,16#53#,16#45#,16#00#],
     [16#14#,16#08#,16#49#,16#4E#,16#49#,16#54#,16#00#,16#A4#,16#00#,16#14#,16#26#,16#54#,16#45#,16#53#,16#54#,16#00#,16#08#,16#54#,16#45#,16#4D#,16#50#,16#12#,16#08#,16#02#,16#54#,16#45#,16#4D#,16#50#,16#0A#,16#11#,16#70#,16#83#,16#88#,16#54#,16#45#,16#4D#,16#50#,16#01#,16#00#,16#60#,16#9D#,16#00#,16#54#,16#45#,16#4D#,16#50#,16#A4#,16#60#], False, 17);
   Run_Case ("r2-runtime-self-Store", Bits_64,
     [16#08#,16#42#,16#41#,16#53#,16#45#,16#00#],
     [16#14#,16#08#,16#49#,16#4E#,16#49#,16#54#,16#00#,16#A4#,16#00#,16#14#,16#2B#,16#54#,16#45#,16#53#,16#54#,16#00#,16#08#,16#54#,16#45#,16#4D#,16#50#,16#12#,16#06#,16#01#,16#54#,16#45#,16#4D#,16#50#,16#70#,16#54#,16#45#,16#4D#,16#50#,16#60#,16#70#,16#12#,16#06#,16#02#,16#0A#,16#22#,16#0A#,16#33#,16#54#,16#45#,16#4D#,16#50#,16#A4#,16#87#,16#83#,16#88#,16#60#,16#00#,16#00#], False, 2);
   Run_Case ("r2-static-data-Store", Bits_64,
     [16#08#,16#42#,16#41#,16#53#,16#45#,16#00#],
     [16#08#,16#53#,16#52#,16#43#,16#30#,16#0A#,16#11#,16#08#,16#50#,16#4B#,16#47#,16#30#,16#12#,16#06#,16#01#,16#53#,16#52#,16#43#,16#30#,16#14#,16#0F#,16#49#,16#4E#,16#49#,16#54#,16#00#,16#9D#,16#0A#,16#11#,16#53#,16#52#,16#43#,16#30#,16#A4#,16#00#,16#14#,16#16#,16#54#,16#45#,16#53#,16#54#,16#00#,16#70#,16#0A#,16#22#,16#53#,16#52#,16#43#,16#30#,16#A4#,16#83#,16#88#,16#50#,16#4B#,16#47#,16#30#,16#00#,16#00#], False, 17);
   Run_Case ("r2-runtime-self-CopyObject", Bits_64,
     [16#08#,16#42#,16#41#,16#53#,16#45#,16#00#],
     [16#14#,16#08#,16#49#,16#4E#,16#49#,16#54#,16#00#,16#A4#,16#00#,16#14#,16#2B#,16#54#,16#45#,16#53#,16#54#,16#00#,16#08#,16#54#,16#45#,16#4D#,16#50#,16#12#,16#06#,16#01#,16#54#,16#45#,16#4D#,16#50#,16#70#,16#54#,16#45#,16#4D#,16#50#,16#60#,16#9D#,16#12#,16#06#,16#02#,16#0A#,16#22#,16#0A#,16#33#,16#54#,16#45#,16#4D#,16#50#,16#A4#,16#87#,16#83#,16#88#,16#60#,16#00#,16#00#], False, 2);
   Run_Case ("r2-static-data-CopyObject", Bits_64,
     [16#08#,16#42#,16#41#,16#53#,16#45#,16#00#],
     [16#08#,16#53#,16#52#,16#43#,16#30#,16#0A#,16#11#,16#08#,16#50#,16#4B#,16#47#,16#30#,16#12#,16#06#,16#01#,16#53#,16#52#,16#43#,16#30#,16#14#,16#0F#,16#49#,16#4E#,16#49#,16#54#,16#00#,16#9D#,16#0A#,16#11#,16#53#,16#52#,16#43#,16#30#,16#A4#,16#00#,16#14#,16#16#,16#54#,16#45#,16#53#,16#54#,16#00#,16#9D#,16#0A#,16#22#,16#53#,16#52#,16#43#,16#30#,16#A4#,16#83#,16#88#,16#50#,16#4B#,16#47#,16#30#,16#00#,16#00#], False, 17);
   Run_Case ("r2-escaped-runtime-self", Bits_64,
     [16#08#,16#42#,16#41#,16#53#,16#45#,16#00#],
     [16#08#,16#53#,16#41#,16#56#,16#45#,16#12#,16#02#,16#00#,16#14#,16#1D#,16#4D#,16#41#,16#4B#,16#45#,16#00#,16#08#,16#54#,16#45#,16#4D#,16#50#,16#12#,16#06#,16#01#,16#54#,16#45#,16#4D#,16#50#,16#70#,16#54#,16#45#,16#4D#,16#50#,16#53#,16#41#,16#56#,16#45#,16#A4#,16#00#,16#14#,16#08#,16#49#,16#4E#,16#49#,16#54#,16#00#,16#A4#,16#00#,16#14#,16#14#,16#54#,16#45#,16#53#,16#54#,16#00#,16#4D#,16#41#,16#4B#,16#45#,16#A4#,16#87#,16#83#,16#88#,16#53#,16#41#,16#56#,16#45#,16#00#,16#00#], True, 0);
   Run_Case ("r2-detached-runtime-cycle", Bits_64,
     [16#08#,16#42#,16#41#,16#53#,16#45#,16#00#],
     [16#14#,16#08#,16#49#,16#4E#,16#49#,16#54#,16#00#,16#A4#,16#00#,16#14#,16#26#,16#54#,16#45#,16#53#,16#54#,16#00#,16#08#,16#54#,16#45#,16#4D#,16#50#,16#12#,16#08#,16#02#,16#54#,16#45#,16#4D#,16#50#,16#0A#,16#11#,16#70#,16#83#,16#88#,16#54#,16#45#,16#4D#,16#50#,16#01#,16#00#,16#60#,16#9D#,16#00#,16#54#,16#45#,16#4D#,16#50#,16#A4#,16#60#], False, 17);
   Ada.Text_IO.Put_Line ("name reference oracle checks" & Checks'Image);
end Name_Reference_Oracle_Tests;
