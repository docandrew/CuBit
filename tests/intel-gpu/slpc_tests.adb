pragma Ada_2022;
with Interfaces; use Interfaces;
with Ada.Text_IO;
with Intel_GPU_GT_Frequency;
with Intel_GPU_SLPC; use Intel_GPU_SLPC;
procedure SLPC_Tests is
   package F renames Intel_GPU_GT_Frequency;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         Ada.Text_IO.Put_Line ("FAIL: " & Name);
         raise Program_Error;
      end if;
      Checks := Checks + 1;
   end Check;
   --  An ADL-N-like capability word: RPn 100 MHz, RP1 700 MHz, RP0 1450 MHz.
   Cap  : constant Unsigned_32 := 16#0002_0E1D#;
   Info : constant Unsigned_32 := 16#0000_0D00#;   --  RPe 13 x 50 = 650 MHz
   Plan : constant Limits := Plan_Limits (Cap, Info);
   GGTT : constant Unsigned_32 := 16#00AD_0000#;
   S : Step := Send_Reset;
   Visited : Natural := 0;
begin
   --  Wire encodings against guc_actions_slpc_abi.h.
   Check (Event_Word (Reset, 2) = 16#0000_0002#, "reset event word");
   Check (Event_Word (Query_Task_State, 2) = 16#0000_0502#, "query event word");
   Check (Event_Word (Parameter_Set, 2) = 16#0000_0602#, "parameter-set event word");
   Check (Request_For (Send_Reset, GGTT, Plan) = [16#3003#, 2, GGTT, 0], "reset request");
   Check (Request_For (Query_State, GGTT, Plan) = [16#3003#, 16#502#, GGTT, 0], "query request");
   Check (Request_For (Verify, GGTT, Plan) = [16#3003#, 16#502#, GGTT, 0], "verify request");
   Check (Request_For (Set_Max, GGTT, Plan) = [16#3003#, 16#602#, 7, 1450], "max = RP0");
   Check (Request_For (Set_Efficient, GGTT, Plan) = [16#3003#, 16#602#, 28, 0],
          "efficient frequency in use");
   Check (Request_For (Set_Min, GGTT, Plan) = [16#3003#, 16#602#, 6, 650], "min = RPe");

   --  Layout of struct slpc_shared_data.
   Check (Shared_Data_Bytes = 8192 and Structure_Bytes = 8192, "two pages");
   Check (Task_State_Offset = 128 and Task_Frequency_Offset = 132, "task state");
   Check (Override_Offset = 192 and Override_Values_Offset = 224, "override block");
   Check (Bit_Word (0) = 48 and Value_Word (0) = 56 and Value_Word (31) = 87,
          "override words");
   Check (Initial_Word (Size_Word) = 8192, "size field");
   Check (Initial_Word (48) = 16#3F#, "six override bits set");
   Check (Initial_Word (56) = 1 and Initial_Word (57) = 0, "GTPERF enabled");
   Check (Initial_Word (58) = 0 and Initial_Word (59) = 1, "balancer disabled");
   Check (Initial_Word (60) = 0 and Initial_Word (61) = 1, "DCC disabled");
   declare
      Nonzero : Natural := 0;
   begin
      for I in Word_Index loop
         if Initial_Word (I) /= 0 then Nonzero := Nonzero + 1; end if;
      end loop;
      Check (Nonzero = 5, "nothing else in the initial image");
   end;

   --  Global state.
   Check (Decode_State (3) = Running and Decode_State (5) = Error and
          Decode_State (0) = Not_Running and Decode_State (6) = Unknown and
          Decode_State (16#FFFF_FFFF#) = Unknown, "global state decode");
   Check (Task_Max_MHz (16#0000_2757#) = 1450 and Task_Min_MHz (16#0000_2757#) = 650,
          "task state frequencies (ratios 87 and 39)");

   --  Limits.
   Check (Plan.Valid and Plan.RP0 = 1450 and Plan.RPe = 650 and Plan.RPn = 100, "limits");
   Check (Plan_Limits (Cap, 0).RPe = 100, "RPe 0 clamps to RPn");
   Check (Plan_Limits (Cap, 16#FF00#).RPe = 1450, "RPe above RP0 clamps to RP0");
   Check (not Plan_Limits (16#0000_0E1D#, Info).Valid, "RPn 0 is invalid");
   Check (not Plan_Limits (16#0005_0E04#, Info).Valid, "RP0 below RPn is invalid");

   --  Fallback software request.
   Check (RP0_Request (Cap).Valid and RP0_Request (Cap).Value = 16#2B80_0000#,
          "RPNSWREQ = 87 << 23");
   Check (F.Requested_MHz (RP0_Request (Cap).Value) = 1450, "fallback decodes to RP0");
   Check (RP0_Request (16#AA#).Valid and not RP0_Request (16#AB#).Valid,
          "ratio must fit bits 31:23");
   Check (not RP0_Request (16#0002_0E00#).Valid, "RP0 0 is invalid");

   --  ARAT unmask.
   Check (Unmask_ARAT (16#FFFF_FFFF#) = 16#FFFF_FDFF# and Unmask_ARAT (0) = 0,
          "ARAT bit cleared, others kept");

   --  The sequence: success walks every step once; any failure ends it.
   while S in Pending loop
      S := Next (S, True);
      Visited := Visited + 1;
   end loop;
   Check (S = Started and Visited = 8, "full start-up sequence");
   for P in Pending loop
      Check (Next (P, False) = Failed, "failure ends the sequence");
   end loop;
   for A in Step loop
      for B in Step loop
         if A in Request_Step and then B in Request_Step and then A /= B then
            Check (Fence_For (A) /= Fence_For (B), "distinct fences");
         end if;
      end loop;
   end loop;
   Check (Fence_For (Send_Reset) = 43 and Fence_For (Verify) = 50, "fence range");

   Ada.Text_IO.Put_Line ("GuC SLPC start-up PASS:" & Natural'Image (Checks) & " checks");
end SLPC_Tests;
