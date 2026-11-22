with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Firmware;
with Intel_GPU_HuC_Firmware; use Intel_GPU_HuC_Firmware;
with Intel_GPU_HuC_Registers; use Intel_GPU_HuC_Registers;
with Intel_GPU_HuC_Load;
with Intel_GPU_Media_Engines;
-- Hosted HuC load/authentication tests against a mocked register file, DMA
-- engine, clock and GuC. Covers success, WOPCM/busy refusals, DMA timeout,
-- authentication timeout, bad signature (GuC refusal), stale status, write
-- and clock faults, poll-limit exhaustion, and reset followed by redo.
-- Model only: not firmware, CT memory or hardware evidence.
procedure HuC_Load_Tests is
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; Label : String) is
   begin
      if not Condition then
         Put_Line ("FAIL: " & Label);
         raise Program_Error;
      end if;
      Checks := Checks + 1;
   end Check;

   -- Mock device -------------------------------------------------------
   type Auth_Mode is (Accept_Then_Set, Accept_Never_Set, Refuse, Transport_Down);
   WOPCM_Value : Register_Word := 0;
   Control : Register_Word := 0;
   Load_Info : Register_Word := 0;
   Written : array (Register_Offset range 16#C300# .. 16#C314#) of Register_Word :=
     [others => 0];
   DMA_Latency : Natural := 3;        -- control reads until START clears
   DMA_Stuck : Boolean := False;
   DMA_Countdown : Natural := 0;
   Mode : Auth_Mode := Accept_Then_Set;
   Auth_Latency : Natural := 4;       -- status reads until bit 0 sets
   Auth_Countdown : Natural := 0;
   Auth_Pending : Boolean := False;
   Requests : Natural := 0;
   Last_RSA : Unsigned_32 := 0;
   Fail_Write_At : Natural := 0;      -- 0: never
   Writes : Natural := 0;
   Clock : Unsigned_64 := 1_000;
   Step : Unsigned_64 := 10;
   Clock_Down : Boolean := False;
   Pauses : Natural := 0;

   procedure Read32 (Offset : Register_Offset; Value : out Register_Word;
                     Success : out Boolean) is
   begin
      Success := True;
      if Offset = GuC_WOPCM_Offset then Value := WOPCM_Value;
      elsif Offset = DMA_Control then
         if (Control and Start_DMA) /= 0 and not DMA_Stuck then
            if DMA_Countdown = 0 then
               Control := Control and not Start_DMA;
            else
               DMA_Countdown := DMA_Countdown - 1;
            end if;
         end if;
         Value := Control;
      elsif Offset = HuC_Kernel_Load_Info then
         if Auth_Pending then
            if Auth_Countdown = 0 then
               Load_Info := Load_Info or HuC_Load_Successful;
               Auth_Pending := False;
            else
               Auth_Countdown := Auth_Countdown - 1;
            end if;
         end if;
         Value := Load_Info;
      else
         Value := Unreadable; Success := False;
      end if;
   end Read32;

   procedure Write32 (Offset : Register_Offset; Value : Register_Word;
                      Success : out Boolean) is
   begin
      Writes := Writes + 1;
      Success := Writes /= Fail_Write_At;
      if not Success then return; end if;
      if Offset in Written'Range then Written (Offset) := Value; end if;
      if Offset = DMA_Control then
         -- Masked register: upper half selects bits.
         declare
            Selected : constant Register_Word := Shift_Right (Value, 16);
         begin
            Control := (Control and not Selected) or (Value and Selected);
            if (Value and Selected and Start_DMA) /= 0 then
               DMA_Countdown := DMA_Latency;
            end if;
         end;
      end if;
   end Write32;

   function Now return Unsigned_64 is
   begin
      if Clock_Down then return Unsigned_64'Last; end if;
      Clock := Clock + Step;
      return Clock;
   end Now;

   procedure Pause is
   begin
      Pauses := Pauses + 1;
   end Pause;

   procedure Request_Authentication (RSA_GGTT : Unsigned_32; Reply : out Auth_Reply) is
   begin
      Requests := Requests + 1;
      Last_RSA := RSA_GGTT;
      case Mode is
         when Accept_Then_Set =>
            Reply := Accepted; Auth_Pending := True; Auth_Countdown := Auth_Latency;
         when Accept_Never_Set => Reply := Accepted;
         when Refuse => Reply := Refused;
         when Transport_Down => Reply := Transport_Failed;
      end case;
   end Request_Authentication;

   -- A GT reset or GuC reload clears HuC state.
   procedure Hardware_Reset is
   begin
      Load_Info := 0; Auth_Pending := False; Control := 0;
   end Hardware_Reset;

   package HuC is new Intel_GPU_HuC_Load
     (Read32, Write32, Now, Pause, Request_Authentication);
   use type HuC.Phase, HuC.Upload_Result, HuC.Auth_Result, HuC.Check_Result,
     HuC.Load_Generation;

   -- The pinned tgl_huc.bin CSS header fields (others zero).
   function Header return Intel_GPU_Firmware.CSS_Header is
      H : Intel_GPU_Firmware.CSS_Header := [others => 0];
      procedure Put (Offset : Natural; Value : Unsigned_32) is
      begin
         for B in 0 .. 3 loop
            H (Offset + B) := Unsigned_8 (Shift_Right (Value, 8 * B) and 16#FF#);
         end loop;
      end Put;
   begin
      Put (0, 6); Put (4, 16#A1#); Put (8, 16#1_0000#); Put (16, 16#8086#);
      Put (24, 16#2_4051#); Put (28, 64); Put (32, 64); Put (36, 1);
      Put (64, Selected_Version);
      return H;
   end Header;

   GuC_Upload : constant Upload_Bytes := 335_232; -- tgl_guc_70 CSS+code
   Layout : constant WOPCM_Layout := Select_Layout (Selected_Upload_Bytes, GuC_Upload);
   Locked_With_HuC : constant Register_Word :=
     WOPCM_Offset_Value (Layout) or WOPCM_Offset_Valid;
   -- Today's GuC-only partition: base 16 KiB, no agent bit.
   Locked_GuC_Only : constant Register_Word := 16#4000# or WOPCM_Offset_Valid;
   Source : constant Unsigned_64 := 16#0080_0000#;
   Pin_Bias : constant Unsigned_64 := Layout.Size;
   RSA_Page : constant Unsigned_64 := 16#0040_0000#;
   Poll_Limit : constant Positive := 1_000;
   Budget_DMA : constant Unsigned_64 := Linux_DMA_Budget_Us;
   Budget_Auth : constant Unsigned_64 := Linux_Auth_Budget_Us;

   L : HuC.Loader;
   U : HuC.Upload_Result;
   A : HuC.Auth_Result;
   C : HuC.Check_Result;

   procedure Fresh_Device is
   begin
      WOPCM_Value := Locked_With_HuC; Control := 0; Load_Info := 0;
      Written := [others => 0]; DMA_Stuck := False; Mode := Accept_Then_Set;
      Auth_Pending := False; Requests := 0; Fail_Write_At := 0; Writes := 0;
      Clock_Down := False; Step := 10;
   end Fresh_Device;

   procedure Load_And_Authenticate (Label : String) is
   begin
      HuC.Upload (L, Header, Selected_Blob_Bytes, Source, Clock + Budget_DMA,
                  Poll_Limit, U);
      Check (U = HuC.Transferred and HuC.Current (L) = HuC.Transferred,
             Label & ": upload");
      HuC.Authenticate (L, RSA_Page, Pin_Bias, Clock + Budget_Auth, Poll_Limit, A);
      Check (A = HuC.Authenticated and HuC.Current (L) = HuC.Authenticated,
             Label & ": authenticate");
   end Load_And_Authenticate;
begin
   -- Firmware policy and WOPCM partition ---------------------------------
   Check (Select_HuC (Intel_GPU_Media_Engines.Decode (16#8086#, 16#46D2#, 0)) =
            TGL_HuC_7_9_3, "select: VCS present");
   Check (Select_HuC (Intel_GPU_Media_Engines.Decode
            (16#8086#, 16#46D2#, 2#101#)) = No_HuC, "select: no VCS");
   Check (Path (TGL_HuC_7_9_3) = "firmware/intel/tgl_huc.bin", "path");
   Check (Selected_Upload_Bytes = 589_632, "upload bytes");
   Check (Matches_Selected_ADLN_HuC (Header, Selected_Blob_Bytes), "header admitted");
   Check (not Matches_Selected_ADLN_HuC (Header, Selected_Blob_Bytes - 4),
          "short blob rejected");
   Check (Layout.Valid and Layout.Base = 16#9_4000#, "layout base 592 KiB");
   Check (Layout.Size = 2 * 1024 * 1024 - 36 * 1024 - 16#9_4000#, "layout size");
   Check (WOPCM_Offset_Value (Layout) = 16#9_4002#, "offset register value");
   Check (WOPCM_Admits_HuC (Locked_With_HuC, Selected_Upload_Bytes), "admits");
   Check (not WOPCM_Admits_HuC (Locked_GuC_Only, Selected_Upload_Bytes),
          "GuC-only partition refuses HuC");
   Check (not Select_Layout (Selected_Upload_Bytes, 1_500_000).Valid,
          "oversized GuC refused");
   Check (RSA_Page_Admissible (RSA_Page, Pin_Bias) and
          not RSA_Page_Admissible (RSA_Page + 8, Pin_Bias) and
          not RSA_Page_Admissible (Pin_Bias - Page_Bytes, Pin_Bias) and
          not RSA_Page_Admissible (GuC_GGTT_Top, Pin_Bias), "RSA placement");

   -- Success --------------------------------------------------------------
   Fresh_Device;
   Load_And_Authenticate ("success");
   Check (Written (DMA_Address_0_Low) = Unsigned_32 (Source) and
          Written (DMA_Address_0_High) = 0 and
          Written (DMA_Address_1_Low) = HuC_Destination and
          Written (DMA_Address_1_High) = DMA_Address_Space_WOPCM and
          Written (DMA_Copy_Size) = 589_632 and
          Written (DMA_Control) = HuC_DMA_Clear, "success: DMA programming");
   Check (HuC_DMA_Start = Masked_Enable (HuC_UKernel or Start_DMA) and
          HuC_DMA_Clear = Masked_Disable (HuC_UKernel),
          "masked control words");
   Check (Requests = 1 and Last_RSA = Unsigned_32 (RSA_Page), "success: one request");
   HuC.Check (L, C);
   Check (C = HuC.Still_Authenticated, "success: check");
   -- Repeating either step is refused without touching hardware.
   Writes := 0;
   HuC.Upload (L, Header, Selected_Blob_Bytes, Source, Clock + Budget_DMA,
               Poll_Limit, U);
   Check (U = HuC.Rejected and Writes = 0, "repeat upload refused");
   HuC.Authenticate (L, RSA_Page, Pin_Bias, Clock + Budget_Auth, Poll_Limit, A);
   Check (A = HuC.Rejected and Requests = 1, "repeat authenticate refused");

   -- Reset then redo -------------------------------------------------------
   Hardware_Reset;
   HuC.Check (L, C);
   Check (C = HuC.Lost and HuC.Current (L) = HuC.Failed, "reset: loss detected");
   HuC.Reset (L);
   Check (HuC.Current (L) = HuC.Idle and HuC.Generation (L) = 1, "reset: idle");
   Load_And_Authenticate ("redo");
   Check (Requests = 2, "redo: second request");
   HuC.Reset (L);
   Check (HuC.Generation (L) = 2, "reset: generation");

   -- Refusals before any write ----------------------------------------------
   Fresh_Device; HuC.Reset (L);
   WOPCM_Value := Locked_GuC_Only;
   HuC.Upload (L, Header, Selected_Blob_Bytes, Source, Clock + Budget_DMA,
               Poll_Limit, U);
   Check (U = HuC.WOPCM_Not_Ready and HuC.Current (L) = HuC.Idle and Writes = 0,
          "GuC-only WOPCM refused");
   WOPCM_Value := Locked_With_HuC and not WOPCM_Offset_Valid;
   HuC.Upload (L, Header, Selected_Blob_Bytes, Source, Clock + Budget_DMA,
               Poll_Limit, U);
   Check (U = HuC.WOPCM_Not_Ready and Writes = 0, "unlocked WOPCM refused");
   WOPCM_Value := Locked_With_HuC;
   Control := Start_DMA; DMA_Stuck := True;  -- another transfer in flight
   HuC.Upload (L, Header, Selected_Blob_Bytes, Source, Clock + Budget_DMA,
               Poll_Limit, U);
   Check (U = HuC.Busy and HuC.Current (L) = HuC.Idle and Writes = 0, "busy");
   Control := 0; DMA_Stuck := False;
   HuC.Upload (L, Header, Selected_Blob_Bytes, Source + 8, Clock + Budget_DMA,
               Poll_Limit, U);
   Check (U = HuC.Rejected and Writes = 0, "unaligned source");
   HuC.Upload (L, Header, Selected_Blob_Bytes, 2 ** 32 - 4096, Clock + Budget_DMA,
               Poll_Limit, U);
   Check (U = HuC.Rejected and Writes = 0, "source crosses 4 GiB");
   HuC.Upload (L, Header, Selected_Blob_Bytes, Source, Clock, Poll_Limit, U);
   Check (U = HuC.Deadline_Passed and Writes = 0, "deadline passed");
   Clock_Down := True;
   HuC.Upload (L, Header, Selected_Blob_Bytes, Source, Clock + Budget_DMA,
               Poll_Limit, U);
   Check (U = HuC.Clock_Unavailable and Writes = 0, "clock unavailable");
   Clock_Down := False;
   HuC.Authenticate (L, RSA_Page, Pin_Bias, Clock + Budget_Auth, Poll_Limit, A);
   Check (A = HuC.Rejected and Requests = 0, "authenticate before upload");

   -- DMA faults --------------------------------------------------------------
   Fresh_Device; HuC.Reset (L);
   DMA_Stuck := True;
   HuC.Upload (L, Header, Selected_Blob_Bytes, Source, Clock + Budget_DMA,
               Poll_Limit, U);
   Check (U = HuC.Timed_Out and HuC.Current (L) = HuC.Failed, "DMA timeout");
   Check ((Control and HuC_UKernel) = 0, "DMA timeout: flag cleared");
   HuC.Authenticate (L, RSA_Page, Pin_Bias, Clock + Budget_Auth, Poll_Limit, A);
   Check (A = HuC.Rejected, "failed: authenticate refused until reset");
   Fresh_Device; HuC.Reset (L);
   DMA_Stuck := True; Step := 0;  -- frozen clock: poll limit terminates
   HuC.Upload (L, Header, Selected_Blob_Bytes, Source, Clock + Budget_DMA,
               Poll_Limit, U);
   Check (U = HuC.Timed_Out and Pauses > 0, "frozen clock bounded");
   Fresh_Device; HuC.Reset (L);
   Fail_Write_At := 3;
   HuC.Upload (L, Header, Selected_Blob_Bytes, Source, Clock + Budget_DMA,
               Poll_Limit, U);
   Check (U = HuC.Write_Failed and HuC.Current (L) = HuC.Failed, "write failure");

   -- Authentication faults ---------------------------------------------------
   Fresh_Device; HuC.Reset (L);
   Mode := Refuse;
   HuC.Upload (L, Header, Selected_Blob_Bytes, Source, Clock + Budget_DMA,
               Poll_Limit, U);
   HuC.Authenticate (L, RSA_Page, Pin_Bias, Clock + Budget_Auth, Poll_Limit, A);
   Check (A = HuC.Signature_Refused and HuC.Current (L) = HuC.Failed,
          "bad signature refused by GuC");
   Fresh_Device; HuC.Reset (L);
   Mode := Accept_Never_Set;
   HuC.Upload (L, Header, Selected_Blob_Bytes, Source, Clock + Budget_DMA,
               Poll_Limit, U);
   HuC.Authenticate (L, RSA_Page, Pin_Bias, Clock + 500, Poll_Limit, A);
   Check (A = HuC.Timed_Out and HuC.Current (L) = HuC.Failed,
          "accepted but never verified: timeout");
   Check (Requests = 1, "timeout: single request, no retry");
   Fresh_Device; HuC.Reset (L);
   Mode := Transport_Down;
   HuC.Upload (L, Header, Selected_Blob_Bytes, Source, Clock + Budget_DMA,
               Poll_Limit, U);
   HuC.Authenticate (L, RSA_Page, Pin_Bias, Clock + Budget_Auth, Poll_Limit, A);
   Check (A = HuC.Transport_Failed and HuC.Current (L) = HuC.Failed, "CT failure");
   Fresh_Device; HuC.Reset (L);
   HuC.Upload (L, Header, Selected_Blob_Bytes, Source, Clock + Budget_DMA,
               Poll_Limit, U);
   Load_Info := HuC_Load_Successful;
   HuC.Authenticate (L, RSA_Page, Pin_Bias, Clock + Budget_Auth, Poll_Limit, A);
   Check (A = HuC.Stale_Status and Requests = 0, "stale status refused");
   Fresh_Device; HuC.Reset (L);
   HuC.Upload (L, Header, Selected_Blob_Bytes, Source, Clock + Budget_DMA,
               Poll_Limit, U);
   HuC.Authenticate (L, RSA_Page + 8, Pin_Bias, Clock + Budget_Auth, Poll_Limit, A);
   Check (A = HuC.Rejected and HuC.Current (L) = HuC.Transferred,
          "bad RSA page refused, phase kept");

   -- Step-wise authentication for a driver loop: one read per turn.
   Fresh_Device; Auth_Latency := 3;
   HuC.Begin_Authentication (L, RSA_Page, Pin_Bias, Clock + Budget_Auth, A);
   Check (A = HuC.Pending and HuC.Current (L) = HuC.Authenticating, "begin");
   for Turn in 1 .. 3 loop
      HuC.Poll_Authentication (L, A);
      Check (A = HuC.Pending, "turn pending");
   end loop;
   HuC.Poll_Authentication (L, A);
   Check (A = HuC.Authenticated and HuC.Current (L) = HuC.Authenticated,
          "turn authenticated");
   HuC.Poll_Authentication (L, A);
   Check (A = HuC.Rejected, "poll after done refused");

   -- Reset mid-authentication, then redo.
   Fresh_Device; HuC.Reset (L);
   HuC.Upload (L, Header, Selected_Blob_Bytes, Source, Clock + Budget_DMA,
               Poll_Limit, U);
   HuC.Begin_Authentication (L, RSA_Page, Pin_Bias, Clock + Budget_Auth, A);
   Hardware_Reset; HuC.Reset (L);
   Check (HuC.Current (L) = HuC.Idle, "reset mid-authentication");
   Load_And_Authenticate ("redo after mid-auth reset");

   Put_Line ("huc_load_tests: PASS (" & Checks'Image & " checks)");
end HuC_Load_Tests;
