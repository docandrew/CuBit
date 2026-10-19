with Intel_GPU_Physical_Extents;
with Intel_GPU_Buffer_Backing;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.System_Facts;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles.Startup;
with Intel_GPU_Replies;
with Startup;
with CuBit.Desktop_Messages;
with CuBit.Display_Protocol;
with CuBit.Display_Planes;
with CuBit.GPU_Plane_Protocol;
with CuBit.Memory_Grants;
with Intel_GPU_Cursor_Program;
with Intel_GPU_Cursor_Surfaces;
with Intel_GPU_Cursor_Plane;
with Intel_GPU_Cursor_DDB;
with Intel_GPU_Primary_Flip;
with CuBit.Monotonic;
with Intel_GPU_Display_Topology;
with Intel_GPU_DMA_Cache;
with Intel_GPU_GGTT;
with Intel_GPU_GGTT_Mapping;
with Intel_GPU_GGTT_Publish;
with Intel_GPU_Native_Cursor_Program;
with Intel_GPU_Native_GGTT;
with Intel_GPU_Pipe_Mapping;
with Intel_GPU_Pipe_Pages;
with CuBit.Monitor_EDID;
with CuBit.Monitor_EDID.Catalog;
with CuBit.Output_Discovery;
with CuBit.Output_Modes;
with Intel_GPU_Display_Registers;
with Intel_GPU_Display_Decode;
with Intel_GPU_Display_Readout;
with Intel_GPU_Display_Mapping;
with Intel_GPU_GMBUS;
with Intel_GPU_DP_AUX;
with Intel_GPU_HDMI_Modeset;
with Intel_GPU_Mode_Admission;
with Intel_GPU_Modeset_Mapping;
with Intel_GPU_Modeset_Pages;
with Intel_GPU_Modeset_Plan;
with Intel_GPU_PHY_Mapping;
with Intel_GPU_PHY_Pages;
with Intel_GPU_Transcoder_Timing;
package body Intel_GPU_Display_Executor is
   package FO renames Intel_GPU_Firmware_Output;
   package PP renames Intel_GPU_Pipe_Pages;
   package CP renames Intel_GPU_Cursor_Program;
   package CS renames Intel_GPU_Cursor_Surfaces;
   package GP renames CuBit.GPU_Plane_Protocol;
   package DPL renames CuBit.Display_Planes;
   package Topology renames Intel_GPU_Display_Topology;
   use type FO.Outcome, CS.Readiness, Topology.Pipe, DPL.Plane_Count;
   use type CuBit.Display_Protocol.Wire_Message;

   Framebuffer_Bits  : constant Unsigned_64 := 32;

   --  Scanout protocol labels display.svc sends a native GPU (0x0A03 Clear,
   --  0x0A04 Get_Status, 0x0A05 Map_Framebuffer, 0x0A07 Present_Buffer,
   --  0x0A08/0x0A09 the output catalog).
   Scanout_First : constant Unsigned_32 := 16#0A03#;
   OP_GPU_Clear : constant Unsigned_32 := 16#0A03#;
   OP_GPU_Get_Status : constant Unsigned_32 := 16#0A04#;
   OP_GPU_Map_Framebuffer : constant Unsigned_32 := 16#0A05#;
   OP_GPU_Present_Buffer : constant Unsigned_32 := 16#0A07#;
   Half_Word : constant := 2 ** 32;
   Scanout_Last  : constant Unsigned_32 := 16#0A09#;
   GPU_Error_Unsupported : constant Unsigned_64 := 5;

   Cursor_Plane_Number : constant DPL.Plane_Number := 1;
   Cursor_Planes : constant DPL.Plane_Number := 1;
   --  Above the five universal planes (i915: cursor zpos num_sprites + 1).
   Cursor_Z : constant DPL.Plane_Z := 6;
   Staging_Bytes : constant := CS.Surface_Bytes;
   Surfaces_Bytes : constant := CS.Surface_Count * CS.Surface_Bytes;
   Guard_Bytes : constant := 4_096;
   Page_Bytes : constant := 4_096;

   Selection : FO.Selection;
   Page : PP.Page_Index := 0;

   function Selected_Held return Boolean is
     (Selection.State = FO.Selected and then
        (case Selection.Pipe is
           when Topology.A => Pipe_A_Held, when Topology.B => Pipe_B_Held,
           when Topology.C => Pipe_C_Held, when Topology.D => Pipe_D_Held));
   function Page_Ready return Boolean is
     (Selection.State = FO.Selected and then Intel_GPU_Pipe_Mapping.Ready (Page) and then
      Display_Owner and then Selected_Held);

   function Page_Address (Index : PP.Page_Index) return System.Address is
     (To_Address (Integer_Address (PP.Address (Index))));
   package Cursor_A is new Intel_GPU_Native_Cursor_Program (Page_Address (0), Pipe_A_Held);
   package Cursor_B is new Intel_GPU_Native_Cursor_Program (Page_Address (1), Pipe_B_Held);
   package Cursor_C is new Intel_GPU_Native_Cursor_Program (Page_Address (2), Pipe_C_Held);
   package Cursor_D is new Intel_GPU_Native_Cursor_Program (Page_Address (3), Pipe_D_Held);

   type Update_Kind is (Full, Position_Only);
   function Apply (Kind : Update_Kind; Values : CP.Register_Values) return Boolean is
      Owner : constant Boolean := Page_Ready;
   begin
      if Selection.State /= FO.Selected then return False; end if;
      case Selection.Pipe is
         when Topology.A =>
            return Cursor_A."=" ((if Kind = Full then Cursor_A.Program (Owner, Values)
                                  else Cursor_A.Move (Owner, Values)), Cursor_A.Written);
         when Topology.B =>
            return Cursor_B."=" ((if Kind = Full then Cursor_B.Program (Owner, Values)
                                  else Cursor_B.Move (Owner, Values)), Cursor_B.Written);
         when Topology.C =>
            return Cursor_C."=" ((if Kind = Full then Cursor_C.Program (Owner, Values)
                                  else Cursor_C.Move (Owner, Values)), Cursor_C.Written);
         when Topology.D =>
            return Cursor_D."=" ((if Kind = Full then Cursor_D.Program (Owner, Values)
                                  else Cursor_D.Move (Owner, Values)), Cursor_D.Written);
      end case;
   end Apply;
   function Program_Cursor (Values : CP.Register_Values) return Boolean is
     (Apply (Full, Values));
   function Move_Cursor (Values : CP.Register_Values) return Boolean is
     (Apply (Position_Only, Values));

   function Read_Register (Offset : Unsigned_32) return Unsigned_32 is
   begin
      if not Page_Ready or else Offset >= Page_Bytes or else Offset mod 4 /= 0 then
         return Unsigned_32'Last;
      end if;
      declare
         Register : Unsigned_32 with Import, Volatile_Full_Access,
           Address => Page_Address (Page) + Storage_Offset (Offset);
      begin
         return Register;
      end;
   end Read_Register;
   procedure Write_Register (Offset, Value : Unsigned_32; Written : out Boolean) is
   begin
      Written := False;
      if not Page_Ready or else Offset >= Page_Bytes or else Offset mod 4 /= 0 then
         return;
      end if;
      declare
         Register : Unsigned_32 with Import, Volatile_Full_Access,
           Address => Page_Address (Page) + Storage_Offset (Offset);
      begin
         Register := Value;
         Written := True;
      end;
   end Write_Register;

   function Clock_Ms return Unsigned_64 is (syscall (SYSCALL_GETTIME));
   procedure Pause_One_Ms is
      Now : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
      Activity : Activity_Result;
      pragma Unreferenced (Activity);
   begin
      if Now /= Unsigned_64'Last then
         Activity := Wait_For_Activity_Until (Now + 1);
      end if;
   end Pause_One_Ms;
   package Cursor is new Intel_GPU_Cursor_Plane
     (Program_Cursor, Move_Cursor, Read_Register, Write_Register,
      Intel_GPU_DMA_Cache.Flush_Range, Clock_Ms, Pause_One_Ms);
   package DDB renames Intel_GPU_Cursor_DDB;

   function Monotonic_Us return Unsigned_64 is
      Reading : constant CuBit.Monotonic.Reading := CuBit.Monotonic.Read;
   begin
      return (if Reading.Available then Reading.Microseconds else Unsigned_64'Last);
   end Monotonic_Us;
   package Flip is new Intel_GPU_Primary_Flip
     (Read_Register, Write_Register, Intel_GPU_DMA_Cache.Flush_Range,
      Monotonic_Us, Pause_One_Ms);
   use type Flip.Takeover_Result, Flip.Submit_Result;
   --  display.svc's boot clear colour (it clears to it right after).
   Boot_Color : constant Unsigned_32 := 16#0013_1518#;
   --  A Present superseded by a newer one (mailbox) is answered with this.
   GPU_Status_Superseded : constant Unsigned_64 := 6;
   GPU_Status_OK : constant Unsigned_64 := 0;
   GPU_Error_Bad_State : constant Unsigned_64 := 3;
   --  The owed Present's saved reply, answered when its flip latches
   --  (CAP-002: a Reply handle, no slot to choose or collide; the NUC
   --  freeze 2026-10-12 had slot 13 already holding the GT frequency page).
   Present_Reply : Intel_GPU_Replies.Owed;
   Present_Reply_Owed : Boolean := False;
   --  Presents answered before their flip latched (no reply could be kept).
   Unpaced_Presents : Natural := 0;
   Scanout_Status : Flip.Takeover_Result := Flip.Not_Configured;
   Scanout_Arena_Pages : constant := Intel_GPU_Buffer_Backing.Page_Count'Last;

   --  DBUF_CTL_S0..S3 (i915 skl_watermark_regs.h), logged only.
   type DBUF_Slice is range 0 .. 3;
   DBUF_Control : constant array (DBUF_Slice) of Unsigned_32 :=
     [16#45008#, 16#44FE8#, 16#44300#, 16#44304#];
   Active_Pipes : Natural := 0;
   Plane_Enable_Bit : constant Unsigned_32 := 16#8000_0000#;

   ---------------------------------------------------------------------------
   --  GGTT publication of display surfaces in the runtime range, outside the
   --  firmware's live scanout. One transaction at a time; the write binding
   --  exists only while Publish runs.
   ---------------------------------------------------------------------------
   Active : Intel_GPU_Buffer_Reply.Backing;
   Active_Key, Active_GPU : Unsigned_64 := 0;
   Bound : Boolean := False;

   function Surface_Range_Allowed (First, Bytes : Unsigned_64) return Boolean is
     (Publication_Ready and then Bytes > 0 and then
      First >= Runtime_First and then First < Runtime_Limit and then
      Bytes <= Runtime_Limit - First and then Scanout_Clear (First, Bytes));
   function Surface_Page (Base, Offset : Unsigned_64) return Unsigned_64 is
     (if not Active.Ready or else Base /= Active_Key or else Offset mod Page_Bytes /= 0 or else
         Offset >= Active.Bytes
      then 0 else Intel_GPU_Buffer_Reply.Page_Address (Active, Offset));
   function Surface_Write_Allowed (Index, Value : Unsigned_64) return Boolean is
     (Bound and then Active.Ready and then Index >= Active_GPU / Page_Bytes and then
      Index - Active_GPU / Page_Bytes < Active.Bytes / Page_Bytes and then
      Surface_Range_Allowed (Index * Page_Bytes, Page_Bytes) and then
      Value = Intel_GPU_GGTT.Encode_System_Page
        (Surface_Page (Active_Key, (Index - Active_GPU / Page_Bytes) * Page_Bytes)));
   package Surface_IO is new Intel_GPU_Native_GGTT
     (Publication_Ready, Intel_GPU_GGTT_Mapping.Bytes, Surface_Write_Allowed);
   procedure Prepare_Surface (First, Bytes : Unsigned_64; Success : out Boolean) is
   begin
      Success := False;
      if Bound or else not Active.Ready or else Bytes /= Active.Bytes or else
        not Surface_Range_Allowed (First, Bytes)
      then return; end if;
      --  The display engine does not snoop the CPU caches.
      if not Intel_GPU_DMA_Cache.Flush_Range (Active.CPU_Address, Bytes) then return; end if;
      Active_GPU := First;
      Bound := True;
      Success := True;
   end Prepare_Surface;
   Surface_Limit_Bytes : constant := Intel_GPU_Physical_Extents.Capacity;
   package Surface_Publication is new Intel_GPU_GGTT_Publish
     (Surface_Range_Allowed, Prepare_Surface, Surface_IO.Read_PTE,
      Surface_IO.Write_PTE, Invalidate, Surface_Limit_Bytes,
      Resolve_Page => Surface_Page);
   use type Surface_Publication.Result;
   --  VT-d scanout alignment, which also covers plane 1's 16 KiB.
   Surface_Alignment : constant := CP.VTd_Base_Alignment;

   procedure Publish
     (Attempt : in out Surface_Publication.Attempt;
      Ledger : in out Intel_GPU_GGTT_Reservations.Ledger;
      Backing : Intel_GPU_Buffer_Reply.Backing; GPU : out Unsigned_64;
      Status : out Surface_Publication.Result) is
   begin
      GPU := 0;
      Active := Backing;
      Active_Key := (if Backing.Ready then Intel_GPU_Buffer_Reply.Page_Address (Backing, 0) else 0);
      Bound := False;
      Surface_Publication.Publish_Available
        (Attempt, Ledger, Active_Key, (if Backing.Ready then Backing.Bytes else 0),
         Surface_Alignment, GPU, Status);
      Bound := False;
      Active := (Ready => False);
      Active_Key := 0;
   end Publish;

   function Describe (Value : Surface_Publication.Result) return String is
     (case Value is
        when Surface_Publication.Published => "PUBLISHED",
        when Surface_Publication.Quarantined => "QUARANTINED",
        when Surface_Publication.Protected_Range => "PROTECTED-RANGE",
        when Surface_Publication.Reservation_Failed => "NO-FREE-RUNTIME-RANGE",
        when Surface_Publication.Read_Failed => "READ-FAILED",
        when Surface_Publication.Prepare_Failed => "PREPARE-FAILED",
        when Surface_Publication.Rejected => "REJECTED");

   function Hex (Value : Unsigned_64) return String is
      Digits_Text : constant String := "0123456789ABCDEF";
      Text : String (1 .. 8);
      Rest : Unsigned_64 := Value;
   begin
      for I in reverse Text'Range loop
         Text (I) := Digits_Text (Natural (Rest and 15) + 1);
         Rest := Shift_Right (Rest, 4);
      end loop;
      return Text;
   end Hex;

   ---------------------------------------------------------------------------
   procedure Register_Service is
      Authority : constant CuBit.Handles.Startup.Optional_Notification :=
        Startup.Registration;
      Result : constant Unsigned_64 :=
        (if Authority.Present then registerDriver (DRIVER_GPU, Authority.Value)
         else Unsigned_64'Last);
   begin
      Log ("intel-gpu: GPU service registration " &
           (if not Authority.Present then "not granted (another GPU service)"
            elsif Result = Unsigned_64'Last then "REFUSED" else "accepted") &
           " (display executor answers display.svc only)");
   end Register_Service;

   ---------------------------------------------------------------------------
   --  DISP-002/004/005: what the firmware lit, the monitor on it (EDID over
   --  GMBUS), and which of its modes this engine can drive. Boot only,
   --  before the request loop (the page grants poll the broker).
   ---------------------------------------------------------------------------
   package R renames Intel_GPU_Display_Registers;
   package DD renames Intel_GPU_Display_Decode;
   package C renames CuBit.Monitor_EDID.Catalog;
   package MA renames Intel_GPU_Mode_Admission;
   package MP renames Intel_GPU_Modeset_Plan;
   package MPages renames Intel_GPU_Modeset_Pages;
   package OD renames CuBit.Output_Discovery;
   package OM renames CuBit.Output_Modes;
   package TT renames Intel_GPU_Transcoder_Timing;
   use type DD.Signal_Mode, DD.Crystal, MA.Verdict, R.DDI_Port;
   package Readout is new Intel_GPU_Display_Readout (Read_MMIO, Log);
   Display_State : Readout.Snapshot;
   Lit : Readout.Lit_Output;

   function Transcoder_Of (Item : Topology.Pipe) return R.Transcoder is
     (R.Transcoder'Val (Topology.Pipe'Pos (Item)));
   function Modeset_Page_Of (Item : R.Transcoder) return MPages.Transcoder_Page is
     (MPages.Page'Val (MPages.Page'Pos (MPages.Transcoder_A) + R.Transcoder'Pos (Item)));

   --  A register through one of the modeset pages, when it is mapped.
   function Modeset_Address (Item : MPages.Page; Offset : Unsigned_32) return Unsigned_64 is
     (if Display_Owner and then Intel_GPU_Modeset_Mapping.Ready (Item) and then
         Unsigned_64 (Offset) >= MPages.Offset (Item) and then
         Unsigned_64 (Offset) < MPages.Offset (Item) + MPages.Page_Bytes and then Offset mod 4 = 0
      then MPages.Address (Item) + (Unsigned_64 (Offset) - MPages.Offset (Item)) else 0);

   procedure Store (Address : Unsigned_64; Value : Unsigned_32; Written : out Boolean) is
   begin
      Written := False;
      if Address = 0 then return; end if;
      declare
         Register : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Integer_Address (Address));
      begin
         Register := Value;
         Written := True;
      end;
   end Store;

   procedure GMBUS_Read (Offset : R.Register_Offset; Value : out Unsigned_32) is
      Address : constant Unsigned_64 := Modeset_Address (MPages.GMBUS, Offset);
   begin
      if Address = 0 then
         Value := Unsigned_32'Last;
         return;
      end if;
      declare
         Register : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Integer_Address (Address));
      begin
         Value := Register;
      end;
   end GMBUS_Read;
   procedure GMBUS_Write (Offset : R.Register_Offset; Value : Unsigned_32; Written : out Boolean) is
   begin
      Store (Modeset_Address (MPages.GMBUS, Offset), Value, Written);
   end GMBUS_Write;
   procedure GMBUS_Clock (Value : out Unsigned_64) is
      Reading : constant CuBit.Monotonic.Reading := CuBit.Monotonic.Read;
   begin
      Value := (if Reading.Available then Reading.Microseconds else Unsigned_64'Last);
   end GMBUS_Clock;
   package DDC is new Intel_GPU_GMBUS (GMBUS_Read, GMBUS_Write, GMBUS_Clock);
   use type DDC.Status;

   --  DP/eDP: the AUX channel of the lit port, through PHY page 0 (0x64000,
   --  Intel_GPU_PHY_Pages), only that channel's control and data words.
   AUX_First : constant := 16#6_4010#;
   AUX_Stride : constant := 16#100#;
   AUX_Words : constant := 6;
   function AUX_Address (Offset : Unsigned_32) return Unsigned_64 is
     (if Display_Owner and then Intel_GPU_PHY_Mapping.Ready and then Lit.Found and then
         Offset >= AUX_First + AUX_Stride * R.DDI_Port'Pos (Lit.Port) and then
         Offset < AUX_First + AUX_Stride * R.DDI_Port'Pos (Lit.Port) + 4 * AUX_Words and then
         Offset mod 4 = 0
      then Intel_GPU_PHY_Pages.Virtual_Base + Unsigned_64 (Offset - R.DDI_BUF_CTL_A) else 0);
   procedure AUX_Read (Offset : R.Register_Offset; Value : out Unsigned_32) is
      Address : constant Unsigned_64 := AUX_Address (Offset);
   begin
      if Address = 0 then
         Value := Unsigned_32'Last;
         return;
      end if;
      declare
         Register : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Integer_Address (Address));
      begin
         Value := Register;
      end;
   end AUX_Read;
   procedure AUX_Write (Offset : R.Register_Offset; Value : Unsigned_32; Written : out Boolean) is
   begin
      Store (AUX_Address (Offset), Value, Written);
   end AUX_Write;
   package AUX is new Intel_GPU_DP_AUX (AUX_Read, AUX_Write, GMBUS_Clock);
   use type AUX.Status;
   function Channel_Of (Port : R.DDI_Port) return AUX.Channel is
     (AUX.Channel'Val (R.DDI_Port'Pos (Port)));

   function Pin_Of (Port : R.DDI_Port) return DDC.Pin is
     (case Port is
        when R.Port_A | R.Port_C => DDC.Pin_DDI_A, when R.Port_B => DDC.Pin_DDI_B,
        when R.Port_TC1 => DDC.Pin_TC1, when R.Port_TC2 => DDC.Pin_TC2,
        when R.Port_TC3 => DDC.Pin_TC3, when R.Port_TC4 => DDC.Pin_TC4);

   --  The monitor's EDID: the base block and every extension it declares.
   EDID_Attempts : constant := 3;
   EDID_Base : C.Block := [others => 0];
   EDID_Extensions : C.Block_Array (1 .. C.Extension_Count'Last) := [others => [others => 0]];
   Extensions_Read : C.Extension_Count := 0;
   Monitor_Known : Boolean := False;
   Monitor : C.Identity;
   Monitor_Size : C.Physical_Size;
   Sink : C.Sink_Limits;
   type Mode_List_Access is access C.Mode_List;
   type Verdict_Array_Access is access MA.Verdict_Array;
   Modes : Mode_List_Access;
   Verdicts : Verdict_Array_Access;
   --  The mode to offer (Intel_GPU_Mode_Admission.Best); zero when none.
   Best_Mode : Natural := 0;
   Facts : MA.Facts;
   --  Catalog revision: bumped by every applied mode change.
   Output_Revision : OD.Catalog_Revision := 1;

   function Image (Value : Natural) return String is
      Text : constant String := Natural'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;
   function Millihertz_Text (Value : Unsigned_64) return String is
      Whole : constant String := Unsigned_64'Image (Value / 1000);
      Fraction : constant String := Unsigned_64'Image (1000 + Value mod 1000);
   begin
      return Whole (Whole'First + 1 .. Whole'Last) & "." & Fraction (Fraction'First + 2 .. Fraction'Last);
   end Millihertz_Text;
   function Mode_Text (Mode : CuBit.Monitor_EDID.Timing) return String is
     (Image (Mode.Width) & "x" & Image (Mode.Height) & "@" &
      Millihertz_Text (CuBit.Monitor_EDID.Refresh_Millihertz (Mode)) & " " &
      Image (Natural (Mode.Clock) * 10) & " kHz");
   function Source_Name (Item : C.Mode_Source) return String is
     (case Item is
        when C.Base_Detailed => "base-dtd", when C.Extension_Detailed => "cta-dtd",
        when C.Video_Code => "cta-vic");

   procedure Log_Block (Index : Natural; Data : C.Block) is
      Digits_Text : constant String := "0123456789abcdef";
      Line : String (1 .. 64);
   begin
      for Row in 0 .. 3 loop
         for Col in 0 .. 31 loop
            Line (2 * Col + 1) := Digits_Text (Natural (Data (Row * 32 + Col) / 16) + 1);
            Line (2 * Col + 2) := Digits_Text (Natural (Data (Row * 32 + Col) mod 16) + 1);
         end loop;
         Log ("intel-gpu: EDID block " & Image (Index) & " row " & Image (Row) & " " & Line);
      end loop;
   end Log_Block;

   --  One EDID block over GMBUS (HDMI/DVI) or AUX (DP/eDP), with retries.
   procedure Read_Block (Over_AUX : Boolean; Index : DDC.EDID_Block_Index; Data : out C.Block;
                         Good : out Boolean) is
      Buffer : DDC.Bytes (1 .. DDC.EDID_Block_Bytes);
      Wire : AUX.Bytes (1 .. AUX.EDID_Block_Bytes);
      Status : DDC.Status := DDC.NAK;
      AUX_Status : AUX.Status := AUX.NAK;
      Fatal : Boolean;
   begin
      Data := [others => 0];
      Good := False;
      for Attempt in 1 .. EDID_Attempts loop
         if Over_AUX then
            AUX.Read_EDID_Block (Channel_Of (Lit.Port), Index, Wire, AUX_Status);
            if AUX_Status = AUX.OK then
               for I in Data'Range loop
                  Data (I) := Wire (I + 1);
               end loop;
               Good := C.Valid_Checksum (Data);
            end if;
            Fatal := AUX_Status = AUX.Not_Writable;
            Log ("intel-gpu: EDID block " & Image (Index) & " attempt " & Image (Attempt) & " aux " &
                 AUX.Name (AUX_Status) & (if AUX_Status = AUX.OK then " checksum " &
                   (if Good then "ok" else "BAD") else ""));
         else
            DDC.Read_EDID_Block (Pin_Of (Lit.Port), Index, Buffer, Status);
            if Status = DDC.OK then
               for I in Data'Range loop
                  Data (I) := Buffer (I + 1);
               end loop;
               Good := C.Valid_Checksum (Data);
            end if;
            Fatal := Status = DDC.Not_Writable;
            Log ("intel-gpu: EDID block " & Image (Index) & " attempt " & Image (Attempt) & " gmbus " &
                 DDC.Name (Status) & (if Status = DDC.OK then " checksum " &
                   (if Good then "ok" else "BAD") else ""));
         end if;
         exit when Good or else Fatal;
      end loop;
   end Read_Block;

   function To_Admission (Item : MA.Verdict) return OM.Admission is
     (case Item is
        when MA.Admitted => OM.Admitted,
        when MA.Not_Digital_Separate_Sync => OM.Sync_Unsupported,
        when MA.Type_C_Port => OM.Port_Unsupported,
        when MA.DisplayPort_Link_Unsupported => OM.Link_Unsupported,
        when MA.Above_Source_TMDS => OM.Above_Source_Clock,
        when MA.Above_Sink_TMDS => OM.Above_Sink_Clock,
        when MA.In_Combo_Clock_Hole => OM.Clock_Hole,
        when MA.Needs_Scrambling_Without_SCDC => OM.Needs_Scrambling,
        when MA.Needs_Higher_CDCLK => OM.Needs_Higher_CD_Clock);

   --  The scanout row bytes for a width (64-byte aligned, PLANE_STRIDE).
   function Pitch_For (Width : Positive) return Positive is
     ((Width * 4 + MP.Stride_Unit - 1) / MP.Stride_Unit * MP.Stride_Unit);

   procedure Identify (Register_Base : Unsigned_64) is
      Pixel : DD.Clock_kHz := 0;
   begin
      Readout.Capture (Display_State);
      Readout.Describe (Display_State);
      Lit := Readout.Lit (Display_State, Transcoder_Of (Selection.Pipe));
      if not Lit.Found then
         Log ("intel-gpu: lit output: transcoder " & FO.Pipe_Name (Selection.Pipe) &
              " has no enabled DDI function; no EDID read");
         return;
      end if;
      Pixel := Lit.Pixel_kHz;
      Log ("intel-gpu: lit output pipe " & FO.Pipe_Name (Selection.Pipe) & " port " &
           Readout.Port_Name (Lit.Port) & " " & Readout.Mode_Name (Lit.Mode) & " " &
           Image (Lit.Timing.Horizontal.Active) & "x" & Image (Lit.Timing.Vertical.Active) &
           " pixel_khz=" & Image (Pixel) &
           (if Pixel /= 0 then " refresh=" & Millihertz_Text (TT.Refresh_Millihertz (Lit.Timing, Pixel))
            else "") &
           " " & R.Combo_PLL'Image (Lit.PLL) & " cdclk_khz=" & Image (Display_State.CDCLK_kHz));
      --  Pages for DDC now and a mode change later: GMBUS, the lit
      --  transcoder, the combo PLL configuration.
      Log ("intel-gpu: modeset page GMBUS " &
           Intel_GPU_Modeset_Mapping.Prepare (Display_Owner, Register_Base, MPages.GMBUS));
      if R.Is_Combo (Lit.Port) then
         Log ("intel-gpu: modeset page transcoder " & R.Transcoder'Image (Transcoder_Of (Selection.Pipe)) &
              " " & Intel_GPU_Modeset_Mapping.Prepare
                (Display_Owner, Register_Base, Modeset_Page_Of (Transcoder_Of (Selection.Pipe))));
         Log ("intel-gpu: modeset page PLL configuration " &
              Intel_GPU_Modeset_Mapping.Prepare (Display_Owner, Register_Base, MPages.PLL_Configuration));
      end if;
      declare
         Good : Boolean;
         Over_AUX : constant Boolean := Lit.Mode in DD.DP_SST | DD.DP_MST;
      begin
         if Over_AUX then
            declare
               Caps : AUX.Bytes (1 .. 3);
               Status : AUX.Status;
            begin
               AUX.Read_DPCD (Channel_Of (Lit.Port), 0, Caps, Status);
               Log ("intel-gpu: DPCD port " & Readout.Port_Name (Lit.Port) & " aux " & AUX.Name (Status) &
                    " rev=" & Readout.Hex (Unsigned_32 (Caps (1))) &
                    " max_link_rate=" & Readout.Hex (Unsigned_32 (Caps (2))) &
                    " max_lanes=" & Readout.Hex (Unsigned_32 (Caps (3))));
            end;
            Log ("intel-gpu: EDID port " & Readout.Port_Name (Lit.Port) & " over AUX (I2C-over-AUX)");
         else
            Log ("intel-gpu: EDID port " & Readout.Port_Name (Lit.Port) & " GMBUS pin " &
                 Image (Natural (DDC.Pin_Code (Pin_Of (Lit.Port)))) & " (i915 default DDC pin; no VBT)");
         end if;
         Read_Block (Over_AUX, 0, EDID_Base, Good);
         if not Good then
            Log ("intel-gpu: EDID unavailable; the firmware mode stays");
            return;
         end if;
         Log_Block (0, EDID_Base);
         for E in 1 .. C.Extensions_Declared (EDID_Base) loop
            Read_Block (Over_AUX, E, EDID_Extensions (E), Good);
            exit when not Good;
            Log_Block (E, EDID_Extensions (E));
            Extensions_Read := E;
         end loop;
      end;
      Monitor_Known := True;
      Monitor := C.Identify (EDID_Base);
      Monitor_Size := C.Size_Of (EDID_Base);
      Sink := C.Limits (EDID_Base, EDID_Extensions (1 .. Extensions_Read));
      Modes := new C.Mode_List (C.Capacity (Extensions_Read));
      C.Collect (EDID_Base, EDID_Extensions (1 .. Extensions_Read), Modes.all);
      Log ("intel-gpu: monitor name=" & Monitor.Name.Text (1 .. Monitor.Name.Length) & " vendor=" &
           Monitor.Vendor & " product=" & Readout.Hex (Unsigned_32 (Monitor.Product)) &
           " serial=" & Readout.Hex (Monitor.Serial) & " year=" & Image (Monitor.Year) &
           " size=" & Image (Monitor_Size.Width_MM) & "x" & Image (Monitor_Size.Height_MM) & "mm" &
           " extensions=" & Image (Extensions_Read) & "/" & Image (C.Extensions_Declared (EDID_Base)));
      Log ("intel-gpu: monitor sink hdmi=" & Boolean'Image (Sink.HDMI) & " max_tmds_khz=" &
           Image (Sink.Max_TMDS) & " scdc=" & Boolean'Image (Sink.SCDC) & " max_pixel_khz=" &
           Image (Sink.Max_Pixel_Clock));
      Facts := (Port => Lit.Port,
                Link => (case Lit.Mode is
                           when DD.DVI => MA.DVI_Link,
                           when DD.DP_SST | DD.DP_MST => MA.DisplayPort_Link,
                           when others => MA.HDMI_Link),
                Sink => Sink,
                CDCLK => Natural'Min (Display_State.CDCLK_kHz, MA.CD_Clock_kHz'Last));
      Verdicts := new MA.Verdict_Array'(MA.Judge_All (Modes.all, Facts));
      for I in 1 .. Modes.Count loop
         Log ("intel-gpu: mode " & Image (I) & " " & Mode_Text (Modes.Items (I).Timing) & " " &
              Source_Name (Modes.Items (I).Source) &
              (if Modes.Items (I).Code /= 0 then " vic " & Image (Modes.Items (I).Code) else "") &
              (if Modes.Items (I).Preferred then " preferred" else "") &
              (if Modes.Items (I).Native then " native" else "") &
              " -> " & MA.Name (Verdicts (I)));
      end loop;
      Best_Mode := MA.Best (Modes.all, Verdicts.all);
      if Best_Mode = 0 then
         Log ("intel-gpu: no admitted mode; the firmware mode stays");
      else
         declare
            M : CuBit.Monitor_EDID.Timing renames Modes.Items (Best_Mode).Timing;
            DPI : constant Natural :=
              (if Monitor_Size.Width_MM = 0 then 0
               else (M.Width * 254 + Monitor_Size.Width_MM * 5) / (Monitor_Size.Width_MM * 10));
         begin
            Log ("intel-gpu: best mode " & Mode_Text (M) & " firmware=" &
                 Image (Selection.Shape.Width) & "x" & Image (Selection.Shape.Height) &
                 (if DPI /= 0 then " dpi=" & Image (DPI) else "") &
                 (if M.Width = Selection.Shape.Width and then M.Height = Selection.Shape.Height
                  then " (the firmware size)" else " (a mode change can switch to it)"));
         end;
      end if;
   end Identify;

   procedure Select_Output
     (Planes : FO.Primaries; Table_Bytes, Register_Base : Unsigned_64)
   is
      --  The firmware framebuffer (kernel system facts publication).
      Firmware : constant CuBit.System_Facts.SP.Framebuffer_Geometry :=
        CuBit.System_Facts.Framebuffer;
      Bits : constant Unsigned_64 := Firmware.Bits_Per_Pixel;
   begin
      Active_Pipes := 0;
      for P in Topology.Pipe loop
         if Planes (P).Collected and then
           (Planes (P).Before.Control and Plane_Enable_Bit) /= 0
         then
            Active_Pipes := Active_Pipes + 1;
         end if;
      end loop;
      Selection := FO.Select_Output
        (Planes, Table_Bytes, Firmware.Width, Firmware.Height, Firmware.Pitch);
      if Selection.State = FO.Selected and then Bits /= Framebuffer_Bits then
         Selection := (State => FO.Invalid_Framebuffer);
      end if;
      if Selection.State /= FO.Selected then
         Log ("intel-gpu: firmware output " & FO.Name (Selection.State) &
              "; display keeps the firmware framebuffer without planes");
         return;
      end if;
      Page := Topology.Pipe'Pos (Selection.Pipe);
      Log ("intel-gpu: firmware output pipe " & FO.Pipe_Name (Selection.Pipe) &
           Positive'Image (Selection.Shape.Width) & " x" &
           Positive'Image (Selection.Shape.Height) & " pitch" &
           Natural'Image (Selection.Shape.Pitch) & " surface=" &
           Hex (Unsigned_64 (Selection.Surface)) & " ctl=" &
           Hex (Unsigned_64 (Selection.Control)));
      Log ("intel-gpu: pipe " & FO.Pipe_Name (Selection.Pipe) & " register page " &
           Intel_GPU_Pipe_Mapping.Prepare (Display_Owner, Register_Base, Page));
      Identify (Register_Base);
   end Select_Output;

   function Output_Selected return Boolean is (Selection.State = FO.Selected);

   procedure Log_DDB (Label : String; Item : Cursor.DDB_Snapshot) is
      function Words (W : DDB.Watermarks) return String is
        (Hex (Unsigned_64 (W (DDB.Level_0))) & " " & Hex (Unsigned_64 (W (DDB.Level_1))) & " " &
         Hex (Unsigned_64 (W (DDB.Level_2))) & " " & Hex (Unsigned_64 (W (DDB.Level_3))) & " " &
         Hex (Unsigned_64 (W (DDB.Level_4))) & " " & Hex (Unsigned_64 (W (DDB.Level_5))) &
         " sagv=" & Hex (Unsigned_64 (W (DDB.SAGV))) &
         " sagv-trans=" & Hex (Unsigned_64 (W (DDB.SAGV_Transition))) &
         " trans=" & Hex (Unsigned_64 (W (DDB.Transition))));
   begin
      Log ("intel-gpu: DDB " & Label & " plane1 buf=" & Hex (Unsigned_64 (Item.Plane_Buffer)) &
           " planes2-5=" & Hex (Unsigned_64 (Item.Other_Buffers (2))) & " " &
           Hex (Unsigned_64 (Item.Other_Buffers (3))) & " " &
           Hex (Unsigned_64 (Item.Other_Buffers (4))) & " " &
           Hex (Unsigned_64 (Item.Other_Buffers (5))) &
           " cursor buf=" & Hex (Unsigned_64 (Item.Cursor_Buffer)) &
           " surf=" & Hex (Unsigned_64 (Item.Plane_Surface)));
      Log ("intel-gpu: DDB " & Label & " plane1 wm=" & Words (Item.Plane_WM));
      Log ("intel-gpu: DDB " & Label & " cursor wm=" & Words (Item.Cursor_WM));
   end Log_DDB;

   Cursor_Attempt : Surface_Publication.Attempt;
   Cursor_Staging : Intel_GPU_Buffer_Reply.Backing;
   Cursor_Active_Logged : Boolean := False;

   procedure Setup_Cursor
     (Ledger : in out Intel_GPU_GGTT_Reservations.Ledger;
      Backing : Intel_GPU_Buffer_Reply.Backing)
   is
      Surfaces : Intel_GPU_Buffer_Reply.Backing;
      GPU : Unsigned_64;
      Status : Surface_Publication.Result;
      Readiness : CS.Readiness;
   begin
      if not Output_Selected or else not Page_Ready then
         Log ("intel-gpu: hardware cursor unavailable (no firmware output page)");
         return;
      end if;
      if not Backing.Ready or else
        Backing.Bytes /= Staging_Bytes + Surfaces_Bytes + Guard_Bytes
      then
         Log ("intel-gpu: hardware cursor unavailable (backing)");
         return;
      end if;
      Surfaces := Intel_GPU_Buffer_Reply.Slice
        (Backing, Staging_Bytes, Surfaces_Bytes + Guard_Bytes);
      Publish (Cursor_Attempt, Ledger, Surfaces, GPU, Status);
      Log ("intel-gpu: cursor surfaces " & Describe (Status) & " gpu=" & Hex (GPU));
      if Status /= Surface_Publication.Published or else GPU > Unsigned_64 (Unsigned_32'Last) then
         return;
      end if;
      Cursor.Configure
        (Staging => Backing.CPU_Address,
         Surfaces_CPU => Backing.CPU_Address + Staging_Bytes,
         Surfaces_GGTT => Unsigned_32 (GPU),
         Pipe_Width => Selection.Shape.Width, Pipe_Height => Selection.Shape.Height,
         Single_Pipe => Active_Pipes <= 1, Result => Readiness);
      Log ("intel-gpu: DBUF S0..S3=" & Hex (Unsigned_64 (Read_MMIO (DBUF_Control (0)))) & " " &
           Hex (Unsigned_64 (Read_MMIO (DBUF_Control (1)))) & " " &
           Hex (Unsigned_64 (Read_MMIO (DBUF_Control (2)))) & " " &
           Hex (Unsigned_64 (Read_MMIO (DBUF_Control (3)))) &
           " active-pipes=" & Natural'Image (Active_Pipes));
      Log_DDB ("before", Cursor.Before);
      Log ("intel-gpu: cursor DDB plan " & DDB.Name (Cursor.Plan_State) &
           " plane-1 levels disabled=" & Natural'Image (Cursor.Plane_Levels_Disabled) &
           " -> " & CS.Readiness_Name (Readiness) &
           (if Readiness = CS.Ready_Driver_Allocation
            then " (cursor watermarks derived from plane-1 lines, method 2)"
            elsif Readiness = CS.Ready_Fallback_Watermark
            then " (fallback level-0 watermark)" else ""));
      if Readiness /= CS.Ready_Firmware_Watermark then
         Log_DDB ("after", Cursor.After);
      end if;
      if Cursor.Ready then
         Cursor_Staging := Intel_GPU_Buffer_Reply.Slice (Backing, 0, Staging_Bytes);
         Log ("intel-gpu: hardware cursor ready on pipe " & FO.Pipe_Name (Selection.Pipe) &
              " (scanout plane; relative pointers allowed)");
      else
         Log ("intel-gpu: hardware cursor unavailable (" & CS.Readiness_Name (Readiness) &
              "); pointer stays composited");
      end if;
   end Setup_Cursor;

   --  The largest frame the buffers must hold: the firmware mode's, or the
   --  best admitted mode's when it is larger (a mode change reuses them).
   function Frame_Bytes_Needed return Unsigned_64 is
      Firmware : constant Unsigned_64 :=
        Unsigned_64 (Selection.Shape.Pitch) * Unsigned_64 (Selection.Shape.Height);
   begin
      if Best_Mode = 0 then
         return Firmware;
      end if;
      return Unsigned_64'Max
        (Firmware, Unsigned_64 (Pitch_For (Modes.Items (Best_Mode).Timing.Width)) *
                   Unsigned_64 (Modes.Items (Best_Mode).Timing.Height));
   end Frame_Bytes_Needed;
   function Scanout_Pages return Natural is
     (if not Output_Selected then 0
      elsif (Frame_Bytes_Needed + Page_Bytes - 1) / Page_Bytes + 1 > Scanout_Arena_Pages then 0
      else Natural ((Frame_Bytes_Needed + Page_Bytes - 1) / Page_Bytes + 1));

   ---------------------------------------------------------------------------
   --  The output catalog, mode list and mode changes for display.svc
   --  (CuBit.Output_Discovery / CuBit.Output_Modes on the GPU endpoint).
   ---------------------------------------------------------------------------
   OP_Catalog : constant Unsigned_32 := OD.Code (OD.GPU_Backend, OD.Get_Catalog);
   OP_Description : constant Unsigned_32 := OD.Code (OD.GPU_Backend, OD.Get_Description);
   OP_Watch : constant Unsigned_32 := OM.Code (OD.GPU_Backend, OM.Watch);
   OP_Set_Mode : constant Unsigned_32 := OM.Code (OD.GPU_Backend, OM.Set_Mode);
   OP_List_Mode : constant Unsigned_32 := OM.Code (OD.GPU_Backend, OM.List_Mode);
   function Mode_Label (Label : Unsigned_32) return Boolean is
     (Label in OP_Catalog | OP_Description | OP_Watch | OP_Set_Mode | OP_List_Mode);

   --  Bytes each scanout buffer holds (frame plus guard), fixed at boot.
   Scanout_Buffer_Bytes : Unsigned_64 := 0;

   function Catalog_Now return OD.Catalog is
      Result : OD.Catalog;
      Advertised_W : OD.Extent := OD.Extent (Flip.Width);
      Advertised_H : OD.Extent := OD.Extent (Flip.Height);
   begin
      if not Output_Selected then
         return Result;
      end if;
      if Best_Mode /= 0 then
         Advertised_W := OD.Extent (Modes.Items (Best_Mode).Timing.Width);
         Advertised_H := OD.Extent (Modes.Items (Best_Mode).Timing.Height);
      end if;
      Result.Count := 1;
      Result.Items (1) :=
        (if Flip.Ready
         then (OD.Backend_Ready, OD.Intel_Display, 0, Advertised_W, Advertised_H,
               OD.Millimetres (Monitor_Size.Width_MM), OD.Millimetres (Monitor_Size.Height_MM),
               OD.Extent (Flip.Width), OD.Extent (Flip.Height))
         else (OD.Detected_Only, OD.Intel_Display, 0, Advertised_W, Advertised_H,
               OD.Millimetres (Monitor_Size.Width_MM), OD.Millimetres (Monitor_Size.Height_MM)));
      return Result;
   end Catalog_Now;

   --  The admitted mode for a size: the preferred one, else the highest
   --  pixel clock of that size. Zero when none.
   function Admitted_Mode (Width, Height : Natural) return Natural is
      Chosen : Natural := 0;
   begin
      if Modes = null or else Verdicts = null then
         return 0;
      end if;
      for I in 1 .. Modes.Count loop
         if Verdicts (I) = MA.Admitted and then Modes.Items (I).Timing.Width = Width and then
           Modes.Items (I).Timing.Height = Height
         then
            if Modes.Items (I).Preferred then
               return I;
            elsif Chosen = 0 or else Modes.Items (I).Timing.Clock > Modes.Items (Chosen).Timing.Clock then
               Chosen := I;
            end if;
         end if;
      end loop;
      return Chosen;
   end Admitted_Mode;

   --  Writes a mode change may make: this output's registers only.
   function Modeset_Target (Offset : Unsigned_32) return Unsigned_64 is
      X : constant R.Transcoder := Transcoder_Of (Selection.Pipe);
      Pipe_First : constant Unsigned_32 := 16#7_0000# + R.Pipe_Stride * R.Transcoder'Pos (X);
      DE_Pipe_IIR : constant Unsigned_32 := 16#4_4408# + 16#10# * R.Transcoder'Pos (X);
      DE_Page : constant Unsigned_32 := 16#4_4000#;
      Clock_Page : constant Unsigned_32 := 16#4_6000#;
      DE_Page_Slot : constant := 2;
      Clock_Page_Slot : constant := 1;
   begin
      if not Display_Owner or else not Lit.Found or else Offset mod 4 /= 0 then
         return 0;
      elsif Offset >= Pipe_First and then Offset < Pipe_First + Page_Bytes then
         return (if Page_Ready then PP.Address (Page) + Unsigned_64 (Offset - Pipe_First) else 0);
      elsif Offset in R.DPLL_CFGCR0 (Lit.PLL) | R.DPLL_CFGCR1 (Lit.PLL) | R.ICL_DPCLKA_CFGCR0 then
         return Modeset_Address (MPages.PLL_Configuration, Offset);
      elsif Unsigned_64 (Offset) >= MPages.Offset (Modeset_Page_Of (X)) and then
        Unsigned_64 (Offset) < MPages.Offset (Modeset_Page_Of (X)) + Page_Bytes
      then
         return Modeset_Address (Modeset_Page_Of (X), Offset);
      elsif Offset = R.DPLL_Enable (Lit.PLL) or else
        Offset = R.TRANS_CLK_SEL_A + R.Clock_Select_Stride * R.Transcoder'Pos (X)
      then
         return (if Intel_GPU_Display_Mapping.Ready
                 then Intel_GPU_Display_Mapping.Virtual_Base + Clock_Page_Slot * Page_Bytes +
                      Unsigned_64 (Offset - Clock_Page)
                 else 0);
      elsif Offset = DE_Pipe_IIR then
         return (if Intel_GPU_Display_Mapping.Ready
                 then Intel_GPU_Display_Mapping.Virtual_Base + DE_Page_Slot * Page_Bytes +
                      Unsigned_64 (Offset - DE_Page)
                 else 0);
      elsif Offset = R.DDI_Buffer_Control (Lit.Port) and then R.Is_Combo (Lit.Port) then
         --  PHY page 0 is 0x64000 (Intel_GPU_PHY_Pages).
         return (if Intel_GPU_PHY_Mapping.Ready
                 then Intel_GPU_PHY_Pages.Virtual_Base + Unsigned_64 (Offset - R.DDI_BUF_CTL_A)
                 else 0);
      end if;
      return 0;
   end Modeset_Target;

   procedure Modeset_Write (Offset, Value : Unsigned_32; Written : out Boolean) is
   begin
      Store (Modeset_Target (Offset), Value, Written);
   end Modeset_Write;
   procedure Modeset_Pause renames Pause_One_Ms;
   procedure Write_TMDS_Config (Value : Unsigned_8; Done : out Boolean) is
      Status : DDC.Status;
   begin
      DDC.Write_Bytes (Pin_Of (Lit.Port), DDC.SCDC, [Unsigned_8 (MP.SCDC_TMDS_Config), Value], Status);
      Done := Status = DDC.OK;
      if not Done then
         Log ("intel-gpu: SCDC write " & DDC.Name (Status));
      end if;
   end Write_TMDS_Config;
   package Modeset is new Intel_GPU_HDMI_Modeset
     (Read_MMIO, Modeset_Write, Monotonic_Us, Modeset_Pause, Write_TMDS_Config, Log);
   use type Modeset.Outcome;

   --  The firmware's configuration (captured at the first change) and what
   --  is live now.
   Firmware_Known : Boolean := False;
   Firmware_Config, Live_Config : MP.Configuration;
   Firmware_Width, Firmware_Height, Firmware_Pitch : Positive := 1;

   procedure Change_Mode (Width, Height : Natural; Status : out CuBit.Display_Protocol.DP.Status_Code) is
      package DP renames CuBit.Display_Protocol.DP;
      Output : MP.Output;
      Index : Natural;
   begin
      Status := DP.Bad_State;
      if Width = Flip.Width and then Height = Flip.Height then
         Status := DP.Success;
         return;
      end if;
      if not Lit.Found or else not Flip.Ready or else Flip.Owed or else
        not R.Is_Combo (Lit.Port) or else Lit.Mode not in DD.HDMI | DD.DVI or else
        Display_State.Crystal = DD.Unknown
      then
         Log ("intel-gpu: mode change refused (no flip-ready combo HDMI output, or a flip is owed)");
         return;
      end if;
      Output := (Transcoder_Of (Selection.Pipe), Lit.Port, Lit.PLL);
      if not Firmware_Known then
         Firmware_Config := Modeset.Capture (Output);
         Live_Config := Firmware_Config;
         Firmware_Width := Flip.Width;
         Firmware_Height := Flip.Height;
         Firmware_Pitch := Flip.Pitch;
         Firmware_Known := True;
      end if;
      declare
         Back_To_Firmware : constant Boolean :=
           Width = Firmware_Width and then Height = Firmware_Height;
         Wanted : MP.Configuration;
         Pitch : Positive;
         Result : Modeset.Outcome;
         Failed : Modeset.Step;
         Resized : Boolean;
      begin
         if Back_To_Firmware then
            Wanted := Firmware_Config;
            Pitch := Firmware_Pitch;
         else
            Index := Admitted_Mode (Width, Height);
            if Index = 0 then
               Log ("intel-gpu: mode change refused:" & Natural'Image (Width) & " x" &
                    Natural'Image (Height) & " is not an admitted mode");
               Status := DP.Invalid_Request;
               return;
            end if;
            Pitch := Pitch_For (Width);
            if Unsigned_64 (Pitch) * Unsigned_64 (Height) + Guard_Bytes > Scanout_Buffer_Bytes then
               Log ("intel-gpu: mode change refused: scanout buffers hold" &
                    Unsigned_64'Image (Scanout_Buffer_Bytes) & " bytes");
               Status := DP.Resources_Exhausted;
               return;
            end if;
            declare
               Planned : constant MP.Target_Result :=
                 MP.Target (Firmware_Config, Modes.Items (Index).Timing, Pitch,
                            DD.PLL_Reference (Display_State.Crystal),
                            DD.Fraction_Halved (Display_State.Crystal), Sink.SCDC);
            begin
               if not Planned.Ready then
                  Log ("intel-gpu: mode change refused: no PLL plan for " &
                       Mode_Text (Modes.Items (Index).Timing));
                  return;
               end if;
               Wanted := Planned.Value;
            end;
            --  The plane keeps scanning the live flip buffer.
            Wanted.Plane_Surface := Live_Config.Plane_Surface;
            Log ("intel-gpu: mode change to " & Mode_Text (Modes.Items (Index).Timing));
         end if;
         Modeset.Change (Output, Live_Config, Wanted, Result, Failed);
         case Result is
            when Modeset.Applied =>
               Live_Config := Wanted;
               Flip.Resize (Width, Height, Pitch, Resized);
               if Output_Revision < OD.Catalog_Revision'Last then
                  Output_Revision := Output_Revision + 1;
               end if;
               Status := (if Resized then DP.Success else DP.Bad_State);
            when Modeset.Rolled_Back =>
               Log ("intel-gpu: mode change failed at " & Modeset.Name (Failed) &
                    "; firmware-mode configuration restored");
            when Modeset.Rollback_Failed =>
               Log ("intel-gpu: mode change failed at " & Modeset.Name (Failed) &
                    " and the restore failed too");
         end case;
      end;
   end Change_Mode;

   procedure Handle_Modes (Sender : Process_ID; Request : Message) is
      package DP renames CuBit.Display_Protocol.DP;
      Wire : constant CuBit.Display_Protocol.Wire_Message := CuBit.Desktop_Messages.To_Wire (Request);
      Result : CuBit.Display_Protocol.Wire_Message;
      Delivered : Unsigned_64;
      pragma Unreferenced (Delivered);
   begin
      if Request.tag.label in OP_Catalog | OP_Description then
         Result := OD.Respond (OD.GPU_Backend, Catalog_Now, Output_Revision, Wire);
      elsif Request.tag.label = OP_List_Mode then
         declare
            Wanted : constant OM.Index_Decoding := OM.Decode_List_Request (OD.GPU_Backend, Wire);
         begin
            if Wanted.Valid and then Modes /= null and then Verdicts /= null and then
              Natural (Wanted.Value) <= Modes.Count and then Modes.Count <= Natural (OM.Mode_Count'Last)
            then
               declare
                  M : CuBit.Monitor_EDID.Timing renames Modes.Items (Natural (Wanted.Value)).Timing;
               begin
                  Result := OM.Encode_Listing
                    (OD.GPU_Backend,
                     (Index => Wanted.Value, Count => OM.Mode_Count (Modes.Count),
                      Item => ((OD.Extent (M.Width), OD.Extent (M.Height)),
                               Unsigned_32 (Unsigned_64'Min (CuBit.Monitor_EDID.Refresh_Millihertz (M),
                                                             Unsigned_64 (Unsigned_32'Last))),
                               Unsigned_32 (M.Clock) * 10,
                               To_Admission (Verdicts (Natural (Wanted.Value))),
                               Modes.Items (Natural (Wanted.Value)).Preferred,
                               M.Width = Flip.Width and then M.Height = Flip.Height)));
               end;
            else
               Result := (Label => Request.tag.label, Length => 1,
                          Words => [DP.Status_Code'Enum_Rep (DP.Bad_Object), 0, 0, 0], others => <>);
            end if;
         end;
      elsif Request.tag.label = OP_Set_Mode then
         declare
            Wanted : constant OM.Mode_Decoding := OM.Decode_Set_Mode (OD.GPU_Backend, Wire);
            Status : DP.Status_Code := DP.Invalid_Request;
         begin
            if Wanted.Valid then
               Change_Mode (Natural (Wanted.Value.Width), Natural (Wanted.Value.Height), Status);
            end if;
            Result := OM.Encode_Result
              (OD.GPU_Backend, (Status, (OD.Extent (Flip.Width), OD.Extent (Flip.Height))));
         end;
      else
         --  No watch: this output's mode changes only when display.svc asks.
         Result := (Label => Request.tag.label, Length => 1,
                    Words => [DP.Status_Code'Enum_Rep (DP.Unsupported), 0, 0, 0], others => <>);
      end if;
      Delivered := Intel_GPU_Replies.Respond (Sender, CuBit.Desktop_Messages.From_Wire (Result));
   end Handle_Modes;

   Scanout_Attempts : array (Flip.Index) of Surface_Publication.Attempt;
   Scanout_Memory : Scanout_Backings;

   procedure Setup_Scanout
     (Ledger : in out Intel_GPU_GGTT_Reservations.Ledger;
      Backings : Scanout_Backings)
   is
      Bases : Flip.GGTT_Bases := [others => 0];
      CPU : Flip.CPU_Addresses := [others => 0];
      GPU : Unsigned_64;
      Status : Surface_Publication.Result;
      Pages : constant Natural := Scanout_Pages;
   begin
      if Pages = 0 or else not Page_Ready then
         Log ("intel-gpu: scanout flips unavailable (" &
              (if not Output_Selected then "no firmware output"
               elsif Pages = 0 then "frame larger than one scanout buffer"
               else "no pipe register page") & "); display keeps the firmware framebuffer");
         return;
      end if;
      for I in Flip.Index loop
         if not Backings (Integer (I)).Ready or else
           Backings (Integer (I)).Bytes /= Unsigned_64 (Pages) * Page_Bytes
         then
            Log ("intel-gpu: scanout flips unavailable (buffer" & Flip.Index'Image (I) &
                 " backing); display keeps the firmware framebuffer");
            return;
         end if;
         Publish (Scanout_Attempts (I), Ledger, Backings (Integer (I)), GPU, Status);
         Log ("intel-gpu: scanout buffer" & Flip.Index'Image (I) & " " & Describe (Status) &
              " gpu=" & Hex (GPU));
         if Status /= Surface_Publication.Published or else GPU > Unsigned_64 (Unsigned_32'Last) then
            Log ("intel-gpu: scanout flips unavailable (GGTT publication); display keeps the firmware framebuffer");
            return;
         end if;
         Bases (I) := Unsigned_32 (GPU);
         CPU (I) := Backings (Integer (I)).CPU_Address;
      end loop;
      Scanout_Memory := Backings;
      Scanout_Buffer_Bytes := Unsigned_64 (Pages) * Page_Bytes;
      Flip.Configure (Bases, CPU, Selection.Shape.Width, Selection.Shape.Height,
                      Selection.Shape.Pitch, Selection.Surface);
      Log ("intel-gpu: scanout configure " & Flip.Problem_Text);
      Flip.Take_Over (Boot_Color, Scanout_Status);
      if Scanout_Status = Flip.Taken then
         Log ("intel-gpu: scanout takeover latched on pipe " & FO.Pipe_Name (Selection.Pipe) &
              ": buffer 0 live, firmware framebuffer retired (never written)");
      else
         Log ("intel-gpu: scanout takeover " & Flip.Result_Name (Scanout_Status) &
              "; firmware surface re-armed, display keeps the firmware framebuffer");
      end if;
   end Setup_Scanout;

   procedure Handle_Scanout (Sender : Process_ID; Request : Message) is
      Reply_Message : Message := NULL_MESSAGE;
      Delivered : Unsigned_64;
      pragma Unreferenced (Delivered);
      W : constant Unsigned_64 := Unsigned_64 (Flip.Width);
      H : constant Unsigned_64 := Unsigned_64 (Flip.Height);
      procedure Answer (Code : Unsigned_64) is
      begin
         Reply_Message.tag := (label => Request.tag.label, length => 1,
                               protocol => 0, ipc => <>, reserved => 0);
         Reply_Message.words := [Code, 0, 0, 0];
         Delivered := Intel_GPU_Replies.Respond (Sender, Reply_Message);
      end Answer;
   begin
      if Request.tag.label = OP_GPU_Get_Status then
         Reply_Message.tag := (label => OP_GPU_Get_Status, length => 4,
                               protocol => 0, ipc => <>, reserved => 0);
         Reply_Message.words :=
           [GPU_Status_OK, (if Flip.Ready then 1 else 0), W, H];
         Delivered := Intel_GPU_Replies.Respond (Sender, Reply_Message);
      elsif not Flip.Ready then
         Answer (GPU_Error_Unsupported);
      elsif Request.tag.label = OP_GPU_Map_Framebuffer then
         declare
            Item : constant Unsigned_64 := Request.words (0);
            Reference : CuBit.Memory_Grants.Grant_Reference;
            Granted : Boolean := False;
         begin
            if Item <= Unsigned_64 (Flip.Index'Last) and then Request.tag.length = 4 and then
              Request.words (1 .. 3) = [0, 0, 0]
            then
               CuBit.Memory_Grants.Create_For_Process
                 (grantee => Sender,
                  localAddr => To_Address (Integer_Address (Flip.CPU_Of (Flip.Index (Item)))),
                  numPages => Natural ((Flip.Frame_Bytes + Page_Bytes - 1) / Page_Bytes),
                  readWrite => True, reference => Reference, success => Granted);
            end if;
            if Granted then
               Reply_Message.tag := (label => OP_GPU_Map_Framebuffer, length => 4,
                                     protocol => 0, ipc => <>, reserved => 0);
               Reply_Message.words :=
                 [Reference.slot, Reference.generation, W + H * Half_Word,
                  Unsigned_64 (Flip.Pitch)];
               Delivered := Intel_GPU_Replies.Respond (Sender, Reply_Message);
            else
               Answer (GPU_Error_Bad_State);
            end if;
         end;
      elsif Request.tag.label = OP_GPU_Clear then
         declare
            Done : Boolean := False;
         begin
            if not Flip.Owed and then Request.tag.length = 4 and then
              Request.words (0) <= Unsigned_64 (Unsigned_32'Last)
            then
               Flip.Clear (Unsigned_32 (Request.words (0)), Done);
            end if;
            Answer (if Done then GPU_Status_OK else GPU_Error_Bad_State);
         end;
      elsif Request.tag.label = OP_GPU_Present_Buffer then
         declare
            Item : constant Unsigned_64 := Request.words (0);
            XY : constant Unsigned_64 := Request.words (1);
            WH : constant Unsigned_64 := Request.words (2);
            Y : constant Unsigned_64 := XY / Half_Word;
            Rows : constant Unsigned_64 := WH / Half_Word;
            Result : Flip.Submit_Result;
            Superseded : Boolean;
            Replied : Unsigned_64;
         begin
            if Request.tag.length /= 4 or else Item > Unsigned_64 (Flip.Index'Last) or else
              Request.words (3) /= 0 or else XY mod Half_Word >= W or else Y >= H or else
              WH mod Half_Word not in 1 .. W or else Rows not in 1 .. H
            then
               Answer (GPU_Error_Unsupported);
               return;
            end if;
            Flip.Submit (Flip.Index (Item), Natural (Y), Natural (Unsigned_64'Min (Rows, H - Y)),
                         Result, Superseded);
            if Result /= Flip.Armed then
               --  display.svc reads 3 as "not shown, buffer free": it drops
               --  the frame and redraws (never a quarantine).
               Log ("intel-gpu: present refused result=" & Flip.Submit_Result'Image (Result) &
                    " buffer=" & Unsigned_64'Image (Item));
               Answer (GPU_Error_Bad_State);
               return;
            end if;
            if Superseded and then Present_Reply_Owed then
               --  Mailbox: the replaced frame is answered now, never lost.
               Reply_Message.tag := (label => OP_GPU_Present_Buffer, length => 1,
                                     protocol => 0, ipc => <>, reserved => 0);
               Reply_Message.words := [GPU_Status_Superseded, 0, 0, 0];
               Replied := Intel_GPU_Replies.Answer (Present_Reply, Reply_Message);
               Present_Reply_Owed := False;
            end if;
            --  Answered when the flip has latched (Service).
            Present_Reply_Owed := Intel_GPU_Replies.Keep (Present_Reply);
            if not Present_Reply_Owed then
               --  Unpaced: the reply precedes the latch. Logged on the first
               --  and every 1024th, never silent.
               if Unpaced_Presents mod 1024 = 0 then
                  Log ("intel-gpu: present answered before latch (reply not saved)" &
                       " count=" & Natural'Image (Unpaced_Presents + 1));
               end if;
               if Unpaced_Presents < Natural'Last then Unpaced_Presents := Unpaced_Presents + 1; end if;
               Answer (GPU_Status_OK);
            end if;
         end;
      else
         Answer (GPU_Error_Unsupported);
      end if;
   end Handle_Scanout;

   --  Every Present is answered within Present_Deadline_Us: at latch, or
   --  as a failure with a logged reason (flips faulted, or no latch seen).
   Stall_Report_Us : constant := 100_000;
   Present_Deadline_Us : constant := 1_000_000;
   Stall_Log_Interval_Us : constant := 1_000_000;
   GPU_Status_Stalled : constant Unsigned_64 := 7;
   Last_Stall_Log_Us : Unsigned_64 := 0;

   procedure Answer_Present (Code : Unsigned_64) is
      Reply_Message : Message := NULL_MESSAGE;
      Replied : Unsigned_64;
      pragma Unreferenced (Replied);
   begin
      Reply_Message.tag := (label => OP_GPU_Present_Buffer, length => 1,
                            protocol => 0, ipc => <>, reserved => 0);
      Reply_Message.words := [Code, 0, 0, 0];
      Replied := Intel_GPU_Replies.Answer (Present_Reply, Reply_Message);
      Present_Reply_Owed := False;
   end Answer_Present;

   procedure Log_Stall (Reason : String; View : Flip.Stall_View) is
   begin
      Log ("intel-gpu: flips stalled reason=" & Reason &
           " owed=" & Boolean'Image (View.Owed) &
           " armed=" & Flip.Index'Image (View.Armed) &
           " owed_ms=" & Unsigned_64'Image (View.Owed_Us / 1_000) &
           " surf=" & Hex (Unsigned_64 (View.Surface_Word)) &
           " surflive=" & Hex (Unsigned_64 (View.Live_Word)) &
           " frame=" & Unsigned_32'Image (View.Frame_Count) &
           " reply_owed=" & Boolean'Image (Present_Reply_Owed));
   end Log_Stall;

   procedure Service is
      Latched : Boolean;
      Now : constant Unsigned_64 := Monotonic_Us;
   begin
      Flip.Service (Latched);
      if Latched and then Present_Reply_Owed then
         Answer_Present (GPU_Status_OK);
      end if;
      if not Present_Reply_Owed or else Now = Unsigned_64'Last then
         return;
      end if;
      declare
         View : constant Flip.Stall_View := Flip.Stall;
      begin
         if not Flip.Ready then
            --  Faulted (an impossible PLANE_SURFLIVE): flips have stopped.
            Log_Stall ("flips-faulted", View);
            Answer_Present (GPU_Error_Bad_State);
         elsif View.Owed and then View.Owed_Us >= Present_Deadline_Us then
            Log_Stall ("no-latch-within-deadline", View);
            Answer_Present (GPU_Status_Stalled);
         elsif View.Owed and then View.Owed_Us >= Stall_Report_Us and then
           Now - Last_Stall_Log_Us >= Stall_Log_Interval_Us
         then
            Last_Stall_Log_Us := Now;
            Log_Stall ("latch-late", View);
         end if;
      end;
   end Service;

   function Wake_Ms (Now_Ms : Unsigned_64) return Unsigned_64 is
      Poll : constant Unsigned_64 := Flip.Next_Poll_Us;
      Now : constant Unsigned_64 := Monotonic_Us;
   begin
      if Poll = Unsigned_64'Last or else Now = Unsigned_64'Last or else
        Now_Ms > Unsigned_64'Last - 1_000
      then
         return Unsigned_64'Last;
      end if;
      return Now_Ms + (if Poll <= Now then 0 else (Poll - Now + 999) / 1_000);
   end Wake_Ms;

   function Owns_Label (Label : Unsigned_32) return Boolean is
     (Label in Scanout_First .. Scanout_Last or else Mode_Label (Label) or else
      Label in GP.Code (GP.Query) .. GP.Code (GP.Describe_Scanout));

   procedure Handle (Sender : Process_ID; Request : Message) is
      Wire : constant CuBit.Display_Protocol.Wire_Message :=
        CuBit.Desktop_Messages.To_Wire (Request);
      Result : CuBit.Display_Protocol.Wire_Message;
      Delivered : Unsigned_64;
      pragma Unreferenced (Delivered);
      Display_Process : constant Process_ID := Registered_Driver (DRIVER_DISPLAY);
      function Status (Kind : GP.Operation; Item : GP.Status)
        return CuBit.Display_Protocol.Wire_Message renames GP.Encode_Status;
      Outcome : Cursor.Status;
      use type Cursor.Status;
      function Plane_Status (Kind : GP.Operation) return CuBit.Display_Protocol.Wire_Message is
        (Status (Kind, (if Outcome = Cursor.Accepted then GP.Accepted else GP.Bad_State)));
   begin
      --  The display executor serves display.svc alone, for output zero.
      if Display_Process = No_Process or else Sender /= Display_Process or else
        Request.tag.reserved /= 0
      then
         if Request.tag.label in Scanout_First .. Scanout_Last then
            Result := (Label => Request.tag.label, Length => 1,
                       Words => [GPU_Error_Unsupported, 0, 0, 0], others => <>);
         else
            Result := Status (GP.Query, GP.Unsupported);
            Result.Label := Request.tag.label;
         end if;
         Delivered := Intel_GPU_Replies.Respond (Sender, CuBit.Desktop_Messages.From_Wire (Result));
         return;
      end if;
      if Mode_Label (Request.tag.label) then
         Handle_Modes (Sender, Request);
         return;
      end if;
      if Request.tag.label in Scanout_First .. Scanout_Last then
         Handle_Scanout (Sender, Request);
         return;
      end if;
      if Wire = GP.Describe_Request then
         Result := (if Output_Selected and then (Cursor.Ready or else Flip.Ready)
                    then GP.Encode (GP.Scanout'(GP.Firmware_Framebuffer,
                                    CuBit.Display_Protocol.DP.Positive_Extent (Selection.Shape.Width),
                                    CuBit.Display_Protocol.DP.Positive_Extent (Selection.Shape.Height),
                                    Selection.Shape.Pitch))
                    else Status (GP.Describe_Scanout, GP.Unsupported));
      elsif Request.tag.label = GP.Code (GP.Query) then
         declare
            D : constant GP.Plane_Decoding := GP.Decode_Plane (GP.Query, Wire);
         begin
            if D.Valid and then D.Value = Cursor_Plane_Number and then Cursor.Ready then
               Result := GP.Encode (GP.Description'
                 (Count => Cursor_Planes,
                  Descriptor =>
                    (Kinds => [DPL.Cursor => True, others => False],
                     Formats => [DPL.ARGB8888 => True, others => False],
                     Max_Width => CS.Edge, Max_Height => CS.Edge,
                     Scaler => False, Z => Cursor_Z,
                     --  Scanned out by the display engine: relative pointers too.
                     Host_Pointer => False)));
            else
               Result := Status (GP.Query, GP.Unsupported);
            end if;
         end;
      elsif Request.tag.label = GP.Code (GP.Map_Plane) then
         declare
            D : constant GP.Plane_Decoding := GP.Decode_Plane (GP.Map_Plane, Wire);
            Reference : CuBit.Memory_Grants.Grant_Reference;
            Granted : Boolean := False;
         begin
            if D.Valid and then D.Value = Cursor_Plane_Number and then Cursor.Ready and then
              Cursor_Staging.Ready
            then
               CuBit.Memory_Grants.Create_For_Process
                 (grantee => Sender,
                  localAddr => To_Address (Integer_Address (Cursor_Staging.CPU_Address)),
                  numPages => Staging_Bytes / Page_Bytes, readWrite => True,
                  reference => Reference, success => Granted);
            end if;
            Result := (if Granted then GP.Encode (GP.Buffer'(Reference, CS.Edge, CS.Edge))
                       else Status (GP.Map_Plane, GP.Bad_State));
         end;
      elsif Request.tag.label = GP.Code (GP.Show_Plane) then
         declare
            D : constant GP.Show_Decoding := GP.Decode_Show (Wire);
         begin
            Outcome := Cursor.Bad_State;
            if D.Valid and then D.Value.Plane = Cursor_Plane_Number then
               Cursor.Show (Integer (D.Value.At_Position.X), Integer (D.Value.At_Position.Y),
                            Outcome);
               if Outcome = Cursor.Accepted and then not Cursor_Active_Logged then
                  Cursor_Active_Logged := True;
                  Log ("intel-gpu: hardware cursor active pipe " & FO.Pipe_Name (Selection.Pipe) &
                       " " & Positive'Image (CP.Pixels (Cursor.Image.Width)) & " x" &
                       Positive'Image (Cursor.Image.Height) & " at" &
                       Integer'Image (Integer (D.Value.At_Position.X)) & "," &
                       Integer'Image (Integer (D.Value.At_Position.Y)) &
                       " enabled=" & Boolean'Image (Cursor.Enabled));
               end if;
            end if;
            Result := Plane_Status (GP.Show_Plane);
         end;
      elsif Request.tag.label = GP.Code (GP.Move_Plane) then
         declare
            D : constant GP.Move_Decoding := GP.Decode_Move (Wire);
         begin
            Outcome := Cursor.Bad_State;
            if D.Valid and then D.Value.Plane = Cursor_Plane_Number then
               Cursor.Move_To (Integer (D.Value.At_Position.X), Integer (D.Value.At_Position.Y),
                               Outcome);
            end if;
            Result := Plane_Status (GP.Move_Plane);
         end;
      elsif Request.tag.label = GP.Code (GP.Hide_Plane) then
         declare
            D : constant GP.Plane_Decoding := GP.Decode_Plane (GP.Hide_Plane, Wire);
         begin
            Outcome := Cursor.Bad_State;
            if D.Valid and then D.Value = Cursor_Plane_Number then
               Cursor.Hide (Outcome);
            end if;
            Result := Plane_Status (GP.Hide_Plane);
         end;
      else
         Result := Status (GP.Query, GP.Unsupported);
         Result.Label := Request.tag.label;
      end if;
      Delivered := Intel_GPU_Replies.Respond (Sender, CuBit.Desktop_Messages.From_Wire (Result));
   end Handle;

   Report_Interval_Ms : constant := 1_000;
   Reported_At : Unsigned_64 := 0;
   procedure Report (Now_Ms : Unsigned_64) is
      Item : Cursor.Counters;
   begin
      if Now_Ms = Unsigned_64'Last or else Now_Ms < Reported_At + Report_Interval_Ms then
         return;
      end if;
      Reported_At := Now_Ms;
      if Flip.Ready then
         declare
            F : Flip.Counters;
         begin
            Flip.Take (F);
            if F.Armed + F.Latched + F.Writes_To_Live + F.Faults > 0 then
               Log ("intel-gpu: flips per-second vblanks=" & Unsigned_32'Image (F.Vblanks) &
                    " armed=" & Natural'Image (F.Armed) &
                    " latched=" & Natural'Image (F.Latched) &
                    " superseded=" & Natural'Image (F.Superseded) &
                    " late=" & Natural'Image (F.Late) &
                    " writes_to_live=" & Natural'Image (F.Writes_To_Live) &
                    " faults=" & Natural'Image (F.Faults) &
                    " latch_us_avg=" & Unsigned_64'Image
                      (if F.Latched = 0 then 0 else F.Latency_Total_Us / Unsigned_64 (F.Latched)) &
                    " latch_us_max=" & Unsigned_64'Image (F.Latency_Max_Us) &
                    " period_us=" & Unsigned_64'Image (F.Period_Us));
            end if;
         end;
      end if;
      Cursor.Take (Item);
      if Item.Shows + Item.Moves + Item.Hides + Item.Failures = 0 then
         return;
      end if;
      Log ("intel-gpu: cursor per-second moves=" & Natural'Image (Item.Moves) &
           " move-writes=" & Natural'Image (Item.Move_Writes) &
           " shows=" & Natural'Image (Item.Shows) &
           " hides=" & Natural'Image (Item.Hides) &
           " off-pipe=" & Natural'Image (Item.Off_Pipe) &
           " failures=" & Natural'Image (Item.Failures));
   end Report;
end Intel_GPU_Display_Executor;
