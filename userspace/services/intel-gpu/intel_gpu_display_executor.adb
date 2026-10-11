with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
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

   --  SYSINFO queries for the firmware framebuffer (kernel/src/sysinfo.ads).
   Sysinfo_FB_Width  : constant Unsigned_64 := 1100;
   Sysinfo_FB_Height : constant Unsigned_64 := 1101;
   Sysinfo_FB_Pitch  : constant Unsigned_64 := 1102;
   Sysinfo_FB_Bits   : constant Unsigned_64 := 1103;
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
   Present_Reply_Slot : constant CapabilitySlot := 13;
   Present_Reply_Owed : Boolean := False;
   Scanout_Status : Flip.Takeover_Result := Flip.Not_Configured;
   Scanout_Arena_Pages : constant := 4_096;

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
   Surface_Limit_Bytes : constant := 16 * 1024 * 1024;
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
      Result : constant Unsigned_64 := registerDriver (DRIVER_GPU);
   begin
      Log ("intel-gpu: GPU service registration " &
           (if Result = Unsigned_64'Last then "REFUSED" else "accepted") &
           " (display executor answers display.svc only)");
   end Register_Service;

   procedure Select_Output
     (Planes : FO.Primaries; Table_Bytes, Register_Base : Unsigned_64)
   is
      Bits : constant Unsigned_64 := getInfo (Sysinfo_FB_Bits);
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
        (Planes, Table_Bytes, getInfo (Sysinfo_FB_Width), getInfo (Sysinfo_FB_Height),
         getInfo (Sysinfo_FB_Pitch));
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

   function Scanout_Pages return Natural is
     (if not Output_Selected then 0
      else (if (Unsigned_64 (Selection.Shape.Pitch) * Unsigned_64 (Selection.Shape.Height) +
                  Page_Bytes - 1) / Page_Bytes + 1 > Scanout_Arena_Pages
            then 0
            else Natural ((Unsigned_64 (Selection.Shape.Pitch) * Unsigned_64 (Selection.Shape.Height) +
                  Page_Bytes - 1) / Page_Bytes + 1)));

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
         Reply_Message.tag := (Request.tag.label, 1, 0, 0);
         Reply_Message.words := [Code, 0, 0, 0];
         Delivered := reply (Sender, Reply_Message);
      end Answer;
   begin
      if Request.tag.label = OP_GPU_Get_Status then
         Reply_Message.tag := (OP_GPU_Get_Status, 4, 0, 0);
         Reply_Message.words :=
           [GPU_Status_OK, (if Flip.Ready then 1 else 0), W, H];
         Delivered := reply (Sender, Reply_Message);
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
               Reply_Message.tag := (OP_GPU_Map_Framebuffer, 4, 0, 0);
               Reply_Message.words :=
                 [Reference.slot, Reference.generation, W + H * Half_Word,
                  Unsigned_64 (Flip.Pitch)];
               Delivered := reply (Sender, Reply_Message);
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
               Answer (GPU_Error_Bad_State);
               return;
            end if;
            if Superseded and then Present_Reply_Owed then
               --  Mailbox: the replaced frame is answered now, never lost.
               Reply_Message.tag := (OP_GPU_Present_Buffer, 1, 0, 0);
               Reply_Message.words := [GPU_Status_Superseded, 0, 0, 0];
               Replied := replyCap (Present_Reply_Slot, Reply_Message);
               Present_Reply_Owed := False;
            end if;
            --  Answered when the flip has latched (Service).
            Present_Reply_Owed := saveReplyCap (Unsigned_64 (Present_Reply_Slot)) = 1;
            if not Present_Reply_Owed then
               Answer (GPU_Status_OK);
            end if;
         end;
      else
         Answer (GPU_Error_Unsupported);
      end if;
   end Handle_Scanout;

   procedure Service is
      Latched : Boolean;
      Reply_Message : Message := NULL_MESSAGE;
      Replied : Unsigned_64;
      pragma Unreferenced (Replied);
   begin
      Flip.Service (Latched);
      if Latched and then Present_Reply_Owed then
         Reply_Message.tag := (OP_GPU_Present_Buffer, 1, 0, 0);
         Reply_Message.words := [GPU_Status_OK, 0, 0, 0];
         Replied := replyCap (Present_Reply_Slot, Reply_Message);
         Present_Reply_Owed := False;
      end if;
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
     (Label in Scanout_First .. Scanout_Last or else
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
         Delivered := reply (Sender, CuBit.Desktop_Messages.From_Wire (Result));
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
      Delivered := reply (Sender, CuBit.Desktop_Messages.From_Wire (Result));
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
