pragma Ada_2022;
with Boot_Font;
with Boot_Panel;
with Boot_QR;
with Boot_QR_Capsule;
with Boot_Output;
with Interfaces; use Interfaces;
with Spinlocks;
with System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;
with Virtmem;

--  Trusted hardware/synchronization boundary. Pure content, glyph addressing
--  and framebuffer arithmetic are checked separately; this is not a proof of
--  concurrent memory, device visibility or firmware truth.
package body Boot_Diagnostics with SPARK_Mode => Off is
   use type System.Address;
   use type Boot_Panel.Phase;
   use type Boot_Panel.Row;
   Lock : Spinlocks.Spinlock;
   Enabled : Boolean := False with Atomic;
   Closed : Boolean := False with Atomic;
   Model : Boot_Panel.State;
   Layout : Boot_Framebuffer.Description;
   Base : System.Address := System.Null_Address;
   --  The admitted mapping and layout, kept after Retire for Heartbeat and
   --  Emergency only.
   Retained_Base : System.Address := System.Null_Address with Atomic;
   Retained_Layout : Boot_Framebuffer.Description;
   --  Heartbeat: toggles every Heartbeat_Ticks timer interrupts (~1 kHz).
   Heartbeat_Ticks : constant := 500;
   Heartbeat_Size : constant := 8;
   Heartbeat_Margin : constant := 4;
   Heartbeat_Lit : constant Unsigned_32 := 16#0080FF20#;
   Heartbeat_Dark : constant Unsigned_32 := 16#00000000#;
   Beats : Natural := 0;
   Lit : Boolean := False;
   --  Emergency banner rows: a title and the message.
   Emergency_Rows : constant := 3;
   Emergency_Background : constant Unsigned_32 := 16#00400010#;
   Scale : Positive range 1 .. 2 := 1;
   QR_Enabled : Boolean := False;
   QR_Scale : Positive range 1 .. 3 := 1;
   Background : constant Unsigned_32 := 16#001B2028#;
   Foreground : constant Unsigned_32 := 16#00E0E6ED#;
   Accent : constant Unsigned_32 := 16#008DCED9#;
   Error_Color : constant Unsigned_32 := 16#00FF9D8F#;

   function Try_Enter return Boolean is
      Acquired : Boolean;
   begin
      Spinlocks.tryEnterCriticalSection (Lock, Acquired);
      return Acquired;
   end Try_Enter;

   procedure Pixel (X, Y : Natural; Color : Unsigned_32) is
   begin
      --  The panel clips on small firmware modes, rather than imposing a new
      --  minimum boot resolution or writing beyond the admitted mapping.
      if X < Layout.Width and then Y < Layout.Height then
         declare
            Value : Unsigned_32 with Import, Volatile,
              Address => Base + Storage_Offset (Boot_Framebuffer.Pixel_Offset (Layout, X, Y));
         begin
            Value := Color;
         end;
      end if;
   end Pixel;

   procedure Text (Value : String; Line : Natural; Color : Unsigned_32) is
      Cell : Natural := 0;
      Left : Natural;
      Top : constant Natural := 16 + Line * (Boot_Font.Height + 3) * Scale;
   begin
      for C of Value loop
         exit when Cell = Boot_Panel.Columns;
         Left := 16 + Cell * (Boot_Font.Width + 1) * Scale;
         exit when Left >= Layout.Width;
         for Y in Boot_Font.Row loop
            for X in 0 .. Boot_Font.Width loop
               for DY in 0 .. Scale - 1 loop
                  for DX in 0 .. Scale - 1 loop
                     Pixel (Left + X * Scale + DX, Top + Y * Scale + DY,
                       (if X < Boot_Font.Width and then Boot_Font.Pixel (C, X, Y)
                        then Color else Background));
                  end loop;
               end loop;
            end loop;
         end loop;
         Cell := Cell + 1;
      end loop;
   end Text;

   --  The panel deliberately keeps ordinary serial output as transient detail.
   --  A small set of trusted bootstrap prefixes additionally advances the
   --  retained startup step.  Thus a later devmgr line cannot hide the last
   --  procmgr/display boundary on a physical machine without a serial cable.
   --  This is diagnostic presentation only; it grants no authority and is
   --  inactive as soon as display.svc retires the boot output.
   function Is_Startup_Milestone (Value : Boot_Panel.Line) return Boolean is
      function Begins (Prefix : String) return Boolean is
      begin
         if Prefix'Length > Value'Length then
            return False;
         end if;
         for I in Prefix'Range loop
            if Value (I - Prefix'First + Value'First) /= Prefix (I) then
               return False;
            end if;
         end loop;
         return True;
      end Begins;
   begin
      return Begins ("procmgr: bootstrap") or else
        Begins ("procmgr: init launch") or else
        Begins ("procmgr: init launched") or else
        Begins ("display: starting") or else
        Begins ("display: boot output initialization failed");
   end Is_Startup_Milestone;

   procedure QR is
      Capsule : constant String := Boot_QR_Capsule.Build
        (Boot_Panel.Content (Model, Boot_Panel.Current_Step),
         Boot_Panel.Content (Model, Boot_Panel.Last_Completed),
         Boot_Panel.Content (Model, Boot_Panel.Latest_Detail),
         Boot_Panel.Content (Model, Boot_Panel.First_Error));
      Code : Boot_QR.Matrix;
      Modules : constant Natural := Boot_QR.Dimension + 2 * Boot_QR.Quiet_Zone;
      Total : constant Natural := Modules * QR_Scale;
      Left : Natural := 0;
      Top : constant Natural := 16;
      Dark : Boolean;
   begin
      if not QR_Enabled then return; end if;
      Left := Layout.Width - Total - 8;
      Boot_QR.Encode (Capsule, Code);
      for Row in 0 .. Modules - 1 loop
         for Column in 0 .. Modules - 1 loop
            Dark := Column >= Boot_QR.Quiet_Zone and then
              Column < Boot_QR.Quiet_Zone + Boot_QR.Dimension and then
              Row >= Boot_QR.Quiet_Zone and then
              Row < Boot_QR.Quiet_Zone + Boot_QR.Dimension and then
              Code (Row - Boot_QR.Quiet_Zone, Column - Boot_QR.Quiet_Zone);
            for DY in 0 .. QR_Scale - 1 loop
               for DX in 0 .. QR_Scale - 1 loop
                  Pixel (Left + Column * QR_Scale + DX,
                         Top + Row * QR_Scale + DY,
                         (if Dark then 16#00000000# else 16#00FFFFFF#));
               end loop;
            end loop;
         end loop;
      end loop;
   end QR;

   procedure Paint (R : Boot_Panel.Row) is
   begin
      Text (Boot_Panel.Content (Model, R), Boot_Panel.Row'Pos (R) * 2 + 1,
            (if R = Boot_Panel.First_Error then Error_Color else Foreground));
      --  The capsule is intentionally rebuilt from the retained bounded model,
      --  never from unbounded serial output. QR rendering is beside the panel.
      QR;
      --  Fence on the WRITING CPU before unlocking, including write-combined
      --  mappings; a retiring CPU cannot drain another CPU's WC buffer.
      System.Machine_Code.Asm ("sfence", Clobber => "memory", Volatile => True);
   end Paint;

   procedure Setup (Item : Boot_Framebuffer.Description) is
   begin
      if Closed or Boot_Output.Is_Retired then return; end if;
      Spinlocks.enterCriticalSection (Lock);
      if not Closed and then Boot_Panel.Lifecycle (Model) = Boot_Panel.Unavailable then
         Layout := Item;
         Base := Virtmem.P2Va (Integer_Address (Item.Base));
         Retained_Layout := Item;
         Retained_Base := Base;
         -- Keep all evidence columns visible at the 1024x768 fallback, too.
         Scale := (if Item.Width >= 32 + Boot_Panel.Columns *
           (Boot_Font.Width + 1) * 2 and Item.Height >=
           16 + (Boot_Panel.Row'Pos (Boot_Panel.Row'Last) * 2 + 3) *
             (Boot_Font.Height + 3) * 2 then 2 else 1);
         QR_Scale := (if Item.Width >= 32 + Boot_Panel.Columns *
           (Boot_Font.Width + 1) * Scale +
           (Boot_QR.Dimension + 2 * Boot_QR.Quiet_Zone) * 3 + 8
           then 3 else 2);
         QR_Enabled := Item.Width >= 32 + Boot_Panel.Columns *
           (Boot_Font.Width + 1) * Scale +
           (Boot_QR.Dimension + 2 * Boot_QR.Quiet_Zone) * QR_Scale + 8
           and then Item.Height >= 16 +
           (Boot_QR.Dimension + 2 * Boot_QR.Quiet_Zone) * QR_Scale;
         Boot_Panel.Initialize (Model);
         --  Clear only the fixed panel once. Subsequent writes touch one text
         --  row, never move old pixels or repaint a full-screen backbuffer.
         for Y in 0 .. Natural'Min (Item.Height,
           16 + (Boot_Panel.Row'Pos (Boot_Panel.Row'Last) * 2 + 3) *
             (Boot_Font.Height + 3) * Scale) - 1 loop
            for X in 0 .. Natural'Min (Item.Width,
              32 + Boot_Panel.Columns * 9 * Scale + 160) - 1 loop
               Pixel (X, Y, Background);
            end loop;
         end loop;
         Text ("BOOT", 0, Accent);
         Text ("CURRENT STEP", 2, Accent);
         Text ("LAST COMPLETED", 4, Accent);
         Text ("LATEST DIAGNOSTIC (best effort)", 6, Accent);
         Text ("FIRST FAILURE", 8, Accent);
         Text ("TIMING EVIDENCE (hex TSC offsets)", 10, Accent);
         Text ("TIMER EVIDENCE (hex registers)", 12, Accent);
         Text ("APIC EVIDENCE (FFFFFFFF = no vector)", 14, Accent);
         Text ("FIRMWARE TIMER TAKEOVER (before > after)", 16, Accent);
         Text ("IRQ TIMING (hex TSC cycles, Ada handler incl. probe)", 18, Accent);
         Text ("IRQ SOURCE (pre-EOI PIC ISR, hex counts)", 20, Accent);
         for R in Boot_Panel.Row loop Paint (R); end loop;
         Enabled := True;
         Boot_Output.Install (Append'Access, Panic'Access, Retire'Access);
      end if;
      Spinlocks.exitCriticalSection (Lock);
   end Setup;

   procedure Begin_Step (Text : String) is
   begin
      if not Enabled or else not Try_Enter then return; end if;
      if not Closed then
         Boot_Panel.Begin_Step (Model, Text);
         Paint (Boot_Panel.Current_Step);
      end if;
      Spinlocks.exitCriticalSection (Lock);
   end Begin_Step;
   procedure Complete_Step (Text : String) is
   begin
      if not Enabled or else not Try_Enter then return; end if;
      if not Closed then
         Boot_Panel.Complete_Step (Model, Text);
         Paint (Boot_Panel.Last_Completed);
      end if;
      Spinlocks.exitCriticalSection (Lock);
   end Complete_Step;
   procedure Set_Evidence (R : Boot_Panel.Evidence_Row; Text : String) is
   begin
      if not Enabled or else not Try_Enter then return; end if;
      if not Closed then
         Boot_Panel.Set_Evidence (Model, R, Text);
         Paint (R);
      end if;
      Spinlocks.exitCriticalSection (Lock);
   end Set_Evidence;
   procedure Append (C : Character) is
      Changed : Boolean;
   begin
      if not Enabled or else not Try_Enter then return; end if;
      if not Closed then
         Boot_Panel.Append (Model, C, Changed);
         if Changed then
            declare
               Latest : constant Boot_Panel.Line :=
                 Boot_Panel.Content (Model, Boot_Panel.Latest_Detail);
            begin
               if Is_Startup_Milestone (Latest) then
                  Boot_Panel.Begin_Step (Model, Latest);
                  Paint (Boot_Panel.Current_Step);
               end if;
            end;
            Paint (Boot_Panel.Latest_Detail);
         end if;
      end if;
      Spinlocks.exitCriticalSection (Lock);
   end Append;
   procedure Panic (Message : System.Address) is
      Value : Boot_Panel.Line := [others => ' '];
   begin
      --  Never wait for another CPU or re-enter a renderer which faulted. A
      --  failed try leaves serial reporting available to the last-chance path.
      if not Enabled or else not Try_Enter then return; end if;
      if not Closed then
         for I in Value'Range loop
            declare
               C : Character with Import, Address => Message + Storage_Offset (I - 1);
            begin
               exit when C = ASCII.NUL;
               Value (I) := C;
            end;
         end loop;
         Boot_Panel.Fail (Model, Value);
         Paint (Boot_Panel.Current_Step);
         Paint (Boot_Panel.First_Error);
      end if;
      Spinlocks.exitCriticalSection (Lock);
   end Panic;
   procedure Retire is
   begin
      --  Close admission before waiting. Writers hold this same IRQ-masking
      --  lock until their final device store, and perform no IPC/allocation.
      --  Their second Closed check handles admission racing this store.
      Closed := True;
      Enabled := False;
      Spinlocks.enterCriticalSection (Lock);
      Enabled := False;
      Boot_Panel.Retire (Model);
      --  Drain WC stores before authorizing another owner to touch hardware.
      --  This is a CPU store fence, not evidence of native GPU DMA completion.
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
      Base := System.Null_Address;
      Spinlocks.exitCriticalSection (Lock);
   end Retire;
   procedure Heartbeat is
      Target : constant System.Address := Retained_Base;
      Color : Unsigned_32;
   begin
      if Target = System.Null_Address or else
        Retained_Layout.Width < Heartbeat_Size + Heartbeat_Margin or else
        Retained_Layout.Height < Heartbeat_Size + Heartbeat_Margin
      then
         return;
      end if;
      Beats := (if Beats = Heartbeat_Ticks - 1 then 0 else Beats + 1);
      if Beats /= 0 then return; end if;
      Lit := not Lit;
      Color := (if Lit then Heartbeat_Lit else Heartbeat_Dark);
      for Y in Retained_Layout.Height - Heartbeat_Size - Heartbeat_Margin ..
               Retained_Layout.Height - Heartbeat_Margin - 1
      loop
         for X in Retained_Layout.Width - Heartbeat_Size - Heartbeat_Margin ..
                  Retained_Layout.Width - Heartbeat_Margin - 1
         loop
            declare
               Value : Unsigned_32 with Import, Volatile,
                 Address => Target + Storage_Offset
                   (Boot_Framebuffer.Pixel_Offset (Retained_Layout, X, Y));
            begin
               Value := Color;
            end;
         end loop;
      end loop;
   end Heartbeat;

   --  Bottom-left panel through the retained mapping; title row plus lines.
   Stuck_Shown : Natural := 0;
   Stuck_Title : constant String := "STUCK CALLS (caller -> server, seconds)  * = watch";
   Stuck_Color : constant Unsigned_32 := 16#00FFD040#;
   Stuck_Background : constant Unsigned_32 := 16#00202020#;

   procedure Stuck_Calls (Lines : Stuck_Lines; Count : Natural) is
      Target : constant System.Address := Retained_Base;
      Row_Height : constant Natural := (Boot_Font.Height + 3) * Scale;
      Cell_Width : constant Natural := (Boot_Font.Width + 1) * Scale;
      Rows : constant Natural := 1 + Stuck_Rows;
      Width : constant Natural := 16 + Stuck_Line'Length * Cell_Width;
      Shown : constant Natural := Natural'Min (Count, Stuck_Rows);
      Top : Natural;

      procedure Put (X, Y : Natural; Color : Unsigned_32) is
      begin
         if X < Retained_Layout.Width and then Y < Retained_Layout.Height then
            declare
               Value : Unsigned_32 with Import, Volatile,
                 Address => Target + Storage_Offset
                   (Boot_Framebuffer.Pixel_Offset (Retained_Layout, X, Y));
            begin
               Value := Color;
            end;
         end if;
      end Put;

      procedure Line (Value : String; Row : Natural; Color : Unsigned_32) is
         Left : Natural := 8;
         Y0 : constant Natural := Top + Row * Row_Height;
      begin
         for C of Value loop
            for Y in Boot_Font.Row loop
               for X in 0 .. Boot_Font.Width loop
                  for DY in 0 .. Scale - 1 loop
                     for DX in 0 .. Scale - 1 loop
                        Put (Left + X * Scale + DX, Y0 + Y * Scale + DY,
                          (if X < Boot_Font.Width and then Boot_Font.Pixel (C, X, Y)
                           then Color else Stuck_Background));
                     end loop;
                  end loop;
               end loop;
            end loop;
            Left := Left + Cell_Width;
         end loop;
         --  The spacing below the glyphs, so rows read as one panel.
         for Y in Y0 + Boot_Font.Height * Scale .. Y0 + Row_Height - 1 loop
            for X in 8 .. Left - 1 loop
               Put (X, Y, Stuck_Background);
            end loop;
         end loop;
      end Line;
   begin
      if Target = System.Null_Address or else
        Retained_Layout.Height < Rows * Row_Height + 32
      then
         return;
      end if;
      if Shown = 0 and then Stuck_Shown = 0 then return; end if;
      Top := Retained_Layout.Height - 24 - Rows * Row_Height;
      if Shown = 0 then
         --  Clear the panel left by the previous report.
         for Y in Top .. Top + Rows * Row_Height - 1 loop
            for X in 0 .. Natural'Min (Width, Retained_Layout.Width) - 1 loop
               Put (X, Y, 16#00000000#);
            end loop;
         end loop;
      else
         Line (Stuck_Title, 0, Error_Color);
         for I in 1 .. Stuck_Rows loop
            Line ((if I <= Shown then Lines (I) else [Stuck_Line'Range => ' ']),
                  I, Stuck_Color);
         end loop;
      end if;
      Stuck_Shown := Shown;
      System.Machine_Code.Asm ("sfence", Clobber => "memory", Volatile => True);
   end Stuck_Calls;

   procedure Emergency (Message : System.Address; Detail : String) is
      Line : String (1 .. Boot_Panel.Columns) := [others => ' '];
      Last : Natural := 0;
   begin
      if Retained_Base = System.Null_Address then return; end if;
      --  Paint through the retained mapping; the stop is final, so the
      --  painter lock and the Closed admission no longer protect anything.
      Base := Retained_Base;
      Layout := Retained_Layout;
      for Y in 0 .. Natural'Min (Layout.Height,
        16 + Emergency_Rows * (Boot_Font.Height + 3) * Scale) - 1
      loop
         for X in 0 .. Layout.Width - 1 loop
            Pixel (X, Y, Emergency_Background);
         end loop;
      end loop;
      Text ("CUBIT KERNEL PANIC", 0, Error_Color);
      if Message /= System.Null_Address then
         for I in Line'Range loop
            declare
               C : Character with Import, Address => Message + Storage_Offset (I - 1);
            begin
               exit when C = ASCII.NUL;
               Line (I) := C;
               Last := I;
            end;
         end loop;
      end if;
      for C of Detail loop
         exit when Last = Line'Last;
         Last := Last + 1;
         Line (Last) := C;
      end loop;
      Text (Line (1 .. Last), 1, Foreground);
      System.Machine_Code.Asm ("sfence", Clobber => "memory", Volatile => True);
   end Emergency;
end Boot_Diagnostics;
