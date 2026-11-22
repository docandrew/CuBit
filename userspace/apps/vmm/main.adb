--  vmm: the per-VM monitor as a desktop app (VMM-001 slice 1). It boots the
--  pinned Linux guest and shows the guest's serial console in a window
--  titled as what it is, and on its guest.console outlet. Keys typed in the
--  focused window go to the guest's serial port as a terminal sends them
--  (serial_keys.ads).
pragma Ada_2022;
with Interfaces; use Interfaces;
with Embedded;
with CuBit.Console_Route;
with CuBit.Handles;
with CuBit.Log_Records;
with CuBit.Handles.Startup;
with CuBit.Monotonic;
with CuBit.Streams;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.App;
with CuBit.UI.Input;
with CuBit.Messages;
with Serial_Keys;
with Vmm_Machine;

procedure Main is
   package App renames CuBit.UI.App;
   use type CuBit.Streams.StreamId;

   Window_Width  : constant := 960;
   Window_Height : constant := 600;
   Margin        : constant := 8;
   --  The desktop protocol carries titles of up to 23 characters; the
   --  banner inside the window names the guest.
   Title : constant String := "Guest serial console";

   Max_Lines : constant := 200;
   type Line_Text is record
      Text : String (1 .. 160) := [others => ' '];
      Length : Natural := 0;
   end record;
   Lines : array (1 .. Max_Lines) of Line_Text;
   Count : Natural := 0;
   Changed : Boolean := True;
   Win : App.Window;
   Outlet : CuBit.Streams.StreamId := CuBit.Streams.NO_STREAM;

   Background : constant Color := 16#12161C#;
   Foreground : constant Color := 16#D8DEE9#;
   Banner     : constant Color := 16#7AC7FF#;

   --  The window's monospace text (Draw_Text's 8x16 cells).
   Cell_Width : constant := 8;
   Row_Height : constant := 16;
   --  Repaints follow the guest's output at most this often (240 Hz); a
   --  key's echo after a quiet spell paints at once.
   Paint_Interval_Ms : constant := 4;
   Painted_Generation : Unsigned_64 := Unsigned_64'Last;
   Painted_At : Unsigned_64 := 0;

   --  The bytes of the key being sent, while the guest's receive FIFO has
   --  no room for them: no further key is taken from the window until they
   --  are in (the window's ring holds the rest), so no key is dropped.
   Pending : Serial_Keys.Sequence;
   Pending_Next : Positive := 1;
   --  Key-to-echo times reported on the monitor's log, for the first keys.
   Echo_Reports : constant := 128;
   Echoes_Reported : Unsigned_64 := 0;

   procedure Add (Text : String) is
   begin
      if Count = Max_Lines then
         Lines (1 .. Max_Lines - 1) := Lines (2 .. Max_Lines);
         Count := Max_Lines - 1;
      end if;
      Count := Count + 1;
      Lines (Count).Length := Natural'Min (Text'Length, Lines (Count).Text'Length);
      Lines (Count).Text (1 .. Lines (Count).Length) :=
        Text (Text'First .. Text'First + Lines (Count).Length - 1);
      Changed := True;
   end Add;

   --  One Information record in logstore (routed): console text
   --  (debugPrint) is a Debug record, which logstore keeps only while it
   --  mirrors to serial, so on hardware the monitor's and the guest's lines
   --  were lost (image 31 and 32: 0 records). Unrouted: the console.
   procedure Publish (Text : String) is
      Made : CuBit.Log_Records.Decoded;
      Accepted : Boolean;
   begin
      if CuBit.Console_Route.Routed then
         Made := CuBit.Log_Records.Make (Text, CuBit.Log_Records.Information);
         if Made.Success then
            CuBit.Console_Route.Emit (Made.Value, Console => True, Accepted => Accepted);
            return;
         end if;
      end if;
      CuBit.Messages.debugPrint (Text & ASCII.LF);
   end Publish;

   procedure Guest_Line (Text : String) is
   begin
      Add (Text);
      if Outlet /= CuBit.Streams.NO_STREAM then
         CuBit.Streams.streamPrint (Outlet, Text & ASCII.LF);
      end if;
      --  The headless test follows the guest on the serial console too.
      Publish ("vmm guest: " & Text);
   end Guest_Line;

   procedure Monitor_Line (Text : String) is
   begin
      Publish ("vmm: " & Text);
   end Monitor_Line;

   procedure Paint is
      Repair : Rect;
      Ready : Boolean;
      C : Canvas;
      --  Rows below the banner and its gap, less one for the partial line
      --  (the prompt) and two the desktop's frame may cover at the bottom
      --  (seen in the vm-desktop screenshot: the last two rows were hidden).
      Visible : constant Natural := (App.Height (Win) - 2 * Margin) / Row_Height - 5;
      First : constant Natural := (if Count > Visible then Count - Visible + 1 else 1);
      Y : Natural := Margin;
   begin
      App.Begin_Paint (Win, App.Full_Rect (Win), Repair, Ready);
      if not Ready then
         return;
      end if;
      C := App.Canvas (Win);
      Fill_Rect (C, App.Full_Rect (Win), Background);
      Draw_Text (C, Margin, Y, "Linux 6.12 in a CuBit VM: its serial console (COM1); type here to talk to it",
                 Banner, Background);
      Y := Y + 2 * Row_Height;
      for I in First .. Count loop
         Draw_Text (C, Margin, Y, Lines (I).Text (1 .. Lines (I).Length), Foreground, Background);
         Y := Y + Row_Height;
      end loop;
      declare
         Partial : constant String := Vmm_Machine.Partial_Line;
         Column : constant Natural := Vmm_Machine.Cursor_Column;
         X : constant Natural := Margin + Column * Cell_Width;
      begin
         Draw_Text (C, Margin, Y, Partial, Foreground, Background);
         --  The cursor: a block, the character under it in reverse.
         if X + Cell_Width <= App.Width (Win) then
            Fill_Rect (C, (x => X, y => Y, w => Cell_Width, h => Row_Height), Foreground);
            if Column < Partial'Length then
               Draw_Glyph (C, X, Y, Partial (Partial'First + Column), Background, Foreground);
            end if;
         end if;
      end;
      App.Present (Win, App.Full_Rect (Win));
      Changed := False;
      Painted_Generation := Vmm_Machine.Console_Generation;
      Painted_At := CuBit.Monotonic.Milliseconds;
   end Paint;

   --  The pending key's bytes into the guest's FIFO, as far as it has room.
   procedure Send_Pending is
   begin
      while Pending_Next <= Pending.Length and then Vmm_Machine.Input_Room > 0 loop
         Vmm_Machine.Send_Input (Pending.Bytes (Pending_Next));
         Pending_Next := Pending_Next + 1;
      end loop;
   end Send_Pending;

   procedure Pump (Stop : out Boolean) is
      Event : App.Input_Event;
      Found : Boolean;
   begin
      Stop := not App.Is_Open (Win);
      Send_Pending;
      while not Stop and then Pending_Next > Pending.Length and then Vmm_Machine.Input_Room > 0 loop
         App.Poll_Input (Win, Event, Found);
         exit when not Found;
         if Event.kind = CuBit.UI.Input.INPUT_KEY_DOWN then
            Pending := Serial_Keys.From_Key (Event.payload0, CuBit.UI.Input.Key_Modifiers (Event));
         elsif Event.kind = CuBit.UI.Input.INPUT_TEXT then
            Pending := Serial_Keys.From_Text (Event.payload0);
         elsif Event.kind = CuBit.UI.Input.INPUT_CLOSE_REQUEST then
            App.Close (Win);
            Stop := True;
            Pending := Serial_Keys.Nothing;
         else
            Pending := Serial_Keys.Nothing;
         end if;
         Pending_Next := 1;
         Send_Pending;
      end loop;
      if Echoes_Reported < Echo_Reports and then Vmm_Machine.Echo_Count > Echoes_Reported then
         Echoes_Reported := Vmm_Machine.Echo_Count;
         Monitor_Line ("key to echo" & Unsigned_64'Image (Vmm_Machine.Last_Echo_Us) & " us");
      end if;
      if App.Is_Open (Win)
        and then (Changed or else Vmm_Machine.Console_Generation /= Painted_Generation)
        and then CuBit.Monotonic.Milliseconds - Painted_At >= Paint_Interval_Ms
      then
         Paint;
      end if;
   end Pump;

   Result : Vmm_Machine.Boot_Result;
   Opened : App.Open_Result;
   use type App.Open_Result;
begin
   CuBit.Console_Route.Route_Named ("logstore");
   Outlet := CuBit.Streams.Open_Outlet ("guest.console");
   App.Open (Win, Window_Width, Window_Height, 0, Opened, Title);
   if Opened /= App.Opened then
      Monitor_Line ("no window: " & App.Image (Opened));
   end if;
   Paint;
   Vmm_Machine.Boot_Linux
     (Authority    => CuBit.Handles.Startup.VM_Create_Named ("vm-create"),
      Guest        => (Kernel => Embedded.Guest_Kernel'Address,
                       Kernel_Bytes => Embedded.Guest_Kernel_Bytes,
                       Initrd => Embedded.Guest_Initrd'Address,
                       Initrd_Bytes => Embedded.Guest_Initrd_Bytes),
      RAM_MiB      => 128,
      Command_Line => "rdinit=/init",
      Run_Ms       => 600_000,
      Console      => Guest_Line'Unrestricted_Access,
      Log          => Monitor_Line'Unrestricted_Access,
      Result       => Result,
      Pump         => Pump'Unrestricted_Access,
      VCPUs        => 2);
   Monitor_Line ("guest ended: " & Vmm_Machine.Boot_Result'Image (Result));
   if App.Is_Open (Win) then
      Paint;   --  the guest's last line
   end if;
   --  The window stays until it is closed, waiting for its input.
   while App.Is_Open (Win) loop
      declare
         Event : App.Input_Event;
         Found : Boolean;
      begin
         App.Wait_Input (Win, Event, Found);
         exit when not Found;
         if Event.kind = CuBit.UI.Input.INPUT_CLOSE_REQUEST then
            App.Close (Win);
         end if;
      end;
   end loop;
end Main;
