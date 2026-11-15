--  hid-launch-check (tests/gpio-launch/hid-driver; headless gpio-launch):
--  the HID start's fixture (SUP-001). devmgr starts it through procmgr's
--  Start_Child for the test's PNP0C50 offer. It logs exactly the typed
--  parameters it decoded (the startup plan's policy plus devmgr's
--  descriptor fact) and which of its "i2c" and "gpio" endpoints it got
--  (with their badges), reports ready to devmgr and exits.
with Ada.Command_Line;
with Interfaces; use Interfaces;
with CuBit.Driver_Readiness;
with CuBit.Handles;
with CuBit.Handles.IPC;
with CuBit.Handles.Startup;
with CuBit.Messages; use CuBit.Messages;
with Parameters;
with Startup;
with CuBit.Kernel_ABI;
with CuBit.Process_IDs;
with Fixture_Probe;
with GPIO_Control_Wire;
with Launch_Configuration;

procedure Main is
   package P renames Parameters;
   Raw : constant P.HID_Parameters_Value := P.Value;

   function Image (N : Long_Long_Integer) return String is
      Text : constant String := Long_Long_Integer'Image (N);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   function Hex (Value : Unsigned_64) return String is
      Digits_Of : constant String := "0123456789ABCDEF";
      Result : String (1 .. 18) := "0x0000000000000000";
      V : Unsigned_64 := Value;
   begin
      for K in reverse 3 .. 18 loop
         Result (K) := Digits_Of (Natural (V and 16#F#) + 1);
         V := Shift_Right (V, 4);
      end loop;
      return Result;
   end Hex;

   --  Native enumeration images are positions: spelled out.
   function Direction (D : P.HID_Direction_Value) return String is
     (case D is when P.As_Reported => "As_Reported", when P.Reversed => "Reversed");
   function Button (B : P.HID_Button_Value) return String is
     (case B is
         when P.Ignore => "Ignore", when P.Primary_Button => "Primary_Button",
         when P.Secondary_Button => "Secondary_Button", when P.Middle_Button => "Middle_Button",
         when P.Side_Button => "Side_Button", when P.Extra_Button => "Extra_Button");
   function Axis (A : P.HID_Axis_Value) return String is
     (Image (Long_Long_Integer (A.numerator)) & "/" & Image (Long_Long_Integer (A.denominator)) & " " &
      Direction (A.direction));

   --  "absent", or "badge 0x..." of the endpoint's authority tag.
   function Endpoint_Text (Item : CuBit.Handles.Startup.Optional_Endpoint) return String is
   begin
      if not Item.Present then
         return "absent";
      end if;
      declare
         D : constant CuBit.Handles.IPC.Description := CuBit.Handles.IPC.Describe (Item.Value);
      begin
         return (if D.Current then "badge " & Hex (D.Authority_Tag) else "not current");
      end;
   end Endpoint_Text;

   Ignore : MessageTag;
begin
   debugPrint
     ("hid-launch-check: parameters descriptor_register=" & Hex (Unsigned_64 (Raw.descriptor_register)) &
      " x=" & Axis (Raw.x) & " y=" & Axis (Raw.y) &
      " confidence=" & (case Raw.confidence is
                          when P.Require_Confident => "Require_Confident",
                          when P.Accept_Unknown => "Accept_Unknown") &
      " buttons=" & Button (Raw.buttons.button_1) & "," & Button (Raw.buttons.button_2) & "," &
      Button (Raw.buttons.button_3) &
      " contact_limit=" & Image (Long_Long_Integer (Raw.contact_limit)) &
      " binding_budget=" & Image (Long_Long_Integer (Raw.binding_budget)) &
      " timing=" & Image (Long_Long_Integer (Raw.timing.startup)) & "/" &
      Image (Long_Long_Integer (Raw.timing.control)) & "/" & Image (Long_Long_Integer (Raw.timing.read)) & "/" &
      Image (Long_Long_Integer (Raw.timing.cleanup)) & "/" &
      Image (Long_Long_Integer (Raw.timing.idle_backoff)) & ASCII.LF);
   debugPrint ("hid-launch-check: i2c " & Endpoint_Text (Startup.I2c) &
               ", gpio " & Endpoint_Text (Startup.Gpio) & ASCII.LF);
   --  Which controller "i2c" reaches (SUP-001): the driver behind it says
   --  where it was launched and the badge the call arrived with.
   if Startup.I2c.Present then
      declare
         Probe : Message := NULL_MESSAGE;
         Tag : MessageTag;
      begin
         Probe.tag := (label => Fixture_Probe.Where_Label, length => Fixture_Probe.Request_Words,
                       protocol => 0, ipc => <>, reserved => 0);
         Tag := capCall (Startup.I2c.Value, Probe, Deadline_After (Fixture_Probe.Probe_Deadline_Ms));
         if Tag.label = CuBit.Kernel_ABI.Reply_OK and then Tag.length = Fixture_Probe.Reply_Words
           and then Probe.words (Fixture_Probe.Location_Word) in Launch_Configuration.Routing_ID
         then
            debugPrint ("hid-launch-check: i2c reaches the driver at " &
                        Launch_Configuration.Location_Image (Probe.words (Fixture_Probe.Location_Word)) &
                        " with badge " & Hex (Probe.words (Fixture_Probe.Badge_Word)) & ASCII.LF);
         else
            debugPrint ("hid-launch-check: i2c probe not answered (label " & Hex (Unsigned_64 (Tag.label)) &
                        ")" & ASCII.LF);
         end if;
      end;
   end if;
   --  The GPIO sink (SUP-001): devmgr had it admitted before this driver
   --  ran, so the GPIO driver's event may already be waiting.
   if Startup.Gpio.Present then
      declare
         Controller : constant CuBit.Handles.IPC.Description := CuBit.Handles.IPC.Describe (Startup.Gpio.Value);
         Deadline : constant Unsigned_64 := Deadline_After (Fixture_Probe.Probe_Deadline_Ms);
         Event : Message;
         Publisher : Process_ID;
         Seen : Boolean := False;
         Waited : Activity_Result;
      begin
         while not Seen and then syscall (SYSCALL_GETTIME) < Deadline loop
            while not Seen and then Poll_Event (Event, Publisher) loop
               Seen := Event.tag.label = GPIO_Control_Wire.OP_GPIO_EVENT
                 and then Controller.Current and then Publisher = CuBit.Process_IDs.From_Word (Controller.Object);
               if Seen then
                  debugPrint ("hid-launch-check: gpio event through the admitted sink from the GPIO driver, badge " &
                              Hex (Event.authorityTag) & ASCII.LF);
               end if;
            end loop;
            if not Seen then
               Waited := Wait_For_Activity_Until (Deadline);
            end if;
         end loop;
         if not Seen then
            debugPrint ("hid-launch-check: no gpio event by the deadline" & ASCII.LF);
         end if;
      end;
   end if;
   Ignore := capSend (Startup.Ready,
     (tag => (label => CuBit.Driver_Readiness.Ready_Label, length => 0, protocol => 0, ipc => <>, reserved => 0),
      authorityTag => 0, words => [others => 0]), Deadline_After (10_000));
   debugPrint ("hid-launch-check: reported ready; exiting" & ASCII.LF);
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
end Main;
