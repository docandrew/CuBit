pragma Ada_2022;
with Ada.Text_IO;
with Interfaces;
with I2C_Channel_Badges;
with DW_I2C_Requests;
with DW_I2C_Request_Types;
with DW_I2C_Owner;
with DW_I2C_Transfer;
with DW_I2C_Registers;
with DW_I2C_Configuration;
with Intel_LPSS_I2C_Reset;
with Intel_LPSS_I2C_Profile;
procedure Request_Tests is
   package T renames DW_I2C_Transfer;
   package R renames DW_I2C_Registers;
   package C renames DW_I2C_Configuration;
   package L renames Intel_LPSS_I2C_Reset;
   package P renames Intel_LPSS_I2C_Profile;
   use type T.Word;
   use type T.Byte;
   use type T.Count;
   use type T.Ticket;
   use type C.Register_ID;
   use type C.Access_Status;
   use type T.Phase;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Context : String) is
   begin
      if not OK then raise Program_Error with Context; end if;
      Checks := Checks + 1;
   end Check;
   Registers : array (Natural range 0 .. 191) of T.Word := [others => 0];
   Raw_IRQ, Abort_Source, RX_Count, Cursor : T.Word := 0;
   IO_Count, Data_Calls, Commands, Fail_Command : Natural := 0;
   Fail_Status : R.Access_Status := R.Access_OK;
   Clock_OK, Lose_Clock_On_Data : Boolean := True;
   Inject_Abort : Boolean := False;
   Unknown_Cleanup, Dirty_TX, Late_Abort : Boolean := False;
   Current_Time : T.Tick := 1;
   Command_Log : array (1 .. 32) of T.Word := [others => 0];
   function Index (Offset : Natural) return Natural is (Offset / 4);
   procedure Reset_Model is
   begin
      Registers := [others => 0]; Registers (Index (16#204#)) := L.Release_Resets;
      Registers (Index (16#FC#)) := R.Component_Signature;
      Registers (Index (16#F4#)) := 16#10100#;
      Registers (Index (16#F8#)) := C.Minimum_Hold_Version;
      Raw_IRQ := 0; Abort_Source := 0; RX_Count := 0; Cursor := 0;
      IO_Count := 0; Data_Calls := 0; Commands := 0; Fail_Command := 0;
      Fail_Status := R.Access_OK; Clock_OK := True; Lose_Clock_On_Data := False;
      Inject_Abort := False; Current_Time := 1; Command_Log := [others => 0];
      Unknown_Cleanup := False; Dirty_TX := False; Late_Abort := False;
   end Reset_Model;
   procedure Clock (Now : out T.Tick; Available : out Boolean) is
   begin Now := Current_Time; Available := Clock_OK; end Clock;
   procedure Reset_Read (ID : L.Register_ID; Value : out L.Word; Status : out L.Access_Status) is
   begin
      IO_Count := IO_Count + 1; Value := Registers (Index (Natural (L.Offset (ID))));
      Status := L.Access_OK;
   end Reset_Read;
   procedure Reset_Write (ID : L.Register_ID; Value : L.Word; Status : out L.Access_Status) is
   begin
      IO_Count := IO_Count + 1; Registers (Index (Natural (L.Offset (ID)))) := Value;
      Status := L.Access_OK;
   end Reset_Write;
   procedure Config_Read (ID : C.Register_ID; Value : out C.Word; Status : out C.Access_Status) is
   begin
      IO_Count := IO_Count + 1; Value := 0; Status := C.Access_OK;
      case ID is
         when C.Enable_Status => Value := Registers (Index (16#6C#)) and C.Enabled_Bit;
         when C.Controller_Status => null;
         when C.TX_Level => Value := (if Dirty_TX and then Commands > 0 then 1 else 0);
         when C.RX_Level => Value := RX_Count;
         when C.Abort_Source => Value := Abort_Source;
         when C.Clear_Interrupts => Raw_IRQ := 0; Abort_Source := 0;
         when C.Data_Command =>
            Check (RX_Count > 0, "stale drain nonempty"); RX_Count := RX_Count - 1;
         when others => Value := Registers (Index (Natural (C.Offset (ID))));
      end case;
   end Config_Read;
   procedure Config_Write (ID : C.Register_ID; Value : C.Word; Status : out C.Access_Status) is
   begin
      IO_Count := IO_Count + 1; Registers (Index (Natural (C.Offset (ID)))) := Value;
      if ID = C.Enable and then Value = C.Enabled_Bit then Cursor := 0; end if;
      Status := C.Access_OK;
      if ID = C.Enable and then Value = 0 and then Commands > 0 then
         if Unknown_Cleanup then Status := C.Unknown_Effect; end if;
         if Late_Abort then Abort_Source := 8; Raw_IRQ := Raw_IRQ or R.Abort_Interrupt; end if;
      end if;
   end Config_Write;
   procedure Data_Read (ID : R.Read_Register; Value : out R.Word; Status : out R.Access_Status) is
   begin
      IO_Count := IO_Count + 1; Status := R.Access_OK;
      case ID is
         when R.Raw_Interrupts => Value := Raw_IRQ;
         when R.Abort_Source => Value := Abort_Source;
         when R.TX_Level => Value := 0;
         when R.RX_Level => Value := RX_Count;
         when R.Read_Data_Command =>
            Check (RX_Count > 0, "owned input read nonempty");
            RX_Count := RX_Count - 1; Cursor := Cursor + 1; Value := 16#A0# + Cursor;
         when others => Value := Registers (Index (Natural (R.Offset (ID))));
      end case;
   end Data_Read;
   procedure Data_Write (ID : R.Write_Register; Value : R.Word; Status : out R.Access_Status) is
      pragma Unreferenced (ID);
   begin
      IO_Count := IO_Count + 1; Data_Calls := Data_Calls + 1;
      Status := (if Data_Calls = Fail_Command then Fail_Status else R.Access_OK);
      if Status in R.Denied | R.Unavailable then return; end if;
      Commands := Commands + 1; Command_Log (Commands) := Value;
      if (Value and T.Read_Command_Bit) /= 0 then
         RX_Count := RX_Count + 1; Check (RX_Count <= 2, "RX credit respected by composed owner");
      end if;
      if (Value and T.Stop_Command_Bit) /= 0 then Raw_IRQ := Raw_IRQ or R.Stop_Interrupt; end if;
      if Inject_Abort then Raw_IRQ := Raw_IRQ or R.Abort_Interrupt; Abort_Source := 8; end if;
      if Lose_Clock_On_Data then Clock_OK := False; end if;
   end Data_Write;
   package O is new DW_I2C_Owner
     (16, Clock, Reset_Read, Reset_Write, Config_Read, Config_Write, Data_Read, Data_Write);
   use type O.Phase;
   use type O.Admission;
   use type O.Failure;
   Parameters : constant O.Startup :=
     (P.Intel_Vendor, P.Alder_Lake_N_I2C1, 16#1234_5678_9000#, 4096,
      30, 40, 100);
   procedure Startup (S : in out O.State) is
      Result : O.Admission;
      Iterations : Natural := 0;
   begin
      O.Start (S, Parameters, Result); Check (Result = O.Accepted, "owner startup accepted");
      while O.Observe (S).At_Phase not in O.Available | O.Faulted loop
         Iterations := Iterations + 1; Check (Iterations < 100, "startup progress");
         O.Advance (S);
      end loop;
      Check (O.Observe (S).At_Phase = O.Available, "actual composition reaches available");
      Check (Registers (Index (16#6C#)) = 0 and then not O.Observe (S).Controller_May_Be_Enabled,
             "available means disabled controller");
      Check (Registers (Index (16#1C#)) = 0 and then Registers (Index (16#20#)) = 0,
             "startup no target timing");
      Check (Registers (Index (16#7C#)) = 0 and then Registers (Index (16#04#)) = 0,
             "startup no invented target");
   end Startup;
   package Q renames DW_I2C_Request_Types;
   use type Q.Reason;
   use type Interfaces.Unsigned_64;
   type Client is new Interfaces.Unsigned_64;
   type Grant is new Interfaces.Unsigned_64;
   type Reply is new Natural;
   Trusted_Client : constant Client := 16#FFFF_1234_5678_9999#;
   package BC renames I2C_Channel_Badges;
   Trusted_Badge : constant Interfaces.Unsigned_64 :=
     BC.Canonical ((BC.Seven_Bit, 16#15#, 400_000));
   Allowed_Badge : Interfaces.Unsigned_64 := Trusted_Badge;
   Input_Result, Output_Result : Q.Copy_Result := Q.Copied;
   Reads, Writes, Replies : Natural := 0;
   Last_Reply : Reply := 0;
   Last_Response : Q.Response;
   Reply_OK : Boolean := True;
   Input_Value : T.Byte := 16#55#;
   Expected_Reads : T.Count := 0;
   function Authorized (Caller : Client; Badge : Interfaces.Unsigned_64) return Boolean is
     (Caller = Trusted_Client and Badge = Allowed_Badge);
   procedure Copy_Input (Caller : Client; Reference : Grant; Data : out O.Bytes;
      Result : out Q.Copy_Result) is
   begin
      Check (Caller = Trusted_Client and Reference = 16#1000_0001#, "full caller and input generation reference");
      Reads := Reads + 1; Data := [others => Input_Value]; Result := Input_Result;
   end Copy_Input;
   procedure Copy_Output (Caller : Client; Reference : Grant; Data : O.Bytes;
      Result : out Q.Copy_Result) is
   begin
      Check (Caller = Trusted_Client and Reference = 16#2000_0002#, "original caller and output generation reference");
      Check (Data'Length = Expected_Reads, "only completed owned read length exported");
      for I in Data'Range loop
         Check (Data (I) = T.Byte (16#A0# + I), "exact owned read prefix");
      end loop;
      Writes := Writes + 1; Result := Output_Result;
   end Copy_Output;
   procedure Answer (Authority : Reply; Item : Q.Response; Delivered : out Boolean) is
   begin
      Replies := Replies + 1; Last_Reply := Authority; Last_Response := Item; Delivered := Reply_OK;
   end Answer;
   package B is new DW_I2C_Requests (O, 16, Client, Grant, Reply, Authorized,
      Copy_Input, Copy_Output, Answer);
   procedure Case_Run (Which : Natural) is
      S : O.State;
      Broker : B.State;
      Request : B.Request := (16#1000_0001#, 16#2000_0002#, 1, 2, 1000, 80);
      Caller : Client := Trusted_Client;
      Badge : Interfaces.Unsigned_64 := Trusted_Badge;
      Before, Before_Commands : Natural;
      Steps : Natural := 0;
   begin
      Reset_Model; Startup (S); Allowed_Badge := Trusted_Badge;
      Reads := 0; Writes := 0; Replies := 0;
      Input_Result := Q.Copied; Output_Result := Q.Copied; Reply_OK := True;
      Input_Value := 16#55#;
      case Which is
         when 1 => Caller := 1;
         when 2 => Badge := Trusted_Badge - 1;
         when 3 => Request.Write_Count := 16;
         when 4 => Request.Write_Count := 0; Request.Read_Count := 0;
         when 5 => Input_Result := Q.Refused;
         when 6 => Input_Result := Q.Uncertain;
         when 7 => Request.Deadline := 1;
         when 8 => Output_Result := Q.Refused;
         when 9 => Output_Result := Q.Uncertain;
         when 10 => Reply_OK := False;
         when 11 => Fail_Command := 1; Fail_Status := R.Unknown_Effect;
         when 12 => Request.Write_Count := 0;
         when 13 => Request.Read_Count := 0;
         when 14 => Request.Write_Count := 16; Request.Read_Count := 0;
         when 15 => Request.Write_Count := 0; Request.Read_Count := 16;
         when 16 => Request.Write_Count := 14;
         when others => null;
      end case;
      Expected_Reads := Request.Read_Count;
      Before := IO_Count;
      B.Submit (Broker, S, Caller, Badge, 7, Request);
      Check (IO_Count = Before, "broker admission has no hardware accesses");
      if Which in 1 .. 7 then
         Check (not B.Observe (Broker).Active and Replies = 1 and Last_Reply = 7,
            "rejected request answered once immediately");
         Check (Last_Response.Status = (case Which is
           when 1 | 2 => Q.Unauthorized, when 3 | 4 => Q.Invalid_Size,
           when 5 => Q.Input_Refused, when 6 => Q.Input_Uncertain,
           when others => Q.Admission_Refused), "typed rejection");
         Check (Commands = 0 and Writes = 0 and Reads = (if Which >= 5 then 1 else 0),
            "rejection cannot touch controller or output grant");
         Check (B.Observe (Broker).Faulted = (Which = 6), "uncertain input release requires supervisor");
      else
         Check (B.Observe (Broker).Active and Replies = 0 and Reads = (if Request.Write_Count = 0 then 0 else 1),
            "accepted request retains reply and copies input");
         Input_Value := 0;
         B.Submit (Broker, S, Trusted_Client, Trusted_Badge, 8, Request);
         Check (Replies = 1 and Last_Reply = 8 and Last_Response.Status = Q.Busy and Reads = (if Request.Write_Count = 0 then 0 else 1),
            "busy request cannot overwrite held request/reply or read input");
         while B.Observe (Broker).Active loop
            Steps := Steps + 1; Check (Steps < 100, "request progress bounded");
            B.Advance (Broker, S);
         end loop;
         Check (Replies = 2 and Last_Reply = 7 and Last_Response.ID /= T.No_Ticket,
            "completion answers original saved authority once");
         Check (Last_Response.Status = (case Which is
           when 8 => Q.Output_Refused, when 9 => Q.Output_Uncertain,
           when others => Q.Completed), "typed completion delivery");
         Check (Last_Response.Hardware_Succeeded = (Which /= 11) and
           Last_Response.Read_Count = (if Which in 8 | 9 | 11 then 0 else Request.Read_Count),
           "output refusal preserves hardware success and hides uncopied count");
         Check (Writes = (if Which = 11 or Request.Read_Count = 0 then 0 else 1), "no partial hardware-failure output");
         Check (B.Observe (Broker).Faulted = (Which in 9 | 11), "uncertainty or hardware fault latches supervisor need");
         Check (B.Observe (Broker).Last_Reply_Delivered = Reply_OK, "failed reply consumed without retry");
         Check ((Command_Log (1) and not T.Stop_Command_Bit) = (if Request.Write_Count = 0 then T.Read_Command_Bit else 16#55#), "hardware consumed owned copy despite caller mutation");
      end if;
      Before := IO_Count; Before_Commands := Commands;
      B.Advance (Broker, S); B.Advance (Broker, S);
      Check (IO_Count = Before and Commands = Before_Commands and
        Replies = (if Which in 1 .. 7 then 1 else 2), "terminal advance never replays IO/copies/reply");
   end Case_Run;
   procedure Badge_Cases is
      S : O.State;
      Broker : B.State;
      Request : constant B.Request := (16#1000_0001#, 16#2000_0002#, 1, 1, 1000, 100);
      Before, Steps : Natural;
      Badge : Interfaces.Unsigned_64;
   begin
      Reset_Model; Startup (S); Reads := 0; Writes := 0; Replies := 0;
      Input_Result := Q.Copied; Output_Result := Q.Copied; Reply_OK := True;
      Input_Value := 16#55#; Expected_Reads := 1;
      for Kind in 1 .. 5 loop
         Badge := (case Kind is
           when 1 => 0,
           when 2 => Trusted_Badge + BC.Reserved_Base,
           when 3 => BC.Canonical ((BC.Ten_Bit, 1023, 100_000)),
           when 4 => BC.Canonical ((BC.Seven_Bit, 16#15#, 1_000_001)),
           when others => BC.Canonical ((BC.Seven_Bit, 16#15#, BC.Hertz'Last)));
         Allowed_Badge := Badge; Before := IO_Count;
         B.Submit (Broker, S, Trusted_Client, Badge, 7, Request);
         Check (not B.Observe (Broker).Active and then not B.Observe (Broker).Faulted
                and then IO_Count = Before and then Reads = 0 and then Writes = 0,
                "invalid/unsupported authenticated badge has no grant or hardware effects");
         Check (Replies = Kind and then Last_Response.Status =
                 (if Kind <= 2 then Q.Invalid_Channel else Q.Unsupported_Channel),
                "bad badge gets exact typed single reply");
      end loop;
      Replies := 0;
      for Channel in 1 .. 2 loop
         Badge := (if Channel = 1 then Trusted_Badge else
                   BC.Canonical ((BC.Seven_Bit, 16#2A#, 100_000)));
         Allowed_Badge := Badge;
         B.Submit (Broker, S, Trusted_Client, Badge, Reply (Channel), Request);
         Check (B.Observe (Broker).Active, "valid channel badge admitted");
         -- A second authenticated channel cannot overwrite the admitted scope.
         Allowed_Badge := BC.Canonical ((BC.Seven_Bit, 16#33#, 10_000));
         Before := IO_Count;
         B.Submit (Broker, S, Trusted_Client, Allowed_Badge, 8, Request);
         Check (Last_Response.Status = Q.Busy and then Last_Reply = 8
                and then IO_Count = Before and then Reads = Channel,
                "other channel cannot replace retained scope or input/reply");
         Steps := 0;
         while B.Observe (Broker).Active loop
            Steps := Steps + 1; Check (Steps < 100, "channel request progress");
            B.Advance (Broker, S);
         end loop;
         Check (Last_Response.Status = Q.Completed and then Last_Response.Hardware_Succeeded
                and then Last_Reply = Reply (Channel) and then Replies = 2 * Channel,
                "original channel reply survives concurrent admission attempt");
         Check (Registers (Index (16#04#)) = (if Channel = 1 then 16#15# else 16#2A#),
                "target comes from original authenticated badge");
         Check (Registers (Index (16#00#)) =
                  (C.Master_Bit or C.Restart_Enable_Bit or C.Slave_Disable_Bit or
                   (if Channel = 1 then C.Fast_Speed_Bits else C.Standard_Speed_Bits)),
                "speed mode comes from original authenticated badge");
      end loop;
   end Badge_Cases;
begin
   for Which in 0 .. 16 loop Case_Run (Which); end loop;
   Badge_Cases;
   Ada.Text_IO.Put_Line ("I2C request owner checks" & Checks'Image);
end Request_Tests;
