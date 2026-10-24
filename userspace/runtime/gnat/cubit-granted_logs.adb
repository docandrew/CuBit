------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Logging;
with CuBit.Handles.IPC;
with CuBit.Handles.Services;
with CuBit.Kernel_ABI;
with CuBit.Log_Records;

package body CuBit.Granted_Logs is
   use CuBit.Messages;
   package LR renames CuBit.Log_Records;

   Line_Characters : constant := 160;
   Held_Lines      : constant := 64;
   Newline         : constant String := [1 => ASCII.LF];

   type Held is record
      Text   : String (1 .. Line_Characters) := [others => ' '];
      Length : Natural range 0 .. Line_Characters := 0;
      Level  : LR.Severity := LR.Information;
   end record;

   function Severity_Of (L : Level) return LR.Severity;
   function Severity_Of (L : Level) return LR.Severity is
     (case L is when Information => LR.Information, when Warning => LR.Warning);
   Backlog   : array (1 .. Held_Lines) of Held;
   Count     : Natural range 0 .. Held_Lines := 0;
   Lost      : Natural := 0;

   Writer    : CuBit.Logging.Publisher := CuBit.Logging.Awaiting_Grant;
   Asked     : Boolean := False;
   Have      : Boolean := False;
   Refused   : Boolean := False;

   procedure Emit (Text : String; Level : LR.Severity);
   procedure Emit (Text : String; Level : LR.Severity) is
      Value : constant LR.Decoded := LR.Make (Text, Level);
      Ignore : Boolean;
   begin
      if Value.Success then
         CuBit.Logging.Emit (Writer, Value.Value, Ignore);
      end if;
   end Emit;

   procedure Line (Text : String; Severity : Level := Information) is
      Wanted : constant LR.Severity := Severity_Of (Severity);
   begin
      debugPrint (Text);
      debugPrint (Newline);
      if Have then
         Emit (Text, Wanted);
      elsif not Refused then
         if Count < Held_Lines then
            Count := Count + 1;
            declare
               Kept : constant Natural := Natural'Min (Text'Length, Line_Characters);
            begin
               Backlog (Count).Text (1 .. Kept) := Text (Text'First .. Text'First + Kept - 1);
               Backlog (Count).Length := Kept;
               Backlog (Count).Level := Wanted;
            end;
         else
            Lost := Lost + 1;
         end if;
      end if;
   end Line;

   procedure Request (Device_Manager : CuBit.Handles.Endpoint) is
      Msg : Message := NULL_MESSAGE;
   begin
      if Have or else Asked or else Refused or else
        Registered_Driver (DRIVER_LOGSTORE) = No_Process
      then
         return;
      end if;
      Msg.tag := (label => Grant_Label, length => 0, protocol => 0, ipc => <>, reserved => 0);
      Asked := CuBit.Handles.IPC.Submit_Accepting (Device_Manager, Msg, Token);
   end Request;

   function Take (C : CompletionEntry) return Boolean is
   begin
      if not Asked or else C.token /= Token then
         return False;
      end if;
      Asked := False;
      if C.status = COMPLETION_OK and then C.msg.tag.label = CuBit.Kernel_ABI.Reply_OK then
         declare
            Collector : constant CuBit.Handles.IPC.Accepted_Endpoint :=
              CuBit.Handles.IPC.Accept_Endpoint (CuBit.Handles.IPC.Carried_By (C.msg));
         begin
            if Collector.OK then
               CuBit.Logging.Attach
                 (Writer, CuBit.Handles.Services.Logstore.Granted (Collector.Value));
               Have := True;
            end if;
         end;
      end if;
      if not Have then
         --  devmgr gives this program no publisher: serial only, and the
         --  backlog is not kept.
         Refused := True;
         Count := 0;
         return True;
      end if;
      for I in 1 .. Count loop
         Emit (Backlog (I).Text (1 .. Backlog (I).Length), Backlog (I).Level);
      end loop;
      Count := 0;
      if Lost > 0 then
         declare
            --  No secondary stack (netstack has none to spare).
            Prefix : constant String := "log lines lost before logstore ran: ";
            Text : String (1 .. Prefix'Length + 10) := [others => ' '];
            Rest : Natural := Lost;
            Pos  : Natural := Text'Last;
         begin
            Text (1 .. Prefix'Length) := Prefix;
            loop
               Text (Pos) := Character'Val (Character'Pos ('0') + Rest mod 10);
               Rest := Rest / 10;
               exit when Rest = 0;
               Pos := Pos - 1;
            end loop;
            Emit (Text, LR.Warning);
         end;
         Lost := 0;
      end if;
      return True;
   end Take;

   function Waiting return Boolean is (Asked);
   function Granted return Boolean is (Have);
   function Settled return Boolean is (Have or else Refused);

end CuBit.Granted_Logs;
