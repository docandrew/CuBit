with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Protocols;
with CuBit.Failures;
with CCL.Binding_Blocks;
with CCL.Catalog;
with CCL.Host_Values;
with CCL.Interfaces.Clock;
with CCL.Interfaces.Logs;
with CCL.Held_Texts;
with CCL.Language;
with CCL.Sessions;
with CCL_Config_Bindings;
with CCL_File_Bindings;
with CCL_Image_Bindings;
with CCL_Log_Bindings;
with CCL_Native_Bindings;
with CCL_Process_Bindings;
with Remote_Capture;
with Remote_Display_Bindings;

package body Remote_Host is
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Host_Values.Value_Kind;
   use type CCL.Language.Interpretation_Status;

   CLOCK_BINDING : constant Unsigned_32 := CCL.Binding_Blocks.First (CCL.Binding_Blocks.Host_Clock);
   SET_MINIMUM_BINDING : constant Unsigned_32 :=
     CCL.Binding_Blocks.First (CCL.Binding_Blocks.Log) +
     CCL.Interfaces.Logs.Operation'Pos (CCL.Interfaces.Logs.Set_Minimum);
   REPLY_OK : constant Unsigned_32 := 16#F000#;

   Catalog : CCL.Catalog.Interface_Catalog;
   Grants : CCL.Catalog.Granted_Bindings;
   Session : CCL.Sessions.Session;
   Ready : Boolean := False;
   --  (display.capture): only with the display-capture grant.
   Display_Ready : Boolean := False;
   --  The last evaluation's result text, whole (Read_Result reads it).
   Last_Result : CCL.Held_Texts.Text;

   procedure Read_Clock (Available : out Boolean; Value : out Unsigned_64) is
      Request : Message := NULL_MESSAGE;
      Tag : MessageTag;
   begin
      Request.tag := (label => CuBit.Protocols.CLOCK_OP_MONOTONIC_MS,
                      length => 1, protocol => 0, ipc => <>, reserved => 0);
      Tag := capCall (CCL_Native_Bindings.Clock, Request, CuBit.Messages.Wait_Forever);
      Available := Tag.label = REPLY_OK and Tag.length = 1;
      Value := (if Available then Request.words (0) else 0);
   end Read_Clock;

   type Context_Type is null record;
   Context : Context_Type;

   procedure Invoke
     (Context : in out Context_Type; Binding : Unsigned_32;
      Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result)
   is
      pragma Unreferenced (Context);
      Milliseconds : Unsigned_64;
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      if Binding = SET_MINIMUM_BINDING then
         Reply.Why := CuBit.Failures.Failed
           (CuBit.Failures.Outside_Scope,
            "a remote session diagnoses: it does not change what logstore keeps",
            "change the minimum on the device (the Logs app's Keep box)");
      elsif CCL_Log_Bindings.Handles (Binding) then
         CCL_Log_Bindings.Invoke (Binding, Argument, Reply);
      elsif CCL_Process_Bindings.Handles (Binding) then
         CCL_Process_Bindings.Invoke (Binding, Argument, Reply);
      elsif CCL_Config_Bindings.Handles (Binding) then
         CCL_Config_Bindings.Invoke (Binding, Argument, Reply);
      elsif CCL_File_Bindings.Handles (Binding) then
         CCL_File_Bindings.Invoke (Binding, Argument, Reply);
      elsif CCL_Image_Bindings.Handles (Binding) then
         CCL_Image_Bindings.Invoke (Binding, Argument, Reply);
      elsif Display_Ready and then Remote_Display_Bindings.Handles (Binding) then
         Remote_Display_Bindings.Invoke (Binding, Argument, Reply);
      elsif Binding = CLOCK_BINDING
        and then Argument.Kind = CCL.Host_Values.Integer_Value and then Argument.Integer = 0
      then
         Read_Clock (Reply.Success, Milliseconds);
         if Reply.Success and then Milliseconds <= Unsigned_64 (Integer_64'Last) then
            Reply.Value := CCL.Host_Values.Integer_Constant (Integer_64 (Milliseconds));
         else
            Reply.Success := False;
         end if;
      end if;
   end Invoke;

   procedure Submit is new CCL.Sessions.Submit_With_Values (Context_Type, Invoke);

   procedure Initialize (Success : out Boolean) is
      Error : CCL.Catalog.Catalog_Error;
      Grant : CCL.Catalog.Grant_Result;
      Operation : CCL.Catalog.Resolved_Operation;
      Found, Available : Boolean;
      Sample : Unsigned_64;
   begin
      Ready := False;
      CCL.Catalog.Initialize (Catalog);
      CCL.Catalog.Initialize (Grants);
      CCL_Config_Bindings.Install (Catalog, Grants, Success);
      if Success then CCL_Log_Bindings.Install (Catalog, Grants, Success); end if;
      if Success then CCL_Process_Bindings.Install (Catalog, Grants, Success); end if;
      if Success then CCL_File_Bindings.Install (Catalog, Grants, Success); end if;
      if Success then CCL_Image_Bindings.Install (Catalog, Grants, Success); end if;
      if not Success then return; end if;
      --  Screenshots, when granted. A catalog that cannot take the display
      --  interface leaves the session without it, not without CCL.
      if Remote_Capture.Available then
         Remote_Display_Bindings.Install (Catalog, Grants, Display_Ready);
         if not Display_Ready then
            debugPrint ("ccl-remote: the display interface was not published; (display.capture) unavailable" &
                        ASCII.LF);
         end if;
      end if;
      CCL.Interfaces.Clock.Publish (Catalog, Error);
      Success := Error = CCL.Catalog.Catalog_Valid;
      if not Success then return; end if;
      CCL.Interfaces.Clock.Resolve_Monotonic_Ms (Catalog, Operation, Found);
      Success := Found;
      if not Success then return; end if;
      --  Grant the clock only after the granted endpoint answered once.
      Read_Clock (Available, Sample);
      if Available then
         CCL.Catalog.Install (Grants, Operation, CLOCK_BINDING, Grant);
         Success := Grant = CCL.Catalog.Grant_Added;
         if not Success then return; end if;
      end if;
      CCL.Sessions.Initialize (Session, Catalog);
      Ready := True;
   end Initialize;

   procedure Evaluate (Source : String; Succeeded : out Boolean) is
      Outcome : CCL.Language.Interpretation_Result;
   begin
      if not Ready then
         Succeeded := False;
         Last_Result := CCL.Held_Texts.To_Text ("the remote session's CCL environment did not start");
         return;
      end if;
      Submit (Session, Source, CCL.Sessions.Default_Fuel, Grants, Context, Outcome);
      Succeeded := Outcome.Status = CCL.Language.Succeeded;
      Last_Result := CCL.Held_Texts.To_Text (CCL.Sessions.Result_Image (Outcome));
   end Evaluate;

   function Result_Length return Natural is (CCL.Held_Texts.Length (Last_Result));

   function Result_Part (Offset, Count : Natural) return String is
     (if Offset >= Result_Length or else Count = 0 then ""
      else CCL.Held_Texts.Slice (Last_Result, Offset + 1, Offset + Natural'Min (Count, Result_Length - Offset)));
end Remote_Host;
