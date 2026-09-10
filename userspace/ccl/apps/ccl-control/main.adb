with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CBOR;
with CCL.Control;
with CCL.Sessions;
with Control_Transport;
with Control_HTTP;
with Control_Wire;
with Control_Host;

procedure Main is
   Host : CCL.Control.Observation;
   Success : Boolean;
   use type Control_HTTP.Parse_State;
   use type Control_HTTP.Method;
   use type CCL.Control.Operation;

   procedure Serve is
      Input : String (1 .. Control_HTTP.Max_Input);
      Used : Natural := 0;
      Count : Natural;
      Frame : Control_HTTP.Request;
      Query : Control_Wire.Request;
      Encoded : Control_Wire.Response;
      Result : CCL.Control.Response;
      Session : CCL.Sessions.Session;
      Valid : Boolean;
      Now : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
      Deadline : constant Unsigned_64 :=
        (if Now > Unsigned_64'Last - 5000 then Unsigned_64'Last else Now + 5000);
   begin
      loop
         Control_Transport.Read_Some (Input (Used + 1 .. Input'Last), Count, Deadline, Success);
         if not Success then return; end if;
         Used := Used + Count;
         Control_HTTP.Parse (Input (1 .. Used), Frame);
         exit when Frame.State /= Control_HTTP.Incomplete or Used = Input'Length;
      end loop;
      if Frame.State /= Control_HTTP.Ready then
         Control_Transport.Write_All (Control_HTTP.Error_Response, Success); return;
      end if;
      if Frame.Verb = Control_HTTP.Preflight then
         Control_Transport.Write_All (Control_HTTP.Preflight_Response, Success); return;
      end if;
      declare
         Bytes : CBOR.Byte_Array (1 .. CBOR.SE_Offset (Frame.Body_Length));
      begin
         for I in Bytes'Range loop
            Bytes (I) := Character'Pos (Input (Frame.Body_First + Natural (I) - 1));
         end loop;
         Control_Wire.Decode (Bytes, Query, Valid);
      end;
      if not Valid then
         Control_Transport.Write_All (Control_HTTP.Error_Response, Success); return;
      end if;
      -- Fresh bounded evaluation state per submission. No remote session or
      -- user identity is implicitly granted authority by this lab adapter.
      CCL.Sessions.Initialize (Session);
      if Query.Op = CCL.Control.Evaluate_Expression then
         Result := (Observed => Host, others => <>);
         Control_Host.Evaluate (Query.Source (1 .. Query.Length), Result.Outcome);
      elsif Query.Op in CCL.Control.Start_Monitor | CCL.Control.Stop_Monitor |
        CCL.Control.Inspect_Monitor
      then
         Result := (Observed => Host, others => <>);
         case Query.Op is
            when CCL.Control.Start_Monitor =>
               Control_Host.Start_Monitor (Query.Source (1 .. Query.Length), Result.Accepted);
            when CCL.Control.Stop_Monitor =>
               Control_Host.Stop_Monitor (Query.Target, Result.Accepted);
            when others => Result.Accepted := True;
         end case;
         Control_Host.Pump;
         Result.Monitor := Control_Host.Monitor;
      else
         Control_Host.Read_Clock (Host.Clock_Available, Host.Monotonic_Ms);
         CCL.Control.Execute (Session, Query.Op, Query.Source (1 .. Query.Length), Host, Result);
      end if;
      Control_Wire.Encode (Query, Result, Encoded);
      if Encoded.Length = 0 then
         Control_Transport.Write_All (Control_HTTP.Error_Response, Success); return;
      end if;
      declare
         Output : String (1 .. Encoded.Length);
      begin
         for I in Output'Range loop Output (I) := Character'Val (Encoded.Data (CBOR.SE_Offset (I))); end loop;
         Control_Transport.Write_All (Control_HTTP.Response_Header (Output'Length) & Output, Success);
      end;
      if Success then debugPrint ("ccl-control: request completed" & ASCII.LF); end if;
   end Serve;
begin
   debugPrint ("ccl-control: DEVELOPMENT PLAINTEXT; own bindings only" & ASCII.LF);
   Host.Process_Id := syscall (SYSCALL_GETPID);
   Host.Clock_Process := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_CLOCK);
   Control_Host.Initialize (Success);
   if not Success then
      debugPrint ("ccl-control: host binding initialization failed" & ASCII.LF); return;
   end if;
   Control_Transport.Listen (Host.Network_Process, Success);
   if not Success then
      debugPrint ("ccl-control: listener failed" & ASCII.LF);
      Control_Transport.Close; return;
   end if;
   debugPrint ("ccl-control: native listener ready" & ASCII.LF);
   loop
      Control_Host.Pump;
      Control_Transport.Accept_Connection (Control_Host.Next_Deadline, Success);
      if Success then Serve; Control_Transport.Close_Connection; end if;
   end loop;
end Main;
