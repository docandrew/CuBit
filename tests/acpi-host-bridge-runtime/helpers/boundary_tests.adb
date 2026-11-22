pragma Ada_2022;
with Ada.Text_IO;
with System.Storage_Elements;
with Firmware_Tables;
with ACPI_Host_Bridge_Channel;
with CuBit.Channels;
with CuBit.Channel_Contracts;
with CuBit.Handles;
with CuBit.Memory_Grants;
with CuBit.Protocols;
procedure Boundary_Tests is
   package B renames ACPI_Host_Bridge_Channel;
   package W renames B.W; package S renames B.S; package M renames B.M;
   package R renames B.RC; package C renames CuBit.Channels; package F renames Firmware_Tables;
   use type W.Word, F.Byte, B.Phase, B.Failure, S.Outcome;
   Maximum_Payload : constant Natural := (CuBit.Channel_Contracts.Ring_Pages'Last - R.Client_Ring_Pages) * R.Page_Bytes;
   Lengths : constant array (Positive range <>) of Natural :=
     [0, R.Page_Bytes - S.Header_Bytes, R.Page_Bytes - S.Header_Bytes + 1,
      Maximum_Payload - S.Header_Bytes, Maximum_Payload - S.Header_Bytes + 1];
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Why; end if; end Check;
   function Base (At_Address : System.Address) return W.Word is
     (W.Word (System.Storage_Elements.To_Integer (At_Address)));
   Peer : constant B.P.Process_ID := B.P.From_Word (16#F123_4567_ABCD_0001#);
begin
   for Size of Lengths loop
      declare
         Own : aliased F.Bytes (1 .. 128 * R.Page_Bytes) := [others => 0] with Alignment => 4096;
         Remote : aliased F.Bytes (1 .. R.Server_Pages * R.Page_Bytes) := [others => 0] with Alignment => 4096;
         -- Synthetic source bytes exercise framing/capacity only; this is not
         -- a claim that an arbitrary tail is a valid or live firmware _CRS.
         Input : F.Bytes (31 .. 30 + Size) := [others => 16#5A#];
         Back : F.Bytes ((if Size = 0 then Natural'Last else Natural'Last - Size + 1)
           .. (if Size = 0 then Natural'Last - 1 else Natural'Last));
         No_Rows : W.Rows (1 .. 0);
         Header : constant S.Header := (State => S.M.Complete, Root_ID => 1,
            Sequence => 1, Required_Descriptors => 0, others => <>);
         Decoded : S.Header; Report : S.Report;
         IO : B.Session; Server : R.Server; Request : R.Queues.Submission;
         Submitted, Handled, Got, Rung, Done : Boolean;
      begin
         C.Mode := C.Good; C.Submit_OK := True; C.Open_Calls := 0; C.Close_Calls := 0;
         C.Own_Base := Base (Own'Address); C.Peer_Base := Base (Remote'Address);
         CuBit.Memory_Grants.Model_Retired := False;
         B.Begin_Open (IO, CuBit.Handles.Model_Endpoint, Peer,
           (Identity => B.M.Schema, Version => 1,
            Sizing => CuBit.Protocols.Fixed_Size, Wire_Size => M.Message_Bytes),
           0, 100, 101, 75, 10, Header, No_Rows, Input, Submitted);
         if Size > Maximum_Payload - S.Header_Bytes then
            Check (not Submitted and C.Open_Calls = 0 and B.Observe (IO).Why = B.Payload_Too_Large,
               "one byte beyond negotiated capacity refuses before effects");
         else
            Check (Submitted and B.Observe (IO).Packet_Bytes = S.Header_Bytes + Size,
               "exact measured payload, including page and maximum boundaries");
            if Size > 0 then Input (Input'First) := 0; end if;
            B.Complete_Open (IO,
              (token => 100, from => B.P.To_Word (Peer), valid => True, others => <>), Handled);
            Check (Handled and B.Observe (IO).Stage = B.Ready, "measured mapping accepted");
            R.Attach_Server (Server, Remote'Address, Own'Address);
            B.Publish (IO, 10, Submitted); Check (Submitted, "boundary payload published");
            R.Take (Server, Request, Got, Rung); Check (Got, "server sees exact request");
            Check (Request.Item (M.Packet_Bytes) = W.Word (S.Header_Bytes + Size), "no length truncation");
            S.Decode (Own (Own'First + R.Client_Payload_At .. Own'First + R.Client_Payload_At + S.Header_Bytes + Size - 1),
              Decoded, No_Rows, Back, Report);
            Check (Report.Status = S.Done and Decoded.Root_ID = Header.Root_ID, "bounded payload decodes");
            for Byte of Back loop Check (Byte = 16#5A#, "payload owns source copy across async setup"); end loop;
            R.Complete (Server, Request.Tag, [M.Root_ID => 1, M.Sequence => 1, M.Disposition => M.Acceptance_Code (M.Accepted)], Rung);
            B.Poll (IO); B.Close (IO, Done); Check (Done, "boundary request drains and closes");
            R.Detach_Server (Server);
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Host bridge channel boundary checks" & Checks'Image);
end Boundary_Tests;
