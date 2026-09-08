with Ada.Text_IO;
with Interfaces; use Interfaces;
with Net;
with TCPSession; use TCPSession;
with TCP_Listeners;

procedure Main is
   Connections : ConnTable;
   R : Result;
   Index : Integer;
   S : SegmentInfo :=
     (srcIP => [10, 0, 2, 2], srcPort => 40000, dstPort => 8080,
      seqNum => 100, ackNum => 0, flagSYN => True,
      flagACK => False, flagFIN => False, flagRST => False,
      winSize => 8192, dataLen => 0, dataOff => 54);
   procedure Fresh is
   begin
      Connections := [others => <>];
      allocateConn (Connections, S.srcIP, Net.ZERO_MAC, S.srcPort, S.dstPort, 1000, Index);
      pragma Assert (Index = 0);
   end Fresh;
   procedure Test_Listeners is
      use TCP_Listeners;
      Listeners : Table;
      Listener, Other, Replacement : Handle;
      Status : Bind_Status;
      Child_Index : TCP_Listeners.Connection_Index;
      Children : Connection_List;
      Success : Boolean;
   begin
      Initialize (Listeners);
      pragma Assert (Next_Deadline (Listeners) = Unsigned_64'Last);
      Bind (Listeners, 42, 0, 8080, Listener, Status);
      pragma Assert (Status = Invalid_Address and Listener = No_Handle);
      Bind (Listeners, 42, 16#0A00_020F#, 8080, Listener, Status);
      pragma Assert (Status = Bound and Listener /= No_Handle);
      pragma Assert (Owned (Listeners, 42, Listener));
      pragma Assert (not Owned (Listeners, 43, Listener));
      pragma Assert (not Owned (Listeners, 42, No_Handle));
      Bind (Listeners, 43, 16#0A00_020F#, 8080, Other, Status);
      pragma Assert (Status = Address_In_Use and Other = No_Handle);
      Bind (Listeners, 43, 16#0A00_020F#, 8081, Other, Status);
      pragma Assert (Status = Bound and Other /= Listener);
      Reserve (Listeners, Listener, 0, 100, Success);
      pragma Assert (Success and Next_Deadline (Listeners) = 100);
      Reserve (Listeners, Other, 0, 101, Success);
      pragma Assert (not Success); -- no duplicate ownership across listeners
      Accept_Ready (Listeners, 42, Listener, Child_Index, Success);
      pragma Assert (not Success); -- a SYN is not yet a usable connection
      Reserve (Listeners, Listener, 1, 200, Success);
      pragma Assert (Success);
      Reserve (Listeners, Listener, 2, 300, Success);
      pragma Assert (not Success); -- bounded half-open + ready backlog
      Mark_Ready (Listeners, 0);
      Accept_Ready (Listeners, 43, Listener, Child_Index, Success);
      pragma Assert (not Success); -- wrong owner cannot consume it
      Accept_Ready (Listeners, 42, Listener, Child_Index, Success);
      pragma Assert (Success and Child_Index = 0);
      pragma Assert (Next_Deadline (Listeners) = 200);
      Close (Listeners, 43, Listener, Children, Success);
      pragma Assert (not Success and Children = Connection_List'[others => False]);
      Close (Listeners, 42, Listener, Children, Success);
      pragma Assert (Success and Children = Connection_List'[1 => True, others => False]);
      pragma Assert (not Owned (Listeners, 42, Listener));
      Bind (Listeners, 42, 16#0A00_020F#, 8080, Replacement, Status);
      pragma Assert (Status = Bound and Replacement /= Listener);
      Reserve (Listeners, Listener, 0, 400, Success);
      pragma Assert (not Success); -- stale handle cannot select reused slot
      Reserve (Listeners, Replacement, 0, 400, Success);
      pragma Assert (Success);
      Reserve (Listeners, Other, 2, 500, Success);
      pragma Assert (Success);
      Mark_Ready (Listeners, 2);
      Expire (Listeners, 399, Children);
      pragma Assert (Children = Connection_List'[others => False]);
      Expire (Listeners, 400, Children);
      pragma Assert (Children = Connection_List'[0 => True, others => False]);
      pragma Assert (Next_Deadline (Listeners) = 500);
      Expire (Listeners, 500, Children);
      pragma Assert (Children = Connection_List'[2 => True, others => False]);
      pragma Assert (Next_Deadline (Listeners) = Unsigned_64'Last);
      Reserve (Listeners, Other, 3, 600, Success);
      pragma Assert (Success);
      Remove (Listeners, 3);
      pragma Assert (Next_Deadline (Listeners) = Unsigned_64'Last);
      Bind (Listeners, 42, 16#0A00_020F#, 8082, Replacement, Status);
      pragma Assert (Status = Bound);
      Bind (Listeners, 42, 16#0A00_020F#, 8083, Replacement, Status);
      pragma Assert (Status = Bound);
      Bind (Listeners, 42, 16#0A00_020F#, 8084, Replacement, Status);
      pragma Assert (Status = Table_Full and Replacement = No_Handle);
      Ada.Text_IO.Put_Line ("TCP listeners: ownership, bounded backlog, stale handles, admission to accept, expiry PASS");
   end Test_Listeners;
begin
   Fresh;
   onPassiveOpen (Connections (0), S, R);
   pragma Assert (Connections (0).state = TCP_SYN_RECEIVED);
   pragma Assert (R.numActions = 1 and R.actions (0).flags = (TCP_FLAG_SYN or TCP_FLAG_ACK));
   pragma Assert (R.actions (0).seqNum = 1000 and R.actions (0).ackNum = 101);
   pragma Assert (Connections (0).sendNext = 1001);
   onSegmentIn (Connections, S, Index, R); -- duplicate SYN
   pragma Assert (R.numActions = 1 and R.actions (0).seqNum = 1000);
   pragma Assert (Connections (0).sendNext = 1001);
   S.flagSYN := False; S.flagACK := True; S.seqNum := 101; S.ackNum := 1002;
   onSegmentIn (Connections, S, Index, R); -- ACK of unsent bytes
   pragma Assert (Connections (0).state = TCP_SYN_RECEIVED and R.numActions = 0);
   S.ackNum := 1001; S.seqNum := 102;
   onSegmentIn (Connections, S, Index, R); -- wrong receive sequence
   pragma Assert (Connections (0).state = TCP_SYN_RECEIVED);
   S.seqNum := 101; S.dataLen := 3; S.flagFIN := True;
   onSegmentIn (Connections, S, Index, R); -- final ACK + data + FIN
   pragma Assert (Connections (0).state = TCP_CLOSE_WAIT);
   pragma Assert (Connections (0).recvNext = 105 and R.numActions = 5);
   pragma Assert (R.actions (0).kind = ACT_NOTIFY_ESTABLISHED);
   pragma Assert (R.actions (2).kind = ACT_NOTIFY_DATA and R.actions (2).dataLen = 3);
   pragma Assert (R.actions (4).kind = ACT_NOTIFY_CLOSED);
   onSend (Connections (0), 8, 0, R); -- response after peer half-close
   pragma Assert (R.numActions = 1 and Connections (0).sendNext = 1009);
   onClose (Connections (0), R);
   pragma Assert (Connections (0).state = TCP_LAST_ACK);

   S.flagFIN := False; S.flagSYN := True; S.flagACK := False;
   S.seqNum := Unsigned_32'Last; S.dataLen := 0;
   Fresh;
   onPassiveOpen (Connections (0), S, R);
   pragma Assert (Connections (0).recvNext = 0); -- sequence wrap
   S.flagSYN := False; S.flagACK := True; S.seqNum := 0; S.ackNum := 1001;
   onSegmentIn (Connections, S, Index, R);
   pragma Assert (Connections (0).state = TCP_ESTABLISHED);
   Connections (0).receiveWindow := 2;
   S.dataLen := 3;
   onSegmentIn (Connections, S, Index, R);
   pragma Assert (Connections (0).recvNext = 0 and R.numActions = 1);
   S.dataLen := 2;
   onSegmentIn (Connections, S, Index, R);
   pragma Assert (Connections (0).recvNext = 2 and Connections (0).receiveWindow = 0);
   S.seqNum := 0; S.flagFIN := True;
   onSegmentIn (Connections, S, Index, R); -- duplicate data cannot consume FIN
   pragma Assert (Connections (0).recvNext = 2 and Connections (0).state = TCP_ESTABLISHED);
   S.flagFIN := False; S.flagRST := True; S.seqNum := 3; S.dataLen := 0;
   onSegmentIn (Connections, S, Index, R);
   pragma Assert (Connections (0).state = TCP_ESTABLISHED);
   S.seqNum := 2;
   onSegmentIn (Connections, S, Index, R);
   pragma Assert (Connections (0).state = TCP_CLOSED);

   Fresh; onConnect (Connections (0), R);
   S.flagRST := False; S.flagSYN := True; S.ackNum := 9999;
   onSegmentIn (Connections, S, Index, R);
   pragma Assert (Connections (0).state = TCP_SYN_SENT);
   S.ackNum := 1001;
   onSegmentIn (Connections, S, Index, R);
   pragma Assert (Connections (0).state = TCP_ESTABLISHED);
   Ada.Text_IO.Put_Line ("TCP session: passive handshake, duplicate SYN, ACK validation, half-close, receive credit, wrap, RST PASS");
   Test_Listeners;
end Main;
