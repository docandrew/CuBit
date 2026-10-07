with CuBit.Authority_Policy; use CuBit.Authority_Policy;
with CuBit.Log_Protocol;
with CuBit.Process_Observer;
package body Policy_Proof with SPARK_Mode is
   procedure Check (Requested, Installation, Session, Issuer : Boolean) is
      Result : constant Decision := Evaluate (Requested, Installation, Session, Issuer);
   begin
      pragma Assert ((Result = Approved) = (Requested and Installation and Session and Issuer));
      pragma Assert (Evaluate (Requested, False, Session, Issuer) /= Approved);
      pragma Assert (Evaluate (Requested, Installation, False, Issuer) /= Approved);
      pragma Assert (Evaluate (Requested, Installation, Session, False) /= Approved);
      pragma Assert (not Bootstrap_Approves (Log_Observation, False));
      pragma Assert (not Bootstrap_Approves (Master_Audio, False));
      pragma Assert (Bootstrap_Approves (Master_Audio, Session) = Session);
      pragma Assert (Bootstrap_Approves (Log_Observation, Session) = Session);
      --  Seeing what runs is observation too: never ambient.
      pragma Assert (not Bootstrap_Approves (Process_Observation, False));
      pragma Assert (Bootstrap_Approves (Process_Observation, Session) = Session);
      --  Only procmgr's process-observer tags may list; tag 0 (the plain
      --  process-manager endpoint) and other observers' tags may not.
      pragma Assert (not CuBit.Process_Observer.Is_Observer (0));
      pragma Assert (not CuBit.Process_Observer.Is_Observer (CuBit.Process_Observer.Observer_Tag_Base));
      pragma Assert (not CuBit.Process_Observer.Is_Observer (CuBit.Log_Protocol.Observer_Authority_Tag));
      pragma Assert (CuBit.Process_Observer.Is_Observer (CuBit.Process_Observer.Observer_Tag (1)));
      --  Changing what logstore keeps is never ambient either.
      pragma Assert (not Bootstrap_Approves (Log_Control, False));
      pragma Assert (Bootstrap_Approves (Log_Control, Session) = Session);
      --  Only log-control tags may set the minimum: not observers, not
      --  publishers, not the untagged endpoint. Any logstore role may read it.
      pragma Assert (CuBit.Log_Protocol.May_Invoke
        (CuBit.Log_Protocol.Control_Tag (1), CuBit.Log_Protocol.Set_Minimum));
      pragma Assert (not CuBit.Log_Protocol.May_Invoke
        (CuBit.Log_Protocol.Observer_Authority_Tag, CuBit.Log_Protocol.Set_Minimum));
      pragma Assert (not CuBit.Log_Protocol.May_Invoke
        (CuBit.Log_Protocol.Publisher_Authority_Tag, CuBit.Log_Protocol.Set_Minimum));
      pragma Assert (not CuBit.Log_Protocol.May_Invoke (0, CuBit.Log_Protocol.Set_Minimum));
      pragma Assert (not CuBit.Log_Protocol.May_Invoke
        (CuBit.Log_Protocol.Control_Tag_Base, CuBit.Log_Protocol.Set_Minimum));
      pragma Assert (CuBit.Log_Protocol.May_Invoke
        (CuBit.Log_Protocol.Publisher_Authority_Tag, CuBit.Log_Protocol.Get_Minimum));
      --  A control holder cannot read records or publish through that tag.
      pragma Assert (not CuBit.Log_Protocol.May_Invoke
        (CuBit.Log_Protocol.Control_Tag (1), CuBit.Log_Protocol.Subscribe));
      pragma Assert (not CuBit.Log_Protocol.May_Publish (CuBit.Log_Protocol.Control_Tag (1)));
      pragma Assert (CuBit.Log_Protocol.May_Publish (CuBit.Log_Protocol.Publisher_Authority_Tag));
   end Check;
end Policy_Proof;
