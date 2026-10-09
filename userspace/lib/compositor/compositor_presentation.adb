package body Compositor_Presentation with SPARK_Mode is
   function Open (Session_ID : Live_ID) return State is
     ((Available, Session_ID, 0, 0));
   procedure Quarantine (S : in out State) is
   begin
      S.Status := Quarantined;
   end Quarantine;
   procedure Prepare (S : in out State; Frame : ID; Started : out Boolean) is
   begin
      Started := Writable (S) and Frame > S.Frame_ID;
      if Started then
         S.Frame_ID := Frame;
         S.Status := Prepared;
         S.Attempt_At := 0;
      else
         Quarantine (S);
      end if;
   end Prepare;
   procedure Submitted (S : in out State; Accepted : Boolean; Now : ID) is
   begin
      if S.Status = Prepared then
         if Accepted then S.Status := In_Flight;
         else S.Attempt_At := (if Now < ID'Last then Now + 1 else ID'Last);
         end if;
      else Quarantine (S);
      end if;
   end Submitted;
   procedure Cancel (S : in out State) is
   begin
      if S.Status = Prepared then S.Status := Available;
      else Quarantine (S);
      end if;
   end Cancel;
   procedure Complete (S : in out State; Reply : Completion) is
   begin
      if Releases (S, Reply) then
         S.Status := Available;
      else
         Quarantine (S);
      end if;
   end Complete;
end Compositor_Presentation;
