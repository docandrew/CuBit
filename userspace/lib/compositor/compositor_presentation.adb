package body Compositor_Presentation with SPARK_Mode is
   function Open (Session_ID : Live_ID) return State is
     ((Available, Session_ID, 0));
   procedure Quarantine (S : in out State) is
   begin
      S.Status := Quarantined;
   end Quarantine;
   procedure Submit (S : in out State; Frame : ID; Started : out Boolean) is
   begin
      Started := Writable (S) and Frame > S.Frame_ID;
      if Started then
         S.Frame_ID := Frame;
         S.Status := In_Flight;
      else
         Quarantine (S);
      end if;
   end Submit;
   procedure Complete (S : in out State; Reply : Completion) is
   begin
      if Releases (S, Reply) then
         S.Status := Available;
      else
         Quarantine (S);
      end if;
   end Complete;
end Compositor_Presentation;
