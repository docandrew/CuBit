with Vulkan_Context_FFI;
package body Vulkan_Context_Owner with SPARK_Mode is
   package F renames Vulkan_Context_FFI;
   use type F.Code;
   procedure Initialize (S : in out State; Description : System.Address; Accepted : out Boolean) is
      Result : F.Code;
   begin
      S.Request := Description;
      F.Create (Description, S.Borrowed, Result);
      if Result = 1 then S.Mode := Closed;
      elsif Result = 0 and S.Borrowed /= System.Null_Address then S.Mode := Live;
      else S.Mode := Quarantined;
      end if;
      Accepted := S.Mode = Live;
   end Initialize;
   procedure Register_Child (S : in out State; Ticket : out Child) is
   begin
      Ticket := No_Child;
      if S.Mode /= Live or S.Last = Serial'Last then return; end if;
      for N in Slot loop
         if S.Children (N) = 0 then
            S.Last := S.Last + 1;
            S.Children (N) := S.Last;
            Ticket := (S.Borrowed, N, S.Last);
            return;
         end if;
      end loop;
   end Register_Child;
   procedure Retire_Child (S : in out State; Ticket : Child; Native_Retired : Boolean) is
   begin
      if Native_Retired and then Held (S, Ticket) then S.Children (Ticket.Index) := 0; end if;
   end Retire_Child;
   procedure Close (S : in out State; Submission : V.State; Released : out Boolean) is
      Result : F.Code;
   begin
      Released := False;
      if not Can_Close (S, Submission) then return; end if;
      F.Release (S.Request, Result);
      if Result = 0 then S.Mode := Closed; Released := True;
      else S.Mode := Quarantined;
      end if;
   end Close;
end Vulkan_Context_Owner;
