with Vulkan_Device_FFI;
package body Vulkan_Device_Owner with SPARK_Mode is
   package F renames Vulkan_Device_FFI;
   use type Interfaces.Unsigned_64;
   procedure Start
     (S : in out State; Context : in out C.State;
      Submission : in out V.State; Slot : Interfaces.Unsigned_64)
   is
      Description : System.Address;
      Accepted : Boolean;
   begin
      if S.Mode /= Fresh then return; end if;
      S.Mode := Software;
      if Slot = 0 then return; end if;
      F.Start (Slot, S.Held, Description);
      if not S.Held then
         if Description /= System.Null_Address then S.Mode := Quarantined; end if;
         return;
      end if;
      if Description = System.Null_Address then return; end if;
      S.Context_Attempted := True;
      C.Initialize (Context, Description, Accepted);
      S.Identity := C.Context (Context);
      if Accepted then
         Submission := V.Open (S.Identity);
         S.Mode := Ready;
      elsif C.Current (Context) = C.Quarantined then
         S.Mode := Quarantined;
      end if;
   end Start;
   procedure Check_Health (S : in out State; Usable : out Boolean) is
   begin
      Usable := False;
      if S.Mode /= Ready then return; end if;
      F.Health (Usable);
      if not Usable then S.Mode := Quarantined; end if;
   end Check_Health;
   procedure Close
     (S : in out State; Context : in out C.State; Submission : V.State)
   is
      Released : Boolean;
      Result : F.Retirement;
   begin
      if S.Mode in Fresh | Retired | Quarantined then return; end if;
      if not S.Held then S.Mode := Retired; return; end if;
      -- Reject a substituted or reset context rather than trusting its idle
      -- state. Native storage is never rebound, including after clean failure.
      if C.Context (Context) /= S.Identity or else
        (S.Context_Attempted and then C.Current (Context) = C.Fresh)
      then return; end if;
      if C.Current (Context) = C.Live then
         C.Close (Context, Submission, Released);
         if C.Current (Context) = C.Quarantined then S.Mode := Quarantined; end if;
         if not Released then return; end if;
      end if;
      if C.Current (Context) not in C.Fresh | C.Closed then
         S.Mode := Quarantined;
         return;
      end if;
      S.Mode := Retiring;
      F.Close (Result);
      case Result is
         when F.Retired => S.Mode := Retired; S.Held := False;
         when F.Pending => null;
         when F.Unsafe => S.Mode := Quarantined;
      end case;
   end Close;
end Vulkan_Device_Owner;
