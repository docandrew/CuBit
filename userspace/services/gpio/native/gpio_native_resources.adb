pragma Ada_2022;
with GPIO_Kernel_Regions;
package body GPIO_Native_Resources is
   use type P.Address, P.Outcome, T.Map_Outcome, T.Header_Outcome;
   Attempted : Boolean := False;
   procedure Setup (Caps : Authorities; Virtual_First, Virtual_Limit, Page_Limit : P.Address;
      Deadline : I.Tick; Result : out Report) is
      Cursor : P.Address := Virtual_First;
      Description : P.Description;
   begin
      Result := (others => <>);
      if Attempted then Result.Repeated := True; return; end if;
      Attempted := True;
      for Resource in G.Memory_Resource_Ordinal loop
         if CuBit.Handles.Valid (Caps (Resource)) then
            GPIO_Kernel_Regions.Inspect (Caps (Resource), Description);
            Result.Resources (Resource).Description := Description;
            if not Description.Current or else not Description.Device_Memory or else
              not Description.Readable or else not Description.Writable
            then Result.Resources (Resource).Status := Invalid_Authority;
            elsif Cursor >= Virtual_Limit or else Virtual_Limit > P.User_Virtual_Limit or else
              Description.Bytes > Virtual_Limit - Cursor or else
              P.Admit (Description, Description.First, Description.Bytes, Cursor, Page_Limit) /= P.Allowed
            then Result.Resources (Resource).Status := Invalid_Window;
            else
               Result.Resources (Resource).Virtual := Cursor;
               GPIO_Native_Communities.Bind (Resource, Caps (Resource), Description.First,
                  Description.Bytes, Cursor, Page_Limit, Deadline, Result.Resources (Resource).Mapping);
               -- A refused mapping may have had partial effects. Never reuse
               -- this reserved VA interval for another resource, even on failure.
               Cursor := Cursor + Description.Bytes;
               if Result.Resources (Resource).Mapping.Status /= T.Bound then
                  Result.Resources (Resource).Status := Mapping_Refused;
               else
                  GPIO_Native_Communities.Probe (Resource, G.Target_HID, Deadline, Result.Resources (Resource).Header);
                  Result.Resources (Resource).Status :=
                    (if Result.Resources (Resource).Header.Status = T.Probed then Ready else Probe_Refused);
               end if;
            end if;
         end if;
      end loop;
      GPIO_Native_Communities.Freeze (Result.Ready_Count);
   end Setup;
end GPIO_Native_Resources;
