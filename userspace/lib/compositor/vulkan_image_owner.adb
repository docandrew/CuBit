with Vulkan_Image_FFI;
package body Vulkan_Image_Owner with SPARK_Mode is
   package F renames Vulkan_Image_FFI;
   package A renames Accounting;
   use type F.U32, F.U64, A.Ticket;
   procedure Rearm (S : in out State; Budget : A.State;
                    Accepted : out Boolean) is
   begin
      Accepted := S.Mode = Closed and then not A.Current (Budget, S.Ticket);
      if Accepted then
         S := (others => <>);
      end if;
   end Rearm;
   procedure Close_Unbound (S : in out State) is
      Result : F.U32;
   begin
      F.Release (S.Request, Result);
      S.Mode := (if Result = 0 then Closed else Quarantined);
   end Close_Unbound;
   procedure Prepare (S : in out State; Request : System.Address) is
      Bytes : F.U64;
      Result : F.U32;
   begin
      S.Request := Request;
      F.Prepare (Request, Bytes, S.Types, Result);
      if Result = 1 then S.Mode := Closed;
      elsif Result /= 0 then S.Mode := Quarantined;
      elsif Bytes = 0 or Bytes > F.U64 (Natural'Last) or S.Types = 0 then
         Close_Unbound (S);
      else S.Bytes := Natural (Bytes); S.Mode := Prepared;
      end if;
   end Prepare;
   procedure Allocate (S : in out State; Budget : in out A.State;
                       Allowed_Types : U32) is
      Result : F.U32;
      Selected : U32 := 32;
   begin
      for I in Natural range 0 .. 31 loop
         if (S.Types and Allowed_Types and Interfaces.Shift_Left (U32 (1), I)) /= 0 then
            Selected := U32 (I); exit;
         end if;
      end loop;
      if Selected = 32 or S.Bytes = 0 then Close_Unbound (S); return; end if;
      A.Reserve (Budget, S.Bytes, S.Ticket);
      if S.Ticket = A.No_Ticket then Close_Unbound (S); return; end if;
      F.Bind (S.Request, F.U64 (S.Bytes), Selected, Result);
      -- Clean rejection has destroyed the unsubmitted image and any backing.
      -- Unknown errors retain the entire charged amount, never a guessed size.
      A.Allocated (Budget, S.Ticket, Result = 0 or Result = 1);
      if Result = 0 then S.Mode := Live;
      elsif Result = 1 then
         A.Begin_Release (Budget, S.Ticket, True);
         A.Released (Budget, S.Ticket, True);
         S.Mode := Closed;
      else S.Mode := Quarantined;
      end if;
   end Allocate;
   procedure Release (S : in out State; Budget : in out A.State;
                      All_Readers_Retired : Boolean) is
      Result : F.U32;
   begin
      if not All_Readers_Retired then return; end if;
      A.Begin_Release (Budget, S.Ticket, True);
      F.Release (S.Request, Result);
      A.Released (Budget, S.Ticket, Result = 0);
      S.Mode := (if Result = 0 then Closed else Quarantined);
   end Release;
end Vulkan_Image_Owner;
