with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Memory_Grants;
with CuBit.Capability_Grants;
with Intel_GPU_Buffer_Views;
with Intel_GPU_Buffer_Handles;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Buffer_Backing;
procedure View_Retention_Tests is
   package V renames Intel_GPU_Buffer_Views;
   package H renames Intel_GPU_Buffer_Handles;
   package G renames CuBit.Memory_Grants;
   use type V.View_State, H.Handle;
   function Backing return Intel_GPU_Buffer_Reply.Backing is
     (Intel_GPU_Buffer_Reply.From_Linear
       (16#2000000#, Intel_GPU_Buffer_Backing.CPU_Base, 4096, 16#2000000#));
   procedure Probe_Share is new V.Share_Completed (Backing);
   OK : Boolean;
begin
   for Invalid in 1 .. 6 loop
      declare
         Pool : H.Registry;
         View : V.View;
         ID : H.Handle;
         Before : constant Natural := G.Creates;
         Offset : constant Unsigned_64 :=
           (case Invalid is when 1 => 1, when 2 => 8192, when others => 0);
         Bytes : constant Unsigned_64 :=
           (case Invalid is when 3 => 0, when 4 => 8192, when others => 4096);
      begin
         H.Register (Pool, 42, Backing, ID);
         CuBit.Capability_Grants.Endpoint_Ready := Invalid /= 5;
         V.Share (View, Pool, 42, ID, 7, 42, Offset, Bytes,
           Writable => Invalid = 6, Presentation => True);
         pragma Assert (G.Creates = Before and V.State (View) = V.Retired);
         pragma Assert (V.Wire_Reference (View) = 0);
         H.Close_Session (Pool, 42);
         H.Release_Retired_Backing (Pool, 42, ID, True, OK);
         pragma Assert (OK);
         V.Recycle (View, OK); pragma Assert (OK);
      end;
   end loop;
   CuBit.Capability_Grants.Endpoint_Ready := True;
   for Scenario in 1 .. 6 loop
      declare
         Pool, Other : H.Registry;
         A, B : V.View;
         ID, Other_ID, Replacement : H.Handle;
      begin
         G.Create_OK := True; G.Revoke_OK := True; G.Gone := False;
         H.Register (Pool, 42, Backing, ID);
         H.Register (Other, 42, Backing, Other_ID);
         pragma Assert (ID = Other_ID);
         if Scenario = 3 then G.Create_OK := False; end if;
         V.Share (A, Pool, 42, ID, 7, 42, 0, 4096, False, True);
         pragma Assert (V.State (A) = (if Scenario = 3 then V.Failed else V.Shared));
         if Scenario = 1 then
            V.Share (B, Pool, 42, ID, 7, 42, 0, 4096, False);
            pragma Assert (V.State (B) = V.Shared);
         end if;
         H.Close_Session (Pool, 42);
         H.Release_Retired_Backing (Pool, 42, ID, True, OK);
         pragma Assert (not OK);
         if Scenario = 4 then G.Revoke_OK := False; end if;
         V.Retire (A, Pool);
         pragma Assert (V.Wire_Reference (A) = 0);
         V.Recycle (A, OK); pragma Assert (not OK);
         H.Replace_Retired (Pool, 42, 43, ID, Backing, True, Replacement);
         pragma Assert (Replacement = H.No_Handle);
         G.Gone := True;
         -- The no-registry path cannot silently discard a BO pin.
         V.Poll_Retirement (A);
         pragma Assert (V.State (A) /= V.Retired);
         if Scenario = 2 then
            V.Poll_Retirement (A, Other); -- wrong root, same name/session
         elsif Scenario = 5 then
            H.Quarantine (Pool);
            V.Poll_Retirement (A, Pool);
         else
            V.Poll_Retirement (A, Pool);
         end if;
         if Scenario in 2 .. 5 then
            pragma Assert (V.State (A) = V.Failed);
            V.Poll_Retirement (A, Pool);
            H.Release_Retired_Backing (Pool, 42, ID, True, OK);
            pragma Assert (not OK);
         else
            pragma Assert (V.State (A) = V.Retired);
            if Scenario = 1 then
               H.Release_Retired_Backing (Pool, 42, ID, True, OK);
               pragma Assert (not OK); -- second reader still owns a pin
               V.Retire (B, Pool);
               pragma Assert (V.State (B) = V.Retired);
            end if;
            V.Poll_Retirement (A, Pool); -- no double return
            H.Release_Retired_Backing (Pool, 42, ID, True, OK);
            pragma Assert (OK);
            V.Recycle (A, OK); pragma Assert (OK);
            H.Replace_Retired (Pool, 42, 43, ID, Backing, True, Replacement);
            pragma Assert (Replacement /= H.No_Handle);
            V.Share (A, Pool, 43, Replacement, 7, 42, 0, 4096, True);
            pragma Assert (V.State (A) = V.Shared);
            V.Retire (A, Pool); pragma Assert (V.State (A) = V.Retired);
         end if;
      end;
   end loop;
   for Scenario in 1 .. 8 loop
      declare
         Pool, Other : H.Registry;
         Source, Empty : H.Retained_Reference;
         Reader : V.View;
         ID : H.Handle;
         Before : constant Natural := G.Creates;
         Forwards : constant Natural := G.Forwardable_Creates;
      begin
         G.Create_OK := Scenario /= 6;
         G.Revoke_OK := Scenario /= 7;
         G.Gone := False;
         CuBit.Capability_Grants.Endpoint_Ready := Scenario /= 5;
         H.Register (Pool, 42, Backing, ID);
         H.Retain_Backing (Pool, 42, ID, Source, OK); pragma Assert (OK);
         H.Close_Session (Pool, 42);
         pragma Assert (not H.Resolve (Pool, 42, ID).Ready);
         if Scenario = 2 then
            V.Share_Retained (Reader, Other, Source, 7, 42, 0, 4096);
         elsif Scenario = 3 then
            V.Share_Retained (Reader, Pool, Empty, 7, 42, 0, 4096);
         else
            V.Share_Retained (Reader, Pool, Source, 7, 42,
              (if Scenario = 4 then 1 else 0), 4096);
         end if;
         pragma Assert (G.Forwardable_Creates = Forwards);
         pragma Assert (G.Creates = Before + (if Scenario in 1 | 6 .. 8 then 1 else 0));
         if Scenario in 1 | 6 .. 8 then pragma Assert (not G.Last_Writable); end if;
         H.Return_Reference (Pool, Source, True, OK); pragma Assert (OK);
         -- The independent reader must survive producer closure/pin return.
         H.Release_Retired_Backing (Pool, 42, ID, True, OK);
         pragma Assert (OK = (Scenario in 2 .. 5));
         if Scenario in 1 | 6 .. 8 then
            if Scenario = 8 then H.Quarantine (Pool); end if;
            V.Retire (Reader, Pool);
            pragma Assert (V.Wire_Reference (Reader) = 0);
            H.Release_Retired_Backing (Pool, 42, ID, True, OK);
            pragma Assert (not OK);
            G.Gone := True;
            V.Poll_Retirement (Reader, Pool);
            H.Release_Retired_Backing (Pool, 42, ID, True, OK);
            pragma Assert (OK = (Scenario = 1));
            pragma Assert (V.State (Reader) = (if Scenario = 1 then V.Retired else V.Failed));
         end if;
         V.Share_Retained (Reader, Pool, Source, 7, 42, 0, 4096);
         pragma Assert (G.Creates = Before + (if Scenario in 1 | 6 .. 8 then 1 else 0));
      end;
   end loop;
   CuBit.Capability_Grants.Endpoint_Ready := True;
   Ada.Text_IO.Put_Line ("Retained reader handoff PASS: closed producer, independent pin, read-only nonforwardable, rejection and uncertainty retention");
   declare Probe : V.View; begin
      G.Create_OK := True; G.Revoke_OK := True; G.Gone := True;
      Probe_Share (Probe, 7, 42);
      pragma Assert (V.State (Probe) = V.Shared);
      V.Retire (Probe);
      pragma Assert (V.State (Probe) = V.Retired);
   end;
   Ada.Text_IO.Put_Line ("CPU export retention PASS: pending, multiple readers, closure, wrong root, creation/revoke failure, quarantine, reuse, probe");
end View_Retention_Tests;
