with Interfaces; use Interfaces;
with CuBit.Messages;
with Intel_GPU_Metadata_Platform;
with Intel_GPU_Record_Store;
with Intel_GPU_Record_Growth;
package body GPU_Metadata_Probe is
   Attempted : Boolean := False;
   function Run return Interfaces.C.int is
   -- This fixture has a C entry point, not an Ada binder-generated main.
   -- Elaborate generic instances and their typed state on entry rather than
   -- relying on a package-body elaboration routine the C loader never calls.
   type Entry_Record is record
      Identity : Unsigned_64 := 73;
      Owner : Unsigned_64 := 99;
   end record;
   package R is new Intel_GPU_Record_Store (Entry_Record, (others => <>));
   Registry : R.Store;
   function Capacity return Positive is (R.Capacity (Registry));
   procedure Publish (Base, Bytes : Unsigned_64; Accepted : out Boolean) is
   begin R.Extend (Registry, Base, Bytes, Accepted); end Publish;
   package G is new Intel_GPU_Record_Growth
     (Intel_GPU_Metadata_Platform.Storage, Capacity, Publish);
   use type G.Phase;
   Controller : G.Controller;
      OK : Boolean;
      Before : Positive;
      function Fail (Text : String) return Interfaces.C.int is
      begin
         CuBit.Messages.debugPrint ("TEST: FAIL native GPU metadata: " & Text & ASCII.LF);
         return 1;
      end Fail;
   begin
      if Attempted then return Fail ("replayed probe"); end if;
      Attempted := True;
      G.Configure (Controller, 1024 * 1024, 40000, OK);
      if not OK then return Fail ("configure"); end if;
      for I in 1 .. Capacity loop R.Put (Registry, I, (Unsigned_64 (I), 123)); end loop;
      for Round in 1 .. 2 loop
         Before := Capacity;
         G.Request (Controller, Round * 10000, OK);
         if not OK then return Fail ("request"); end if;
         for Turn in 1 .. 128 loop
            G.Step (Controller);
            exit when G.Snapshot (Controller).State in G.Idle | G.Failed;
         end loop;
         if G.Snapshot (Controller).State /= G.Idle or else Capacity < Round * 10000 then
            return Fail ("growth incomplete");
         end if;
         for I in 1 .. Before loop
            if R.Get (Registry, I) /= (Unsigned_64 (I), 123) then
               return Fail ("old record changed");
            end if;
         end loop;
         for I in Before + 1 .. Capacity loop
            if R.Get (Registry, I) /= (73, 99) then return Fail ("typed default"); end if;
            R.Put (Registry, I, (Unsigned_64 (I), 123));
         end loop;
      end loop;
      G.Request (Controller, 40001, OK);
      if OK then return Fail ("quota accepted"); end if;
      CuBit.Messages.debugPrint
        ("TEST: PASS native GPU metadata growth 20000 records (NO GPU)" & ASCII.LF);
      -- Intentionally retain registry and reservation for process lifetime.
      return 0;
   end Run;
end GPU_Metadata_Probe;
