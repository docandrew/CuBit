with Interfaces; use Interfaces;
with Process_Memory_Budget; use Process_Memory_Budget;
with Ada.Text_IO;
procedure Process_Memory_Contention_Test is
   protected Accounting is
      procedure Configure;
      procedure Take (Kind : Charge_Kind; OK : out Boolean);
      procedure Give (Kind : Charge_Kind);
      procedure Fault;
      procedure Snapshot (Count, Outstanding : out Unsigned_64; Bad : out Boolean);
   private
      State : Ledger;
      Successes : Unsigned_64 := 0;
      Broken : Boolean := False;
   end Accounting;
   protected body Accounting is
      procedure Configure is
         OK : Boolean;
      begin
         Adopt (State, 4, OK);
         Broken := not OK;
      end Configure;
      procedure Take (Kind : Charge_Kind; OK : out Boolean) is
      begin
         Reserve (State, Kind, 1, OK);
         if OK then Successes := Successes + 1; end if;
         Broken := Broken or Used (State) > 4;
      end Take;
      procedure Give (Kind : Charge_Kind) is
         OK : Boolean;
      begin
         Release (State, Kind, 1, OK);
         Broken := Broken or not OK or Used (State) > 4;
      end Give;
      procedure Fault is
      begin Broken := True; end Fault;
      procedure Snapshot (Count, Outstanding : out Unsigned_64; Bad : out Boolean) is
      begin Count := Successes; Outstanding := Used (State); Bad := Broken; end Snapshot;
   end Accounting;
   Count, Outstanding : Unsigned_64;
   Bad : Boolean;
begin
   Accounting.Configure;
   declare
      task type Worker;
      task body Worker is
         OK : Boolean;
      begin
         for Turn in 1 .. 2000 loop
            for Kind in Charge_Kind loop
               Accounting.Take (Kind, OK);
               if OK then
                  delay 0.0;
                  Accounting.Give (Kind);
               end if;
            end loop;
         end loop;
      exception when others => Accounting.Fault;
      end Worker;
      Workers : array (1 .. 8) of Worker;
   begin
      null; -- Task master waits for every worker before observing the ledger.
   end;
   Accounting.Snapshot (Count, Outstanding, Bad);
   if Bad or Outstanding /= 0 or Count = 0 then
      raise Program_Error with "concurrent ledger accounting failed";
   end if;
   Ada.Text_IO.Put_Line ("PASS common ledger contention: 8 hosted tasks, 48000 attempts, no oversubscription or leaked charge");
end Process_Memory_Contention_Test;
