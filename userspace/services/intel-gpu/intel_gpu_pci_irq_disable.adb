with Interfaces; use Interfaces;
with Intel_GPU_PCI_Interrupts;
package body Intel_GPU_PCI_IRQ_Disable is
   use Intel_GPU_PCI_Power;
   Value : Phase := Fresh;
   function State return Phase is (Value);
   function Admitted (Data : Configuration) return Boolean is
     (Data (0) = 16#86# and then Data (1) = 16#80# and then Data (2) = 16#D2# and then
      Data (3) = 16#46# and then Decode (Data) = D0);
   function Matches (Expected, Actual : Configuration) return Boolean is
     ((Expected (6) and 16#10#) = (Actual (6) and 16#10#) and then
      (for all I in Configuration'Range =>
         (if I not in 6 .. 7 then Expected (I) = Actual (I))));
   procedure Execute (Owner_Ready : Boolean; Status : out Result) is
      use Intel_GPU_PCI_Interrupts;
      Expected, Actual : Configuration;
      Plan : Disable_Plan;
      IRQ : Snapshot;
      OK : Boolean;
   begin
      Status := Rejected;
      if Value /= Fresh or else not Owner_Ready then return; end if;
      Value := Consumed_No_Writes;
      Read_Config (Expected, OK);
      if not OK then Status := Read_Failed; return; end if;
      if not Admitted (Expected) then Status := Invalid_Device; return; end if;
      Plan := Plan_Disable (Expected);
      if not Valid (Plan) then Status := Invalid_Plan; return; end if;
      for I in 1 .. Count (Plan) loop
         Read_Config (Actual, OK);
         if not OK then Status := Read_Failed; return; end if;
         if not Admitted (Actual) or else not Matches (Expected, Actual) then
            Status := Configuration_Changed; return;
         end if;
         -- Mark uncertainty BEFORE calling a writer: failure can be reported
         -- after the PCI device accepted the transaction.
         Value := Uncertain;
         Write_Word (Offset (Plan, I), After (Plan, I), OK);
         if not OK then Status := Write_Failed; return; end if;
         Expected (Offset (Plan, I)) := Unsigned_8 (After (Plan, I) and 255);
         Expected (Offset (Plan, I) + 1) := Unsigned_8 (Shift_Right (After (Plan, I), 8));
      end loop;
      Read_Config (Actual, OK);
      if not OK then Status := Read_Failed; return; end if;
      if not Admitted (Actual) or else not Matches (Expected, Actual) then
         Status := Verification_Failed; return;
      end if;
      IRQ := Intel_GPU_PCI_Interrupts.Decode (Actual);
      if not IRQ.Valid or else not IRQ.INTx_Disabled or else IRQ.MSI_Enabled or else
        IRQ.MSIX_Enabled or else (IRQ.MSIX_Present and not IRQ.MSIX_Masked)
      then Status := Verification_Failed; return; end if;
      Value := PCI_Disabled;
      Status := Complete;
   end Execute;
end Intel_GPU_PCI_IRQ_Disable;
