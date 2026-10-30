with Interfaces;

--  Private devmgr -> nvme.drv startup protocol: the first message the
--  driver receives, accepted only from the registered devmgr. devmgr has
--  programmed the controller's MSI-X table itself and installed the vectors'
--  interrupt capability in the driver's manifest-named slot
--  (docs/msi-interrupts.md); the driver never writes the table.
package CuBit.NVMe_Control with SPARK_Mode => On is
   use Interfaces;
   type Operation is (Configure_Interrupts, Configure_Polled);
   for Operation use
     (Configure_Interrupts => 16#0420#, Configure_Polled => 16#0421#);
   --  Neither operation has words. Configure_Interrupts: completion queues
   --  interrupt through message Completion_Message (an MSI-X entry, or
   --  the single MSI message). Configure_Polled: completions are polled.
   --  Either way devmgr drops the function mask only after a REPLY_OK.
   Completion_Message : constant Unsigned_32 := 0;
end CuBit.NVMe_Control;
