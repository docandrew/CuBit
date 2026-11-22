------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  What CuBit.Block_Driver hands a block driver's hardware hooks, and what
--  the hooks hand back (DEV-002, docs/device-classes.md).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Block_Requests; use CuBit.Block_Requests;

package CuBit.Block_Work with SPARK_Mode is

   --  A block transfer holds the caller's grant; a cache flush holds none.
   type Work_Kind is (Block_Transfer, Cache_Flush);

   type Work_Order (Kind : Work_Kind := Cache_Flush) is record
      case Kind is
         when Block_Transfer =>
            --  Valid_Transfer holds for Granted.Item against the device as
            --  the sender may use it; Granted.Buffer maps Bytes (Item).
            Granted : Loan;
         when Cache_Flush =>
            null;
      end case;
   end record;

   --  A finished command. Moved only matters for a transfer: the bytes that
   --  really completed from the caller's view (for a read, what is in the
   --  caller's grant; for a write, what reached the device). A transfer
   --  that is not Passed or is short is an error reporting that prefix; a
   --  failed command never reports the whole transfer. A read's data must
   --  be in the caller's grant before Collect returns.
   type Work_Outcome is record
      Passed : Boolean := False;
      Moved  : Unsigned_64 := 0;
   end record;

   --  Who may write. Write_Grant_Required: only a request carrying the
   --  write grant's badge (Write_Authority_Tag); everyone else sees the
   --  device read only. Any_Client_Writes: every client that reaches the
   --  driver may write (drivers devmgr does not yet mint write badges for).
   type Write_Admission is (Write_Grant_Required, Any_Client_Writes);

   --  Counted events the driver may log (Note is called on the first and
   --  then on every doubling, never per occurrence).
   type Driver_Event is
     (Write_Refused,     --  a write without the write grant (when required)
      Command_Expired);  --  a command outlived its deadline and was cancelled
end CuBit.Block_Work;
