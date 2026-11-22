--  Why a live VM update (0x0A28) was refused while its page tables were
--  being reserved, before anything changed (BUG-001). The VM image, its
--  generation, table metadata and every reservation are still as they were,
--  so a client can fail just that one binding and keep its session.
package Intel_GPU_VM_Refusals with SPARK_Mode, Pure is
   type Cause is
     (Range_Invalid,
      --  No plan for the range: the image changed under the inspection, or
      --  the range is outside the 48-bit space.
      Range_Occupied,
      --  Part of the range is mapped already.
      Table_Ceiling,
      --  The new tables would take the session past its named table ceiling.
      Reservation_Refused);
      --  The tables fit, but their reservation was refused: the session's
      --  byte budget, allocation records or table-metadata growth.

   function Classify (Planned, Occupied, Fits_Ceiling : Boolean) return Cause is
     (if not Planned then (if Occupied then Range_Occupied else Range_Invalid)
      elsif not Fits_Ceiling then Table_Ceiling
      else Reservation_Refused);

   --  A capacity refusal answers the client with the CAPACITY status: retry
   --  after freeing memory, the session is intact. The others keep the
   --  generic Unavailable answer.
   function Is_Capacity (Why : Cause) return Boolean is
     (Why in Table_Ceiling | Reservation_Refused);

   --  For the log line (enumeration images are not kept in the driver).
   function Name (Why : Cause) return String is
     (case Why is
         when Range_Invalid => "range-invalid",
         when Range_Occupied => "range-occupied",
         when Table_Ceiling => "table-ceiling",
         when Reservation_Refused => "reservation-refused");
end Intel_GPU_VM_Refusals;
