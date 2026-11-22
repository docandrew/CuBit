------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Which entry a new publishing channel takes (Publisher_Rings.Open): the
--  same publisher's earlier channel, else a free entry, else a new one if
--  the table may grow; never another publisher's live channel (no lost
--  records: such a publisher would go on writing into a ring nobody
--  reads). Hosted test: tests/log-publisher-slots.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

generic
   --  Entries 1 .. Count exist; Live (I): entry I holds an open channel,
   --  opened by Owner (I) under Authority (I).
   with function Count return Natural;
   with function Live (I : Positive) return Boolean;
   with function Owner (I : Positive) return Unsigned_64;
   with function Authority (I : Positive) return Unsigned_64;
package Publisher_Slots with SPARK_Mode is
   type Choice_Kind is (Same_Publisher, Free_Entry, Grow, No_Room);
   type Choice (Kind : Choice_Kind := No_Room) is record
      case Kind is
         when Same_Publisher | Free_Entry => Index : Positive;
         when Grow | No_Room => null;
      end case;
   end record;

   function Choose (From, With_Authority : Unsigned_64; May_Grow : Boolean) return Choice
   with Post =>
     (case Choose'Result.Kind is
        when Same_Publisher =>
          Choose'Result.Index <= Count and then Live (Choose'Result.Index)
          and then Owner (Choose'Result.Index) = From
          and then Authority (Choose'Result.Index) = With_Authority,
        when Free_Entry =>
          Choose'Result.Index <= Count and then not Live (Choose'Result.Index),
        when Grow => May_Grow and then (for all I in 1 .. Count => Live (I)),
        when No_Room => not May_Grow and then (for all I in 1 .. Count => Live (I)));
end Publisher_Slots;
