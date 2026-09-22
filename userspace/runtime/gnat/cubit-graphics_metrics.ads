with Interfaces; use Interfaces;

--  Cumulative payload bytes, not estimated bus traffic or CPU time. One
--  region may contain several row copies. Overflow invalidates the sample.
package CuBit.Graphics_Metrics with Pure, SPARK_Mode is
   type Stage is (Desktop_Staging, Display_Backend, Display_Repair,
                  GPU_Upload_Request, GPU_Legacy_Copy);
   type Counter is record
      Bytes : Unsigned_64 := 0;
      Regions : Unsigned_64 := 0;
      Overflowed : Boolean := False;
   end record;
   procedure Add (Item : in out Counter; Bytes : Unsigned_64) with
     Post =>
       (if Item'Old.Overflowed or Bytes = 0 then Item = Item'Old
        elsif Bytes > Unsigned_64'Last - Item'Old.Bytes or
              Item'Old.Regions = Unsigned_64'Last
        then Item.Overflowed and Item.Bytes = Item'Old.Bytes and
             Item.Regions = Item'Old.Regions
        else not Item.Overflowed and Item.Bytes = Item'Old.Bytes + Bytes and
             Item.Regions = Item'Old.Regions + 1);
end CuBit.Graphics_Metrics;
