pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Metric_Records;

--  metrics.svc IPC protocol. Authority tags are kernel-stamped from the
--  capability procmgr minted; knowing these numeric values grants nothing.
package CuBit.Metric_Protocol with Pure, SPARK_Mode is
   package Records renames CuBit.Metric_Records;

   Publisher_Service_Role : constant Unsigned_64 := 24;
   Observer_Service_Role : constant Unsigned_64 := 25;

   Family_Mask : constant Unsigned_64 := 16#FFFF_FFFF_0000_0000#;
   Issuance_Mask : constant Unsigned_64 := 16#0000_0000_FFFF_FFFF#;
   Publisher_Tag_Base : constant Unsigned_64 := 16#4D45_5000_0000_0000#;
   Observer_Tag_Base : constant Unsigned_64 := 16#4D45_4F00_0000_0000#;
   subtype Issuance is Unsigned_64 range 1 .. Issuance_Mask;

   function Publisher_Tag (Item : Issuance) return Unsigned_64 is
     (Publisher_Tag_Base + Item);
   function Observer_Tag (Item : Issuance) return Unsigned_64 is
     (Observer_Tag_Base + Item);
   function Is_Publisher (Tag : Unsigned_64) return Boolean is
     ((Tag and Family_Mask) = Publisher_Tag_Base and then
      (Tag and Issuance_Mask) /= 0);
   function Is_Observer (Tag : Unsigned_64) return Boolean is
     ((Tag and Family_Mask) = Observer_Tag_Base and then
      (Tag and Issuance_Mask) /= 0);

   type Operation is (Publish_Batch, Query_Summaries, Query_Raw);
   for Operation use (Publish_Batch => 16#0D00#, Query_Summaries => 16#0D01#, Query_Raw => 16#0D02#);

   type Status is (OK, Denied, Invalid_Request, Exhausted, Unavailable);
   for Status use
     (OK => 16#F000#, Denied => 16#F002#, Invalid_Request => 16#F003#,
      Exhausted => 16#F004#, Unavailable => 16#F007#);

   --  Every request and reply has four words, zero flags and reserved.
   Message_Words : constant := 4;
   --  Publish_Batch: read-only grant slot, generation, batch bytes, zero.
   --    OK reply: accepted records, rejected records, zero, zero.
   --  Query_Summaries: series cursor, writable grant slot, generation,
   --    capacity (Records.Page_Bytes).
   --    OK reply: rows written, next cursor, total series slots, zero.
   --  Buffers belong to clients; the service returns acquisitions before
   --  replying and decodes only private copies.

   function May_Invoke (Tag : Unsigned_64; Op : Operation) return Boolean is
     (case Op is
         when Publish_Batch => Is_Publisher (Tag),
         when Query_Summaries | Query_Raw => Is_Observer (Tag));

   --  Raw query is observer-only. Same grant layout as Query_Summaries:
   --  request [next desired sequence, slot, generation, 4096]. Cursor starts1.
   --  reply [rows, next sequence, overwritten gap, terminal history drops].
   --  Cursor lifetime is tied to the service endpoint incarnation; reacquiring
   --  an endpoint requires a fresh cursor. Never merge histories across restart.
   --  Rows carry kernel-authenticated publisher identity, not caller payload IDs.
   Raw_Row_Words : constant := 16;
   Raw_Rows_Per_Page : constant := 32;
   subtype Raw_Word_Index is Natural range 0 .. Raw_Row_Words - 1;
   type Raw_Row is array (Raw_Word_Index) of Unsigned_64;
   subtype Raw_Row_Count is Natural range 0 .. Raw_Rows_Per_Page;
   subtype Raw_Row_Index is Raw_Row_Count range 0 .. Raw_Rows_Per_Page - 1;
   type Raw_Page is array (Raw_Row_Index) of Raw_Row;
   --  words0..5: sequence, PID, publisher tag, batch, producer drops, batch gaps.
   --  words6..7 reserved zero; words8..15 exact existing metric slot encoding.

   --  Summary rows: 32 words (256 bytes); 16 rows fill one 4 KiB page.
   Row_Words : constant := 32;
   Rows_Per_Page : constant := Records.Page_Bytes /
     (Row_Words * Records.Bytes_Per_Word);
   subtype Row_Word_Index is Natural range 0 .. Row_Words - 1;
   type Summary_Row is array (Row_Word_Index) of Unsigned_64;
   subtype Row_Count is Natural range 0 .. Rows_Per_Page;
   subtype Row_Index is Row_Count range 0 .. Rows_Per_Page - 1;
   type Summary_Page is array (Row_Index) of Summary_Row;

   Row_Source : constant Row_Word_Index := 0;
   Row_Publisher_Tag : constant Row_Word_Index := 1;
   Row_Key : constant Row_Word_Index := 2;
   Row_Kind : constant Row_Word_Index := 3;
   Row_Unit : constant Row_Word_Index := 4;
   Row_Count_Word : constant Row_Word_Index := 5;
   Row_Minimum : constant Row_Word_Index := 6;
   Row_Maximum : constant Row_Word_Index := 7;
   Row_P50 : constant Row_Word_Index := 8;
   Row_P90 : constant Row_Word_Index := 9;
   Row_P95 : constant Row_Word_Index := 10;
   Row_P99 : constant Row_Word_Index := 11;
   Row_P999 : constant Row_Word_Index := 12;
   --  Counter: saturating total. Gauge: latest value. Others: sum.
   Row_Total : constant Row_Word_Index := 13;
   Row_Flags : constant Row_Word_Index := 14;
   Row_Series_Rejected : constant Row_Word_Index := 15;
   Row_Last_Time : constant Row_Word_Index := 16;
   Row_Source_Batches : constant Row_Word_Index := 17;
   Row_Source_Batch_Gaps : constant Row_Word_Index := 18;
   Row_Source_Producer_Dropped : constant Row_Word_Index := 19;
   Row_Source_Rejected : constant Row_Word_Index := 20;
   Row_First_Name : constant Row_Word_Index := 24;

   Flag_Total_Saturated : constant Unsigned_64 := 1;
   Flag_Histogram_Saturated : constant Unsigned_64 := 2;

   --  Percentiles are reported in permille of the sample count.
   subtype Permille is Natural range 1 .. 1000;
   P50 : constant Permille := 500;
   P90 : constant Permille := 900;
   P95 : constant Permille := 950;
   P99 : constant Permille := 990;
   P999 : constant Permille := 999;
end CuBit.Metric_Protocol;
