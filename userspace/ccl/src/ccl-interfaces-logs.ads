with Interfaces;
with CCL.Catalog;
with CCL.Objects;
with CCL.Types;

--  The logs interface: (logs.recent "service") returns the recent records of
--  one service as a typed List<LogEntry> (TYPE_SOURCE). (logs.minimum)
--  says the least severe record logstore keeps; (logs.set-minimum
--  Severity.Debug) changes it, with log-control, and returns the previous
--  one. A host (the Workbench, the console) answers from logstore; this
--  package holds only the types, the catalog publication and the images.
package CCL.Interfaces.Logs with SPARK_Mode is
   use Standard.Interfaces;

   --  The types, as CCL source: checked by the CCL type checker when the
   --  interface is published. The digest and keys are SHA-256 of it (then
   --  "#" and the type's name); tests/ccl-console checks them.
   TYPE_SOURCE : constant String :=
     "(type Severity (enum Trace Debug Information Warning Error Critical)) (type LogEntry " &
     "(record (time Integer) (severity Severity) (source Integer) (message String)))";
   DIGEST : constant CCL.Catalog.Descriptor_Digest :=
     [16#37C7_1F64_3948_DFB1#,
      16#2182_30CA_744A_B455#,
      16#C700_9ED2_A2EF_AD49#,
      16#BDFB_2CDF_15BF_2E88#];
   SCHEMA_KEY : constant CCL.Objects.Schema_Key :=
     [16#1602_9AD9_358E_2388#,
      16#DE1C_3F38_85A6_8C95#,
      16#2CE6_4265_B64F_E2BA#,
      16#0BC1_9DD9_CF76_CD66#];

   SEVERITY_KEY : constant CCL.Objects.Schema_Key :=
     [16#A440_5E89_83D9_7C3D#,
      16#0BF8_FB3B_C650_881E#,
      16#907F_B936_0AC2_EBE7#,
      16#4C5F_5247_2B52_0191#];

   --  Severity's members, in order (CuBit.Log_Records.Severity).
   type Severity is (Trace, Debug, Information, Warning, Error, Critical);

   --  A service name in (logs.recent "name").
   MAX_SERVICE_NAME : constant := 64;

   --  Cells one entry takes in an image: the record, its time, its severity
   --  (member and unit payload), its source and its message.
   ENTRY_CELLS : constant := 6;
   --  The entries one result can carry: the list's count cell, then entries.
   MAX_ENTRIES : constant := (CCL.Objects.Maximum_Cells - 1) / ENTRY_CELLS;
   subtype Entry_Count is Natural range 0 .. MAX_ENTRIES;

   --  Severity, LogEntry and List-LogEntry in Types, and the bindings of the
   --  list type to SCHEMA_KEY and of Severity to SEVERITY_KEY.
   procedure Define_Types
     (Types : in out CCL.Types.Registry; Entries : out CCL.Types.Type_Reference;
      Contract, Severity_Contract : out CCL.Objects.Binding; Accepted : out Boolean);

   type Operation is (Recent, Minimum, Set_Minimum);
   function Name (Op : Operation) return String is
     (case Op is when Recent => "recent", when Minimum => "minimum", when Set_Minimum => "set-minimum");

   --  The schema and the logs interface (recent), for discovery only: a host
   --  separately installs a binding for the operations it answers.
   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error);

   --  A Severity value, and the member a Severity value holds.
   procedure Severity_Value
     (Contract : CCL.Objects.Binding; Level : Severity; Result : out CCL.Objects.Image; Built : out Boolean);
   procedure Severity_Of
     (Contract : CCL.Objects.Binding; Value : CCL.Objects.Image; Level : out Severity; Found : out Boolean);

   --  Building a result: Start an empty list, Add entries while Added, the
   --  image is then complete and valid for the contract.
   procedure Start (Contract : CCL.Objects.Binding; Image : out CCL.Objects.Image);
   procedure Add
     (Image : in out CCL.Objects.Image; Time : Unsigned_64; Level : Severity;
      Source : Unsigned_64; Message : String; Added : out Boolean);
end CCL.Interfaces.Logs;
