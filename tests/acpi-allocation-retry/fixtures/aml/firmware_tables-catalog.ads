pragma Ada_2022;

-- Metadata for immutable, admitted SDTs. This is not a memory grant: the raw
-- adapter must validate backing and pin it for the lifetime of any consumer.
package Firmware_Tables.Catalog with SPARK_Mode, Pure is
   use type Address_Value;
   Max_Tables : constant := 256;
   -- Representation bound only; consumers apply their own allocation quota.
   subtype Table_Length is Positive range Table_Header_Size .. Positive'Last;
   type Descriptor is record
      Physical : Root_Address := 1;
      Extent : Table_Length := Table_Header_Size;
      Name : Signature := "    ";
      Revision : Byte := 0;
   end record;
   type Phase is (Receiving, Ready, Failed);
   type State is private;
   function Current (S : State) return Phase;
   function Count (S : State) return Natural
     with Post => Count'Result <= Max_Tables
       and then (if Current (S) /= Ready then Count'Result = 0);
   function Fits (D : Descriptor) return Boolean is
     (Address_Value (D.Extent - 1) <= Address_Value'Last - D.Physical);
   function Item (S : State; Index : Positive) return Descriptor
     with Pre => Index <= Count (S),
       Post => Fits (Item'Result)
         and then Item'Result.Name /= "FACS"
         and then (if Index = 1 then Item'Result.Name = "DSDT"
                   else Item'Result.Name /= "DSDT");
   procedure Reset (S : out State)
     with Post => Current (S) = Receiving and then Count (S) = 0;

   function Other_Count (S : State) return Natural with Ghost;
   function Other (S : State; Index : Positive) return Descriptor
     with Ghost, Pre => Index <= Other_Count (S);
   procedure Reject (S : in out State)
     with Post => Current (S) = Failed and then Count (S) = 0;

   -- DSDT is always exported first. Other tables retain discovery order.
   -- Re-observing the identical DSDT is harmless; conflicting DSDTs fail closed.
   -- FACS is live shared state and must never enter this immutable inventory.
   procedure Include (S : in out State; D : Descriptor)
     with Post => Current (S) /= Ready and then Count (S) = 0
       and then (if Current (S'Old) = Failed then Current (S) = Failed)
       and then
         (if Current (S'Old) = Receiving and then Fits (D)
           and then D.Name /= "DSDT" and then D.Name /= "FACS"
           and then Other_Count (S'Old) < Max_Tables - 1
          then Current (S) = Receiving
            and then Other_Count (S) = Other_Count (S'Old) + 1
            and then Other (S, Other_Count (S)) = D
            and then (for all I in 1 .. Other_Count (S'Old) =>
              Other (S, I) = Other (S'Old, I)));
   -- Called only after the entire boot discovery succeeds. Missing DSDT,
   -- capacity exhaustion and conflicting descriptors cannot publish a prefix.
   procedure Seal (S : in out State)
     with Post => Current (S) /= Receiving
       and then (if Current (S'Old) = Failed then Current (S) = Failed)
       and then (if Current (S) = Ready then
         Count (S) = Other_Count (S'Old) + 1
         and then (for all I in 1 .. Other_Count (S'Old) =>
           Item (S, I + 1) = Other (S'Old, I)));
private
   subtype Other_Descriptor is Descriptor
     with Dynamic_Predicate => Fits (Other_Descriptor)
       and then Other_Descriptor.Name /= "DSDT"
       and then Other_Descriptor.Name /= "FACS";
   subtype DSDT_Descriptor is Descriptor
     with Dynamic_Predicate => Fits (DSDT_Descriptor)
       and then DSDT_Descriptor.Name = "DSDT";
   type Entries is array (Positive range 1 .. Max_Tables - 1) of Other_Descriptor;
   type State is record
      Mode : Phase := Receiving;
      Has_DSDT : Boolean := False;
      DSDT : DSDT_Descriptor := (Name => "DSDT", others => <>);
      Used : Natural range 0 .. Max_Tables - 1 := 0;
      Tables : Entries;
   end record with Type_Invariant => (if State.Mode = Ready then State.Has_DSDT);
end Firmware_Tables.Catalog;
