pragma Ada_2022;
with HID_Report_Schema;
with HID_Field_Usages;
generic
   with package Schema is new HID_Report_Schema (<>);
   with package Names is new HID_Field_Usages (Schema);
package HID_Input_Bindings with SPARK_Mode is
   type Work_Count is range 0 .. 2 ** 32 - 1;
   type Status is (Matched, Missing, Ambiguous, Invalid_Schema,
                   Invalid_Tables, Budget_Exhausted);
   type Binding is record
      Field_Number : Schema.Count := 0;
      Element : Schema.Bit_Count := 0;
   end record;
   procedure Ancestor
     (Layout : Schema.Result; Collections : Schema.Collection_Table;
      Start : Schema.Count; Wanted_Usage : Schema.Word; Wanted_Kind : Schema.Byte;
      Budget : Work_Count; Found : out Schema.Count;
      Charged : out Work_Count; Outcome : out Status);
   -- Closest matching ancestor, including Start. Every visited parent must
   -- have a lower ordinal, as emitted by Parse; validate through the root even
   -- after a match. Missing is a valid absence, never a guessed identity.
   procedure Find
     (Layout : Schema.Result; Fields : Schema.Field_Table;
      Usages : Schema.Usage_Table; Collections : Schema.Collection_Table;
      Under_Collection : Schema.Count; Wanted_Usage : Schema.Word;
      Budget : Work_Count; Item : out Binding;
      Charged : out Work_Count; Outcome : out Status);
   -- Select a unique variable Input element within this collection subtree.
   -- The ordinal is relative to the parser table, independent of array bounds.
   -- Duplicate matches are Ambiguous. No native authority is created here.
   -- Budget charges every field, parent visit, element and usage term examined;
   -- usage-term cost is reserved before calling the pure name resolver.
   -- Item/Found are zero on all non-Matched outcomes; partial results are not
   -- usable. These procedures take tables from a single successful Parse.
end HID_Input_Bindings;
