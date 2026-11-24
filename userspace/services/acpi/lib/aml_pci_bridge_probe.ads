pragma Ada_2022;
with Interfaces;
package AML_PCI_Bridge_Probe with SPARK_Mode, Pure is
   use type Interfaces.Unsigned_8;
   subtype Byte is Interfaces.Unsigned_8;
   type Stage is (Header, Primary, Secondary, Complete);
   type State is private;
   function At_Stage (S : State) return Stage;
   function Is_Bridge (S : State) return Boolean;
   function Containing_Bus (S : State) return Byte;
   function Downstream_Bus (S : State) return Byte;
   function Remaining (S : State) return Natural;
   function Offset (S : State) return Natural
     with Pre => At_Stage (S) /= Complete,
       Post => Offset'Result =
         (case At_Stage (S) is when Header => 16#0E#,
           when Primary => 16#18#, when Secondary => 16#19#,
           when Complete => 0);
   function Bridge_Header (Value : Byte) return Boolean;
   procedure Start (S : out State; Bus : Byte)
     with Post => At_Stage (S) = Header and not Is_Bridge (S)
       and Containing_Bus (S) = Bus and Downstream_Bus (S) = Bus;
   procedure Apply_Byte (S : in out State; Value : Byte)
     with Pre => At_Stage (S) /= Complete,
       Post => Remaining (S) < Remaining (S'Old)
         and then (case At_Stage (S'Old) is
           when Header =>
             Is_Bridge (S) = Bridge_Header (Value)
             and At_Stage (S) = (if Bridge_Header (Value) then Primary else Complete)
             and Containing_Bus (S) = Containing_Bus (S'Old)
             and Downstream_Bus (S) = Downstream_Bus (S'Old),
           when Primary =>
             At_Stage (S) = Secondary and Containing_Bus (S) = Value
             and Downstream_Bus (S) = Downstream_Bus (S'Old)
             and Is_Bridge (S) = Is_Bridge (S'Old),
           when Secondary =>
             At_Stage (S) = Complete and Downstream_Bus (S) = Value
             and Containing_Bus (S) = Containing_Bus (S'Old)
             and Is_Bridge (S) = Is_Bridge (S'Old),
           when Complete => False);
   -- Metadata state only. Caller admits and performs each read, validates its
   -- completion, charges fuel, and passes one byte. Failure never calls Apply_Byte.
private
   type State is record
      Position : Stage := Complete;
      Bridge : Boolean := False;
      Parent_Bus, Child_Bus : Byte := 0;
   end record;
   function At_Stage (S : State) return Stage is (S.Position);
   function Is_Bridge (S : State) return Boolean is (S.Bridge);
   function Containing_Bus (S : State) return Byte is (S.Parent_Bus);
   function Downstream_Bus (S : State) return Byte is (S.Child_Bus);
   function Remaining (S : State) return Natural is
     (case S.Position is when Header => 3, when Primary => 2,
       when Secondary => 1, when Complete => 0);
end AML_PCI_Bridge_Probe;
