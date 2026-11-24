pragma Ada_2022;
package body AML_PCI_Bridge_Probe with SPARK_Mode is
   function Bridge_Header (Value : Byte) return Boolean is
     ((Value and 16#7F#) in 1 .. 2);
   function Offset (S : State) return Natural is
     (case S.Position is when Header => 16#0E#,
       when Primary => 16#18#, when Secondary => 16#19#,
       when Complete => 0);
   procedure Start (S : out State; Bus : Byte) is
   begin
      S := (Header, False, Bus, Bus);
   end Start;
   procedure Apply_Byte (S : in out State; Value : Byte) is
   begin
      case S.Position is
         when Header =>
            S.Bridge := Bridge_Header (Value);
            S.Position := (if S.Bridge then Primary else Complete);
         when Primary => S.Parent_Bus := Value; S.Position := Secondary;
         when Secondary => S.Child_Bus := Value; S.Position := Complete;
         when Complete => null;
      end case;
   end Apply_Byte;
end AML_PCI_Bridge_Probe;
