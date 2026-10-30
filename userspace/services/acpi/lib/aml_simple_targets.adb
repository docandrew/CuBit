pragma Ada_2022;
package body AML_Simple_Targets with SPARK_Mode is
   function Read_Target (Data : AML_Decode.Bytes) return Target_Result is
      Name : AML_Names.Name_Result;
      First : AML_Decode.Byte;
   begin
      if Data'Length = 0 then return (Kind => Truncated, Consumed => 0); end if;
      First := Data (Data'First);
      if First in 16#60# .. 16#67# then
         return (Kind => Local_Target, Consumed => 1, Slot => Natural (First - 16#60#));
      elsif First in 16#68# .. 16#6E# then
         return (Kind => Argument_Target, Consumed => 1, Slot => Natural (First - 16#68#));
      end if;
      Name := AML_Names.Read_Name (Data);
      case Name.Kind is
         when AML_Names.Accepted =>
            return (Kind => Name_Target, Consumed => Name.Consumed, Path => Name);
         when AML_Names.Truncated => return (Kind => Truncated, Consumed => 0);
         when AML_Names.Malformed => return (Kind => Malformed, Consumed => 0);
         when AML_Names.Limit_Exceeded => return (Kind => Limit_Exceeded, Consumed => 0);
      end case;
   end Read_Target;
end AML_Simple_Targets;
