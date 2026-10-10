pragma Ada_2022;
with AML_Decode;
with AML_Names;
package AML_Simple_Targets with SPARK_Mode, Pure is
   use type AML_Decode.Byte;
   type Target_Kind is (Local_Target, Argument_Target, Name_Target,
                        Truncated, Malformed, Limit_Exceeded);
   subtype Accepted_Name is AML_Names.Name_Result (AML_Names.Accepted);
   type Target_Result (Kind : Target_Kind := Truncated) is record
      Consumed : Natural := 0;
      case Kind is
         when Local_Target | Argument_Target => Slot : Natural range 0 .. 7;
         when Name_Target => Path : Accepted_Name;
         when others => null;
      end case;
   end record;
   -- Syntax only. A zero byte is NullName, not an integer/discard target.
   -- The executor must reject an empty name before publishing a write.
   function Read_Target (Data : AML_Decode.Bytes) return Target_Result
   with Post => Read_Target'Result.Consumed <= Data'Length
     and then (case Read_Target'Result.Kind is
       when Local_Target =>
         Data'Length > 0 and then Data (Data'First) in 16#60# .. 16#67#
         and then Read_Target'Result.Slot = Natural (Data (Data'First) - 16#60#)
         and then Read_Target'Result.Consumed = 1,
       when Argument_Target =>
         Data'Length > 0 and then Data (Data'First) in 16#68# .. 16#6E#
         and then Read_Target'Result.Slot = Natural (Data (Data'First) - 16#68#)
         and then Read_Target'Result.Slot <= 6
         and then Read_Target'Result.Consumed = 1,
       when Name_Target =>
         Read_Target'Result.Consumed = Read_Target'Result.Path.Consumed
         and then (if Read_Target'Result.Path.Rooted then Read_Target'Result.Path.Parents = 0)
         and then (for all I in 1 .. Read_Target'Result.Path.Count =>
           AML_Names.Valid (Read_Target'Result.Path.Parts (I))),
       when others => Read_Target'Result.Consumed = 0);
end AML_Simple_Targets;
