pragma Ada_2022;

package body VMX_CR_Access with SPARK_Mode => On is

   function Decide (Qualification, Source, CR0, CR4, EFER, CR4_Allowed : Unsigned_64)
                    return Decision
   is
      Unchanged : constant Decision :=
        (Outcome => Not_Modelled, CR0 => CR0, CR4 => CR4, EFER => EFER, Flush => False);
      Fault : constant Decision := (Unchanged with delta Outcome => General_Protection);
      Kind : constant Access_Kind := Kind_Of (Qualification);
   begin
      case Number_Of (Qualification) is
         when 0 =>
            declare
               Wanted : Unsigned_64;
            begin
               case Kind is
                  when Move_To_CR =>
                     if (Source and not Low_32) /= 0 then
                        return Fault;
                     end if;
                     --  Reserved low bits are ignored; ET is hardwired to 1.
                     Wanted := (Source and CR0_Defined) or CR0_ET;
                  when Clear_TS =>
                     Wanted := CR0 and not CR0_TS;
                  when Load_MSW =>
                     Wanted := (CR0 and not MSW_Bits)
                       or (MSW_Source_Of (Qualification) and MSW_Bits)
                       or (CR0 and CR0_PE);
                  when Move_From_CR =>
                     return Unchanged;
               end case;
               Wanted := Wanted and CR0_Defined;
               if not Valid_CR0 (Wanted, CR4, EFER) then
                  return Fault;
               end if;
               return (Outcome => Applied, CR0 => Wanted, CR4 => CR4,
                       EFER => Long_Mode_Follows (Wanted, EFER),
                       Flush => ((Wanted xor CR0) and CR0_Paging_Bits) /= 0);
            end;
         when 4 =>
            if Kind /= Move_To_CR then
               return Unchanged;
            elsif (Source and CR4_VMXE) /= 0
              or else (Source and not CR4_Allowed) /= 0
              or else ((EFER and EFER_LMA) /= 0 and then (Source and CR4_PAE) = 0)
            then
               return Fault;
            end if;
            return (Outcome => Applied, CR0 => CR0, CR4 => Source, EFER => EFER,
                    Flush => ((Source xor CR4) and CR4_Paging_Bits) /= 0);
         when others =>
            return Unchanged;
      end case;
   end Decide;

end VMX_CR_Access;
