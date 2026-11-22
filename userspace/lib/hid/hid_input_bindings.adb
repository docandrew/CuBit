pragma Ada_2022;
package body HID_Input_Bindings with SPARK_Mode is
   use type Schema.Status;
   use type Schema.Report_Kind;
   use type Schema.Count;
   use type Schema.Bit_Count;
   use type Schema.Word;
   use type Schema.Byte;
   use type Schema.Wide;
   use type Names.Status;
   Constant_Flag : constant Schema.Word := 1;
   Variable_Flag : constant Schema.Word := 2;
   procedure Spend (Amount : Schema.Wide; Budget : Work_Count;
                    Charged : in out Work_Count; Allowed : out Boolean) is
   begin
      Allowed := Amount <= Schema.Wide (Budget - Charged);
      if Allowed then Charged := Charged + Work_Count (Amount); end if;
   end Spend;
   procedure Ancestor
     (Layout : Schema.Result; Collections : Schema.Collection_Table;
      Start : Schema.Count; Wanted_Usage : Schema.Word; Wanted_Kind : Schema.Byte;
      Budget : Work_Count; Found : out Schema.Count;
      Charged : out Work_Count; Outcome : out Status)
   is
      Current : Schema.Count := Start;
      Candidate : Schema.Count := 0;
      C : Schema.Collection;
      Allowed : Boolean;
   begin
      Found := 0; Charged := 0; Outcome := Invalid_Schema;
      if Layout.Outcome /= Schema.Decoded then return; end if;
      Outcome := Invalid_Tables;
      if Schema.Wide (Layout.Collections_Required) > Schema.Wide (Collections'Length) then return; end if;
      while Current /= 0 loop
         if Current > Layout.Collections_Required then return; end if;
         Spend (1, Budget, Charged, Allowed);
         if not Allowed then Outcome := Budget_Exhausted; return; end if;
         C := Collections (Collections'First + Natural (Current - 1));
         if C.Parent >= Current then return; end if;
         if Candidate = 0 and then C.Kind = Wanted_Kind and then C.Usage = Wanted_Usage then Candidate := Current; end if;
         Current := C.Parent;
      end loop;
      Found := Candidate; Outcome := (if Found = 0 then Missing else Matched);
   end Ancestor;
   procedure Find
     (Layout : Schema.Result; Fields : Schema.Field_Table;
      Usages : Schema.Usage_Table; Collections : Schema.Collection_Table;
      Under_Collection : Schema.Count; Wanted_Usage : Schema.Word;
      Budget : Work_Count; Item : out Binding;
      Charged : out Work_Count; Outcome : out Status)
   is
      Candidate : Binding;
      Seen, Duplicate, Within : Boolean := False;
      Current : Schema.Count;
      F : Schema.Field_Span;
      C : Schema.Collection;
      Allowed : Boolean;
      Usage : Schema.Word;
      Name_Outcome : Names.Status;
   begin
      Item := (0, 0); Charged := 0; Outcome := Invalid_Schema;
      if Layout.Outcome /= Schema.Decoded then return; end if;
      Outcome := Invalid_Tables;
      if Under_Collection = 0 or else Under_Collection > Layout.Collections_Required or else
         Schema.Wide (Layout.Fields_Required) > Schema.Wide (Fields'Length) or else
         Schema.Wide (Layout.Usages_Required) > Schema.Wide (Usages'Length) or else
         Schema.Wide (Layout.Collections_Required) > Schema.Wide (Collections'Length) then return; end if;
      -- Validate the requested scope even when no fields refer to it.
      Current := Under_Collection;
      while Current /= 0 loop
         if Current > Layout.Collections_Required then return; end if;
         Spend (1, Budget, Charged, Allowed);
         if not Allowed then Outcome := Budget_Exhausted; return; end if;
         C := Collections (Collections'First + Natural (Current - 1));
         if C.Parent >= Current then return; end if;
         Current := C.Parent;
      end loop;
      for N in Schema.Count range 1 .. Layout.Fields_Required loop
         Spend (1, Budget, Charged, Allowed);
         if not Allowed then Outcome := Budget_Exhausted; return; end if;
         F := Fields (Fields'First + Natural (N - 1)); Current := F.Collection_ID; Within := False;
         while Current /= 0 loop
            if Current > Layout.Collections_Required then return; end if;
            Spend (1, Budget, Charged, Allowed);
         if not Allowed then Outcome := Budget_Exhausted; return; end if;
            C := Collections (Collections'First + Natural (Current - 1));
            if C.Parent >= Current then return; end if;
            if Current = Under_Collection then Within := True; end if;
            Current := C.Parent;
         end loop;
         if Within and then F.Kind = Schema.Input and then (F.Flags and Constant_Flag) = 0
            and then (F.Flags and Variable_Flag) /= 0 then
            if F.Elements = 0 then return; end if;
            for E in Schema.Bit_Count range 0 .. F.Elements - 1 loop
               Spend (1 + Schema.Wide (F.Usage_Terms), Budget, Charged, Allowed);
               if not Allowed then Outcome := Budget_Exhausted; return; end if;
               Names.Resolve (Layout, F, Usages, E, Usage, Name_Outcome);
               if Name_Outcome not in Names.Resolved | Names.No_Usage then return; end if;
               if Name_Outcome = Names.Resolved and then Usage = Wanted_Usage then
                  if Seen then Duplicate := True; else Seen := True; Candidate := (N, E); end if;
               end if;
            end loop;
         end if;
      end loop;
      if Duplicate then Outcome := Ambiguous;
      elsif Seen then Item := Candidate; Outcome := Matched;
      else Outcome := Missing; end if;
   end Find;
end HID_Input_Bindings;
