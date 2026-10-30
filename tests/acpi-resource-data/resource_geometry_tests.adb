pragma Ada_2022;
with Ada.Text_IO;
with ACPI_Resource_Data.Address_Spaces;
with ACPI_Resource_Data.Address_Spaces.Geometry;
procedure Resource_Geometry_Tests is
   package A renames ACPI_Resource_Data.Address_Spaces;
   package G renames A.Geometry;
   use type A.Address_Value;
   use type A.Address_Width;
   use type G.Geometry_Status;
   use type G.Resource_Use;
   use type G.Descriptor_Form;
   Checks : Natural:=0;
   procedure Verify(Good : Boolean; Label : String) is
   begin Checks:=Checks+1; if not Good then raise Program_Error with Label; end if; end Verify;
   -- Independent small-domain specification: integer division and exhaustive
   -- endpoint enumeration, without the implementation's masks or rounding.
   function Model (Gran, Lo, Hi, Len : Natural; Min_Fixed, Max_Fixed : Boolean;
      Purpose : G.Resource_Use) return Boolean is
      Step : constant Positive:=Gran+1;
      Power : Positive:=1;
   begin
      while Power<Step loop Power:=Power*2; end loop;
      if Power/=Step then return False; end if;
      if Len=0 then
         if Purpose/=G.Possible_Use or else (Min_Fixed and Max_Fixed) then return False; end if;
      elsif Min_Fixed and Max_Fixed then
         if Gran/=0 then return False; end if;
      elsif Min_Fixed or Max_Fixed or Purpose/=G.Possible_Use then return False;
      end if;
      for First in Lo .. Hi loop
         for Last in First .. Hi loop
            if First mod Step=0 and then (Last+1) mod Step=0
              and then (not Min_Fixed or else First=Lo)
              and then (not Max_Fixed or else Last=Hi)
              and then (Len=0 or else Last-First+1=Len) then return True; end if;
         end loop;
      end loop;
      return False;
   end Model;
   procedure Status (P : G.Parameters; Purpose : G.Resource_Use; Expected : G.Geometry_Status) is
   begin Verify(G.Check(P,Purpose).Status=Expected,"boundary status"); end Status;
   procedure Witness (P : G.Parameters; Purpose : G.Resource_Use; First, Last : A.Address_Value) is
      R : constant G.Geometry_Result:=G.Check(P,Purpose);
   begin
      Verify(R.Status=G.Feasible,"boundary feasible");
      Verify(R.First=First and then R.Last=Last,"boundary witness");
   end Witness;
begin
   Verify(G.Assess((Status=>A.Unsupported_Descriptor),G.Assigned_Use).Status=G.Not_Decoded,"non-decoded adapter");
   for Width in A.Address_Width loop
      for Gran in Natural range 0 .. 15 loop
         for Lo in Natural range 0 .. 15 loop
            for Hi in Natural range 0 .. 15 loop
               for Len in Natural range 0 .. 16 loop
                  for Min_Fixed in Boolean loop
                     for Max_Fixed in Boolean loop
                        for Purpose in G.Resource_Use loop
                           declare
                              P : constant G.Parameters:=(Width,A.Address_Value(Gran),A.Address_Value(Lo),
                                A.Address_Value(Hi),A.Address_Value(Len),Min_Fixed,Max_Fixed);
                              R : constant G.Geometry_Result:=G.Check(P,Purpose);
                              Expected : constant Boolean:=Model(Gran,Lo,Hi,Len,Min_Fixed,Max_Fixed,Purpose);
                           begin
                              Verify((R.Status=G.Feasible)=Expected,"independent enumerated feasibility");
                              if Expected then
                                 Verify(R.First>=P.Minimum and then R.Last<=P.Maximum and then R.First<=R.Last,
                                   "bounded witness");
                                 Verify(Natural(R.First) mod (Gran+1)=0 and then (Natural(R.Last)+1) mod (Gran+1)=0
                                   and then (not Min_Fixed or else R.First=P.Minimum)
                                   and then (not Max_Fixed or else R.Last=P.Maximum)
                                   and then (Len=0 or else R.Last-R.First=P.Length-1),"independent witness validation");
                                 Verify(R.Form=(if Min_Fixed and Max_Fixed then G.Fixed_Range
                                   elsif Len=0 then G.Possible_Variable_Size else G.Possible_Fixed_Size),"descriptor form");
                              end if;
                           end;
                        end loop;
                     end loop;
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
      declare
         Limit : constant A.Address_Value:=G.Width_Maximum(Width);
         P : G.Parameters:=(Width,0,Limit,Limit,1,True,True);
      begin
         Witness(P,G.Assigned_Use,Limit,Limit);
         P.Minimum:=1; P.Length:=Limit;
         Witness(P,G.Assigned_Use,1,Limit);
         P.Minimum:=0;
         Status(P,G.Assigned_Use,G.Invalid_Fixed_Length);
         P.Length:=0;
         Status(P,G.Possible_Use,G.Invalid_Combination);
         P.Minimum_Fixed:=False; P.Maximum_Fixed:=False; P.Granularity:=Limit;
         Witness(P,G.Possible_Use,0,Limit); -- Full width; no representable length needed.
         Status(P,G.Assigned_Use,G.Requires_Possible_Use);
         P.Minimum:=1;
         Status(P,G.Possible_Use,G.No_Feasible_Range);
         P.Minimum:=0; P.Maximum_Fixed:=True;
         Witness(P,G.Possible_Use,0,Limit);
         P.Minimum_Fixed:=True; P.Maximum_Fixed:=False;
         Witness(P,G.Possible_Use,0,Limit);
         P.Granularity:=1; P.Minimum:=Limit; P.Minimum_Fixed:=False;
         Status(P,G.Possible_Use,G.No_Feasible_Range);
         P.Granularity:=0; P.Length:=1;
         Witness(P,G.Possible_Use,Limit,Limit);
         if Width/=A.QWord_Width then
            P.Minimum:=Limit+1; Status(P,G.Possible_Use,G.Outside_Width);
            P.Minimum:=0; P.Maximum:=Limit+1; Status(P,G.Possible_Use,G.Outside_Width);
            P.Maximum:=Limit; P.Length:=Limit+1; Status(P,G.Possible_Use,G.Outside_Width);
            P.Length:=0; P.Granularity:=Limit+1; Status(P,G.Possible_Use,G.Outside_Width);
         end if;
      end;
   end loop;
   for Bit in Natural range 1 .. 63 loop
      declare
         Step : A.Address_Value:=1;
         P : G.Parameters:=(A.QWord_Width,0,0,A.Address_Value'Last,0,False,False);
      begin
         for I in 1 .. Bit loop Step:=Step*2; end loop;
         P.Granularity:=Step-1; P.Minimum:=Step-1;
         Witness(P,G.Possible_Use,Step,Step+(Step-1));
         P.Length:=Step;
         Witness(P,G.Possible_Use,Step,Step+(Step-1));
         P.Length:=1;
         Status(P,G.Possible_Use,G.Invalid_Length_Alignment);
         P.Length:=0; P.Minimum:=A.Address_Value'Last-(Step-2);
         Status(P,G.Possible_Use,G.No_Feasible_Range);
      end;
   end loop;
   Ada.Text_IO.Put_Line("Resource geometry checks" & Natural'Image(Checks));
end Resource_Geometry_Tests;
