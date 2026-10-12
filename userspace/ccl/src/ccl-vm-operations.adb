package body CCL.VM.Operations with SPARK_Mode => On is

   procedure Find_Operation (Immediate : Integer_64; Item : out T.Operation; Found : out Boolean) is
   begin
      Item := T.Operation'First;
      Found := False;
      for Candidate in T.Operation loop
         if Integer_64 (T.Operation'Enum_Rep (Candidate)) = Immediate then
            Item := Candidate;
            Found := True;
         end if;
      end loop;
   end Find_Operation;

   procedure Find_List_Operation (Immediate : Integer_64; Item : out L.Operation; Found : out Boolean) is
   begin
      Item := L.Operation'First;
      Found := False;
      for Candidate in L.Operation loop
         if Integer_64 (L.Operation'Enum_Rep (Candidate)) = Immediate then
            Item := Candidate;
            Found := True;
         end if;
      end loop;
   end Find_List_Operation;

   procedure Find_View (Immediate : Integer_64; Item : out CCL.Streams.View_Kind; Found : out Boolean) is
   begin
      Item := CCL.Streams.View_Kind'First;
      Found := False;
      for Candidate in CCL.Streams.View_Kind loop
         if Integer_64 (CCL.Streams.View_Kind'Enum_Rep (Candidate)) = Immediate then
            Item := Candidate;
            Found := True;
         end if;
      end loop;
   end Find_View;

   procedure Find_Apply_Operation (Immediate : Integer_64; Item : out L.Apply_Operation; Found : out Boolean) is
   begin
      Item := L.Apply_Operation'First;
      Found := False;
      for Candidate in L.Apply_Operation loop
         if Integer_64 (L.Apply_Operation'Enum_Rep (Candidate)) = Immediate then
            Item := Candidate;
            Found := True;
         end if;
      end loop;
   end Find_Apply_Operation;

   function List_Of
     (Types : CCL.Types.Registry; Result : CCL.Types.Type_Reference) return CCL.Types.Type_Reference is
      Element : constant CCL.Types.Type_Reference :=
        (if CCL.Types.Is_Range (Types, Result) then CCL.Types.Integer_Type else Result);
   begin
      for Ref in CCL.Types.Declared_Type'First .. CCL.Types.Last (Types) loop
         if CCL.Types.Is_List (Types, Ref) and then CCL.Types.Element_Of (Types, Ref) = Element then
            return Ref;
         end if;
      end loop;
      return CCL.Types.Invalid_Type;
   end List_Of;
end CCL.VM.Operations;
