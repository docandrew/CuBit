with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_References;
with AML_Table_Backing;
procedure Container_Tests is
   use type Byte; use type Integer_Value;
   package NS is new AML_Namespace (32, AML_Delays.Unavailable_Provider);
   use NS; use NS.Owned;
   A, Foreign : Arena;
   OK : Boolean;
   Loaded_Status : Load_Status;
   Checks : Natural := 0;
   Input : aliased AML_Table_Backing.State (1, 36);
   Result : Execution_Result;
   Ref : AML_References.Reference;
   Metadata : Reference_Metadata;
   type Container_Kind is (Power, Processor, Thermal);
   function Opcode (K : Container_Kind) return Byte is
     (case K is when Power => 16#84#, when Processor => 16#83#, when Thermal => 16#85#);
   function Attributes (K : Container_Kind) return Bytes is
     (case K is when Power => [16#FF#,16#34#,16#12#],
       when Processor => [16#A5#,16#12#,16#34#,16#56#,16#78#,16#FE#],
       when Thermal => Bytes'[1 .. 0 => 0]);
   function Container (K : Container_Kind; Tail : Bytes) return Bytes is
     (Bytes'[16#5B#,Opcode (K),Byte (5 + Tail'Length),67,78,84,48] & Tail);
   function Method (Code : Bytes) return Bytes is
     (Bytes'[16#14#,Byte (6 + Code'Length),84,69,83,84,0] & Code);
   Path : constant Bytes := [16#5C#,16#2E#,67,78,84,48,84,77,80,48];
   Region_Code : constant Bytes := Bytes'[16#5B#,16#88#] & Path &
     Bytes'[16#0D#,68,83,68,84,0,16#0D#,0,16#0D#,0,16#A4#,16#8E#] & Path;
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Checks'Image; end if; end Check;
   procedure Reject (Code : Bytes; Expected : Load_Status; Width : Integer_Width) is
      Before : constant NS.State := Snapshot (A);
   begin
      Load (A, Code, Width, Loaded_Status);
      Check (Loaded_Status = Expected and then Snapshot (A) = Before);
   end Reject;
   procedure Bindings (Positive_Scope : Boolean) is
      Before : constant NS.State := Snapshot (A);
      Node : Node_ID; Bound_Status : Bind_Status;
   begin
      Bind_Table_Region (A, 1, "RGN0", (1,1), Node, Bound_Status);
      Check (Bound_Status = (if Positive_Scope then Bound else Binding_Invalid));
      if not Positive_Scope then Check (Snapshot (A) = Before); end if;
      Bind_Table_Field (A, 1, "FLD0", ((1,1),0,8), Node, Bound_Status);
      Check (Bound_Status = (if Positive_Scope then Bound else Binding_Invalid));
      if not Positive_Scope then Check (Snapshot (A) = Before); end if;
   end Bindings;
begin
   Input.Count := 1; Input.Tables (1) := (0,36);
   Input.Data (1 .. 4) := [68,83,68,84];
   Input.Data (5) := 36; Input.Data (9) := 2;
   declare Sum : Byte := 0; begin
      for B of Input.Data loop Sum := Sum + B; end loop;
      Input.Data (10) := 0 - Sum;
   end;
   for Width in Integer_Width loop
      for K in Container_Kind loop
         Reset (A, OK); Check (OK);
         Load (A, Container (K, Attributes (K)), Width, Loaded_Status);
         Check (Loaded_Status = Loaded and then Node_Count (A) = 1);
         Check (Kind (A,1) = (case K is when Power => Power_Resource_Object,
           when Processor => Processor_Object, when Thermal => Thermal_Zone_Object));
         Check (Values_Used (A).Objects = 0 and then Values_Used (A).Bytes = 0);
         if K = Power then
            Check (Power_Data (A,1).System_Level = 16#FF# and then Power_Data (A,1).Order = 16#1234#);
         elsif K = Processor then
            Check (Processor_Data (A,1) = (16#A5#,16#78563412#,16#FE#));
         end if;
         if K /= Thermal then
            for Maximum in Boolean loop
               Reset (A,OK); Check (OK);
               declare Tail : constant Bytes (1 .. Attributes (K)'Length) :=
                 [others => (if Maximum then 255 else 0)];
               begin Load (A,Container (K,Tail),Width,Loaded_Status); end;
               Check (Loaded_Status = Loaded);
               if K = Power then
                  Check (Power_Data (A,1) =
                    (if Maximum then Power_Attributes'(255,Resource_Order'Last) else Power_Attributes'(0,0)));
               else
                  Check (Processor_Data (A,1) =
                    (if Maximum then Processor_Attributes'(255,Processor_Block_Address'Last,255)
                     else Processor_Attributes'(0,0,0)));
               end if;
            end loop;
         end if;
         Make_Named_Identity (A,1,Ref,OK); Check (OK);
         Describe_Named_Identity (A,Ref,Metadata);
         Check (Metadata.Kind = Metadata_Only and then Metadata.Object_Type =
           (case K is when Power => Power_Metadata, when Processor => Processor_Metadata, when Thermal => Thermal_Metadata));
         Describe_Named_Identity (Foreign,Ref,Metadata); Check (Metadata.Kind = Invalid_Reference);
         Bindings (True);
         Reset (A,OK); Check (OK);
         Describe_Named_Identity (A,Ref,Metadata); Check (Metadata.Kind = Invalid_Reference);
         -- Every truncated fixed header stays inside its enclosing package.
         for Length in 0 .. Attributes (K)'Length - 1 loop
            Reject (Container (K, Attributes (K)(1 .. Length)) & Bytes'[0], Bad_Package, Width);
         end loop;
         Reject (Bytes'[16#5B#,Opcode (K),63,67,78,84,48], Bad_Package, Width);
         Load (A, Container (K, Attributes (K) & Bytes'[16#08#,86,65,76,48,1]), Width, Loaded_Status);
         Check (Loaded_Status = Loaded and then Parent (Snapshot (A),2) = 1);
         Load (A, Bytes'[16#10#,11,67,78,84,48,16#08#,78,69,87,48,1], Width, Loaded_Status);
         Check (Loaded_Status = Loaded and then Parent (Snapshot (A),3) = 1);
         Reject (Container (K,Attributes (K)), Duplicate_Name, Width);
         Reset (A,OK); Check (OK);
         -- Nested method resolves its parent container's integer.
         Load (A, Container (K, Attributes (K) & Bytes'[16#08#,86,65,76,48,1]
           & Method (Bytes'[16#A4#,86,65,76,48])), Width, Loaded_Status);
         Check (Loaded_Status = Loaded);
         Invoke (A,Input,3,[others => <>],0,100,Result);
         Check (Result.Status = Returned and then Result.Value = 1);
         -- Runtime Name and Method declarations may target a container, then expire.
         for Declare_Method in Boolean loop
            Reset (A,OK); Check (OK);
            declare
               Body_Code : constant Bytes :=
                 (if Declare_Method then Bytes'[16#14#,14] & Path & Bytes'[0,16#A4#,1]
                  else Bytes'[16#08#] & Path & Bytes'[1]);
            begin
               Load (A,Container (K,Attributes (K)) & Method (Body_Code & Bytes'[16#A4#] & Path),Width,Loaded_Status);
            end;
            Check (Loaded_Status = Loaded);
            Invoke (A,Input,2,[others => <>],0,200,Result);
            Check (Result.Status = Returned and then Result.Value = 1);
            Check (Child (Snapshot (A),1,"TMP0") = Root);
         end loop;
         Reset (A,OK); Check (OK);
         Load (A,Container (K,Attributes (K)) & Method (Region_Code),Width,Loaded_Status);
         Check (Loaded_Status = Loaded);
         Invoke (A,Input,2,[others => <>],0,200,Result);
         Check (Result.Status = Returned and then Result.Value = 10);
         Check (Child (Snapshot (A),1,"TMP0") = Root);
         Reset (A,OK); Check (OK);
         declare
            Code : constant Bytes := Container (K,Attributes (K));
            High : constant Bytes (Positive'Last - Code'Length + 1 .. Positive'Last) := Code;
         begin Load (A,High,Width,Loaded_Status); Check (Loaded_Status = Loaded); end;
      end loop;
      for Mutex in Boolean loop
         Reset (A,OK); Check (OK);
         declare Leaf : constant Bytes :=
           (if Mutex then Bytes'[16#5B#,1,67,78,84,48,0] else Bytes'[16#5B#,2,67,78,84,48]);
         begin
            Load (A,Leaf,Width,Loaded_Status); Check (Loaded_Status = Loaded);
            Bindings (False);
            Reject (Bytes'[16#10#,5,67,78,84,48],Missing_Scope,Width);
            Reset (A,OK); Check (OK);
            Load (A,Leaf & Method (Region_Code),Width,Loaded_Status);
            Check (Loaded_Status = Loaded);
            Invoke (A,Input,2,[others => <>],0,200,Result);
            Check (Result.Status = Unknown_Name and then Child (Snapshot (A),1,"TMP0") = Root);
            for Declare_Method in Boolean loop
               Reset (A,OK); Check (OK);
               declare Body_Code : constant Bytes :=
                 (if Declare_Method then Bytes'[16#14#,14] & Path & Bytes'[0,16#A4#,1]
                  else Bytes'[16#08#] & Path & Bytes'[1]);
               begin Load (A,Leaf & Method (Body_Code & Bytes'[16#A4#,0]),Width,Loaded_Status); end;
               Check (Loaded_Status = Loaded);
               Invoke (A,Input,2,[others => <>],0,200,Result);
               Check (Result.Status = Unknown_Name and then Child (Snapshot (A),1,"TMP0") = Root);
            end loop;
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Container checks" & Checks'Image);
end Container_Tests;
