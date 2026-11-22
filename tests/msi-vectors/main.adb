--  Hosted tests for device interrupt vectors (docs/msi-interrupts.md): the
--  kernel allocator (Device_Vectors), the shared ABI words, and devmgr's
--  MSI / MSI-X programming sequence (PCI_Message_Functions) against a device
--  model. run.sh builds and runs them; --prove adds gnatprove level 2;
--  mutations.sh checks that a double allocation and a missing mask fail here.
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Device_Vector_ABI; use Device_Vector_ABI;
with Device_Vectors; use Device_Vectors;
with PCI_Message_Interrupts; use PCI_Message_Interrupts;
with PCI_Message_Functions;
with Device_Model;

procedure Main is
   package Model_Function is new PCI_Message_Functions
     (Device_Model.Write_Entry, Device_Model.Read_Config_16,
      Device_Model.Write_Config_16, Device_Model.Write_Config_32);

   subtype Block_Size is Device_Vector_ABI.Block_Size;
   subtype Vector is Device_Vector_ABI.Device_Vector;

   Failures : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   OWNER_A : constant Owner_Key := 1;
   OWNER_B : constant Owner_Key := 2;
   OWNER_C : constant Owner_Key := 3;
   CPU_DESTINATION : constant Destination_ID := 2;

   --------------------------------------------------------------------------
   --  The allocator: every vector handed out exactly once.
   --------------------------------------------------------------------------
   procedure Test_Allocation is
      T : Vector_Table := EMPTY_TABLE;
      Seen : array (Vector) of Owner_Key := [others => NO_OWNER];
      Head : Vector;
      Status : Allocation_Status;
      Outcome : Release_Status;
      Owner : Owner_Key;
      Handed_Out : Natural := 0;
      Blocks : Natural := 0;
      Sizes : constant array (1 .. 6) of Block_Size := [1, 4, 32, 2, 7, 1];
   begin
      Allocate (T, NO_OWNER, 1, Head, Status);
      Check (Status = Invalid_Owner and T = EMPTY_TABLE, "no owner refused");

      --  Mixed block sizes until the range is exhausted, owners rotating.
      --  Every round binds at least one vector, so a correct allocator is
      --  exhausted within Vector_Count rounds; one that hands vectors out
      --  twice never is.
      Allocation_Loop :
      for Round in 1 .. Vector_Count + 1 loop
         if Round > Vector_Count then
            Check (False, "allocation never exhausted the range");
            exit Allocation_Loop;
         end if;
         for Size of Sizes loop
            Owner := Owner_Key (Blocks mod 3 + 1);
            Allocate (T, Owner, Size, Head, Status);
            exit Allocation_Loop when Status /= Allocated;
            Blocks := Blocks + 1;
            for V in Head .. Head + Size - 1 loop
               Check (Seen (V) = NO_OWNER,
                      "vector" & V'Image & " handed out twice");
               Seen (V) := Owner;
               Check (Recipient (T, V) = Owner, "recipient is the owner");
               Handed_Out := Handed_Out + 1;
            end loop;
         end loop;
      end loop Allocation_Loop;
      Check (Status = Exhausted, "range exhausted");
      Check (Consistent (T), "table consistent");
      for V in Vector loop
         Check (Seen (V) = NO_OWNER or else T (V).State = Bound,
                "every handed-out vector bound");
      end loop;
      Put_Line ("  allocated" & Handed_Out'Image & " of" & Vector_Count'Image &
                " vectors in" & Blocks'Image & " blocks, none twice");

      --  Single vectors fill every remaining gap, still never twice.
      for Gap in 1 .. Vector_Count + 1 loop
         Allocate (T, OWNER_C, 1, Head, Status);
         exit when Status /= Allocated;
         Check (Gap <= Vector_Count, "gap filling never exhausted the range");
         exit when Gap > Vector_Count;
         Check (Seen (Head) = NO_OWNER, "gap vector handed out twice");
         Seen (Head) := OWNER_C;
      end loop;
      for V in Vector loop
         Check (T (V).State = Bound, "range full");
      end loop;

      --  An owner dies: its vectors stop delivering but stay reserved.
      Retire (T, OWNER_A);
      for V in Vector loop
         Check (Recipient (T, V) /= OWNER_A, "retired owner gets nothing");
         if Seen (V) = OWNER_A then
            Check (T (V).State = Retired, "retired vector reserved");
         end if;
      end loop;
      Allocate (T, OWNER_B, 1, Head, Status);
      Check (Status = Exhausted, "retired vectors are not reused");

      --  Release only by block head; then the vectors are free again.
      for V in Vector loop
         if Seen (V) = OWNER_A and then T (V).State /= Free and then T (V).Head /= V then
            Release (T, V, Outcome);
            Check (Outcome = Not_A_Block, "a non-head member is not a block");
         end if;
      end loop;
      for V in Vector loop
         if Seen (V) = OWNER_A and then T (V).State = Retired and then T (V).Head = V then
            Release (T, V, Outcome);
            Check (Outcome = Released, "retired block released");
         end if;
      end loop;
      for V in Vector loop
         Check ((Seen (V) = OWNER_A) = (T (V).State = Free),
                "exactly the retired blocks are free");
      end loop;
      for V in Vector loop
         if Seen (V) = OWNER_A then
            Release (T, V, Outcome);
            Check (Outcome = Not_A_Block, "double release refused");
         end if;
      end loop;
      Allocate (T, OWNER_B, 1, Head, Status);
      Check (Status = Allocated and then Seen (Head) = OWNER_A,
             "released vector reused only after release");
      Check (Consistent (T), "table consistent after churn");
   end Test_Allocation;

   --------------------------------------------------------------------------
   --  ABI words.
   --------------------------------------------------------------------------
   procedure Test_ABI is
      Word : Unsigned_64;
      Hit : array (Pending_Word_Index, Bit_Index) of Boolean :=
        [others => [others => False]];
   begin
      for H in Device_Vector loop
         for C in Block_Size loop
            for D in Destination_ID range 0 .. 3 loop
               Word := Allocation_Word (H, C, D);
               Check (Valid_Allocation (Word) and then Head_Of (Word) = H and then
                      Count_Of (Word) = C and then Destination_Of (Word) = D,
                      "allocation word round trip");
            end loop;
         end loop;
         Check (not Hit (Word_Of (H), Bit_Of (H)), "pending bit shared");
         Hit (Word_Of (H), Bit_Of (H)) := True;
      end loop;
      Check (not Valid_Allocation (Unsigned_64'Last), "all ones invalid");
      Check (not Valid_Allocation (Allocation_Word (First_Vector, 1, 0) - 1),
             "vector below the range invalid");
   end Test_ABI;

   --------------------------------------------------------------------------
   --  Encodings.
   --------------------------------------------------------------------------
   procedure Test_Encodings is
   begin
      Check (Message_Address (0) = 16#FEE0_0000#, "address APIC 0");
      Check (Message_Address (5) = 16#FEE0_5000#, "address APIC 5");
      Check (Message_Data (48) = 48, "data: fixed, edge, vector 48");
      Check (Table_Size (16#0010#) = 17, "table size is N-1 encoded");
      Check (Locator_BAR (16#0000_2004#) = 4 and Locator_Offset (16#0000_2004#) = 16#2000#,
             "locator BIR/offset");
      Check (Entry_Offset (3, Vector_Control) = 16#3C#, "entry 3 vector control");
      Check (MSIX_Enabled_Masked (16#0007#) = 16#C007#, "enable masked");
      Check (MSIX_Function_Unmasked (16#C007#) = 16#8007#, "function unmask");
      Check (MSI_Enabled_Single (16#00F6#) = 16#0087#, "one MSI message");
   end Test_Encodings;

   --------------------------------------------------------------------------
   --  devmgr's sequence: a device's interrupts reach their owner; nothing is
   --  lost across the masked window; a closed device never reaches the next
   --  owner of its released vectors.
   --------------------------------------------------------------------------
   procedure Test_MSIX_Lifecycle is
      use Device_Model;
      Head : Vector;
      Status : Allocation_Status;
      Outcome : Release_Status;
      COUNT : constant Block_Size := 3;
   begin
      Kernel := EMPTY_TABLE;
      Delivered := [others => 0]; Stray := 0; Hazards := 0; Wrong_Address := 0;
      Reset_Device (MSIX => True);

      Allocate (Kernel, OWNER_A, COUNT, Head, Status);
      Check (Status = Allocated, "device A vectors");
      Model_Function.Program_MSIX (Head, COUNT, CPU_DESTINATION);
      --  Interrupts raised before the driver is ready are held.
      Raise_Interrupt (0);
      Raise_Interrupt (2);
      Check (Delivered (OWNER_A) = 0 and Pending (0) and Pending (2),
             "masked interrupts held pending");
      Model_Function.Open_MSIX (COUNT);
      Check (Delivered (OWNER_A) = 2, "held interrupts delivered on open");
      Raise_Interrupt (1);
      Check (Delivered (OWNER_A) = 3, "open entry delivers");

      --  Driver A exits; devmgr closes the function, then releases.
      Retire (Kernel, OWNER_A);
      Model_Function.Close_MSIX (COUNT);
      Release (Kernel, Head, Outcome);
      Check (Outcome = Device_Vectors.Released, "A's block released");
      --  The vectors go to driver B; A's device misbehaves.
      Allocate (Kernel, OWNER_B, COUNT, Head, Status);
      Check (Status = Allocated, "device B vectors");
      for I in 0 .. COUNT - 1 loop
         Raise_Interrupt (I);
      end loop;
      Check (Delivered (OWNER_B) = 0, "closed device never reaches the next owner");
      Check (Stray = 0 and Wrong_Address = 0, "no stray or misdirected message");
      Check (Hazards = 0, "no entry written while unmasked");
   end Test_MSIX_Lifecycle;

   procedure Test_MSI_Lifecycle is
      use Device_Model;
      Head : Vector;
      Status : Allocation_Status;
      Outcome : Release_Status;
   begin
      Kernel := EMPTY_TABLE;
      Delivered := [others => 0]; Stray := 0; Hazards := 0; Wrong_Address := 0;
      Reset_Device (MSIX => False);
      Allocate (Kernel, OWNER_C, 1, Head, Status);
      Check (Status = Allocated, "MSI vector");
      Model_Function.Program_MSI (Head, CPU_DESTINATION);
      Raise_Interrupt (0);
      Check (Delivered (OWNER_C) = 1, "MSI delivers to its owner");
      --  Reprogramming (a driver restart) disables first: no hazard.
      Model_Function.Program_MSI (Head, CPU_DESTINATION);
      Check (Hazards = 0, "MSI never rewritten while enabled");
      Retire (Kernel, OWNER_C);
      Model_Function.Close_MSI;
      Release (Kernel, Head, Outcome);
      Allocate (Kernel, OWNER_B, 1, Head, Status);
      Raise_Interrupt (0);
      Check (Delivered (OWNER_B) = 0, "closed MSI never reaches the next owner");
      Check (Stray = 0 and Wrong_Address = 0, "MSI: nothing misdirected");
   end Test_MSI_Lifecycle;

begin
   Test_Allocation;
   Test_ABI;
   Test_Encodings;
   Test_MSIX_Lifecycle;
   Test_MSI_Lifecycle;
   if Failures = 0 then
      Put_Line ("MSI vectors: allocator, ABI, encodings, MSI-X and MSI lifecycles PASS");
   else
      Put_Line ("MSI vectors:" & Failures'Image & " failures");
      raise Program_Error with "msi-vectors failed";
   end if;
end Main;
