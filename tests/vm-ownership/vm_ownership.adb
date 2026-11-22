pragma Ada_2022;
package body VM_Ownership with SPARK_Mode is

   -- Lemmas ----------------------------------------------------------------

   procedure Lemma_Not_Target (F : Frame_Index) is
   begin
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Uses_Ok);
      pragma Assert (for all QE in Frame_Index => Use_Ok (QE));
      pragma Assert (Use_Ok (F));
      if Use_VM (F) /= No_VM then
         pragma Assert (Target_Ok (Use_VM (F), Use_Page (F)));
      end if;
      pragma Assert (for all V in VM_Index =>
                       (for all G in Page_Index => Target_Ok (V, G)));
   end Lemma_Not_Target;

   procedure Lemma_Free_Frame (F : Frame_Index) is
   begin
      Lemma_Not_Target (F);
      pragma Assert (Guests_Ok);
      pragma Assert (for all QE in Frame_Index => Guest_Frame_Ok (QE));
      pragma Assert (Guest_Frame_Ok (F));
      pragma Assert (Devices_Ok);
      pragma Assert (for all QE in Frame_Index => Device_Frame_Ok (QE));
      pragma Assert (Device_Frame_Ok (F));
      pragma Assert (Tables_Ok);
      pragma Assert (for all QE in Frame_Index => Table_Frame_Ok (QE));
      pragma Assert (Table_Frame_Ok (F));
      pragma Assert (Controls_Ok);
      pragma Assert (for all QE in Frame_Index => Control_Frame_Ok (QE));
      pragma Assert (Control_Frame_Ok (F));
      pragma Assert (Views_Ok);
      pragma Assert (for all QE in Frame_Index => View_Ok (QE));
      pragma Assert (View_Ok (F));
      pragma Assert (Table_Caches_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QE in Frame_Index => Table_Cache_Ok (QD, QE)));
      pragma Assert (for all C in CPU_Index => Table_Cache_Ok (C, F));
   end Lemma_Free_Frame;

   -- Theorems ------------------------------------------------------------

   procedure Leaf_Owned (V : VM_Index; G : Page_Index) is
      F : constant Frame_Index := Target (V) (G);
   begin
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (for all W in VM_Index =>
                       (for all H in Page_Index => Target_Ok (W, H)));
      pragma Assert (Target_Ok (V, G));
      pragma Assert (Pools_Ok);
      pragma Assert (for all QE in Frame_Index => Pool_Ok (QE));
      pragma Assert (Pool_Ok (F));
      pragma Assert (Tables_Ok);
      pragma Assert (for all QE in Frame_Index => Table_Frame_Ok (QE));
      pragma Assert (Table_Frame_Ok (F));
      pragma Assert (Controls_Ok);
      pragma Assert (for all QE in Frame_Index => Control_Frame_Ok (QE));
      pragma Assert (Control_Frame_Ok (F));
   end Leaf_Owned;

   procedure Leaves_Disjoint (V, W : VM_Index; G, H : Page_Index) is
   begin
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Target_Ok (V, G));
      pragma Assert (Target_Ok (W, H));
   end Leaves_Disjoint;

   procedure Tables_Private (F : Frame_Index) is
   begin
      pragma Assert (Tables_Ok);
      pragma Assert (for all QE in Frame_Index => Table_Frame_Ok (QE));
      pragma Assert (Table_Frame_Ok (F));
      pragma Assert (Controls_Ok);
      pragma Assert (for all QE in Frame_Index => Control_Frame_Ok (QE));
      pragma Assert (Control_Frame_Ok (F));
      pragma Assert (Owners (F) = Kernel);
      pragma Assert (Views_Ok);
      pragma Assert (for all QE in Frame_Index => View_Ok (QE));
      pragma Assert (View_Ok (F));
      Lemma_Not_Target (F);
   end Tables_Private;

   procedure Cached_Owned (C : CPU_Index; V : VM_Index; G : Page_Index) is
   begin
      pragma Assert (Caches_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QW in VM_Index =>
                          (for all QH in Page_Index => Cache_Ok (QD, QW, QH))));
      pragma Assert (Cache_Ok (C, V, G));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Target_Ok (V, G));
   end Cached_Owned;

   procedure Reclaimed (V : VM_Index) is
   begin
      pragma Assert (VMs_Ok);
      pragma Assert (for all QW in VM_Index => VM_Ok (QW));
      pragma Assert (VM_Ok (V));
      pragma Assert (Guests_Ok);
      pragma Assert (for all QE in Frame_Index => Guest_Frame_Ok (QE));
      pragma Assert (Devices_Ok and then Tables_Ok);
      pragma Assert (for all F in Frame_Index =>
                       Guest_Frame_Ok (F) and then Device_Frame_Ok (F)
                       and then Table_Frame_Ok (F));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (for all G in Page_Index => Target_Ok (V, G));
      pragma Assert (Uses_Ok);
      pragma Assert (for all QE in Frame_Index => Use_Ok (QE));
      pragma Assert (for all F in Frame_Index => Use_Ok (F));
      pragma Assert (for all F in Frame_Index =>
                       (if Use_VM (F) /= No_VM then
                          Target_Ok (Use_VM (F), Use_Page (F))));
      pragma Assert (VCPUs_Ok);
      pragma Assert (for all QY in VCPU_Index => VCPU_Ok (QY));
      pragma Assert (for all X in VCPU_Index => VCPU_Ok (X));
      pragma Assert (Caches_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QW in VM_Index =>
                          (for all QH in Page_Index => Cache_Ok (QD, QW, QH))));
      pragma Assert (for all C in CPU_Index =>
                       (for all G in Page_Index => Cache_Ok (C, V, G)));
   end Reclaimed;

   -- Frames outside any VM -----------------------------------------------

   procedure Alloc_Kernel (F : Frame_Index) is
   begin
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      Lemma_Free_Frame (F);
      Owners (F) := Kernel;
      pragma Assert (Pool_Ok (F) and then Guest_Frame_Ok (F) and then View_Ok (F));
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (Entries_Ok);
   end Alloc_Kernel;

   procedure Alloc_Process (F : Frame_Index) is
   begin
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      Lemma_Free_Frame (F);
      Owners (F) := Process;
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (Entries_Ok);
   end Alloc_Process;

   procedure Make_Device (F : Frame_Index) is
   begin
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      Lemma_Free_Frame (F);
      Owners (F) := Device;
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (Entries_Ok);
   end Make_Device;

   -- VMs -----------------------------------------------------------------

   procedure Create_VM (V : VM_Index; Root : Frame_Index; P : Process_Index) is
   begin
      pragma Assert (Table_Caches_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QE in Frame_Index => Table_Cache_Ok (QD, QE)));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      Lemma_Free_Frame (Root);
      pragma Assert (VMs_Ok);
      pragma Assert (for all QW in VM_Index => VM_Ok (QW));
      pragma Assert (VM_Ok (V));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (for all G in Page_Index => Target_Ok (V, G));
      Phase (V) := Live;
      Monitor (V) := P;
      Owners (Root) := Kernel;
      Table_Of (Root) := V;
      Table_Linked (Root) := True;
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (Entries_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QE in Frame_Index => Table_Cache_Ok (QD, QE)));
      pragma Assert (Table_Caches_Ok);
   end Create_VM;

   procedure Add_Table (V : VM_Index; F : Frame_Index) is
   begin
      pragma Assert (Table_Caches_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QE in Frame_Index => Table_Cache_Ok (QD, QE)));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Table_Caches_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QE in Frame_Index => Table_Cache_Ok (QD, QE)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      Lemma_Free_Frame (F);
      Owners (F) := Kernel;
      Table_Of (F) := V;
      Table_Linked (F) := True;
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (Entries_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QE in Frame_Index => Table_Cache_Ok (QD, QE)));
      pragma Assert (Table_Caches_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (Entries_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QE in Frame_Index => Table_Cache_Ok (QD, QE)));
      pragma Assert (Table_Caches_Ok);
   end Add_Table;

   procedure Unlink_Table (F : Frame_Index) is
   begin
      pragma Assert (Table_Caches_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QE in Frame_Index => Table_Cache_Ok (QD, QE)));
      pragma Assert (Table_Caches_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QE in Frame_Index => Table_Cache_Ok (QD, QE)));
      Table_Linked (F) := False;
      Table_Stamp (F) := Round (Table_Of (F));
      pragma Assert (for all QD in CPU_Index =>
                       (for all QE in Frame_Index => Table_Cache_Ok (QD, QE)));
      pragma Assert (Table_Caches_Ok);
   end Unlink_Table;

   procedure Free_Table (F : Frame_Index) is
   begin
      pragma Assert (Table_Caches_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QE in Frame_Index => Table_Cache_Ok (QD, QE)));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Tables_Ok);
      pragma Assert (for all QE in Frame_Index => Table_Frame_Ok (QE));
      pragma Assert (Table_Frame_Ok (F));
      pragma Assert (Owners (F) = Kernel);
      Lemma_Not_Target (F);
      pragma Assert (Guests_Ok);
      pragma Assert (for all QE in Frame_Index => Guest_Frame_Ok (QE));
      pragma Assert (Guest_Frame_Ok (F));
      pragma Assert (Devices_Ok);
      pragma Assert (for all QE in Frame_Index => Device_Frame_Ok (QE));
      pragma Assert (Device_Frame_Ok (F));
      pragma Assert (Views_Ok);
      pragma Assert (for all QE in Frame_Index => View_Ok (QE));
      pragma Assert (View_Ok (F));
      pragma Assert (Table_Caches_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QE in Frame_Index => Table_Cache_Ok (QD, QE)));
      pragma Assert (for all C in CPU_Index => Table_Cache_Ok (C, F));
      pragma Assert (for all C in CPU_Index => not TCached (C) (F));
      Owners (F) := Free;
      Table_Of (F) := No_VM;
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (Entries_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (Entries_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QE in Frame_Index => Table_Cache_Ok (QD, QE)));
      pragma Assert (Table_Caches_Ok);
   end Free_Table;

   procedure Alloc_Guest (V : VM_Index; F : Frame_Index) is
   begin
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      Lemma_Free_Frame (F);
      Owners (F) := Guest;
      Guest_Of (F) := V;
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (Entries_Ok);
   end Alloc_Guest;

   procedure Free_Guest (F : Frame_Index) is
   begin
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      Lemma_Not_Target (F);
      pragma Assert (Devices_Ok);
      pragma Assert (for all QE in Frame_Index => Device_Frame_Ok (QE));
      pragma Assert (Device_Frame_Ok (F));
      pragma Assert (Tables_Ok);
      pragma Assert (for all QE in Frame_Index => Table_Frame_Ok (QE));
      pragma Assert (Table_Frame_Ok (F));
      pragma Assert (Controls_Ok);
      pragma Assert (for all QE in Frame_Index => Control_Frame_Ok (QE));
      pragma Assert (Control_Frame_Ok (F));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (for all V in VM_Index =>
                       (for all G in Page_Index => Target_Ok (V, G)));
      Owners (F) := Free;
      Guest_Of (F) := No_VM;
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (Entries_Ok);
   end Free_Guest;

   procedure Grant_Device (V : VM_Index; F : Frame_Index) is
   begin
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      Device_Of (F) := V;
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (Entries_Ok);
   end Grant_Device;

   procedure Revoke_Device (F : Frame_Index) is
   begin
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Uses_Ok);
      pragma Assert (for all QE in Frame_Index => Use_Ok (QE));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (for all V in VM_Index =>
                       (for all G in Page_Index => Target_Ok (V, G)));
      pragma Assert (for all V in VM_Index =>
                       (for all G in Page_Index =>
                          (if Kind (V) (G) /= Absent then Target (V) (G) /= F)));
      Device_Of (F) := No_VM;
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (Entries_Ok);
   end Revoke_Device;

   -- EPT entries ---------------------------------------------------------

   procedure Map (V : VM_Index; G : Page_Index; F : Frame_Index) is
   begin
      pragma Assert (Caches_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QW in VM_Index =>
                          (for all QH in Page_Index => Cache_Ok (QD, QW, QH))));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Uses_Ok);
      pragma Assert (for all QE in Frame_Index => Use_Ok (QE));
      pragma Assert (for all E in Frame_Index => Use_Ok (E));
      pragma Assert (for all E in Frame_Index =>
                       (if Use_VM (E) /= No_VM then
                          Use_VM (E) /= V or else Use_Page (E) /= G));
      pragma Assert (Caches_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QW in VM_Index =>
                          (for all QH in Page_Index => Cache_Ok (QD, QW, QH))));
      pragma Assert (for all C in CPU_Index => Cache_Ok (C, V, G));
      pragma Assert (for all C in CPU_Index => not Cached (C) (V) (G));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (for all W in VM_Index =>
                       (for all H in Page_Index =>
                          (if Kind (W) (H) /= Absent then Target (W) (H) /= F)));
      Kind (V) (G) := Mapped;
      Target (V) (G) := F;
      Use_VM (F) := V;
      Use_Page (F) := G;
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (Entries_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (Entries_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QW in VM_Index =>
                          (for all QH in Page_Index => Cache_Ok (QD, QW, QH))));
      pragma Assert (Caches_Ok);
   end Map;

   procedure Unmap (V : VM_Index; G : Page_Index) is
   begin
      pragma Assert (Caches_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QW in VM_Index =>
                          (for all QH in Page_Index => Cache_Ok (QD, QW, QH))));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Caches_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QW in VM_Index =>
                          (for all QH in Page_Index => Cache_Ok (QD, QW, QH))));
      Kind (V) (G) := Stale;
      Stamp (V) (G) := Round (V);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (Entries_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QW in VM_Index =>
                          (for all QH in Page_Index => Cache_Ok (QD, QW, QH))));
      pragma Assert (Caches_Ok);
   end Unmap;

   function Clear_VM (Row : VM_Page_Flags; V : VM_Index) return VM_Page_Flags is
     ([for W in VM_Index => (if W = V then [for G in Page_Index => False] else Row (W))]);

   function Clear_Tables (Row : Frame_Flags; V : VM_Index) return Frame_Flags is
     ([for F in Frame_Index => Row (F) and then Table_Of (F) /= V]);

   procedure Complete_Round (V : VM_Index) is
      Was_On : constant CPU_Flags := Ran_On (V);
   begin
      pragma Assert (Caches_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QW in VM_Index =>
                          (for all QH in Page_Index => Cache_Ok (QD, QW, QH))));
      pragma Assert (Table_Caches_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QE in Frame_Index => Table_Cache_Ok (QD, QE)));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Caches_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QW in VM_Index =>
                          (for all QH in Page_Index => Cache_Ok (QD, QW, QH))));
      pragma Assert (Table_Caches_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QE in Frame_Index => Table_Cache_Ok (QD, QE)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Tables_Ok);
      pragma Assert (for all QE in Frame_Index => Table_Frame_Ok (QE));
      --  INVEPT / ASID flush on exactly the CPUs the VM ran on since its
      --  last round. The invariant (Cache_Ok) is what makes that enough.
      Cached := [for C in CPU_Index =>
                   (if Was_On (C) then Clear_VM (Cached (C), V) else Cached (C))];
      TCached := [for C in CPU_Index =>
                    (if Was_On (C) then Clear_Tables (TCached (C), V) else TCached (C))];
      pragma Assert (for all C in CPU_Index =>
                       (for all G in Page_Index => not Cached (C) (V) (G)));
      pragma Assert (for all C in CPU_Index =>
                       (for all F in Frame_Index =>
                          (if TCached (C) (F) then Table_Of (F) /= V)));
      Round (V) := Round (V) + 1;
      Ran_On (V) := [for C in CPU_Index =>
                       Current (C) /= No_VCPU and then VM_Of (Current (C)) = V];
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (Entries_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QE in Frame_Index => Table_Cache_Ok (QD, QE)));
      pragma Assert (Table_Caches_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QW in VM_Index =>
                          (for all QH in Page_Index => Cache_Ok (QD, QW, QH))));
      pragma Assert (Caches_Ok);
   end Complete_Round;

   procedure Retire (V : VM_Index; G : Page_Index) is
      F : constant Frame_Index := Target (V) (G);
   begin
      pragma Assert (Caches_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QW in VM_Index =>
                          (for all QH in Page_Index => Cache_Ok (QD, QW, QH))));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Target_Ok (V, G));
      pragma Assert (Domains_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Domain_Ok (V, G));
      pragma Assert (Caches_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QW in VM_Index =>
                          (for all QH in Page_Index => Cache_Ok (QD, QW, QH))));
      pragma Assert (for all C in CPU_Index => Cache_Ok (C, V, G));
      pragma Assert (for all C in CPU_Index => not Cached (C) (V) (G));
      pragma Assert (for all W in VM_Index =>
                       (for all H in Page_Index =>
                          (if Kind (W) (H) /= Absent and then (W /= V or else H /= G)
                           then Target (W) (H) /= F)));
      Kind (V) (G) := Absent;
      Use_VM (F) := No_VM;
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (Entries_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (Entries_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QW in VM_Index =>
                          (for all QH in Page_Index => Cache_Ok (QD, QW, QH))));
      pragma Assert (Caches_Ok);
   end Retire;

   -- IOMMU guest domains and monitor views --------------------------------

   procedure Map_Domain (V : VM_Index; G : Page_Index) is
   begin
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      Dom (V) (G) := True;
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (Entries_Ok);
   end Map_Domain;

   procedure Unmap_Domain (V : VM_Index; G : Page_Index) is
   begin
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      Dom (V) (G) := False;
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (Entries_Ok);
   end Unmap_Domain;

   procedure Map_View (P : Process_Index; V : VM_Index; G : Page_Index) is
   begin
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Target_Ok (V, G));
      View_By (Target (V) (G)) := P;
   end Map_View;

   procedure Unmap_View (F : Frame_Index) is
   begin
      View_By (F) := No_Process;
   end Unmap_View;

   -- vCPUs -----------------------------------------------------------------

   procedure Create_VCPU (V : VM_Index; X : VCPU_Index; F : Frame_Index) is
   begin
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      Lemma_Free_Frame (F);
      pragma Assert (VCPUs_Ok);
      pragma Assert (for all QY in VCPU_Index => VCPU_Ok (QY));
      pragma Assert (VCPU_Ok (X));
      pragma Assert (Controls_Ok);
      pragma Assert (for all QE in Frame_Index => Control_Frame_Ok (QE));
      pragma Assert (for all E in Frame_Index => Control_Frame_Ok (E));
      pragma Assert (for all E in Frame_Index => Control_Of (E) /= X);
      VM_Of (X) := V;
      Owners (F) := Kernel;
      Control_Of (F) := X;
      Tag_Stale (X) := True;
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (Entries_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (Entries_Ok);
   end Create_VCPU;

   procedure Destroy_VCPU (X : VCPU_Index; F : Frame_Index) is
   begin
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (CPUs_Ok);
      pragma Assert (for all QD in CPU_Index => CPU_Ok (QD));
      pragma Assert (for all C in CPU_Index => CPU_Ok (C));
      pragma Assert (for all C in CPU_Index => Current (C) /= X);
      pragma Assert (Controls_Ok);
      pragma Assert (for all QE in Frame_Index => Control_Frame_Ok (QE));
      pragma Assert (Control_Frame_Ok (F));
      pragma Assert (Owners (F) = Kernel);
      Lemma_Not_Target (F);
      pragma Assert (Guests_Ok);
      pragma Assert (for all QE in Frame_Index => Guest_Frame_Ok (QE));
      pragma Assert (Guest_Frame_Ok (F));
      pragma Assert (Devices_Ok);
      pragma Assert (for all QE in Frame_Index => Device_Frame_Ok (QE));
      pragma Assert (Device_Frame_Ok (F));
      pragma Assert (Tables_Ok);
      pragma Assert (for all QE in Frame_Index => Table_Frame_Ok (QE));
      pragma Assert (Table_Frame_Ok (F));
      pragma Assert (Views_Ok);
      pragma Assert (for all QE in Frame_Index => View_Ok (QE));
      pragma Assert (View_Ok (F));
      VM_Of (X) := No_VM;
      Owners (F) := Free;
      Control_Of (F) := No_VCPU;
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (Entries_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (Entries_Ok);
   end Destroy_VCPU;

   procedure Load (X : VCPU_Index; C : CPU_Index) is
      V : constant VM_Index := VM_Of (X);
   begin
      pragma Assert (Caches_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QW in VM_Index =>
                          (for all QH in Page_Index => Cache_Ok (QD, QW, QH))));
      pragma Assert (Table_Caches_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QE in Frame_Index => Table_Cache_Ok (QD, QE)));
      pragma Assert (Table_Caches_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QE in Frame_Index => Table_Cache_Ok (QD, QE)));
      Active_On (X) := C;
      Current (C) := X;
      Ran_On (V) (C) := True;
      if Last_On (X) /= C then
         Tag_Stale (X) := True;
      end if;
      Last_On (X) := C;
      pragma Assert (for all QD in CPU_Index =>
                       (for all QE in Frame_Index => Table_Cache_Ok (QD, QE)));
      pragma Assert (Table_Caches_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QE in Frame_Index => Table_Cache_Ok (QD, QE)));
      pragma Assert (Table_Caches_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QW in VM_Index =>
                          (for all QH in Page_Index => Cache_Ok (QD, QW, QH))));
      pragma Assert (Caches_Ok);
   end Load;

   procedure Clear (X : VCPU_Index) is
      C : constant CPU_Index := Active_On (X);
   begin
      pragma Assert (VCPUs_Ok);
      pragma Assert (for all QY in VCPU_Index => VCPU_Ok (QY));
      pragma Assert (VCPU_Ok (X));
      Active_On (X) := No_CPU;
      Current (C) := No_VCPU;
   end Clear;

   procedure Enter (C : CPU_Index) is
   begin
      Running (C) := True;
   end Enter;

   procedure Leave (C : CPU_Index) is
   begin
      Running (C) := False;
   end Leave;

   procedure Guest_Touch (C : CPU_Index; G : Page_Index) is
      V : constant VM_Index := VM_Of (Current (C));
   begin
      pragma Assert (Caches_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QW in VM_Index =>
                          (for all QH in Page_Index => Cache_Ok (QD, QW, QH))));
      pragma Assert (CPUs_Ok);
      pragma Assert (for all QD in CPU_Index => CPU_Ok (QD));
      pragma Assert (CPU_Ok (C));
      Cached (C) (V) (G) := True;
      pragma Assert (for all QD in CPU_Index =>
                       (for all QW in VM_Index =>
                          (for all QH in Page_Index => Cache_Ok (QD, QW, QH))));
      pragma Assert (Caches_Ok);
   end Guest_Touch;

   procedure Table_Walk (C : CPU_Index; F : Frame_Index) is
   begin
      pragma Assert (Table_Caches_Ok);
      pragma Assert (for all QD in CPU_Index =>
                       (for all QE in Frame_Index => Table_Cache_Ok (QD, QE)));
      pragma Assert (CPUs_Ok);
      pragma Assert (for all QD in CPU_Index => CPU_Ok (QD));
      pragma Assert (CPU_Ok (C));
      TCached (C) (F) := True;
      pragma Assert (for all QD in CPU_Index =>
                       (for all QE in Frame_Index => Table_Cache_Ok (QD, QE)));
      pragma Assert (Table_Caches_Ok);
   end Table_Walk;

   procedure Set_Arch_State (X : VCPU_Index; Paging_Changed : Boolean) is
   begin
      pragma Assert (CPUs_Ok);
      pragma Assert (for all QD in CPU_Index => CPU_Ok (QD));
      if Paging_Changed then
         Tag_Stale (X) := True;
      end if;
      pragma Assert (for all QD in CPU_Index => CPU_Ok (QD));
      pragma Assert (CPUs_Ok);
   end Set_Arch_State;

   procedure Flush_Tag (X : VCPU_Index) is
   begin
      pragma Assert (CPUs_Ok);
      pragma Assert (for all QD in CPU_Index => CPU_Ok (QD));
      Tag_Stale (X) := False;
      pragma Assert (for all QD in CPU_Index => CPU_Ok (QD));
      pragma Assert (CPUs_Ok);
   end Flush_Tag;

   -- Teardown ----------------------------------------------------------------

   procedure Destroy (V : VM_Index) is
   begin
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Guests_Ok);
      pragma Assert (for all QE in Frame_Index => Guest_Frame_Ok (QE));
      pragma Assert (Devices_Ok and then Tables_Ok and then Targets_Ok);
      Phase (V) := Closing;
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (Entries_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (Entries_Ok);
   end Destroy;

   procedure Finish_Destroy (V : VM_Index) is
   begin
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (VCPUs_Ok);
      pragma Assert (for all QY in VCPU_Index => VCPU_Ok (QY));
      Phase (V) := Gone;
      Monitor (V) := No_Process;
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Target_Ok (QW, QH)));
      pragma Assert (Targets_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Stamp_Ok (QW, QH)));
      pragma Assert (Stamps_Ok);
      pragma Assert (for all QW in VM_Index =>
                       (for all QH in Page_Index => Domain_Ok (QW, QH)));
      pragma Assert (Domains_Ok);
      pragma Assert (Entries_Ok);
   end Finish_Destroy;

end VM_Ownership;
