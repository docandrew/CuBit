with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Build_Declarations;
with Build_Store;
with Storehouse;

--  The Ada program rule family (docs/ccl-build.md, "Granularity"): the
--  manifest, its object, one gcc -c per unit reached from main through the
--  .ali files, gnatbind -x, the binder unit and the link (D5).
package Build_Ada_Programs is
   use Ada.Strings.Unbounded;
   use Storehouse;

   type Product is record
      Path : Unbounded_String;     --  relative to the source tree's root
      Kind : Entry_Kind;
      Identity : Digest;
   end record;
   package Product_Vectors is new Ada.Containers.Vectors (Positive, Product);

   type Report is record
      Products : Product_Vectors.Vector;   --  every step's outputs, in step order
      Ran, Answered : Natural := 0;        --  steps run, steps answered by the cache
      Not_Reproducible : Natural := 0;
   end record;

   --  Builds Program from the tree at Source_Root with Profile's runtime and
   --  manifest tools, the compiler tree Compiler and the binutils tree
   --  Binutils (both already in Store). Each step is logged as it finishes.
   procedure Build
     (Store : Build_Store.Store; Source_Root : String;
      Profile : Build_Declarations.Parameters; Program : Build_Declarations.Ada_Program;
      Compiler, Binutils : Digest; Result : out Report);

end Build_Ada_Programs;
