------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  CuBit.Launch_Arguments.Launch_Failure in plain words, for the people who
--  launched a program (Desktop's launch notice, the CCL console). Kept out
--  of CuBit.Launch_Arguments: libc compiles that unit without a secondary
--  stack, and these return unconstrained strings.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Launch_Arguments; use CuBit.Launch_Arguments;

package CuBit.Launch_Failure_Text with Pure, SPARK_Mode is

   --  The reason in plain words, for a person: completes "could not start
   --  because ...".
   function Explanation (Reason : Launch_Failure) return String is
     (case Reason is
        when Malformed_Request     => "the launch request was malformed",
        when Grant_Unavailable     => "procmgr could not read the launch request",
        when Arguments_Rejected    => "its arguments were refused",
        when Spawn_Failed          => "its process could not be set up",
        when Not_Granted           => "it needs authority its launcher does not hold",
        when Executable_Not_Found  => "the program file was not found",
        when Startup_Table_Missing => "it has no startup table (rebuild it)",
        when Startup_Table_Invalid => "its startup table is invalid",
        when Manifest_Refused      => "its manifest was refused",
        when Capability_Denied     => "a capability it requires was denied",
        when Service_Unavailable   => "a service it requires is not running",
        when Render_Denied         => "GPU rendering was not admitted",
        when Out_Of_Memory         => "the system is out of memory",
        when Parameters_Missing    => "it requires typed parameters and the launch gave none",
        when Parameters_Rejected   => "its typed parameters are not of its parameter record",
        when Parameters_Unexpected => "it declares no parameters and the launch gave some",
        when Parameter_Signature_Invalid => "its parameter signature (.cubit.parameters) is malformed");

end CuBit.Launch_Failure_Text;
