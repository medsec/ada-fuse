--
--
--

with AUnit;
with AUnit.Test_Cases;

package Test_Setup is


   -- Testcase type used with AUnit.
   --
   type TC is new AUnit.Test_Cases.Test_Case with null record;


   -- Procedure is called one time, at the beginning of all tests.
   --
   overriding procedure Set_Up_Case(T: in out TC);


   -- Procedures are called before / after every single testcase
   --
   overriding procedure Set_Up(T: in out TC);
   overriding procedure Tear_Down(T: in out TC);


   -- Procedure is called one time, at the end of all tests.
   --
   overriding procedure Tear_Down_Case(T: in out TC);


   -- Procedure is necessary to tell AUnit which tests it should run.
   --
   procedure Register_Tests(T: in out TC);


   -- Function is used for cmd-line output to print name of testcase.
   --
   function Name(T: TC) return AUnit.Message_String;


end Test_Setup;
