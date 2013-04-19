----------------------------------
-- unittesting of Ada-Fuse library
----------------------------------

--
-- used packages
--

with AUnit.Reporter.Text;
with AUnit.Run;
with Testcases_Suite;

--
-- main
--

procedure test_main is

   procedure Runner is new AUnit.Run.Test_Runner(Testcases_Suite.Suite);
   Reporter: AUnit.Reporter.Text.Text_Reporter;

begin

   Runner(Reporter);

end test_main;
