pragma solidity ^0.8.20;

import { Test, console2 } from "forge-std/Test.sol";

import { IBasicTestsProgram, createBasicTestsProgram } from "yol-build/Contracts.sol";


contract BasicProgramTest is Test {
  IBasicTestsProgram private _p;

  constructor () {
    _p = createBasicTestsProgram();
  }

  function testEmbeddings() external view {
    _p.embUnit$p(42);
    assertTrue(_p.embTrue$p(), "embTrue$p");
    assertTrue(_p.embTrue$l(), "embTrue$l");
  }

  function testRevetIfTrue() external {
    vm.expectRevert();
    _p.revertIfTrue(true, 42);
    assertEq(_p.revertIfTrue(false, 42), 42);
  }

  function testRangeSum() external view {
    assertEq(_p.rangeSum$p(0, 1, 10), 55, "result1");
    assertEq(_p.rangeSum$l(0, 1, 11), 66, "result1");
  }
}
